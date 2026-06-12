"""
BioBERT embedding server for canonical mapping candidate retrieval.

Loads a biomedical sentence encoder once and serves embedding requests over
HTTP so the R workflow can call it regardless of whether the model is running
locally or on a remote GPU node.

Usage (local CPU):
    pip install -r embed_requirements.txt
    python embed_server.py

Usage (remote GPU node, accessible at <host>:<port>):
    python embed_server.py --host 0.0.0.0 --port 8000

From R, point add_embedding_candidates() at the server:
    candidates <- add_embedding_candidates(
        ...,
        embed_server_url = "http://localhost:8000"
    )
"""

import argparse
import logging
import time
from contextlib import asynccontextmanager
from typing import List

import numpy as np
import torch
import uvicorn
from fastapi import FastAPI, HTTPException
from pydantic import BaseModel
from transformers import AutoModel, AutoTokenizer

# ── Configuration ──────────────────────────────────────────────────────────

DEFAULT_MODEL = "dmis-lab/biobert-base-cased-v1.2"
DEFAULT_HOST  = "127.0.0.1"
DEFAULT_PORT  = 8000
MAX_BATCH     = 256   # maximum texts per request

logging.basicConfig(level=logging.INFO, format="%(asctime)s %(levelname)s %(message)s")
log = logging.getLogger(__name__)

# ── Model loading ──────────────────────────────────────────────────────────

_model_state: dict = {}


def _load_model(model_name: str) -> None:
    log.info("Loading tokenizer: %s", model_name)
    tokenizer = AutoTokenizer.from_pretrained(model_name)
    log.info("Loading model: %s", model_name)
    model = AutoModel.from_pretrained(model_name)
    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    model.to(device)
    model.eval()
    log.info("Model loaded on %s", device)
    _model_state.update(
        tokenizer=tokenizer, model=model, device=device, model_name=model_name
    )


def _mean_pool(token_embeddings: torch.Tensor,
               attention_mask: torch.Tensor) -> np.ndarray:
    """Mean-pool token embeddings, ignoring padding."""
    mask = attention_mask.unsqueeze(-1).expand(token_embeddings.size()).float()
    summed = torch.sum(token_embeddings * mask, dim=1)
    counts = torch.clamp(mask.sum(dim=1), min=1e-9)
    pooled = (summed / counts).cpu().numpy()
    # L2-normalise so dot product == cosine similarity
    norms = np.linalg.norm(pooled, axis=1, keepdims=True)
    norms = np.where(norms == 0, 1.0, norms)
    return (pooled / norms).astype(np.float32)


@torch.no_grad()
def _embed(texts: List[str], batch_size: int = 32) -> np.ndarray:
    tokenizer = _model_state["tokenizer"]
    model     = _model_state["model"]
    device    = _model_state["device"]

    all_embeddings = []
    for start in range(0, len(texts), batch_size):
        batch = texts[start : start + batch_size]
        encoded = tokenizer(
            batch,
            padding=True,
            truncation=True,
            max_length=512,
            return_tensors="pt",
        )
        encoded = {k: v.to(device) for k, v in encoded.items()}
        outputs = model(**encoded)
        emb = _mean_pool(outputs.last_hidden_state, encoded["attention_mask"])
        all_embeddings.append(emb)

    return np.vstack(all_embeddings)


# ── FastAPI app ────────────────────────────────────────────────────────────

@asynccontextmanager
async def lifespan(app: FastAPI):
    model_name = app.state.model_name
    _load_model(model_name)
    yield


app = FastAPI(title="BioBERT Embed Server", lifespan=lifespan)


class EmbedRequest(BaseModel):
    texts: List[str]


class EmbedResponse(BaseModel):
    embeddings: List[List[float]]
    model: str
    dim: int
    n_texts: int
    elapsed_ms: float


@app.get("/health")
def health():
    if "model" not in _model_state:
        raise HTTPException(status_code=503, detail="Model not loaded")
    return {
        "status": "ok",
        "model": _model_state["model_name"],
        "device": str(_model_state["device"]),
    }


@app.post("/embed", response_model=EmbedResponse)
def embed(request: EmbedRequest):
    if "model" not in _model_state:
        raise HTTPException(status_code=503, detail="Model not loaded")
    if len(request.texts) == 0:
        raise HTTPException(status_code=422, detail="texts must be non-empty")
    if len(request.texts) > MAX_BATCH:
        raise HTTPException(
            status_code=422,
            detail=f"Batch too large ({len(request.texts)} > {MAX_BATCH}). Split the request.",
        )

    t0  = time.perf_counter()
    emb = _embed(request.texts)
    elapsed_ms = (time.perf_counter() - t0) * 1000

    return EmbedResponse(
        embeddings  = emb.tolist(),
        model       = _model_state["model_name"],
        dim         = emb.shape[1],
        n_texts     = emb.shape[0],
        elapsed_ms  = round(elapsed_ms, 1),
    )


# ── Entry point ────────────────────────────────────────────────────────────

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="BioBERT embedding server")
    parser.add_argument("--model", default=DEFAULT_MODEL,
                        help="HuggingFace model name or local path")
    parser.add_argument("--host",  default=DEFAULT_HOST)
    parser.add_argument("--port",  default=DEFAULT_PORT, type=int)
    parser.add_argument("--batch-size", default=32, type=int,
                        help="Token-batch size for inference")
    args = parser.parse_args()

    app.state.model_name = args.model
    app.state.batch_size = args.batch_size

    log.info("Starting server: model=%s  host=%s  port=%d", args.model, args.host, args.port)
    uvicorn.run(app, host=args.host, port=args.port, log_level="warning")
