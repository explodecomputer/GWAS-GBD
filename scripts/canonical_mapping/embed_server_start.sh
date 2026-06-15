#!/usr/bin/env bash
# Start the BioBERT embed server in an isolated virtual environment.
#
# First run (creates the venv and installs packages):
#   bash scripts/canonical_mapping/embed_server_start.sh
#
# Subsequent runs reuse the existing venv.
#
# Options (passed through to embed_server.py):
#   --host 0.0.0.0     expose on all interfaces (for remote GPU node)
#   --port 8001        use a different port
#   --model dmis-lab/biobert-base-cased-v1.2   override model

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VENV_DIR="${SCRIPT_DIR}/.venv_embed"

# ── Create venv if missing ──────────────────────────────────────────────────
if [ ! -d "${VENV_DIR}" ]; then
    echo "Creating virtual environment: ${VENV_DIR}"
    python3 -m venv "${VENV_DIR}"
fi

# ── Install / upgrade dependencies ─────────────────────────────────────────
echo "Installing requirements..."
"${VENV_DIR}/bin/pip" install --quiet --upgrade pip
"${VENV_DIR}/bin/pip" install --quiet -r "${SCRIPT_DIR}/embed_requirements.txt"

# ── Start server ───────────────────────────────────────────────────────────
echo "Starting embed server..."
exec "${VENV_DIR}/bin/python" "${SCRIPT_DIR}/embed_server.py" "$@"
