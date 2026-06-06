library(testthat)

# ── to_share ──────────────────────────────────────────────────────────────────

test_that("to_share sums to 1", {
  s <- to_share(c(1, 3))
  expect_equal(sum(s), 1)
})

test_that("to_share preserves proportions", {
  s <- to_share(c(1, 3))
  expect_equal(s[1], 0.25)
  expect_equal(s[2], 0.75)
})

test_that("to_share returns NA when total is zero", {
  expect_true(all(is.na(to_share(c(0, 0)))))
})

test_that("to_share handles NA in input", {
  s <- to_share(c(NA, 2, 2))
  # NA is treated as 0 by na.rm = TRUE; total = 4
  expect_false(any(is.na(s[2:3])))
})

# ── mismatch_share ────────────────────────────────────────────────────────────

test_that("mismatch_share equals burden_share minus attention_share", {
  expect_equal(mismatch_share(0.5, 0.3), 0.2)
  expect_equal(mismatch_share(0.1, 0.4), -0.3)
})

# ── is_eligible ───────────────────────────────────────────────────────────────

test_that("is_eligible true when burden_share > threshold and burden > attention", {
  expect_true(is_eligible(0.02, 0.01))
})

test_that("is_eligible true at exactly 1% threshold", {
  expect_true(is_eligible(0.01, 0.00))
})

test_that("is_eligible false just below 1% threshold", {
  expect_false(is_eligible(0.0099, 0.00))
})

test_that("is_eligible false when burden_share equals attention_share", {
  expect_false(is_eligible(0.05, 0.05))
})

test_that("is_eligible false when burden_share < attention_share", {
  expect_false(is_eligible(0.02, 0.05))
})

test_that("is_eligible true for zero-attention condition with sufficient burden", {
  expect_true(is_eligible(0.05, 0.00))
})

test_that("is_eligible vectorised", {
  result <- is_eligible(c(0.02, 0.005, 0.01), c(0.01, 0.00, 0.01))
  expect_equal(result, c(TRUE, FALSE, FALSE))
})

# ── is_under_attended ─────────────────────────────────────────────────────────

test_that("is_under_attended true when burden > attention", {
  expect_true(is_under_attended(0.1, 0.05))
})

test_that("is_under_attended false when burden equals attention", {
  expect_false(is_under_attended(0.1, 0.1))
})

# ── share_alignment ───────────────────────────────────────────────────────────

test_that("share_alignment is 1 when shares are perfectly aligned", {
  bs <- c(0.5, 0.3, 0.2)
  expect_equal(share_alignment(bs, bs), 1)
})

test_that("share_alignment is between 0 and 1", {
  bs <- c(0.9, 0.1)
  as_ <- c(0.1, 0.9)
  val <- share_alignment(bs, as_)
  expect_gte(val, 0)
  expect_lte(val, 1)
})

test_that("share_alignment returns NA when inputs are all NA", {
  expect_true(is.na(share_alignment(NA_real_, NA_real_)))
})

# ── under_attended_burden_share ───────────────────────────────────────────────

test_that("under_attended_burden_share counts only under-attended conditions", {
  # 3 conditions: 2 under-attended (dalys 100 + 200 = 300), 1 not (dalys 200)
  dalys <- c(100, 200, 200)
  bs    <- c(0.2, 0.4, 0.4)
  as_   <- c(0.1, 0.1, 0.8)  # conditions 1 & 2 are under-attended
  result <- under_attended_burden_share(dalys, bs, as_)
  expect_equal(result, 300 / 500)
})

test_that("under_attended_burden_share returns NA when total dalys is zero", {
  expect_true(is.na(under_attended_burden_share(c(0, 0), c(0.5, 0.5), c(0.3, 0.7))))
})

# ── under_attended_burden ─────────────────────────────────────────────────────

test_that("under_attended_burden sums dalys for under-attended conditions", {
  dalys <- c(100, 200, 300)
  bs    <- c(0.5, 0.3, 0.2)
  as_   <- c(0.2, 0.4, 0.4)   # only condition 1 is under-attended
  expect_equal(under_attended_burden(dalys, bs, as_), 100)
})

# ── zero_attention_burden ─────────────────────────────────────────────────────

test_that("zero_attention_burden sums dalys where attention_score is zero", {
  dalys  <- c(100, 200, 300)
  scores <- c(0, 500, 0)
  expect_equal(zero_attention_burden(dalys, scores), 400)
})

test_that("zero_attention_burden is zero when no zero-attention conditions", {
  expect_equal(zero_attention_burden(c(100, 200), c(10, 20)), 0)
})

# ── country_summary ───────────────────────────────────────────────────────────

test_that("country_summary total_dalys matches sum of dalys", {
  df <- data.frame(
    dalys           = c(100, 200, 300),
    burden_share    = c(1/6, 2/6, 3/6),
    attention_share = c(0, 0, 1),
    attention_score = c(0, 0, 100)
  )
  s <- country_summary(df)
  expect_equal(s$total_dalys, 600)
})

test_that("country_summary n_conditions counts all rows", {
  df <- data.frame(
    dalys = c(1, 2, 3), burden_share = c(.2, .3, .5),
    attention_share = c(0, 0, 1), attention_score = c(0, 0, 100)
  )
  expect_equal(country_summary(df)$n_conditions, 3)
})

test_that("country_summary handles all-zero attention gracefully", {
  df <- data.frame(
    dalys           = c(100, 200),
    burden_share    = c(0.33, 0.67),
    attention_share = c(0, 0),
    attention_score = c(0, 0)
  )
  s <- country_summary(df)
  expect_false(is.nan(s$share_alignment))
  expect_equal(s$zero_attention_burden, 300)
})
