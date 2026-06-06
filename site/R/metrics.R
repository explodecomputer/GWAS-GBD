# metrics.R — pure functions only, no I/O
# All functions operate on plain numeric vectors or data frames.
# Tested in site/tests/testthat/test-metrics.R

# Normalize a numeric vector to shares summing to 1.
# Returns NA for each element when total is zero or NA.
to_share <- function(x) {
  total <- sum(x, na.rm = TRUE)
  if (is.na(total) || total <= 0) return(rep(NA_real_, length(x)))
  x / total
}

# Mismatch share: burden share minus attention share (element-wise).
mismatch_share <- function(burden_share, attention_share) {
  burden_share - attention_share
}

# Is a condition an eligible opportunity?
# Eligible = burden_share >= threshold AND burden_share > attention_share.
is_eligible <- function(burden_share, attention_share, threshold = 0.01) {
  !is.na(burden_share) &
    !is.na(attention_share) &
    burden_share >= threshold &
    burden_share > attention_share
}

# Is a condition under-attended?
# Under-attended = burden_share > attention_share (no threshold).
is_under_attended <- function(burden_share, attention_share) {
  !is.na(burden_share) & !is.na(attention_share) & burden_share > attention_share
}

# Share alignment: 1 - 0.5 * sum(|burden_share - attention_share|).
# Returns NA when either vector has no valid pairs.
share_alignment <- function(burden_share, attention_share) {
  valid <- !is.na(burden_share) & !is.na(attention_share)
  if (sum(valid) == 0) return(NA_real_)
  1 - 0.5 * sum(abs(burden_share[valid] - attention_share[valid]))
}

# Fraction of total burden carried by under-attended conditions.
under_attended_burden_share <- function(dalys, burden_share, attention_share) {
  total <- sum(dalys, na.rm = TRUE)
  if (is.na(total) || total <= 0) return(NA_real_)
  ua <- is_under_attended(burden_share, attention_share)
  sum(dalys[ua], na.rm = TRUE) / total
}

# Absolute DALYs in under-attended conditions.
under_attended_burden <- function(dalys, burden_share, attention_share) {
  ua <- is_under_attended(burden_share, attention_share)
  sum(dalys[ua], na.rm = TRUE)
}

# Absolute DALYs in zero-attention conditions.
zero_attention_burden <- function(dalys, attention_score) {
  sum(dalys[!is.na(attention_score) & attention_score == 0], na.rm = TRUE)
}

# Country-level summary from a per-condition data frame.
# Required columns: dalys, burden_share, attention_share, attention_score.
# Returns a named list.
country_summary <- function(df) {
  total_dalys <- sum(df$dalys, na.rm = TRUE)
  list(
    total_dalys             = total_dalys,
    n_conditions            = nrow(df),
    share_alignment         = share_alignment(df$burden_share, df$attention_share),
    under_attended_burden_share = under_attended_burden_share(
      df$dalys, df$burden_share, df$attention_share
    ),
    under_attended_burden   = under_attended_burden(
      df$dalys, df$burden_share, df$attention_share
    ),
    zero_attention_burden   = zero_attention_burden(df$dalys, df$attention_score),
    n_under_attended        = sum(is_under_attended(df$burden_share, df$attention_share)),
    n_eligible              = sum(is_eligible(df$burden_share, df$attention_share))
  )
}
