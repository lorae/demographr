# standardization.R
#
# WORK IN PROGRESS / EXPERIMENTAL
#
# Direct standardization of weighted means across a categorical stratifier
# (e.g. age-standardized household size by country and decade, using a single
# reference age distribution).
#
# This is a thin wrapper over `crosstab_mean()` and `crosstab_percent()`. It is
# not yet hardened, generalized, or tested. The TODOs below capture known
# limitations and the broader package-design question of how this should sit
# alongside the crosstab and (future) Kitagawa-Oaxaca-Blinder families.
#
# ============================================================================
#  TODO list — known limitations and pending work
# ============================================================================
#
# API generalization (must-do before stable release):
#   - `category` is currently a scalar string; should accept a character vector
#     so callers can do multi-dim standardization (age x race, age x education).
#     The crosstab_* primitives already handle vector group_by; the join key
#     and the `select(all_of(category))` call need to be updated, and tests
#     should cover both scalar and vector usage.
#   - Hardcoded NA-propagation behavior. Add `na_action` argument with options:
#       * "propagate" (current default): any missing (by x category) cell ->
#         std_mean is NA for that `by` group
#       * "renormalize": ref_pct re-summed to 100 over present cells; gives a
#         best-effort std_mean using only available data (transparent partial
#         coverage)
#       * "drop": na.rm = TRUE in the sumprod; silent under-weighting (mostly
#         useful as an explicit opt-in for backward compatibility)
#   - Consider `match_by` argument for per-stratum references (e.g. US-born age
#     distribution *within each decade*, rather than a single fixed reference).
#     Useful when the reference itself shifts along a `by` dimension.
#   - Consider accepting already-aggregated cell tables instead of microdata
#     (a sibling `standardize_from_cells()` for users who pre-aggregate).
#   - No standard errors. Direct standardization SEs are non-trivial (reweighted
#     binomial / replicate-weight approaches); leave for a later pass.
#
# Theoretical context — broader package design:
#   - This function is essentially a crude *Kitagawa decomposition* primitive.
#     Kitagawa decomposes the difference between two group means into a "rate"
#     component (within-category differences in the outcome) and a "composition"
#     component (different category distributions). Direct standardization
#     produces the counterfactual mean — what the comparison group's outcome
#     *would* be if it had the reference's category composition. Subtracting
#     standardized from raw isolates the composition effect; subtracting
#     standardized from the reference mean isolates the rate effect.
#   - A Kitagawa-Oaxaca-Blinder implementation already exists, but is itself
#     crude and is currently duplicated across at least three projects:
#       * american-housing-shortfalls/kob/scripts/kob-function.R
#       * household-size-demographics/kob/scripts/kob-function.R
#       * households-over-the-years/bedroom-allocation/src/kob/kob-function.R
#     Three copies of the same logic across analysis projects is the loudest
#     possible signal that this should live in demographr (or a sibling
#     package) and be imported. Consolidating it here, alongside `crosstab_*`
#     and `standardize_*`, makes sense because they are the same machinery
#     applied at different levels of theoretical abstraction:
#       * crosstab_*       — weighted aggregations across categorical strata
#       * standardize_*    — counterfactual reweighting using a reference
#                            distribution (one-step Kitagawa)
#       * kitagawa / OB    — full decomposition of between-group differences
#                            into rate and composition components
#     When the existing KOB function gets rewritten / hardened, lifting it
#     into demographr at the same time would let `standardize_mean()` become
#     a public building block of `kitagawa_decompose()` rather than a
#     parallel re-implementation.
#
# Testing — none yet. Targets for tests/testthat/test-standardization.R:
#   - Identity case: comparison and reference share a category distribution ->
#     std_mean == raw_mean (within float tolerance)
#   - Hand-computable synthetic case: 2-group x 3-bucket data with the answer
#     worked out by hand; assert equality
#   - Sum-to-100 error path: feed pre-broken reference -> expect_error()
#   - Missing cells: synthetic data with a hole in the comparison ->
#     std_mean is NA for that group; warning fires; attr "missing_cells" has
#     the expected row
#   - `include_raw`: column present when TRUE, absent when FALSE
#   - `na_action = "renormalize"` (once implemented): hand-computable case
#     where the renormalized result matches the std mean computed over only
#     the present buckets
#   - Multi-category standardization (once implemented): synthetic 2-dim
#     category with hand-computed answer
#   - Scalar vs vector `category` arg: ensure both code paths work
#
# ============================================================================


#' Direct Standardization of a Weighted Mean
#'
#' EXPERIMENTAL. Computes a directly-standardized weighted mean of `value`
#' for each combination of `by` columns in `comparison_data`, using the
#' category distribution observed in `reference_data` as the standard.
#'
#' Implementation is a wrapper over [crosstab_mean()] and [crosstab_percent()]:
#' compute reference proportions over `category`, compute weighted cell means
#' for each (`by` x `category`) cell, multiply, and sum within `by`.
#'
#' @param comparison_data A data frame or DuckDB tbl. The rows that get
#'   standardized; one standardized mean is returned per combination of `by`.
#' @param reference_data A data frame or DuckDB tbl. The rows that define the
#'   reference distribution over `category`.
#' @param value String. Outcome column whose weighted mean is to be standardized.
#' @param wt_col String. Survey weight column (e.g. `"PERWT"`).
#' @param by Character vector. Comparison group columns
#'   (e.g. `c("country", "decade")`).
#' @param category String (currently scalar; vector support pending — see TODOs).
#'   The standardization stratifier (e.g. `"age_bucket"`).
#' @param include_raw Logical. If `TRUE`, also returns the unstandardized
#'   weighted mean as `raw_mean` for side-by-side comparison.
#'
#' @return A tibble with one row per `by` combination, with columns
#'   `<by>`, `std_mean`, `n_cells`, `n_obs`, and (optionally) `raw_mean`.
#'   The returned object has an attribute `"missing_cells"` listing any
#'   (`by` x `category`) cells with no observations (zero-row tibble if none).
#'
#' @export
standardize_mean <- function(
  comparison_data,
  reference_data,
  value,
  wt_col,
  by,
  category,
  include_raw = FALSE
) {
  # 1. Reference proportions over `category`
  ref_props <- crosstab_percent(
    data = reference_data,
    wt_col = wt_col,
    group_by = category,
    percent_group_by = character(0)
  ) |>
    select(all_of(category), ref_pct = percent)

  ref_pct_sum <- sum(ref_props$ref_pct, na.rm = TRUE)
  if (!isTRUE(all.equal(ref_pct_sum, 100, tolerance = 1e-6))) {
    stop("Reference proportions do not sum to 100 (got ", ref_pct_sum, ")")
  }

  # 2. Cell means with every_combo = TRUE so missing cells appear with NA mean
  cell_means <- crosstab_mean(
    data = comparison_data,
    value = value,
    wt_col = wt_col,
    group_by = c(by, category),
    every_combo = TRUE
  )

  missing_cells <- cell_means |> filter(is.na(weighted_mean))
  if (nrow(missing_cells) > 0) {
    warning(
      "standardize_mean: ", nrow(missing_cells),
      " (by x category) cells have no observations; ",
      "std_mean will be NA for affected groups. ",
      "See attr(result, 'missing_cells') for details.",
      call. = FALSE
    )
  }

  # 3. Standardize — na.rm = FALSE so any missing cell -> NA std_mean for the group
  result <- cell_means |>
    left_join(ref_props, by = category) |>
    group_by(across(all_of(by))) |>
    summarize(
      std_mean = sum(weighted_mean * ref_pct / 100),
      n_cells  = n(),
      n_obs    = sum(count, na.rm = TRUE),
      .groups  = "drop"
    )

  # 4. Optional raw (unstandardized) mean
  if (include_raw) {
    raw <- crosstab_mean(
      data = comparison_data,
      value = value,
      wt_col = wt_col,
      group_by = by
    ) |>
      select(all_of(by), raw_mean = weighted_mean)

    result <- result |> left_join(raw, by = by)
  }

  attr(result, "missing_cells") <- missing_cells
  result
}
