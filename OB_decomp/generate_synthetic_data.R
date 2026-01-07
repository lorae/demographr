# ============================================================
# Synthetic household + subfamily data generator
# ============================================================

set.seed(123)

# ------------------------------------------------------------
# 1. Core utility functions
# ------------------------------------------------------------

generate_household_head_ages <- function(mean_age, n = 1, sd_age = 30) {
  ages <- integer(0)
  while (length(ages) < n) {
    draw <- round(rnorm(n, mean = mean_age, sd = sd_age))
    draw <- draw[draw >= 22 & draw <= 100]
    ages <- c(ages, draw)
  }
  ages[seq_len(n)]
}

stochastic_round <- function(x, min = 0, max = NULL) {
  lower <- floor(x)
  frac  <- x - lower
  y <- lower + rbinom(1, size = 1, prob = frac)
  y <- max(min, y)
  if (!is.null(max)) y <- min(max, y)
  y
}

linear_predict <- function(age, b0, b1, sd_e = 0) {
  e <- rnorm(1, 0, sd_e)
  b0 + b1 * age + e
}

# ------------------------------------------------------------
# 2. Subfamily component generators
# ------------------------------------------------------------

generate_children_round <- function(age, min = 0, max = NULL) {
  pred <- linear_predict(age, b0 = 2.5, b1 = -0.04, sd_e = 1.0)
  stochastic_round(pred, min, max)
}

generate_spouse_round <- function(age, min = 0, max = 1) {
  pred <- linear_predict(age, b0 = 1.22, b1 = -0.01, sd_e = 0.4)
  stochastic_round(pred, min, max)
}

# number of *additional* subfamilies
generate_nonsf_round <- function(age, min = 0, max = NULL) {
  pred <- linear_predict(age, b0 = 1.8, b1 = -0.04, sd_e = 0.4)
  stochastic_round(pred, min, max)
}

# ------------------------------------------------------------
# 3. Household generator (main logic)
# ------------------------------------------------------------

generate_households <- function(
    n_households = 1000,
    mean_hoh_age = 45
) {
  
  all_households <- list()
  household_id <- 1
  
  for (h in seq_len(n_households)) {
    
    # ---- HOH subfamily ----
    hoh_age <- generate_household_head_ages(mean_hoh_age, 1)
    
    hoh_partners <- generate_spouse_round(hoh_age)
    hoh_children <- generate_children_round(hoh_age)
    
    hoh_sf <- data.frame(
      household_id     = household_id,
      subfamily_id     = 1,
      is_hoh_subfamily = TRUE,
      subfam_head_age  = hoh_age,
      n_partners       = hoh_partners,
      n_children       = hoh_children,
      subfamily_size   = 1 + hoh_partners + hoh_children
    )
    
    sf_rows <- list(hoh_sf)
    
    # ---- Other subfamilies ----
    n_other_sf <- generate_nonsf_round(hoh_age)
    
    if (n_other_sf > 0) {
      for (s in seq_len(n_other_sf)) {
        
        other_age <- generate_household_head_ages(mean_hoh_age, 1)
        
        other_partners <- generate_spouse_round(other_age)
        other_children <- generate_children_round(other_age)
        
        sf_rows[[length(sf_rows) + 1]] <- data.frame(
          household_id     = household_id,
          subfamily_id     = s + 1,
          is_hoh_subfamily = FALSE,
          subfam_head_age  = other_age,
          n_partners       = other_partners,
          n_children       = other_children,
          subfamily_size   = 1 + other_partners + other_children
        )
      }
    }
    
    all_households[[h]] <- do.call(rbind, sf_rows)
    household_id <- household_id + 1
  }
  
  df <- do.call(rbind, all_households)
  # ---- Attach household totals ----
  hh_sizes <- aggregate(
    subfamily_size ~ household_id,
    df,
    sum
  )
  names(hh_sizes)[2] <- "hh_size"
  
  # ---- Attach number of subfamilies (including HOH) ----
  n_subfamilies <- aggregate(
    subfamily_id ~ household_id,
    df,
    length
  )
  names(n_subfamilies)[2] <- "n_subfamilies"
  
  # merge both household-level summaries onto the subfamily rows
  out <- merge(df, hh_sizes, by = "household_id")
  out <- merge(out, n_subfamilies, by = "household_id")
  out
}

# ------------------------------------------------------------
# 4. Run the generator
# ------------------------------------------------------------

synthetic_households <- generate_households(
  n_households = 10,
  mean_hoh_age = 45
)

# ------------------------------------------------------------
# 5. Quick sanity checks
# ------------------------------------------------------------

head(synthetic_households, 10)

table(synthetic_households$household_id) |> summary()

summary(synthetic_households$hh_size)

# Example: inspect one household
subset(synthetic_households, household_id == 1)
