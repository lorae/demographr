# ============================================================
# Household → adult expansion functions
# ============================================================

# Assumes config.R has already been sourced:
# - AGE_MIN (global minimum adult age, e.g. 22)

# ------------------------------------------------------------
# Expand subfamily-level data to adult-level rows
# ------------------------------------------------------------

expand_households_to_adults <- function(households_df) {
  
  stopifnot(
    all(c(
      "household_id",
      "subfamily_id",
      "is_hoh_subfamily",
      "subfam_head_age",
      "n_partners",
      "subfamily_size",
      "hh_size",
      "n_subfamilies"
    ) %in% names(households_df))
  )
  
  adult_rows <- vector("list", nrow(households_df))
  adult_counter <- 1
  
  for (i in seq_len(nrow(households_df))) {
    
    row <- households_df[i, ]
    
    n_adults <- 1 + row$n_partners
    
    # create adult rows for this subfamily
    adults <- vector("list", n_adults)
    
    for (a in seq_len(n_adults)) {
      
      adults[[a]] <- data.frame(
        adult_id           = adult_counter,
        household_id       = row$household_id,
        subfamily_id       = row$subfamily_id,
        is_hoh_subfamily   = row$is_hoh_subfamily,
        is_subfamily_head  = (a == 1),
        adult_age          = max(row$subfam_head_age, AGE_MIN),
        subfam_head_age    = row$subfam_head_age,
        subfamily_size     = row$subfamily_size,
        hh_size            = row$hh_size,
        n_subfamilies      = row$n_subfamilies
      )
      
      adult_counter <- adult_counter + 1
    }
    
    adult_rows[[i]] <- do.call(rbind, adults)
  }
  
  do.call(rbind, adult_rows)
}
