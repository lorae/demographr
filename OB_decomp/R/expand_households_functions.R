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
      "n_children",
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
        
        # identifiers
        household_id       = row$household_id,
        subfamily_id       = row$subfamily_id,
        
        # structure flags
        is_hoh_subfamily   = row$is_hoh_subfamily,
        is_subfamily_head  = (a == 1),
        
        # age
        age                = max(row$subfam_head_age, AGE_MIN),
        
        # KOB-ready covariates
        n_children          = row$n_children,
        n_spouses           = row$n_partners,
        n_nonsf             = row$hh_size - row$subfamily_size,
        
        # household structure
        subfamily_size      = row$subfamily_size,
        hh_size             = row$hh_size,
        n_subfamilies       = row$n_subfamilies
      )
      
      adult_counter <- adult_counter + 1
    }
    
    adult_rows[[i]] <- do.call(rbind, adults)
  }
  
  do.call(rbind, adult_rows)
}
