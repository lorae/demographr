source("R/config.R")
# only needed if you want to re-run component-by-age functions
source("R/generate_households_functions.R")

synthetic_households <- readRDS(SF_PATH)

# ------------------------------------------------------------
# A. Component behavior by age (single stochastic draw)
# ------------------------------------------------------------

ages <- 22:100

children_by_age <- vapply(
  ages,
  generate_children_round,
  numeric(1)
)

spouse_by_age <- vapply(
  ages,
  generate_spouse_round,
  numeric(1)
)

nonsf_by_age <- vapply(
  ages,
  generate_nonsf_round,
  numeric(1)
)

par(mfrow = c(1, 3))

plot(
  ages, children_by_age,
  type = "l",
  xlab = "Age",
  ylab = "# children",
  main = "Children by age\n(stochastic draw)"
)

plot(
  ages, spouse_by_age,
  type = "l",
  xlab = "Age",
  ylab = "Partner indicator",
  main = "Partner by age\n(stochastic draw)"
)

plot(
  ages, nonsf_by_age,
  type = "l",
  xlab = "Age",
  ylab = "# other subfamilies",
  main = "Other subfamilies by age\n(stochastic draw)"
)

par(mfrow = c(1, 1))

# ------------------------------------------------------------
# Distribution of number of subfamilies per household
# ------------------------------------------------------------

sf_counts <- unique(
  synthetic_households[, c("household_id", "n_subfamilies")]
)

hist(
  sf_counts$n_subfamilies,
  breaks = seq(0.5, max(sf_counts$n_subfamilies) + 0.5, 1),
  xlab = "Number of subfamilies",
  main = "Subfamilies per household"
)

# ------------------------------------------------------------
# Household size distribution
# ------------------------------------------------------------

hist(
  synthetic_households$hh_size,
  breaks = 30,
  xlab = "Household size",
  main = "Household size distribution"
)


# ------------------------------------------------------------
# HOH age vs number of subfamilies
# ------------------------------------------------------------

hoh_rows <- synthetic_households[
  synthetic_households$is_hoh_subfamily, ]

plot(
  hoh_rows$subfam_head_age,
  hoh_rows$n_subfamilies,
  pch = 16, cex = 0.6,
  xlab = "HOH age",
  ylab = "Number of subfamilies",
  main = "HOH age vs subfamilies per household"
)

# ------------------------------------------------------------
# HOH age vs household size
# ------------------------------------------------------------

plot(
  hoh_rows$subfam_head_age,
  hoh_rows$hh_size,
  pch = 16, cex = 0.6,
  xlab = "HOH age",
  ylab = "Household size",
  main = "HOH age vs household size"
)


# Average household size by number of subfamilies
aggregate(
  hh_size ~ n_subfamilies,
  hoh_rows,
  mean
)

# Share of households with multiple subfamilies
mean(hoh_rows$n_subfamilies > 1)

