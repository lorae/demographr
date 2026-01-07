# The purpose of this file is to provide examples of the results of Oaxaca-Blinder
# decompositions of household size when
# 1. Just immigrant status is involved
# 2. just age is involved
# 3. Age and immigration status are involved.

# ----- Step 0: Config ----- #
set.seed(123)

# ----- Step 1: Functions ----- #
generate_household_head_ages <- function(mean_age, n = 100, sd_age = 30) {
  ages <- integer(0)
  
  while (length(ages) < n) {
    draw <- round(rnorm(n, mean = mean_age, sd = sd_age))
    draw <- draw[draw >= 22 & draw <= 100]
    ages <- c(ages, draw)
  }
  
  ages[1:n]
}

stochastic_round <- function(x, min = 0, max = NULL) {
  lower <- floor(x)
  frac  <- x - lower
  
  # coin flip: round up with prob = frac
  y <- lower + rbinom(1, size = 1, prob = frac)
  
  # enforce bounds
  y <- max(min, y)
  if (!is.null(max)) y <- min(max, y)
  
  y
}

linear_predict <- function(age, b0, b1, sd_e = 0) {
  e <- rnorm(1, mean = 0, sd = sd_e)
  b0 + b1 * age + e
}

generate_children_exact <- function(age, b0 = 2.5, b1 = -0.04, sd_e = 0) {
  linear_predict(age, b0, b1, sd_e)
}
generate_children_round <- function(age, min = 0, max = NULL) {
  pred <- generate_children_exact(age)
  stochastic_round(pred, min = min, max = max)
}

generate_spouse_exact <- function(age, b0 = 1.22, b1 = -0.01, sd_e = 0) {
  linear_predict(age, b0, b1, sd_e)
}
generate_spouse_round <- function(age, min = 0, max = 1) {
  pred <- generate_spouse_exact(age)
  stochastic_round(pred, min = min, max = max)
}

generate_nonsf_exact <- function(age, b0 = 1.8, b1 = -0.04, sd_e = 0) {
  linear_predict(age, b0, b1, sd_e)
}
generate_nonsf_round <- function(age, min = 0, max = NULL) {
  pred <- generate_nonsf_exact(age)
  stochastic_round(pred, min = min, max = max)
}


expand_household_to_adults <- function(df) {
  rows <- vector("list", nrow(df))
  
  for (i in seq_len(nrow(df))) {
    row <- df[i, ]
    
    out <- list()
    
    # 1. Household head
    out[[1]] <- row
    
    # 2. Spouse(s): duplicate the household row
    if (row$n_spouses > 0) {
      for (s in seq_len(row$n_spouses)) {
        out[[length(out) + 1]] <- row
      }
    }
    
    # 3. Non-subfamily adults
    if (row$n_nonsf > 0) {
      for (n in seq_len(row$n_nonsf)) {
        nonsf_row <- row
        nonsf_row$n_children <- 0
        nonsf_row$n_spouses  <- 0
        nonsf_row$n_nonsf    <- row$hh_size - 1
        out[[length(out) + 1]] <- nonsf_row
      }
    }
    
    rows[[i]] <- do.call(rbind, out)
  }
  
  do.call(rbind, rows)
}


# ----- Step 2: Model calibration ----- #
# These are sanity checks of our synthetic parameters before generating full data.

ages <- 22:100 # 22 = age of majority in our model

# --- Children --- #
# Evaluate the children regression once at each integer age from 22 to 100
# and plot the implied relationship. This is without stochastic rounding.

children_pred_exact <- vapply(
  ages,
  function(a) generate_children_exact(age = a),
  numeric(1)
)

plot(
  ages, children_pred_exact,
  type = "l",
  xlab = "Age",
  ylab = "Predicted number of children (exact)",
  main = "Children prediction by age"
)

# Evaluate the children regression once at each integer age from 22 to 100
# and plot the implied relationship once stochastic rounding is applied.
children_pred_round <- vapply(
  ages,
  function(a) generate_children_round(age = a),
  numeric(1)
)

plot(
  ages, children_pred_round,
  type = "l",
  xlab = "Age",
  ylab = "Number of children (stochastic realization)",
  main = "Children by age (one stochastic draw)"
)

# --- Spouses --- #
spouse_pred_exact <- vapply(
  ages,
  function(a) generate_spouse_exact(age = a),
  numeric(1)
)

plot(
  ages, spouse_pred_exact,
  type = "l",
  xlab = "Age",
  ylab = "Predicted number of spouses (exact)",
  main = "Spouse prediction by age"
)

# Evaluate the spouse regression once at each integer age from 22 to 100
# and plot one stochastic realization after rounding to {0, 1}.
spouse_pred_round <- vapply(
  ages,
  function(a) generate_spouse_round(age = a),
  numeric(1)
)

plot(
  ages, spouse_pred_round,
  type = "l",
  xlab = "Age",
  ylab = "Spouse indicator (stochastic realization)",
  main = "Spouse by age (one stochastic draw)"
)

# --- Non-subfamily members --- #

# Evaluate the non-subfamily regression once at each integer age from 22 to 100
# and plot the implied relationship. This is without stochastic rounding.
nonsf_pred_exact <- vapply(
  ages,
  function(a) generate_nonsf_exact(age = a),
  numeric(1)
)

plot(
  ages, nonsf_pred_exact,
  type = "l",
  xlab = "Age",
  ylab = "Predicted number of non-subfamily members (exact)",
  main = "Non-subfamily members by age"
)

# Evaluate the non-subfamily regression once at each integer age from 22 to 100
# and plot one stochastic realization after rounding.
nonsf_pred_round <- vapply(
  ages,
  function(a) generate_nonsf_round(age = a),
  numeric(1)
)

plot(
  ages, nonsf_pred_round,
  type = "l",
  xlab = "Age",
  ylab = "Number of non-subfamily members (stochastic realization)",
  main = "Non-subfamily members by age (one stochastic draw)"
)


# ----- Step 3: Create synthetic data ----- #
n_obs <- 100000
# Generate synthetic "household head" ages for each year.
hoh_ages_2000 <- generate_household_head_ages(mean_age = 25, n = n_obs)
hoh_ages_2019 <- generate_household_head_ages(mean_age = 45, n = n_obs)
hoh_ages_2000 |> hist()
hoh_ages_2019 |> hist()

# Now generate n_children, n_spouses, and n_nonsf for each person using the stochastic
# rounded versions of the above function.
# Generate household components for each household head (stochastic versions)

generate_household_components <- function(ages) {
  n <- length(ages)
  
  n_children <- vapply(ages, generate_children_round, numeric(1))
  n_spouses  <- vapply(ages, generate_spouse_round,  numeric(1))
  n_nonsf    <- vapply(ages, generate_nonsf_round,    numeric(1))
  
  data.frame(
    age        = ages,
    n_children = n_children,
    n_spouses  = n_spouses,
    n_nonsf    = n_nonsf
  )
}

# Generate synthetic household data for each year
hh_2000 <- generate_household_components(hoh_ages_2000)
hh_2019 <- generate_household_components(hoh_ages_2019)

# Define total household size:
# 1 (household head) + spouses + children + non-subfamily members
hh_2000$hh_size <- 1 + hh_2000$n_spouses + hh_2000$n_children + hh_2000$n_nonsf
hh_2019$hh_size <- 1 + hh_2019$n_spouses + hh_2019$n_children + hh_2019$n_nonsf

summary(hh_2000$hh_size)
summary(hh_2019$hh_size)

hist(hh_2000$hh_size, main = "Household size (2000)", xlab = "Household size")
hist(hh_2019$hh_size, main = "Household size (2019)", xlab = "Household size")

person_2000 <- expand_household_to_adults(hh_2000)
person_2019 <- expand_household_to_adults(hh_2019)

# ----- Step 4: Apply KOB ---- #

hhsize_2000 <- person_2000$hh_size |> mean()
hhsize_2019 <- person_2019$hh_size |> mean()

age_2000 <- person_2000$age |> mean()
age_2019 <- person_2019$age |> mean()

children_2000 <- person_2000$n_children |> mean()
children_2019 <- person_2019$n_children |> mean()

hhsize_2019 - hhsize_2000

# ----- Step 4a: Component regressions by year ----- #

# Children regressions
reg_children_2000 <- lm(n_children ~ age, data = person_2000)
reg_children_2019 <- lm(n_children ~ age, data = person_2019)

# Spouse regressions
reg_spouse_2000 <- lm(n_spouses ~ age, data = person_2000)
reg_spouse_2019 <- lm(n_spouses ~ age, data = person_2019)

# Non-subfamily regressions
reg_nonsf_2000 <- lm(n_nonsf ~ age, data = person_2000)
reg_nonsf_2019 <- lm(n_nonsf ~ age, data = person_2019)


# --- Results

make_coef_table <- function(reg_2000, reg_2019,
                            year_names = c("2000", "2019")) {
  coefs_2000 <- coef(reg_2000)
  coefs_2019 <- coef(reg_2019)
  
  coef_names <- union(names(coefs_2000), names(coefs_2019))
  
  out <- cbind(
    `2000` = coefs_2000[coef_names],
    `2019` = coefs_2019[coef_names]
  )
  
  rownames(out) <- coef_names
  out
}

# Children coefficients
coef_children <- make_coef_table(
  reg_children_2000,
  reg_children_2019
)

# Spouse coefficients
coef_spouse <- make_coef_table(
  reg_spouse_2000,
  reg_spouse_2019
)

# Non-subfamily coefficients
coef_nonsf <- make_coef_table(
  reg_nonsf_2000,
  reg_nonsf_2019
)

# Children
coef_children
i_children <- coef_children["(Intercept)", "2019"] - coef_children["(Intercept)", "2000"]
e_children <- (age_2019 - age_2000)*coef_children["age", "2019"]
c_children <- (coef_children["age", "2019"] - coef_children["age", "2000"])*age_2000

i_children + c_children + e_children
children_diff <- children_2019 - children_2000
# it works!

i_children / children_diff
e_children / children_diff
c_children / children_diff

# --- Spouse decomposition --- #
coef_spouse

spouse_2000 <- person_2000$n_spouses |> mean()
spouse_2019 <- person_2019$n_spouses |> mean()

i_spouse <- coef_spouse["(Intercept)", "2019"] -
  coef_spouse["(Intercept)", "2000"]

e_spouse <- (age_2019 - age_2000) *
  coef_spouse["age", "2019"]

c_spouse <- (coef_spouse["age", "2019"] -
               coef_spouse["age", "2000"]) *
  age_2000

# Check decomposition identity
i_spouse + c_spouse + e_spouse

spouse_diff <- spouse_2019 - spouse_2000

# Shares
i_spouse / spouse_diff
e_spouse / spouse_diff
c_spouse / spouse_diff

# --- Non-subfamily decomposition --- #

nonsf_2000 <- person_2000$n_nonsf |> mean()
nonsf_2019 <- person_2019$n_nonsf |> mean()

i_nonsf <- coef_nonsf["(Intercept)", "2019"] -
  coef_nonsf["(Intercept)", "2000"]

e_nonsf <- (age_2019 - age_2000) *
  coef_nonsf["age", "2019"]

c_nonsf <- (coef_nonsf["age", "2019"] -
              coef_nonsf["age", "2000"]) *
  age_2000

# Check decomposition identity
i_nonsf + c_nonsf + e_nonsf

nonsf_diff <- nonsf_2019 - nonsf_2000

# Shares
i_nonsf / nonsf_diff
e_nonsf / nonsf_diff
c_nonsf / nonsf_diff

nonsf_diff + spouse_diff + children_diff
# --- Non-subfamily decomposition --- #

nonsf_2000 <- person_2000$n_nonsf |> mean()
nonsf_2019 <- person_2019$n_nonsf |> mean()

i_nonsf <- coef_nonsf["(Intercept)", "2019"] -
           coef_nonsf["(Intercept)", "2000"]

e_nonsf <- (age_2019 - age_2000) *
           coef_nonsf["age", "2019"]

c_nonsf <- (coef_nonsf["age", "2019"] -
            coef_nonsf["age", "2000"]) *
            age_2000

# Check decomposition identity
i_nonsf + c_nonsf + e_nonsf

nonsf_diff <- nonsf_2019 - nonsf_2000

# Shares
i_nonsf / nonsf_diff
e_nonsf / nonsf_diff
c_nonsf / nonsf_diff

# ---
spouse_diff + children_diff + nonsf_diff
hhsize_2019 - hhsize_2000
