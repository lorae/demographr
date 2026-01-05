# The purpose of this file is to provide examples of the results of Oaxaca-Blinder
# decompositions of household size when
# 1. Just immigrant status is involved
# 2. just age is involed
# 3. Age and immigration status are involved.

# ----- Step 0: Config ----- #
set.seed(123)

# ----- Step 1: Functions ----- #
generate_household_heads <- function(mean_age, n = 100, sd_age = 30) {
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

generate_children_exact <- function(age, b0, b1, sd_e = 0) {
  e <- rnorm(1, mean = 0, sd = sd_e)
  b0 + b1 * age + e
}

generate_children_round <- function(age, b0, b1, sd_e = 0, min = 0, max = NULL) {
  pred <- generate_children_exact(age = age, b0 = b0, b1 = b1, sd_e = sd_e)
  stochastic_round(pred, min = min, max = max)
}



# ----- Step 2: Model calibration ----- #


# Evaluate the children regression once at each integer age from 22 to 100
# and plot the implied relationship (line graph).
ages <- 22:100
children_pred <- vapply(
  ages,
  function(a) generate_children_exact(age = a, b0 = 2.5, b1 = -0.04, sd_e = 0),
  numeric(1)
)

plot(
  ages, children_pred,
  type = "l",
  xlab = "Age",
  ylab = "Predicted number of children (exact)",
  main = "Children prediction by age"
)

# Evaluate the children regression once at each integer age from 22 to 100
# and plot the implied relationship (line graph).
ages <- 22:100
children_pred <- vapply(
  ages,
  function(a) generate_children_round(age = a, b0 = 2.5, b1 = -0.04, sd_e = 0),
  numeric(1)
)

plot(
  ages, children_pred,
  type = "l",
  xlab = "Age",
  ylab = "Predicted number of children (exact)",
  main = "Children prediction by age"
)


# ----- Step 3: Create syntehtic data ----- #
# Generate synthetic "household head" ages for each year.
hoh_ages_2000 <- generate_household_heads(mean_age = 25)
hoh_ages_2019 <- generate_household_heads(mean_age = 45)
hoh_ages_2000 |> hist()
hoh_ages_2019 |> hist()