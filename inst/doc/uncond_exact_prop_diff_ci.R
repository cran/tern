## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
mk_data <- function(n11, n21, n1, n2) {
  rsp <- c(rep(TRUE, n21), rep(FALSE, n2 - n21), rep(TRUE, n11), rep(FALSE, n1 - n11))
  grp <- factor(c(rep("B", n2), rep("A", n1)), levels = c("B", "A"))
  list(rsp = rsp, grp = grp)
}

## -----------------------------------------------------------------------------
n11_obs <- 40
n21_obs <- 5
n1 <- 78
n2 <- 17
dta <- mk_data(n11 = n11_obs, n21 = n21_obs, n1 = n1, n2 = n2)

example1 <- tern::prop_diff_uncond_exact(rsp = dta$rsp, grp = dta$grp, conf_level = 0.95)

example1$diff
example1$diff_ci

# Expected from SAS (only 4 digits are available):
expected_estimate1 <- 0.2187
expected_conf_int1 <- c(lower = -0.0466, upper = 0.4676)

# Compare:
all.equal(example1$diff, expected_estimate1, tol = 1e-4)
all.equal(example1$diff_ci, expected_conf_int1, tol = 1e-4)

## -----------------------------------------------------------------------------
n11_obs <- 27
n21_obs <- 3
n1 <- 57
n2 <- 3
dta <- mk_data(n11 = n11_obs, n21 = n21_obs, n1 = n1, n2 = n2)

example2 <- tern::prop_diff_uncond_exact(rsp = dta$rsp, grp = dta$grp, conf_level = 0.95)

example2$diff
example2$diff_ci

# Expected from SAS (only 4 digits are available):
expected_estimate2 <- -0.5263
expected_conf_int2 <- c(lower = -0.9057, upper = 0.1197)

# Compare:
all.equal(example2$diff, expected_estimate2, tol = 1e-4)
all.equal(example2$diff_ci, expected_conf_int2, tol = 1e-4)

## -----------------------------------------------------------------------------
n11_obs <- 27
n21_obs <- 3
n1 <- 57
n2 <- 3
dta <- mk_data(n11 = n11_obs, n21 = n21_obs, n1 = n1, n2 = n2)

example3 <- tern::prop_diff_uncond_exact(rsp = dta$rsp, grp = dta$grp, conf_level = 0.99)

example3$diff
example3$diff_ci

# Expected from SAS (only 4 digits are available):
expected_estimate3 <- -0.5263
expected_conf_int3 <- c(lower = -0.9586, upper = 0.2677)

# Compare:
all.equal(example3$diff, expected_estimate3, tol = 1e-4)
all.equal(example3$diff_ci, expected_conf_int3, tol = 1e-4)

## -----------------------------------------------------------------------------
n11_obs <- 0
n21_obs <- 2
n1 <- 2
n2 <- 2
dta <- mk_data(n11 = n11_obs, n21 = n21_obs, n1 = n1, n2 = n2)

example4 <- tern::prop_diff_uncond_exact(rsp = dta$rsp, grp = dta$grp, conf_level = 0.90)

example4$diff
example4$diff_ci

# Expected from paper (only 4 digits are available):
expected_estimate4 <- -1
expected_conf_int4 <- c(lower = -1, upper = 0.0543)

# Compare:
all.equal(example4$diff, expected_estimate4, tol = 1e-4)
all.equal(example4$diff_ci, expected_conf_int4, tol = 1e-4)

