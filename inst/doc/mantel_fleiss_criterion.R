## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(tern)

## -----------------------------------------------------------------------------
set.seed(123)
n <- 80

grp <- factor(sample(c("Active", "Control"), n, replace = TRUE))
rsp <- sample(c(TRUE, FALSE), n, replace = TRUE)
strata1 <- factor(sample(c("A", "B"), n, replace = TRUE))
strata2 <- factor(sample(c("x", "y"), n, replace = TRUE))
strata <- interaction(strata1, strata2)

tbl <- table(grp, rsp, strata)
tbl

## -----------------------------------------------------------------------------
mantel_fleiss_crit(tbl)

## -----------------------------------------------------------------------------
mantel_fleiss_crit(tbl, include_value = TRUE)

## -----------------------------------------------------------------------------
mantel_fleiss_crit(tbl, threshold = 15, include_value = TRUE)

## -----------------------------------------------------------------------------
is_mf_satisfied <- mantel_fleiss_crit(tbl)

if (is_mf_satisfied) {
  # Large enough sample: use the asymptotic CMH estimate.
  prop_diff_cmh(rsp, grp, strata)$diff
} else {
  # Sparse data: fall back to the exact (unstratified) method.
  prop_diff_uncond_exact(rsp, grp)$diff
}

## -----------------------------------------------------------------------------
if (is_mf_satisfied) {
  prop_cmh(tbl)
} else {
  prop_fisher(table(grp, rsp))
}

## -----------------------------------------------------------------------------
empty_tbl <- table(
  factor(character(0), levels = c("Active", "Control")),
  factor(logical(0), levels = c("TRUE", "FALSE")),
  factor(character(0), levels = "A")
)

mantel_fleiss_crit(empty_tbl, include_value = TRUE)

