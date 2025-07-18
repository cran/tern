testthat::test_that("or_glm estimates right OR and CI", {
  data <- data.frame(
    rsp = as.logical(c(1, 1, 0, 1, 0, 0, 1, 1, 0, 0)),
    grp = letters[c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3)],
    stringsAsFactors = TRUE
  )

  data_ab <- subset(data, grp %in% c("a", "b"))
  data_ab$grp <- droplevels(data_ab$grp)

  result <- or_glm(data_ab, conf_level = 0.95)[[1]]

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Because `rtables` works by column (compared to the reference), we verified
  # that the model fitted on the complete dataset (grp: a, b, c) provides equal
  # estimations to the model fitted to the subset group and reference (grp: a, b).
  model_fit <- stats::glm(rsp ~ grp, data, family = stats::binomial(link = "logit"))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("or_clogit estimates right OR and CI", {
  data <- data.frame(
    rsp = as.logical(c(1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0)),
    grp = letters[c(1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 1, 1, 1, 2, 2, 2, 3, 3, 2, 2)],
    strata = LETTERS[c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)],
    stringsAsFactors = TRUE
  )

  # https://github.com/therneau/survival/issues/240
  withr::with_options(
    opts_partial_match_old,
    result <- or_clogit(data, conf_level = 0.95)
  )

  # from SAS
  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_odds_ratio estimates right OR and CI (unstratified analysis)", {
  data <- data.frame(
    rsp = as.logical(c(1, 1, 0, 1, 0, 0, 1, 1, 0, 0)),
    grp = letters[c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3)]
  )

  result <- s_odds_ratio(
    df = subset(data, grp == "b"),
    .var = "rsp",
    .ref_group = subset(data, grp == "a"),
    .in_ref_col = FALSE
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_odds_ratio estimates right OR and CI (stratified analysis)", {
  set.seed(12)
  dta <- data.frame(
    rsp = sample(c(TRUE, FALSE), 100, TRUE),
    grp = factor(rep(c("A", "B"), each = 50), levels = c("B", "A")),
    strata = factor(sample(c("C", "D"), 100, TRUE))
  )

  # https://github.com/therneau/survival/issues/240
  withr::with_options(
    opts_partial_match_old,
    result <- s_odds_ratio(
      df = subset(dta, grp == "A"),
      .var = "rsp",
      .ref_group = subset(dta, grp == "B"),
      .in_ref_col = FALSE,
      .df_row = dta,
      variables = list(arm = "grp", strata = "strata")
    )
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_odds_ratio returns error for incorrect groups", {
  set.seed(2)
  data <- data.frame(
    rsp = sample(c(TRUE, FALSE), 100, TRUE),
    grp = factor(rep(c("A", "B", "C", "D"), each = 25), levels = c("A", "B", "C", "D")),
    strata = factor(sample(c("A", "B"), 100, TRUE))
  )
  groups <- list(
    "Arms A+B" = c("A", "B")
  )

  # https://github.com/therneau/survival/issues/240
  withr::with_options(
    opts_partial_match_old,
    result <- testthat::expect_error(s_odds_ratio(
      df = subset(data, grp == "A"),
      .var = "rsp",
      .ref_group = subset(data, grp == "B"),
      .in_ref_col = FALSE,
      .df_row = data,
      variables = list(arm = "grp", strata = "strata"),
      groups_list = groups
    ))
  )
})

testthat::test_that("estimate_odds_ratio estimates right OR and CI (unstratified analysis)", {
  data <- data.frame(
    rsp = as.logical(c(1, 1, 0, 1, 0, 0, 1, 1, 0, 0)),
    grp = letters[c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3)]
  )

  result <- basic_table() %>%
    split_cols_by(var = "grp", ref_group = "a") %>%
    estimate_odds_ratio(vars = "rsp") %>%
    build_table(df = data)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("estimate_odds_ratio estimates right OR and CI (stratified analysis)", {
  set.seed(12)
  data <- data.frame(
    rsp = sample(c(TRUE, FALSE), 100, TRUE),
    grp = factor(rep(c("A", "B"), each = 50), levels = c("B", "A")),
    strata = factor(sample(c("C", "D"), 100, TRUE))
  )

  # https://github.com/therneau/survival/issues/240
  withr::with_options(
    opts_partial_match_old,
    result <- basic_table() %>%
      split_cols_by(var = "grp", ref_group = "A", split_fun = ref_group_position("first")) %>%
      estimate_odds_ratio(vars = "rsp", variables = list(arm = "grp", strata = "strata")) %>%
      build_table(df = data)
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("estimate_odds_ratio works with strata and combined groups", {
  set.seed(1, kind = "Mersenne-Twister")
  anl <- data.frame(
    rsp = sample(x = c(TRUE, FALSE), size = 100, replace = TRUE),
    ARM = factor(
      sample(x = c("C: Combination", "A: Drug X", "B: Placebo"), size = 100, replace = TRUE),
      levels = c("C: Combination", "A: Drug X", "B: Placebo")
    ),
    SEX = factor(sample(x = c("D", "E"), size = 100, replace = TRUE))
  )
  groups <- combine_groups(fct = anl[["ARM"]])
  lyt <- basic_table() %>%
    split_cols_by_groups(
      var = "ARM",
      groups_list = groups,
      ref_group = names(groups)[1]
    ) %>%
    estimate_odds_ratio(
      vars = "rsp",
      variables = list(arm = "ARM", strata = "SEX"),
      conf_level = 0.95,
      table_names = "s_est_or",
      groups_list = groups
    )

  # https://github.com/therneau/survival/issues/240
  withr::with_options(
    opts_partial_match_old,
    result <- build_table(lyt = lyt, df = anl)
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_odds_ratio method argument works", {
  set.seed(1)
  nex <- 2000 # Number of example rows
  dta <- data.frame(
    "rsp" = sample(c(TRUE, FALSE), nex, TRUE),
    "grp" = sample(c("A", "B"), nex, TRUE),
    "f1" = sample(c("a1", "a2"), nex, TRUE),
    "f2" = sample(c("x", "y", "z"), nex, TRUE),
    strata = factor(sample(c("C", "D"), nex, TRUE)),
    stringsAsFactors = TRUE
  )

  # https://github.com/therneau/survival/issues/240
  withr::with_options(
    opts_partial_match_old,
    res <- s_odds_ratio(
      df = subset(dta, grp == "A"),
      .var = "rsp",
      .ref_group = subset(dta, grp == "B"),
      .in_ref_col = FALSE,
      .df_row = dta,
      variables = list(arm = "grp", strata = "strata"),
      method = "approximate"
    )
  )

  testthat::expect_false(all(is.na(res$or_ci)))

  # warning works
  expect_warning(
    s_odds_ratio(
      df = subset(dta, grp == "A"),
      .var = "rsp",
      .ref_group = subset(dta, grp == "B"),
      .in_ref_col = FALSE,
      .df_row = dta,
      variables = list(arm = "grp", strata = "strata")
    )
  )
})

testthat::test_that("estimate_odds_ratio method argument works", {
  nex <- 2000 # Number of example rows
  set.seed(12)
  dta <- data.frame(
    "rsp" = sample(c(TRUE, FALSE), nex, TRUE),
    "grp" = sample(c("A", "B"), nex, TRUE),
    "f1" = sample(c("a1", "a2"), nex, TRUE),
    "f2" = sample(c("x", "y", "z"), nex, TRUE),
    strata = factor(sample(c("C", "D"), nex, TRUE)),
    stringsAsFactors = TRUE
  )

  lyt <- basic_table() %>%
    split_cols_by(var = "grp", ref_group = "B") %>%
    estimate_odds_ratio(vars = "rsp", variables = list(arm = "grp", strata = "strata"), method = "approximate")

  # https://github.com/therneau/survival/issues/240
  withr::with_options(
    opts_partial_match_old,
    result <- build_table(lyt, df = dta)
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
