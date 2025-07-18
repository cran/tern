testthat::test_that("make_afun works with healthy input statistics function taking `df`", {
  sfun <- function(df) {
    checkmate::assert_data_frame(df)
    list(
      nrows = nrow(df),
      ncols = length(df)
    )
  }
  indent_mods <- c(nrows = 0L, ncols = 2L)
  formats <- c(nrows = "xx.", ncols = "xx.xx")

  afun <- testthat::expect_silent(
    make_afun(
      sfun,
      .indent_mods = indent_mods,
      .formats = formats
    )
  )

  testthat::expect_type(afun, "closure")
  testthat::expect_identical(
    names(formals(afun)),
    names(formals(sfun))
  )

  # Make sure function works with defaults.
  df <- data.frame(
    USUBJID = as.character(c(1, 1, 2, 2)),
    AVISIT = factor(c("BASELINE", "WEEK 1", "BASELINE", "WEEK 1")),
    ANRIND = factor(c("NORMAL", "LOW", "HIGH", "HIGH"))
  )
  result <- afun(df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Now call a second time to overwrite formatting.
  afun2 <- make_afun(
    afun,
    .stats = "ncols",
    .labels = c(ncols = "number columns"),
    .formats = c(ncols = "xx"),
    .indent_mods = c(ncols = 1)
  )
  result <- afun2(df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("make_afun processes additional rtables arguments correctly", {
  sfun <- function(df, .in_ref_col, .N_col) { # nolint
    checkmate::assert_data_frame(df)
    list(
      nrows = nrow(df),
      ncols = length(df),
      incol = .in_ref_col,
      nincol = .N_col
    )
  }
  indent_mods <- c(nrows = 0L, ncols = 2L, incol = 0L, nincol = 0L)
  formats <- c(nrows = "xx.", ncols = "xx.xx", incol = "xx", nincol = "xx")

  afun <- testthat::expect_silent(
    make_afun(
      sfun,
      .indent_mods = indent_mods,
      .formats = formats
    )
  )

  # Make sure the function signature is correct.
  testthat::expect_type(afun, "closure")
  testthat::expect_identical(
    names(formals(afun)),
    names(formals(sfun))
  )

  # Make sure function works with defaults.
  df <- data.frame(
    USUBJID = as.character(c(1, 1, 2, 2)),
    AVISIT = factor(c("BASELINE", "WEEK 1", "BASELINE", "WEEK 1")),
    ANRIND = factor(c("NORMAL", "LOW", "HIGH", "HIGH"))
  )
  result <- afun(df, .in_ref_col = FALSE, .N_col = 3)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("make_afun works with healthy input function taking `x`", {
  sfun <- function(x) {
    list(
      n = length(x),
      mean = mean(x),
      median = stats::median(x)
    )
  }
  indent_mods <- c(n = 0L, mean = 2L, median = 1L)
  formats <- c(n = "xx.", mean = "xx.xx", median = "xx")

  afun <- testthat::expect_silent(
    make_afun(
      sfun,
      .indent_mods = indent_mods,
      .formats = formats
    )
  )

  # Make sure the function signature is correct.
  testthat::expect_type(afun, "closure")
  testthat::expect_identical(
    names(formals(afun)),
    names(formals(sfun))
  )

  # Make sure function works with defaults.
  x <- c(1, 0, -1, 2, 5, 3, 2.5, 7.1)
  result <- afun(x)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # And with custom overwrites.
  afun2 <- make_afun(
    afun,
    .indent_mods = c(median = 3L),
    .stats = c("n", "median"),
    .formats = c(median = "xx.xx"),
    .labels = c(n = "Number of numbers")
  )
  result <- afun2(x)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("make_afun produces empty cells and keeps labels when applied to empty character", {
  # sfun which returns empty string results when `in_ref` (toy example resembling comparison problems).
  sfun <- function(x, in_ref = FALSE) {
    list(
      n = formatters::with_label(`if`(in_ref, character(), length(x)), "Number of patients"),
      mean = formatters::with_label(`if`(in_ref, character(), mean(x)), "Mean"),
      median = formatters::with_label(`if`(in_ref, character(), stats::median(x)), "Median")
    )
  }
  indent_mods <- c(n = 0L, mean = 2L, median = 1L)
  formats <- c(n = "xx.", mean = "xx.xx", median = "xx")

  afun <- testthat::expect_silent(
    make_afun(
      sfun,
      .indent_mods = indent_mods,
      .formats = formats
    )
  )

  # Make sure the function signature is correct.
  testthat::expect_type(afun, "closure")
  testthat::expect_identical(
    names(formals(afun)),
    names(formals(sfun))
  )

  # Make sure function works when not `in_ref`.
  x <- c(1, 0, -1, 2, 5, 3, 2.5, 7.1)
  result <- afun(x)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # And now with `in_ref`.
  result <- afun(x, in_ref = TRUE)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Use now in table.
  sfun <- function(x, .in_ref_col) {
    list(
      n = formatters::with_label(`if`(.in_ref_col, character(), length(x)), "Number of patients")
    )
  }
  afun <- make_afun(sfun, .null_ref_cells = FALSE)
  result <- basic_table() %>%
    split_cols_by("Species", ref_group = "setosa") %>%
    analyze("Sepal.Length", afun = afun) %>%
    build_table(iris)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("make_afun by default removes results from `.in_ref_col`", {
  sfun <- function(df, .var, .ref_group, .in_ref_col) {
    list(
      range = formatters::with_label(
        `if`(.in_ref_col, "", range(df[[.var]])),
        "Label for Range"
      )
    )
  }
  afun <- make_afun(
    sfun,
    formats = c(range = c("(xx.xx, xx.xx)")),
    indent_mods = c(range = 0L)
  )
  result <- basic_table() %>%
    split_cols_by("Species", ref_group = "setosa") %>%
    analyze("Sepal.Length", afun = afun) %>%
    build_table(iris)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("make_afun works with nested lists", {
  s_grp <- function(df, .N_col, a = 1, b = 2) { # nolint
    list(
      nrow_df = nrow(df),
      .N_col = .N_col,
      letters = list(
        a = a,
        b = b
      )
    )
  }
  a_grp <- make_afun(
    s_grp,
    b = 3,
    .labels = c(nrow_df = "row count", .N_col = "count in column"),
    .formats = c(nrow_df = "xx.", .N_col = "xx."),
    .ungroup_stats = "letters"
  )
  result <- a_grp(iris, 40)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("make_afun can subset on non-nested results when unnesting took place", {
  sfun <- function(df, .N_col, a = 1, b = 2) { # nolint
    list(
      nrow_df = nrow(df),
      .N_col = .N_col,
      letters = list(
        a = a,
        b = b
      )
    )
  }
  afun <- make_afun(
    sfun,
    .formats = c(nrow_df = "xx.", letters = "xx"),
    .indent_mods = c(.N_col = 1L, letters = 2L),
    .ungroup_stats = "letters"
  )
  afun2 <- make_afun(
    afun,
    .stats = "nrow_df",
    .formats = c(nrow_df = "xx.xx")
  )
  result <- afun2(iris, 40)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
