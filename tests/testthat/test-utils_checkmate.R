# assert_list_of_variables ----

testthat::test_that("assert_list_of_variables is silent with healthy input", {
  testthat::expect_silent(assert_list_of_variables(list(a = "bla", b = "bli")))
  testthat::expect_silent(assert_list_of_variables(list(a = "123")))
  testthat::expect_silent(assert_list_of_variables(list(a = c("bla", "bli"))))
})

testthat::test_that("assert_list_of_variables fails with wrong input", {
  testthat::expect_error(assert_list_of_variables(list("bla", b = "bli")))
  testthat::expect_error(assert_list_of_variables(list(a = 1, b = "bla")))
  testthat::expect_error(assert_list_of_variables(c(a = "blo", b = "bla")))
  testthat::expect_error(assert_list_of_variables(c(a = 1, a = 2)))
})

# assert_df_with_variables ----

testthat::test_that("assert_df_with_variables is silent with healthy input", {
  testthat::expect_silent(assert_df_with_variables(
    df = data.frame(a = 5, b = 3),
    variables = list(val = "a")
  ))
  testthat::expect_silent(assert_df_with_variables(
    df = data.frame(a = 5, b = 3),
    variables = list(val = c("a", "b"))
  ))
  testthat::expect_silent(assert_df_with_variables(
    df = data.frame(a = "A", b = 3),
    variables = list(vars = c("a", "b")),
    na_level = "<Missing>"
  ))
})

testthat::test_that("assert_df_with_variables fails with wrong input", {
  testthat::expect_error(assert_df_with_variables(
    df = matrix(1:6, nrow = 3, ncol = 2),
    variables = list(val = "c")
  ))
  testthat::expect_error(assert_df_with_variables(
    df = data.frame(a = 5, b = 3),
    variables = list(val = "c")
  ))
  testthat::expect_error(assert_df_with_variables(
    df = data.frame(a = 5, b = 3),
    variables = list("c")
  ))
  testthat::expect_error(assert_df_with_variables(
    df = list(a = 5, b = 3),
    variables = list(aval = "b")
  ))
  testthat::expect_error(assert_df_with_variables(
    df = data.frame(a = "A", b = "<Missing>"),
    variables = list(vars = c("a", "b")),
    na_level = "<Missing>"
  ))
})

# assert_valid_factor ----

testthat::test_that("assert_valid_factor is silent with healthy input", {
  testthat::expect_silent(assert_valid_factor(factor(c("a", "b"))))
  testthat::expect_silent(assert_valid_factor(factor(NA, exclude = factor())))
})

testthat::test_that("assert_valid_factor fails with wrong input", {
  testthat::expect_error(assert_valid_factor(c(5L, 3L)))
  testthat::expect_error(assert_valid_factor(NULL))
  testthat::expect_error(assert_valid_factor(factor(c("a", ""))))
  testthat::expect_error(assert_valid_factor(factor()))
})

# assert_df_with_factors ----

testthat::test_that("assert_df_with_factors is silent with healthy input", {
  testthat::expect_silent(assert_df_with_factors(
    df = data.frame(a = factor("A", levels = c("A", "B")), b = 3),
    variables = list(val = "a")
  ))
  testthat::expect_silent(assert_df_with_factors(
    df = data.frame(a = factor("A", levels = c("A", "B")), b = 3),
    variables = list(val = "a"),
    min.levels = 1
  ))
  testthat::expect_silent(assert_df_with_factors(
    df = data.frame(a = factor("A", levels = c("A", NA, "B")), b = 3),
    variables = list(val = "a"),
    min.levels = 2,
    max.levels = 2
  ))
  testthat::expect_silent(assert_df_with_factors(
    df = data.frame(a = factor(c("A", NA, "B")), b = 3),
    variables = list(val = "a"),
    min.levels = 2,
    max.levels = 2
  ))
})

testthat::test_that("assert_df_with_factors fails with wrong input", {
  testthat::expect_error(assert_df_with_factors(
    df = data.frame(a = 1, b = 3),
    variables = list(val = "a")
  ))
  testthat::expect_error(assert_df_with_factors(
    df = data.frame(a = 1, b = factor("x", levels = c("a", "b", "x"))),
    variables = list(val = "b"),
    min.levels = 5
  ))
  testthat::expect_error(assert_df_with_factors(
    df = data.frame(a = 1, b = factor("x", levels = c("a", "b", "x"))),
    variables = list(val = "b"),
    min.levels = 2,
    max.levels = 2
  ))
  testthat::expect_error(assert_df_with_factors(
    df = data.frame(a = 1, b = factor("x", levels = c("a", "b", "x"))),
    variables = list(val = "b"),
    min.levels = 5,
    max.levels = 3
  ))
  bdf <- data.frame(a = factor(letters[1:3]), b = factor(c(1, 2, 3)), d = 3)
  testthat::expect_error(
    assert_df_with_factors(df = bdf, variables = list(val = "a", val = "b", val = ""))
  )
  testthat::expect_error(
    assert_df_with_factors(df = bdf, variables = list(val = "a", val = "b", val = "d", val = "e"))
  )
  testthat::expect_error(
    assert_df_with_factors(df = bdf, variables = list(val = "a", val = "b", val = "e"))
  )
  testthat::expect_error(
    assert_df_with_factors(df = bdf, variables = list(val = "a", val = "b"), min.levels = 1, max.levels = 1)
  )
})

# assert_proportion_value ----

testthat::test_that("assert_proportion_value is silent with healthy input", {
  testthat::expect_silent(assert_proportion_value(0.99))
  testthat::expect_silent(assert_proportion_value(0.01))
  testthat::expect_silent(assert_proportion_value(0, include_boundaries = TRUE))
  testthat::expect_silent(assert_proportion_value(1, include_boundaries = TRUE))
})

testthat::test_that("assert_proportion_value fails with wrong input", {
  testthat::expect_error(assert_proportion_value(0))
  testthat::expect_error(assert_proportion_value(1))
  testthat::expect_error(assert_proportion_value(-1.01))
  testthat::expect_error(assert_proportion_value("abc"))
  testthat::expect_error(assert_proportion_value(c(0.4, 0.3)))
})
