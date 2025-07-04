adrs <- tern_ex_adrs
n_records <- 20
adrs_labels <- formatters::var_labels(adrs, fill = TRUE)
adrs <- adrs %>%
  dplyr::filter(PARAMCD == "BESRSPI") %>%
  dplyr::filter(ARM %in% c("A: Drug X", "B: Placebo")) %>%
  dplyr::slice(seq_len(n_records)) %>%
  droplevels() %>%
  dplyr::mutate(rsp = AVALC == "CR")

formatters::var_labels(adrs) <- c(adrs_labels, "Response")
df <- extract_rsp_subgroups(
  variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "STRATA2")),
  data = adrs
)

testthat::test_that("g_forest default plot works", {
  tbl <- basic_table() %>%
    tabulate_rsp_subgroups(df)

  testthat::expect_silent(g_forest <- g_forest(tbl))

  expect_snapshot_ggplot("g_forest", g_forest, width = 15, height = 3)

  # Odds ratio only
  tbl_or <- basic_table() %>%
    tabulate_rsp_subgroups(df, vars = c("n_tot", "or", "ci"))

  testthat::expect_silent(g_forest_or <- g_forest(
    tbl_or,
    forest_header = c("Comparison\nBetter", "Treatment\nBetter"),
    rel_width_forest = 0.4
  ))

  expect_snapshot_ggplot("g_forest_or", g_forest_or, width = 8, height = 3)
})

testthat::test_that("g_forest works with custom arguments", {
  tbl <- rtable(
    header = rheader(
      rrow("", rcell("A", colspan = 2)),
      rrow("", "c1", "c2")
    ),
    rrow("row 1", 1, c(.8, 1.2)),
    rrow("row 2", 1.2, c(1.1, 1.4))
  )

  testthat::expect_silent(g_forest_custom_1 <- g_forest(
    tbl = tbl,
    col_x = 1,
    col_ci = 2,
    xlim = c(0.5, 2),
    x_at = c(0.5, 1, 2),
    vline = 0.9,
    forest_header = c("Hello", "World")
  ))

  expect_snapshot_ggplot("g_forest_custom_1", g_forest_custom_1, width = 4, height = 2)

  testthat::expect_silent(g_forest_custom_2 <- g_forest(
    tbl = tbl,
    col_x = 1,
    col_ci = 2,
    logx = FALSE,
    xlim = c(0.5, 1.5),
    x_at = seq(0.5, 1.5, by = 0.2),
    lbl_col_padding = -3,
    width_columns = c(4, 3, 3),
    col = "purple"
  ))

  expect_snapshot_ggplot("g_forest_custom_2", g_forest_custom_2, width = 10, height = 5)

  testthat::expect_silent(g_forest_custom_3 <- g_forest(
    tbl = tbl,
    col_x = 1,
    col_ci = 2,
    xlim = c(0.5, 2),
    x_at = c(0.5, 1, 2),
    vline = 0.9,
    forest_header = c("c1\nis\nbetter", "c2\nis\nbetter"),
    rel_width_forest = 0.6,
    font_size = 6,
    col = c("red", "green")
  ))

  expect_snapshot_ggplot("g_forest_custom_3", g_forest_custom_3, width = 10, height = 5)
})

testthat::test_that("g_forest as_list argument works", {
  tbl <- basic_table() %>%
    tabulate_rsp_subgroups(df)

  testthat::expect_silent(f <- g_forest(tbl, as_list = TRUE))
  g_forest_table_only <- f$table
  g_forest_plot_only <- f$plot

  expect_snapshot_ggplot("g_forest_table_only", g_forest_table_only, width = 9, height = 3)
  expect_snapshot_ggplot("g_forest_plot_only", g_forest_plot_only, width = 2, height = 3)
})

testthat::test_that("g_forest argument deprecation warnings work", {
  tbl <- basic_table() %>%
    tabulate_rsp_subgroups(df)

  lifecycle::expect_deprecated(
    lifecycle::expect_deprecated(
      lifecycle::expect_deprecated(
        lifecycle::expect_deprecated(
          lifecycle::expect_deprecated(
            g_forest(
              tbl,
              width_row_names = "test_warning",
              width_forest = "test_warning",
              gp = "test_warning",
              draw = "test_warning",
              newpage = "test_warning"
            )
          )
        )
      )
    )
  )
})

## Deprecated functions ----

testthat::test_that("forest_grob works", {
  tbl <- rtable(
    header = rheader(
      rrow("", "E", rcell("CI", colspan = 2), "N"),
      rrow("", "A", "B", "C", "D")
    ),
    rrow("row 1", 1, 0.8, 1.1, 16),
    rrow("row 2", 1.4, 0.8, 1.6, 25),
    rrow("row 3", 1.2, 0.8, 1.6, 36)
  )

  x <- c(1, 1.4, 1.2)
  lower <- c(0.8, 0.8, 0.8)
  upper <- c(1.1, 1.6, 1.6)
  symbol_scale <- c(1, 1.25, 1.5)

  lifecycle::expect_deprecated(
    lifecycle::expect_deprecated(
      lifecycle::expect_deprecated(
        lifecycle::expect_deprecated(
          lifecycle::expect_deprecated(
            lifecycle::expect_deprecated(
              lifecycle::expect_deprecated(
                p <- forest_grob(tbl, x, lower, upper,
                  vline = 1, forest_header = c("A", "B"),
                  x_at = c(.1, 1, 10), xlim = c(0.1, 10), logx = TRUE, symbol_size = symbol_scale,
                  vp = grid::plotViewport(margins = c(1, 1, 1, 1))
                )
              )
            )
          )
        )
      )
    )
  )
})

testthat::test_that("forest_viewport works", {
  tbl <- rtable(
    header = rheader(
      rrow("", "E", rcell("CI", colspan = 2)),
      rrow("", "A", "B", "C")
    ),
    rrow("row 1", 1, 0.8, 1.1),
    rrow("row 2", 1.4, 0.8, 1.6),
    rrow("row 3", 1.2, 0.8, 1.2)
  )

  lifecycle::expect_deprecated(lifecycle::expect_deprecated(lifecycle::expect_deprecated(v <- forest_viewport(tbl))))
})
