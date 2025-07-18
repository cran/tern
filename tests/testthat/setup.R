# Extra libraries (suggested) for tests
library(dplyr)

# skip_if_too_deep
skip_if_too_deep <- function(depth) {
  checkmate::assert_number(depth, lower = 0, upper = 5)

  testing_depth <- getOption("TESTING_DEPTH")
  if (is.null(testing_depth)) testing_depth <- Sys.getenv("TESTING_DEPTH")

  testing_depth <- tryCatch(
    as.numeric(testing_depth),
    error = function(error) 3,
    warning = function(warning) 3
  )

  if (length(testing_depth) != 1 || is.na(testing_depth)) testing_depth <- 3

  if (testing_depth < depth) {
    testthat::skip(paste("testing depth", testing_depth, "is below current testing specification", depth))
  }
}

# expect_snapshot_ggplot - set custom plot dimensions
expect_snapshot_ggplot <- function(title, fig, width = NA, height = NA, no_plot_snapshots = TRUE) {
  testthat::skip_on_ci()
  testthat::skip_if_not_installed("svglite")
  testthat::skip_if(no_plot_snapshots)

  name <- paste0(title, ".svg")
  path <- tempdir()
  withr::with_options(
    opts_partial_match_old,
    suppressMessages(ggplot2::ggsave(name, fig, path = path, width = width, height = height))
  )
  path <- file.path(path, name)

  testthat::announce_snapshot_file(name = name)
  testthat::expect_snapshot_file(path, name)
}
