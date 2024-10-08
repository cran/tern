#' Add titles, footnotes, page Number, and a bounding box to a grid grob
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This function is useful to label grid grobs (also `ggplot2`, and `lattice` plots)
#' with title, footnote, and page numbers.
#'
#' @inheritParams grid::grob
#' @param grob (`grob`)\cr a grid grob object, optionally `NULL` if only a `grob` with the decoration should be shown.
#' @param titles (`character`)\cr titles given as a vector of strings that are each separated by a newline and wrapped
#'   according to the page width.
#' @param footnotes (`character`)\cr footnotes. Uses the same formatting rules as `titles`.
#' @param page (`string` or `NULL`)\cr page numeration. If `NULL` then no page number is displayed.
#' @param width_titles (`grid::unit`)\cr width of titles. Usually defined as all the available space
#'   `grid::unit(1, "npc")`, it is affected by the parameter `outer_margins`. Right margins (`outer_margins[4]`)
#'   need to be subtracted to the allowed width.
#' @param width_footnotes (`grid::unit`)\cr width of footnotes. Same default and margin correction as `width_titles`.
#' @param border (`flag`)\cr whether a border should be drawn around the plot or not.
#' @param padding (`grid::unit`)\cr padding. A unit object of length 4. Innermost margin between the plot (`grob`)
#'   and, possibly, the border of the plot. Usually expressed in 4 identical values (usually `"lines"`). It defaults
#'   to `grid::unit(rep(1, 4), "lines")`.
#' @param margins (`grid::unit`)\cr margins. A unit object of length 4. Margins between the plot and the other
#'   elements in the list (e.g. titles, plot, and footers). This is usually expressed in 4 `"lines"`, where the
#'   lateral ones are 0s, while top and bottom are 1s. It defaults to `grid::unit(c(1, 0, 1, 0), "lines")`.
#' @param outer_margins (`grid::unit`)\cr outer margins. A unit object of length 4. It defines the general margin of
#'   the plot, considering also decorations like titles, footnotes, and page numbers. It defaults to
#'   `grid::unit(c(2, 1.5, 3, 1.5), "cm")`.
#' @param gp_titles (`gpar`)\cr a `gpar` object. Mainly used to set different `"fontsize"`.
#' @param gp_footnotes (`gpar`)\cr a `gpar` object. Mainly used to set different `"fontsize"`.
#'
#' @return A grid grob (`gTree`).
#'
#' @details The titles and footnotes will be ragged, i.e. each title will be wrapped individually.
#'
#' @examples
#' library(grid)
#'
#' titles <- c(
#'   "Edgar Anderson's Iris Data",
#'   paste(
#'     "This famous (Fisher's or Anderson's) iris data set gives the measurements",
#'     "in centimeters of the variables sepal length and width and petal length",
#'     "and width, respectively, for 50 flowers from each of 3 species of iris."
#'   )
#' )
#'
#' footnotes <- c(
#'   "The species are Iris setosa, versicolor, and virginica.",
#'   paste(
#'     "iris is a data frame with 150 cases (rows) and 5 variables (columns) named",
#'     "Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, and Species."
#'   )
#' )
#'
#' ## empty plot
#' grid.newpage()
#'
#' grid.draw(
#'   decorate_grob(
#'     NULL,
#'     titles = titles,
#'     footnotes = footnotes,
#'     page = "Page 4 of 10"
#'   )
#' )
#'
#' # grid
#' p <- gTree(
#'   children = gList(
#'     rectGrob(),
#'     xaxisGrob(),
#'     yaxisGrob(),
#'     textGrob("Sepal.Length", y = unit(-4, "lines")),
#'     textGrob("Petal.Length", x = unit(-3.5, "lines"), rot = 90),
#'     pointsGrob(iris$Sepal.Length, iris$Petal.Length, gp = gpar(col = iris$Species), pch = 16)
#'   ),
#'   vp = vpStack(plotViewport(), dataViewport(xData = iris$Sepal.Length, yData = iris$Petal.Length))
#' )
#' grid.newpage()
#' grid.draw(p)
#'
#' grid.newpage()
#' grid.draw(
#'   decorate_grob(
#'     grob = p,
#'     titles = titles,
#'     footnotes = footnotes,
#'     page = "Page 6 of 129"
#'   )
#' )
#'
#' ## with ggplot2
#' library(ggplot2)
#'
#' p_gg <- ggplot2::ggplot(iris, aes(Sepal.Length, Sepal.Width, col = Species)) +
#'   ggplot2::geom_point()
#' p_gg
#' p <- ggplotGrob(p_gg)
#' grid.newpage()
#' grid.draw(
#'   decorate_grob(
#'     grob = p,
#'     titles = titles,
#'     footnotes = footnotes,
#'     page = "Page 6 of 129"
#'   )
#' )
#'
#' ## with lattice
#' library(lattice)
#'
#' xyplot(Sepal.Length ~ Petal.Length, data = iris, col = iris$Species)
#' p <- grid.grab()
#' grid.newpage()
#' grid.draw(
#'   decorate_grob(
#'     grob = p,
#'     titles = titles,
#'     footnotes = footnotes,
#'     page = "Page 6 of 129"
#'   )
#' )
#'
#' # with gridExtra - no borders
#' library(gridExtra)
#' grid.newpage()
#' grid.draw(
#'   decorate_grob(
#'     tableGrob(
#'       head(mtcars)
#'     ),
#'     titles = "title",
#'     footnotes = "footnote",
#'     border = FALSE
#'   )
#' )
#'
#' @export
decorate_grob <- function(grob,
                          titles,
                          footnotes,
                          page = "",
                          width_titles = grid::unit(1, "npc"),
                          width_footnotes = grid::unit(1, "npc"),
                          border = TRUE,
                          padding = grid::unit(rep(1, 4), "lines"),
                          margins = grid::unit(c(1, 0, 1, 0), "lines"),
                          outer_margins = grid::unit(c(2, 1.5, 3, 1.5), "cm"),
                          gp_titles = grid::gpar(),
                          gp_footnotes = grid::gpar(fontsize = 8),
                          name = NULL,
                          gp = grid::gpar(),
                          vp = NULL) {
  # External margins need to be taken into account when defining the width of titles and footers
  # because the text is split in advance depending on only the width of the viewport.
  if (any(as.numeric(outer_margins) > 0)) {
    width_titles <- width_titles - outer_margins[4]
    width_footnotes <- width_footnotes - outer_margins[4]
  }

  st_titles <- split_text_grob(
    titles,
    x = 0, y = 1,
    just = c("left", "top"),
    width = width_titles,
    vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1),
    gp = gp_titles
  )

  st_footnotes <- split_text_grob(
    footnotes,
    x = 0, y = 1,
    just = c("left", "top"),
    width = width_footnotes,
    vp = grid::viewport(layout.pos.row = 3, layout.pos.col = 1),
    gp = gp_footnotes
  )

  pg_footnote <- grid::textGrob(
    paste("\n", page),
    x = 1, y = 0,
    just = c("right", "bottom"),
    vp = grid::viewport(layout.pos.row = 4, layout.pos.col = 1),
    gp = gp_footnotes
  )

  # Initial decoration of the grob -> border, paddings, and margins are used here
  main_plot <- grid::gTree(
    children = grid::gList(
      if (border) grid::rectGrob(),
      grid::gTree(
        children = grid::gList(
          grob
        ),
        vp = grid::plotViewport(margins = padding) # innermost margins of the grob plot
      )
    ),
    vp = grid::vpStack(
      grid::viewport(layout.pos.row = 2, layout.pos.col = 1),
      grid::plotViewport(margins = margins) # margins around the border plot
    )
  )

  grid::gTree(
    grob = grob,
    titles = titles,
    footnotes = footnotes,
    page = page,
    width_titles = width_titles,
    width_footnotes = width_footnotes,
    outer_margins = outer_margins,
    gp_titles = gp_titles,
    gp_footnotes = gp_footnotes,
    children = grid::gList(
      grid::gTree(
        children = grid::gList(
          st_titles,
          main_plot, # main plot with border, padding, and margins
          st_footnotes,
          pg_footnote
        ),
        childrenvp = NULL,
        name = "titles_grob_footnotes",
        vp = grid::vpStack(
          grid::plotViewport(margins = outer_margins), # Main external margins
          grid::viewport(
            layout = grid::grid.layout(
              nrow = 4, ncol = 1,
              heights = grid::unit.c(
                grid::grobHeight(st_titles),
                grid::unit(1, "null"),
                grid::grobHeight(st_footnotes),
                grid::grobHeight(pg_footnote)
              )
            )
          )
        )
      )
    ),
    name = name,
    gp = gp,
    vp = vp,
    cl = "decoratedGrob"
  )
}

# nocov start
#' @importFrom grid validDetails
#' @noRd
validDetails.decoratedGrob <- function(x) {
  checkmate::assert_character(x$titles)
  checkmate::assert_character(x$footnotes)

  if (!is.null(x$grob)) {
    checkmate::assert_true(grid::is.grob(x$grob))
  }
  if (length(x$page) == 1) {
    checkmate::assert_character(x$page)
  }
  if (!grid::is.unit(x$outer_margins)) {
    checkmate::assert_vector(x$outer_margins, len = 4)
  }
  if (!grid::is.unit(x$margins)) {
    checkmate::assert_vector(x$margins, len = 4)
  }
  if (!grid::is.unit(x$padding)) {
    checkmate::assert_vector(x$padding, len = 4)
  }

  x
}

#' @importFrom grid widthDetails
#' @noRd
widthDetails.decoratedGrob <- function(x) {
  grid::unit(1, "null")
}

#' @importFrom grid heightDetails
#' @noRd
heightDetails.decoratedGrob <- function(x) {
  grid::unit(1, "null")
}

#' Split text according to available text width
#'
#' Dynamically wrap text.
#'
#' @inheritParams grid::grid.text
#' @param text (`string`)\cr the text to wrap.
#' @param width (`grid::unit`)\cr a unit object specifying maximum width of text.
#'
#' @return A text `grob`.
#'
#' @details This code is taken from `R Graphics by Paul Murell, 2nd edition`
#'
#' @keywords internal
split_text_grob <- function(text,
                            x = grid::unit(0.5, "npc"),
                            y = grid::unit(0.5, "npc"),
                            width = grid::unit(1, "npc"),
                            just = "centre",
                            hjust = NULL,
                            vjust = NULL,
                            default.units = "npc", # nolint
                            name = NULL,
                            gp = grid::gpar(),
                            vp = NULL) {
  text <- gsub("\\\\n", "\n", text) # fixing cases of mixed behavior (\n and \\n)

  if (!grid::is.unit(x)) x <- grid::unit(x, default.units)
  if (!grid::is.unit(y)) y <- grid::unit(y, default.units)
  if (!grid::is.unit(width)) width <- grid::unit(width, default.units)
  if (grid::unitType(x) %in% c("sum", "min", "max")) x <- grid::convertUnit(x, default.units)
  if (grid::unitType(y) %in% c("sum", "min", "max")) y <- grid::convertUnit(y, default.units)
  if (grid::unitType(width) %in% c("sum", "min", "max")) width <- grid::convertUnit(width, default.units)

  if (length(gp) > 0) { # account for effect of gp on text width -> it was bugging when text was empty
    horizontal_npc_width_no_gp <- grid::convertWidth(
      grid::grobWidth(
        grid::textGrob(
          paste0(text, collapse = "\n")
        )
      ), "npc",
      valueOnly = TRUE
    )
    horizontal_npc_width_with_gp <- grid::convertWidth(grid::grobWidth(
      grid::textGrob(
        paste0(text, collapse = "\n"),
        gp = gp
      )
    ), "npc", valueOnly = TRUE)

    # Adapting width to the input gpar (it is normalized so does not matter what is text)
    width <- width * horizontal_npc_width_no_gp / horizontal_npc_width_with_gp
  }

  ## if it is a fixed unit then we do not need to recalculate when viewport resized
  if (!inherits(width, "unit.arithmetic") && !is.null(attr(width, "unit")) &&
    attr(width, "unit") %in% c("cm", "inches", "mm", "points", "picas", "bigpts", "dida", "cicero", "scaledpts")) { # nolint
    attr(text, "fixed_text") <- paste(vapply(text, split_string, character(1), width = width), collapse = "\n")
  }

  # Fix for split_string in case of residual \n (otherwise is counted as character)
  text2 <- unlist(
    strsplit(
      paste0(text, collapse = "\n"), # for "" cases
      "\n"
    )
  )

  # Final grid text with cat-friendly split_string
  grid::grid.text(
    label = split_string(text2, width),
    x = x, y = y,
    just = just,
    hjust = hjust,
    vjust = vjust,
    rot = 0,
    check.overlap = FALSE,
    name = name,
    gp = gp,
    vp = vp,
    draw = FALSE
  )
}

#' @importFrom grid validDetails
#' @noRd
validDetails.dynamicSplitText <- function(x) {
  checkmate::assert_character(x$text)
  checkmate::assert_true(grid::is.unit(x$width))
  checkmate::assert_vector(x$width, len = 1)
  x
}

#' @importFrom grid heightDetails
#' @noRd
heightDetails.dynamicSplitText <- function(x) {
  txt <- if (!is.null(attr(x$text, "fixed_text"))) {
    attr(x$text, "fixed_text")
  } else {
    paste(vapply(x$text, split_string, character(1), width = x$width), collapse = "\n")
  }
  grid::stringHeight(txt)
}

#' @importFrom grid widthDetails
#' @noRd
widthDetails.dynamicSplitText <- function(x) {
  x$width
}

#' @importFrom grid drawDetails
#' @noRd
drawDetails.dynamicSplitText <- function(x, recording) {
  txt <- if (!is.null(attr(x$text, "fixed_text"))) {
    attr(x$text, "fixed_text")
  } else {
    paste(vapply(x$text, split_string, character(1), width = x$width), collapse = "\n")
  }

  x$width <- NULL
  x$label <- txt
  x$text <- NULL
  class(x) <- c("text", class(x)[-1])

  grid::grid.draw(x)
}
# nocov end

# Adapted from Paul Murell R Graphics 2nd Edition
# https://www.stat.auckland.ac.nz/~paul/RG2e/interactgrid-splittext.R
split_string <- function(text, width) {
  strings <- strsplit(text, " ")
  out_string <- NA
  for (string_i in seq_along(strings)) {
    newline_str <- strings[[string_i]]
    if (length(newline_str) == 0) newline_str <- ""
    if (is.na(out_string[string_i])) {
      out_string[string_i] <- newline_str[[1]][[1]]
      linewidth <- grid::stringWidth(out_string[string_i])
    }
    gapwidth <- grid::stringWidth(" ")
    availwidth <- as.numeric(width)
    if (length(newline_str) > 1) {
      for (i in seq(2, length(newline_str))) {
        width_i <- grid::stringWidth(newline_str[i])
        # Main conversion of allowed text width -> npc units are 0<npc<1. External viewport is used for conversion
        if (grid::convertWidth(linewidth + gapwidth + width_i, grid::unitType(width), valueOnly = TRUE) < availwidth) {
          sep <- " "
          linewidth <- linewidth + gapwidth + width_i
        } else {
          sep <- "\n"
          linewidth <- width_i
        }
        out_string[string_i] <- paste(out_string[string_i], newline_str[i], sep = sep)
      }
    }
  }
  paste(out_string, collapse = "\n")
}

#' Update page number
#'
#' Automatically updates page number.
#'
#' @param npages (`numeric(1)`)\cr total number of pages.
#' @param ... arguments passed on to [decorate_grob()].
#'
#' @return Closure that increments the page number.
#'
#' @keywords internal
decorate_grob_factory <- function(npages, ...) {
  current_page <- 0
  function(grob) {
    current_page <<- current_page + 1
    if (current_page > npages) {
      stop(paste("current page is", current_page, "but max.", npages, "specified."))
    }
    decorate_grob(grob = grob, page = paste("Page", current_page, "of", npages), ...)
  }
}

#' Decorate set of `grob`s and add page numbering
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Note that this uses the [decorate_grob_factory()] function.
#'
#' @param grobs (`list` of `grob`)\cr a list of grid grobs.
#' @param ... arguments passed on to [decorate_grob()].
#'
#' @return A decorated grob.
#'
#' @examples
#' library(ggplot2)
#' library(grid)
#' g <- with(data = iris, {
#'   list(
#'     ggplot2::ggplotGrob(
#'       ggplot2::ggplot(mapping = aes(Sepal.Length, Sepal.Width, col = Species)) +
#'         ggplot2::geom_point()
#'     ),
#'     ggplot2::ggplotGrob(
#'       ggplot2::ggplot(mapping = aes(Sepal.Length, Petal.Length, col = Species)) +
#'         ggplot2::geom_point()
#'     ),
#'     ggplot2::ggplotGrob(
#'       ggplot2::ggplot(mapping = aes(Sepal.Length, Petal.Width, col = Species)) +
#'         ggplot2::geom_point()
#'     ),
#'     ggplot2::ggplotGrob(
#'       ggplot2::ggplot(mapping = aes(Sepal.Width, Petal.Length, col = Species)) +
#'         ggplot2::geom_point()
#'     ),
#'     ggplot2::ggplotGrob(
#'       ggplot2::ggplot(mapping = aes(Sepal.Width, Petal.Width, col = Species)) +
#'         ggplot2::geom_point()
#'     ),
#'     ggplot2::ggplotGrob(
#'       ggplot2::ggplot(mapping = aes(Petal.Length, Petal.Width, col = Species)) +
#'         ggplot2::geom_point()
#'     )
#'   )
#' })
#' lg <- decorate_grob_set(grobs = g, titles = "Hello\nOne\nTwo\nThree", footnotes = "")
#'
#' draw_grob(lg[[1]])
#' draw_grob(lg[[2]])
#' draw_grob(lg[[6]])
#'
#' @export
decorate_grob_set <- function(grobs, ...) {
  n <- length(grobs)
  lgf <- decorate_grob_factory(npages = n, ...)
  lapply(grobs, lgf)
}
