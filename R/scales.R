#' The Data Collective palette with ramped colours
#'
#' @param palette Choose from 'tdc_palettes' list
#'
#' @param alpha transparency
#'
#' @param reverse If TRUE, the direction of the colours is reversed.
#'
#' @examples
#' library(scales)
#' show_col(tdc_pal()(10))
#'
#' filled.contour(volcano,color.palette = tdc_pal(), asp=1)
#'
#' @export
tdc_pal_con <- function(palette = "tdc", alpha = 1, reverse = FALSE, ...) {
  pal <- tdc_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

#' The Data Collective palette with ramped colours
#'
#' @param palette Choose from 'tdc_palettes' list
#'
#' @param alpha transparency
#'
#' @param reverse If TRUE, the direction of the colours is reversed.
#'
#' @examples
#' library(scales)
#' show_col(tdc_pal()(10))
#'
#' filled.contour(volcano,color.palette = tdc_pal(), asp=1)
#'
#' @export
tdc_pal <- function(palette = "tdc", alpha = 1, reverse = FALSE, ...) {
  pal <- tdc_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  f <- scales::manual_pal(pal)
  attr(f, "max_n") <- length(pal)
  f
}

#' Setup colour palette for ggplot2
#'
#' @rdname scale_colour_tdc
#'
#' @param palette Choose from 'tdc_palettes' list
#'
#' @param reverse logical, Reverse the order of the colours?
#'
#' @param alpha transparency
#'
#' @param discrete whether to use a discrete colour palette
#'
#' @param ... additional arguments to pass to scale_color_gradientn
#'
#' @inheritParams viridis::scale_color_viridis
#'
#' @importFrom ggplot2 scale_colour_manual
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point(aes(colour = factor(cyl))) +
#'   scale_colour_tdc(palette="colour_blind_15")
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point(aes(colour = hp)) +
#'   scale_colour_tdc(palette="colour_blind_15", discrete = FALSE)
#' ggplot(data = mpg) +
#'   geom_point(mapping = aes(x = displ, y = hwy, color = class)) +
#'   scale_colour_tdc(palette="colour_blind")
#' ggplot(diamonds) + geom_bar(aes(x = cut, fill = clarity)) +
#'   scale_fill_tdc()
#' @export
#'
#' @importFrom ggplot2 discrete_scale scale_color_gradientn
scale_colour_tdc <- function(..., palette="tdc",
                             discrete = TRUE, alpha = 1, reverse = FALSE) {
  if (discrete) {
    if (palette == "tdc_named") {
      scale_colour_manual(values=tdc_palettes[[palette]])
    } else {
      discrete_scale("colour", "tdc", palette = tdc_pal(palette, alpha = alpha, reverse = reverse))
    }
  } else {
    scale_color_gradientn(colours = tdc_pal_con(palette, alpha = alpha, reverse = reverse, ...)(256))
  }
  #scale_colour_manual(values=tdc_palettes[[palette]])
}

#' @rdname scale_colour_tdc
#' @export
scale_color_tdc <- scale_colour_tdc

#' #' Setup fill palette for ggplot2
#'
#' @param palette Choose from 'tdc_palettes' list
#'
#' @inheritParams viridis::scale_fill_viridis
#' @inheritParams tdc_pal
#'
#' @param discrete whether to use a discrete colour palette
#'
#' @param ... additional arguments to pass to scale_color_gradientn
#'
#' @importFrom ggplot2 scale_fill_manual discrete_scale scale_fill_gradientn
#'
#' @export
scale_fill_tdc <- function(..., palette="tdc",
                           discrete = TRUE, alpha=1, reverse = FALSE) {
  if (discrete) {
    if (palette == "tdc_named") {
      scale_fill_manual(values = tdc_palettes[[palette]])
    } else {
      discrete_scale("fill", "tdc", palette = tdc_pal(palette, alpha = alpha, reverse = reverse))
    }
  } else {
    scale_fill_gradientn(colours = tdc_pal_con(palette, alpha = alpha, reverse = reverse, ...)(256))
  }
}

# The following is taken from the hrbrthemes package
# https://github.com/hrbrmstr/hrbrthemes/blob/master/R/scales.r

# Stolen from ggplot2
is.waive <- function (x) { inherits(x, "waiver") }
is.sec_axis <- function (x) { inherits(x, "AxisSecondary") }
is.formula <- function (x) { inherits(x, "formula") }

#' X & Y scales with opinionated pre-sets for percent & comma label formats
#'
#' The `_comma` ones set comma format for axis text and `expand=c(0,0)` (you need to set limits).
#'
#' The `_percent` ones set precent format for axis text and `expand=c(0,0)` (you need to set limits).
#'
#' @md
#' @param name The name of the scale. Used as axis or legend title. If
#'   `waiver()`, the default, the name of the scale is taken from the first
#'   mapping used for that aesthetic. If `NULL`, the legend title will be
#'   omitted.
#' @param breaks One of:
#'   - `NULL` for no breaks
#'   - `waiver()` for the default breaks computed by the
#'     transformation object
#'   - A numeric vector of positions
#'   - A function that takes the limits as input and returns breaks
#'     as output
#' @param minor_breaks One of:
#'   - `NULL` for no minor breaks
#'   - `waiver()` for the default breaks (one minor break between
#'     each major break)
#'   - A numeric vector of positions
#'   - A function that given the limits returns a vector of minor breaks.
#' @param labels Specifying overrides the default format (i.e. you really don't
#'   want to do that). `NULL` means no labels.
#' @param limits A numeric vector of length two providing limits of the scale.
#'   Use `NA` to refer to the existing minimum or maximum.
#' @param oob Function that handles limits outside of the scale limits
#'   (out of bounds). The default replaces out of bounds values with NA.
#' @param na.value If `na.translate = TRUE`, what value aesthetic
#'   value should missing be displayed as? Does not apply to position scales
#'   where `NA` is always placed at the far right.
#' @param expand same as in ggplot2
#' @param trans Either the name of a transformation object, or the
#'   object itself. Built-in transformations include "asn", "atanh",
#'   "boxcox", "exp", "identity", "log", "log10", "log1p", "log2",
#'   "logit", "probability", "probit", "reciprocal", "reverse" and "sqrt".
#' @param position The position of the axis. "left" or "right" for vertical
#' scales, "top" or "bottom" for horizontal scales
#' @param sec.axis specify a secondary axis
#' @export
scale_x_percent <- function(position = "bottom", sec.axis = waiver(), labels,
                            accuracy = 1, scale = 100, prefix = "", suffix = "%",
                            big.mark = ",", decimal.mark = ".", trim = TRUE, ...) {

  if (missing(labels)) {
    scales::percent_format(
      accuracy = accuracy,
      scale = scale,
      prefix = prefix,
      suffix = suffix,
      big.mark = big.mark,
      decimal.mark = decimal.mark,
      trim = trim,
      ...
    ) -> labels
  }

  ggplot2::scale_x_continuous(
    labels = labels,
    position =  position
  ) -> sc

  if (!is.waive(sec.axis)) {

    if (is.formula(sec.axis)) sec.axis <- ggplot2::sec_axis(sec.axis)
    if (!is.sec_axis(sec.axis)) stop("Secondary axes must be specified using 'sec_axis()'")

    sc$secondary.axis <- sec.axis

  }

  sc

}

#' @rdname scale_x_percent
#' @export
scale_y_percent <- function(position = "left", sec.axis = waiver(), labels,
                            accuracy = 1, scale = 100, prefix = "", suffix = "%",
                            big.mark = ",", decimal.mark = ".", trim = TRUE, ...) {

  if (missing(labels)) {
    scales::percent_format(
      accuracy = accuracy,
      scale = scale,
      prefix = prefix,
      suffix = suffix,
      big.mark = big.mark,
      decimal.mark = decimal.mark,
      trim = trim,
      ...
    ) -> labels
  }

  ggplot2::scale_y_continuous(
    labels = labels,
    position =  position
  ) -> sc

  if (!is.waive(sec.axis)) {

    if (is.formula(sec.axis)) sec.axis <- ggplot2::sec_axis(sec.axis)
    if (!is.sec_axis(sec.axis)) stop("Secondary axes must be specified using 'sec_axis()'")

    sc$secondary.axis <- sec.axis

  }

  sc

}

#' @rdname scale_x_percent
#' @param accuracy,scale,prefix,suffix,big.mark,decimal.mark,trim See
#'        [scales::comma_format()] or [scales::percent_format()]
#' @param ... passed on to [scales::comma_format()] or [scales::percent_format()]
#' @export
scale_x_comma <- function(position = "bottom", sec.axis = waiver(), labels,
                          accuracy = 1, scale = 1, prefix = "", suffix = "",
                          big.mark = ",", decimal.mark = ".", trim = TRUE,
                          ...) {

  if (missing(labels)) {
    scales::comma_format(
      accuracy = accuracy,
      scale = scale,
      prefix = prefix,
      suffix = suffix,
      big.mark = big.mark,
      decimal.mark = decimal.mark,
      trim = trim,
      ...
    ) -> labels
  }

  ggplot2::scale_x_continuous(
    labels = labels,
    position =  position
  ) -> sc

  if (!is.waive(sec.axis)) {

    if (is.formula(sec.axis)) sec.axis <- ggplot2::sec_axis(sec.axis)
    if (!is.sec_axis(sec.axis)) stop("Secondary axes must be specified using 'sec_axis()'")

    sc$secondary.axis <- sec.axis

  }

  sc

}

#' @rdname scale_x_percent
#' @export
scale_y_comma <- function(position = "left", sec.axis = waiver(), labels,
                          accuracy = 1, scale = 1, prefix = "", suffix = "",
                          big.mark = ",", decimal.mark = ".", trim = TRUE,
                          ...) {

  if (missing(labels)) {
    scales::comma_format(
      accuracy = accuracy,
      scale = scale,
      prefix = prefix,
      suffix = suffix,
      big.mark = big.mark,
      decimal.mark = decimal.mark,
      trim = trim,
      ...
    ) -> labels
  }

  ggplot2::scale_y_continuous(
    labels = labels,
    position =  position
  ) -> sc

  if (!is.waive(sec.axis)) {

    if (is.formula(sec.axis)) sec.axis <- ggplot2::sec_axis(sec.axis)
    if (!is.sec_axis(sec.axis)) stop("Secondary axes must be specified using 'sec_axis()'")

    sc$secondary.axis <- sec.axis

  }

  sc

}