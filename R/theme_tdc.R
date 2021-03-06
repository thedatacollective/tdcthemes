
#' Theme TDC
#'
#' A consistent ggplot theme for use by The Data Collective
#'
#' @param base_family,base_size base font family and size
#' @param plot_title_family,plot_title_size plot title font family and size
#' @param subtitle_family,subtitle_size subtitle font family and size
#' @param title_family,title_size subtitle font family and size
#' @param caption_family,caption_size caption font family and size
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' # example scatterplot
#' ggplot(iris, aes(Sepal.Length, Petal.Length, colour = as.factor(Species))) +
#'   geom_hline(yintercept = 0, colour = "#344055", size = 0.5) +
#'   geom_point() +
#'   labs(title = "Fisher's *Iris* data set",
#'        subtitle = "Demonstrating the TDC theme for ggplot2",
#'        x = "The x axis", y = "The y axis",
#'        caption = "Fisher's **Iris** data set") +
#'   facet_wrap(~ Species) +
#'   theme_tdc()
#' }
theme_tdc <- function(
  base_family = "Roboto Condensed",
  base_size = 12,
  plot_title_family = base_family,
  plot_title_size = 28,
  subtitle_family = "Roboto Condensed",
  subtitle_size = 12,
  title_family = base_family,
  title_size = 14,
  caption_family = base_family,
  caption_size = 10
) {

  ret <- ggplot2::theme_minimal(base_family=base_family, base_size=base_size)

  # position key items in the plotting area
  ret <- ret + theme(plot.title.position = "plot")
  ret <- ret + theme(plot.caption.position = "plot")
  ret <- ret + theme(legend.position = "bottom")

  # set text elements
  ret <- ret + theme(text = ggplot2::element_text(family = base_family,
                                                  colour = "#707070"))
  ret <- ret + theme(title = ggtext::element_markdown(family = title_family,
                                                      size = title_size,
                                                      colour = "#707070",
                                                      lineheight = 1.2))
  ret <- ret + theme(plot.title = ggtext::element_markdown(family = plot_title_family,
                                                           size = plot_title_size,
                                                           face = "bold",
                                                           colour = "#1C1C1C",
                                                           lineheight = 1.2))
  ret <- ret + theme(plot.subtitle = ggtext::element_textbox_simple(family = subtitle_family,
                                                                    size = subtitle_size,
                                                                    colour = "#1C1C1C",
                                                                    lineheight = 1.2,
                                                                    padding = margin(5.5, 5.5, 5.5, 0),
                                                                    margin = margin(0, 0, 5.5, 0)))

  # set axis defaults
  ret <- ret + theme(axis.text = ggtext::element_markdown(size = base_size))
  ret <- ret + theme(axis.title.y = ggtext::element_markdown(hjust = 1,
                                                             padding = margin(0, 5.5, 5.5, 0)))
  ret <- ret + theme(axis.title.x = ggtext::element_markdown(hjust = 1,
                                                             padding = margin(5.5, 0, 0, 0)))

  # facet headings & panel detail
  ret <- ret + theme(strip.text = ggtext::element_markdown(family = title_family,
                                                           size = base_size,
                                                           face = "bold",
                                                           colour = "#1C1C1C",
                                                           hjust = 0))
  ret <- ret + theme(strip.background = element_blank())
  ret <- ret + theme(panel.background = element_blank())
  ret <- ret + theme(panel.spacing = ggplot2::unit(25, "points"))
  ret <- ret + theme(panel.grid = ggplot2::element_line(colour = "#F4F4F4",
                                                        size = 0.5))

  # set legend elements
  ret <- ret + theme(legend.direction = "horizontal")
  ret <- ret + theme(legend.title = ggplot2::element_blank())
  ret <- ret + theme(legend.text = ggtext::element_markdown(hjust = 0, size = base_size))
  ret <- ret + theme(legend.key = ggplot2::element_blank())

  # captions
  ret <- ret + theme(plot.caption = ggtext::element_markdown(family = caption_family,
                                                             size = caption_size,
                                                             face = "plain",
                                                             colour = "#707070",
                                                             hjust = 0))

  ret
}


#' Theme Clean
#'
#' A consistent ggplot theme for use by The Data Collective
#'
#' @param base_family,base_size,base_colour base font family, size and colour
#' @param plot_title_family,plot_title_size,plot_title_colour plot title font family, size and colour
#' @param subtitle_family,subtitle_size,subtitle_colour subtitle font family size, and colour
#' @param title_family,title_size,title_colour title font family, size and colour
#' @param caption_family,caption_size,caption_colour caption font family, size and colour
#' @param panel_spacing the gap between panels in points
#' @param grid_y,grid_x the linestyle numeric 0-8 for the x/y gridlines
#' @param grid_colour the colour of the grid lines
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' # example scatterplot
#' ggplot(iris, aes(Sepal.Length, Petal.Length, colour = as.factor(Species))) +
#'   geom_hline(yintercept = 0, colour = "#344055", size = 0.5) +
#'   geom_point() +
#'   labs(title = "Fisher's *Iris* data set",
#'        subtitle = "Demonstrating the TDC theme for ggplot2",
#'        x = "The x axis", y = "The y axis",
#'        caption = "Fisher's **Iris** data set") +
#'   facet_wrap(~ Species) +
#'   theme_clean()
#' }
theme_clean <- function(
base_family = "Roboto Condensed", base_size = 12, base_colour = "#1C1C1C",
plot_title_family = base_family, plot_title_size = base_size * 2.5, plot_title_colour = base_colour,
subtitle_family = "Roboto Thin", subtitle_size = base_size, subtitle_colour = base_colour,
title_family = base_family, title_size = base_size * 1.2, title_colour = base_colour,
caption_family = subtitle_family, caption_size = base_size * 0.8, caption_colour = base_colour,
panel_spacing = 25, grid_y = 1, grid_x = 1, grid_colour = "#ECECEC") {
  ret <- ggplot2::theme_minimal(base_family = base_family, base_size = base_size)
  ret <- ret + theme(plot.title.position = "plot")
  ret <- ret + theme(plot.caption.position = "plot")
  ret <- ret + theme(plot.title = ggtext::element_markdown(family = plot_title_family,
                                              size = plot_title_size, face = "bold",
                                              colour = plot_title_colour,
                                              lineheight = 1.2))
  ret <- ret + theme(plot.subtitle = ggtext::element_textbox_simple(family = subtitle_family,
                                                       size = subtitle_size,
                                                       colour = subtitle_colour, lineheight = 1.1,
                                                       padding = margin(5.5, 5.5, 5.5, 0),
                                                       margin = margin(0, 0, 5.5, 0)))
  ret <- ret + theme(title = ggtext::element_markdown(family = title_family, colour = title_colour,
                                         size = title_size, lineheight = 1.2))
  ret <- ret + theme(axis.title.y = ggtext::element_markdown(hjust = 1,
                                                padding = margin(0, 0, 5.5, 0)))
  ret <- ret + theme(axis.title.x = ggtext::element_markdown(hjust = 1,
                                                padding = margin(5.5, 0, 0, 0)))
  ret <- ret + theme(axis.text = ggtext::element_markdown(size = base_size, colour = base_colour))
  ret <- ret + theme(legend.position = "bottom")
  ret <- ret + theme(legend.direction = "horizontal")
  ret <- ret + theme(legend.title = ggplot2::element_blank())
  ret <- ret + theme(legend.text = ggtext::element_markdown(hjust = 0,
                                               size = base_size))
  ret <- ret + theme(legend.key = ggplot2::element_blank())
  ret <- ret + theme(strip.text = ggtext::element_markdown(family = title_family,
                                              size = base_size, face = "bold", colour = base_colour,
                                              hjust = 0))
  ret <- ret + theme(strip.background = element_blank())
  ret <- ret + theme(panel.background = element_blank())
  ret <- ret + theme(panel.spacing = ggplot2::unit(panel_spacing, "points"))
  ret <- ret + theme(panel.grid.major.x = ggplot2::element_line(colour = grid_colour,
                                                   size = 0.5, linetype = grid_x))
  ret <- ret + theme(panel.grid.minor.x = ggplot2::element_line(colour = grid_colour,
                                                   size = 0.25,linetype = grid_x))
  ret <- ret + theme(panel.grid.major.y = ggplot2::element_line(colour = grid_colour,
                                                   size = 0.5, linetype = grid_y))
  ret <- ret + theme(panel.grid.minor.y = ggplot2::element_line(colour = grid_colour,
                                                   size = 0.25,linetype = grid_y))
  ret <- ret + theme(plot.caption = ggtext::element_markdown(family = caption_family,
                                                size = caption_size, face = "plain", colour = caption_colour,
                                                hjust = 0))
  ret
}



#' Create a new ggplot
#'
#' This function masks `ggplot()` from the ggplot2 package in order to apply additional
#' styles appropriate to The Data Collective charts
#'
#' @param ... arguments intended for a ggplot
#' @param zero_line a boolean value indicating whether to include a horizontal line
#' @param legend a boolean value indicating whether to include the legend
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' # example scatterplot
#' ggplot(iris, aes(Sepal.Length, Petal.Length, colour = as.factor(Species))) +
#'   geom_point() +
#'   labs(title = "Fisher's *Iris* data set",
#'        subtitle = "Demonstrating the TDC theme for ggplot2",
#'        x = "The x axis", y = "The y axis",
#'        caption = "Fisher's **Iris** data set") +
#'        style_tdc()
#' }
style_tdc <- function(
  ...,
  zero_line = TRUE,
  legend = FALSE
) {

  if (zero_line) {
    zero_line <-  ggplot2::geom_hline(yintercept = 0,
                                 size = 0.5,
                                 colour = "#344055")
  } else {
    zero_line <-  ggplot2::expand_limits(y = 0)
  }

  if (!legend) {
    legend <- theme(legend.position = "none")
  } else {
    legend <- theme(legend.position = "bottom")
  }

  list(theme_tdc(), zero_line, legend)
}

#' Create a new ggplot
#'
#' This function masks `ggplot()` from the ggplot2 package in order to apply additional
#' styles appropriate to The Data Collective charts
#'
#' @param ... arguments intended for a ggplot
#' @param zero_line a HEX colour value for the horizontal line (NA for no line)
#' @param legend a boolean value indicating whether to include the legend
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' # example scatterplot
#' ggplot(iris, aes(Sepal.Length, Petal.Length, colour = as.factor(Species))) +
#'   geom_point() +
#'   labs(title = "Fisher's *Iris* data set",
#'        subtitle = "Demonstrating the TDC theme for ggplot2",
#'        x = "The x axis", y = "The y axis",
#'        caption = "Fisher's **Iris** data set") +
#'        style_tdc()
#' }
style_clean <- function(
  ...,
  zero_line = "#344055",
  legend = FALSE
) {

  if (!is.na(zero_line)) {
    zero_line <-  ggplot2::geom_hline(yintercept = 0,
                                      size = 0.5,
                                      colour = zero_line)
  } else {
    zero_line <-  ggplot2::expand_limits(y = 0)
  }

  if (!legend) {
    legend <- theme(legend.position = "none")
  } else {
    legend <- theme(legend.position = "bottom")
  }

  list(theme_clean(...), zero_line, legend)
}