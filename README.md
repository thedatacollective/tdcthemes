
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tdcthemes

<!-- badges: start -->

<!-- badges: end -->

TDC Themes is a package designed to make consistent styling possible
across projects for The Data Collective. While the package is publicly
available on github, it is purely to provide others with context on how
they might approach this for their organisation.

Much of this has been riffed off
[hrbrthemes](https://github.com/hrbrmstr/hrbrthemes),
[bbplot](https://github.com/bbc/bbplot) and
[ochRe](https://github.com/ropenscilabs/ochRe)

The package does make use of the
[ggtext](https://github.com/wilkelab/ggtext) to enhance text styling
options.

## Installation

To make full use of the `ggtext` components it is necessary to have
`gridtext` installed.

``` r
remotes::install_github("wilkelab/gridtext")
```

To install `tdcthemes`

``` r
remotes::install_github("thedatacollective/tdcthemes")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ggplot2)
library(tdcthemes)

## basic example code
ggplot(iris, aes(Sepal.Length, Petal.Length, colour = as.factor(Species))) +
  geom_point() +
  labs(title = "Fisher's *Iris* data set",
       subtitle = "Demonstrating the TDC theme for ggplot2",
       x = "The x axis", y = "The y axis",
       caption = "Fisher's **Iris** data set") +
  facet_wrap(~ Species) +
  scale_colour_tdc() +
  style_tdc(legend = T) +
  NULL
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />
