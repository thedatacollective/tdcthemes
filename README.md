# tdcthemes

<!-- badges: start -->
<!-- badges: end -->

TDC Themes is a package designed to make consistent styling possible across projects for The Data Collective. While the package is publicly available on github, it is purely to provide others with context on how they might approach this for their organisation.

Much of this has been riffed off [hrbrthemes](https://github.com/hrbrmstr/hrbrthemes) and [bbplot](https://github.com/bbc/bbplot).

The package does make use of the [ggtext](https://github.com/wilkelab/ggtext) to enhance text styling options.

## Installation

``` r
remotes::install_github("thedatacollective/tdcthemes")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(tdcthemes)
## basic example code
ggplot(iris, aes(Sepal.Length, Petal.Length, colour = as.factor(Species))) +
geom_point() +
labs(title = "Fisher's *Iris* data set",
subtitle = "Demonstrating the TDC theme for ggplot2",
x = "The x axis", y = "The y axis",
caption = "Fisher's **Iris** data set") +
facet_wrap(~ Species) +
style_tdc() +
NULL
```

