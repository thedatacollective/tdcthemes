#' Australian Colour Palettes
#'
#'A collection of colour palettes inspired by the landscape and wildlife of Australia. The list of available palettes is:
#' namatjira_qual, namatjira_div
#' mccrea
#' parliament
#' tasmania
#' nolan_ned
#' winmar
#' olsen_qual
#' williams_pilbara
#' healthy_reef, dead_reef
#' galah, lorikeet, jumping_frog
#' emu_woman_paired
#'
#'@examples
#'
#' # Make an x-y plot using the Tdc palette
#' library(tidyverse)
#' df <- data.frame(x = rnorm(100, 0, 20),
#'           y = rnorm(100, 0, 20),
#'           cl = sample(letters[1:8], 100, replace=TRUE))
#' ggplot(df, aes(x, y, colour=cl, shape=cl)) +
#'   geom_point(size=4) + scale_colour_ochre() +
#'   theme_bw() + theme(aspect.ratio=1)
#'
#' # Make a histogram using the McCrea Collins Street palette
#' ggplot(df, aes(x, fill=cl)) + geom_histogram() +
#'   scale_fill_ochre(palette="mccrea")
#'
#' \dontrun{
#'   data(elev)
#'   library(elevatr)
#'   library(raster)
#'   library(tdcthemes)
#'   colpal <- tdc_pal()(5)
#'   ex <- extent(110, 155, -45, -10)
#'   elev <- raster::crop(elev, ex)
#'   elev[elev < 0] <- NA
#'   topo <- list(x = xFromCol(elev), y = rev(yFromRow(elev)),
#'                z = t(as.matrix(elev))[, nrow(elev):1])
#'                par(mar = rep(0, 4), bg = "#444444")
#'                image(topo, useRaster = TRUE, col = colpal,
#'                      axes = FALSE, xlab = "", ylab = "",
#'                            asp = cos(27.5 * pi/180))
#' }
#' @export
tdc_palettes <- list(
  # based on information from http://mkweb.bcgsc.ca/colorblind/
  colour_blind = c(
    "#e69f00",
    "#56b4e9",
    "#009e73",
    "#f0e43c",
    "#0072b2",
    "#d55e00",
    "#cc79a7",
    "#3c3c3c"
  ),

  # based on https://ux.stackexchange.com/questions/94696/color-palette-for-all-types-of-color-blindness
  colour_blind_15 = c(
    "#084751",
    "#029191",
    "#fe6cb5",
    "#fdb5da",
    "#490092",
    "#006dda",
    "#b56cfe",
    "#6cb6ff",
    "#b6dafe",
    "#920000",
    "#924900",
    "#db6d00",
    "#26fd23",
    "#feff6c"
  ),

  # data collective named colours
  tdc_named = c(
    # gift types
    "bequest"       = "#A81E91",
    "Bequest"       = "#A81E91",
    "regular gift"  = "#00A8E5",
    "Regular Gift"  = "#00A8E5",
    "rg"            = "#00A8E5",
    "RG"            = "#00A8E5",
    "cash"          = "#44AF69",
    "Cash"          = "#44AF69",
    "event"         = "#FFC914",
    "Event"         = "#FFC914",
    "community"     = "#FFC914",
    "Community"     = "#FFC914",
    "lottery"       = "#D72638",
    "Lottery"       = "#D72638",
    "major donor"   = "#F4832C",
    "Major Donor"   = "#F4832C",
    "Mid Value Donor" = "#44AF69",
    "MV" = "#44AF69",
    "Standard Value Donor" = "#0055D4",
    "SV" = "#0055D4",
    "other"         = "#344055",
    "Other"         = "#344055",
    "nondonor"      = "#9096A2",
    "rgcash"        = "#2BEFCB",
    # channels
    "Direct Mail"   = "#348AA7",
    "direct mail"   = "#348AA7",
    "Online"        = "#FCA311",
    "online"        = "#FCA311",
    "Phone"         = "#513B56",
    "phone"         = "#513B56",
    "Media"         = "#CE3131",
    "media"         = "#CE3131",
    "Unaddressed"   = "#FFC43D",
    "unaddressed"   = "#FFC43D",
    "Face to Face"  = "#00B262",
    "face to face"  = "#00B262",
    "Brand"         = "#0F4DBC",
    "brand"         = "#0F4DBC",
    # gender
    "Male"          = "#35A7FF",
    "M"             = "#35A7FF",
    "Female"        = "#EF476F",
    "F"             = "#EF476F",
    "U"             = "#344055",
    # title
    "Mr"            = "#35A7FF",
    "Mrs"           = "#EF476F",
    "Ms"            = "#992e47",
    "Miss"          = "#F489A3",
    "Joint"         = "#9096A2",
    "Not provided"  = "#1D232F",
    # donor category
    "I"             = "#0F4DBC",
    "O"             = "#9096A2",
    "Individual"    = "#0F4DBC",
    "Organisation"  = "#9096A2",
    # retention
    # retention
    "New"           = "#44AF69",
    "Retained"      = "#227D41",
    "Reactivated"   = "#74D582",
    # other
    "None"          = "#9096A2",
    "Community / Event" =  "#FFC914",
    "Corporate / Grants / Payroll" = "#0059A1",
    "In Mem / Celebration" = "#344055"
  ),

  tdc = c(
    "#0055d4",
    "#d72638",
    "#ffc914",
    "#44af69",
    "#344055"
  )
)