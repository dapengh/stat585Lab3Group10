#' lab3 work with team 1 function
#'
#' @param file
#' @param tolerance
#'
#' @return plot
#' @import sf tidyverse
#' @export
#' @examples
#' team_1(file=ozbig, tolerance=0.1)
team_1=function(file, tolerance){

  if(tolerance <= 0){stop('Choose a value which larger than 0.')}

  ozbig <- sf::read_sf(file)
  assertthat::assert_that(assertthat::is.readable(file), msg='file is not readable')
  stopifnot(is.list(ozbig))

  oz_st <- maptools::thinnedSpatialPoly(as(ozbig, "Spatial"), tolerance = tolerance, minarea = 0.001, topologyPreserve = TRUE)
  oz <- sf::st_as_sf(oz_st)
  df.oz.purr <- oz$geometry %>%
  map_depth(3, data.frame) %>%
  purrr::flatten() %>%
  purrr::flatten() %>%
  bind_rows(.id = "group") %>%
  rename("lat" = y, "long" = x)

  if(is.null(dim(df.oz.purr)) | !is.data.frame(df.oz.purr)) {warning('No data returned. Check geometry object or file path')}

  return(df.oz.purr)
}
# usethis::use_pipe()
# usethis::use_testthat()
# usethis::use_test("team_1")
