#' lab3 work with team_1 function
#'
#' @param file path to a shapefile (.shp) with spatial geometry data OR (if \code{fileread = F}) a simple features object
#' @param tolerance A numeric value greater than 0.
#' @param fileread If the file can be read in memory or not. Default is TRUE, use fileread=FALSE if the object is already a simple features object
#' @return plot
#' @import sf tidyverse
#' @export
#' @examples
#' ozbig1 <- ozbig
#' team_1(file=ozbig1, tolerance=0.1, fileread=FALSE)
team_1=function(file, tolerance, fileread=TRUE){

  if(tolerance <= 0){stop('Choose a value which larger than 0.')}

  #read file as object
  ozbig1<-file
  if(fileread==T){
    assertthat::assert_that(is.character(file) , assertthat::is.readable(file),
                            msg = 'File path not readable')

    ozbig1 <- sf::read_sf(file)
    stopifnot(is.list(ozbig1))
  }

  oz_st <- maptools::thinnedSpatialPoly(as(ozbig1, "Spatial"), tolerance = tolerance, minarea = 0.001, topologyPreserve = TRUE)
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
