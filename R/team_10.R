#' Polygon and geographic information from a shapefile
#' @description Get polygon and geographic information from a shapefile that is thinned to a more feasible dimension. This is team 10's results
#' @name team_10
#' @param file path to a shapefile (.shp) with spatial geometry data OR (if \code{fileread = F}) a simple features object that contains a geometry column
#' @param tolerance  A numeric value greater than 0. How much should the data set be thinned?
#' @param fileread Should the data be read from a file? Default is TRUE, use fileread=FALSE if the object is already a simple features object in the environment,
#' perhaps read in via \code{read_sf()}. This may be useful if you already have the file read into the environment for another purpose
#' @export
#' @return A data frame, with each row corresponding to a unique polygon point and its associated polygon and geographic information.
#' @examples
#' #get data for plotting
#' oz <- ozbig
#' aus <- team_10(file = oz, tolerance = 0.1, fileread=FALSE)
#'
#' #plot using ggplot2
#' library(ggplot2)
#' ggplot(data=aus)+geom_path(aes(x=long,y=lat,group=pgroup)) + theme_bw()
#'
team_10 <- function(file, tolerance=0.1, fileread=TRUE){
  assertthat::assert_that(is.logical(fileread), msg='fileread error. Are you reading from directly from a file? Enter a logical value.')
  #test tolerance level
  assertthat::assert_that(tolerance>0, is.numeric(tolerance), length(tolerance)==1, msg='Invalid tolerance. Choose a postive numeric value')
  #read file as object
  if(fileread==T){
    assertthat::assert_that(is.character(file) , is.readable(file), msg = 'File path not readable')
    stbig <- sf::read_sf(file)
  } else {
    stbig <- file
  }

  if(!is.list(stbig)){stop('Check file type. Simple features not output as list')}
  if(!('geometry'%in%names(stbig))){stop('No geometry information found')}

  shp_st <- maptools::thinnedSpatialPoly(
    as(stbig, "Spatial"), tolerance = tolerance,
    minarea = 0.001, topologyPreserve = TRUE)
  shp <- st_as_sf(shp_st)
  shpSmall <- shp %>% select(NAME_1, geometry) %>%
    group_by() %>%
    mutate(coord = geometry %>% map(.f = function(m) flatten(.x=m)),region = row_number()) %>%  unnest
  st_geometry(shpSmall) <- NULL
  shpSmall <- shpSmall %>% mutate(coord = coord %>% map(.f = function(m) as_tibble(m)),group = row_number()) %>%  unnest %>% setNames(c("name", "region","group", "long", "lat"))

  shpdf <- shpSmall %>% mutate(Country = shp$NAME_0[region], Name = st$NAME_1[region],Type = st$ENGTYPE_1[region], Abbr = st$HASC_1[region], pgroup = as.factor(paste(region,group,sep='.')))

  if(is.null(dim(shpdf)) | !is.data.frame(shpdf)) {warning('No data returned. Check geometry object or file path')}

  return(shpdf)
}
