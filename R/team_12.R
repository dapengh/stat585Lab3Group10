#' Get polygon and geographic information from a shapefile that is thinned to a more feasible dimension. This is team 12's results
#' @name team_12
#' @param file path to a shapefile (.shp) with spatial geometry data OR a simple features object that contains a geometry column
#' @param tolerance a value greater than 0
#' @param fileread Should the data be read from a file? Default is TRUE, use fileread=FALSE if the object is already a simple features object in the environment
#' @export
#' @return A data frame, with each row corresponding to a unique polygon point and its associated polygon and geographic information.
#' @examples
#' #get data for plotting
#' oz <- ozbig
#'
#' aus <- team_12(file = oz, tolerance = 0.1, fileread=FALSE)
#'
#' head(aus)
#'

team_12 <- function(file, tolerance=0.1, fileread=TRUE){

  if(tolerance <= 0){stop('tolerance less than 0. Choose a different value')}

  stbig<-file

  if(fileread==T){
    assert_that(is.character(file) , is.readable(file),
               msg = 'File path not readable')

    stbig <- read_sf(file)
  }

  if(!is.list(stbig)){stop('Check file type. Simple features not output as list')}
  if(!('geometry'%in%names(stbig))){stop('No geometry information found')}

  st <- maptools::thinnedSpatialPoly(as(stbig, "Spatial"), tolerance = tolerance, minarea = 0.001, topologyPreserve = TRUE)
  st <- st_as_sf(st)

  res <- purrr::map(st$geometry, .f=helper)
  res <- add_layer(res)
  colnames(res) <- c('long','lat','order','group','geo')
  res <- as.data.frame(res)

  outdf <- res %>% mutate(Country = st$NAME_0[geo], Name = st$NAME_1[geo], Type = st$ENGTYPE_1[geo], Abbr = st$HASC_1[geo])

  if(is.null(dim(outdf)) | !is.data.frame(outdf)) {warning('No data returned. Check geometry object or file path')}
  return(outdf)
}


helper <- function(d){
  d <- unlist(d,recursive = FALSE)
  d <- purrr::map(d,.f=add_order)
  d <- add_layer(d)
  return(d)
}
add_order <- function(d){
  l <- nrow(d)
  return(cbind(d,seq(1,l,by=1)))
}
add_layer <- function(d){
  ll <- unlist(lapply(d,nrow))
  d <- do.call(rbind,d)
  d <- cbind(d,rep(c(1:length(ll)),time=ll))
  return(d)
}
