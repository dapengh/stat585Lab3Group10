#' Get polygon and geographic information from a shapefile that is thinned to a more feasible dimension
#'
#' @param file path to a shapefile (.shp) with spatial geometry data OR a simple features object that contains a geometry column
#' @param tolerance a value greater than 0
#' @param fileread Should the data be read from a file? Default is TRUE, use fileread=FALSE if the object is already a simple features object in the environment
#' @export
#' @return A data frame, with each row corresponding to a unique polygon and its information. The last column ('data') is a dataframe with a set of points defining the extent of the polygon
#' @examples
#' #get data for plotting
#' oz <- ozbig
#'
#' aus <- team_4(file = oz, tolerance = 0.1, fileread=FALSE)
#'
#' head(aus)
#'
#' #the last column contains polygon geometry
#' aus$data[[1]]
#'

team_4 <- function(file, tolerance=0.1, fileread=TRUE){

  if(tolerance <= 0){stop('tolerance less than 0. Choose a different value')}

  st<-file
  if(fileread==T){
    assertthat::assert_that(is.character(file) , assertthat::is.readable(file),
               msg = 'File path not readable')

    st <- read_sf(file)
  }

  if(!is.list(st)){stop('Check file type. Simple features not output as list')}
    spdf <- makedata(st, tolerance=tolerance)
  outdf <- spdf %>% group_by(Country = NAME_0, Name = NAME_1, Type = ENGTYPE_1, HASC = HASC_1, polygon=group) %>%
    nest(long, lat, order)
  if(is.null(dim(outdf)) | !is.data.frame(outdf)) {warning('No data returned. Check geometry object or file path')}

  return(outdf)
}

# Helper function to convert polygon to data frame
poly2df <- function(feature, unlist = T) {
  if (unlist) feature <- unlist(feature, recursive = F)
  lapply(feature, function(x) {
    mutate(rename_all(data.frame(x), ~ c("long", "lat")), order = row_number())
  }) %>% tibble(polygon = .)
}

# Helper function to convert polygon to data frame
makedata <- function(data, thin = T, unlist = T, tolerance=0.1) {
  if (thin == T) {
    oz_st <- maptools::thinnedSpatialPoly(as(data, "Spatial"), tol = tolerance, min = 0.001, topologyPreserve = T)
    oz <- st_as_sf(oz_st)  # install package: rgeos
  } else oz <- data
  as_tibble(oz) %>% mutate(new = map(geometry, poly2df, unlist = unlist)) %>% select(-geometry) %>%
    unnest() %>% mutate(group = row_number()) %>% unnest()
}
