#' Helper function to convert polygon to data frame
makedata <- function(data, thin = T, unlist = T, tolerance=0.1) {
  if (thin) {
    oz_st <- maptools::thinnedSpatialPoly(as(data, "Spatial"), tol = tolerance, min = 0.001, topologyPreserve = T)
    oz <- st_as_sf(oz_st)  # install package: rgeos
  } else oz <- data
  as_tibble(oz) %>% mutate(new = map(geometry, poly2df, unlist = unlist)) %>% select(-geometry) %>%
    unnest() %>% mutate(group = row_number()) %>% unnest()
}
