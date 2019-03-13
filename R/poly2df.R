#' Helper function to convert polygon to data frame
poly2df <- function(feature, unlist = T) {
  if (unlist) feature <- unlist(feature, recursive = F)
  lapply(feature, function(x) {
    mutate(rename_all(data.frame(x), ~ c("long", "lat")), order = row_number())
  }) %>% tibble(polygon = .)
}
