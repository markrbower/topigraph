aLink <- function( time1, time2, cc ) {
  #' @export
  options(scipen=999)
  link <- list( cc )
  names(link) <- as.character( paste0( format(time1, scientific = FALSE), ',', format(time2, scientific = FALSE) ) )
  return( link )  
}
