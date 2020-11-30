csv2vec <- function( csv ) {
  library( stringr )
  vec <- as.numeric( unlist( str_split( csv, ',' ) ) )
  return( unlist(vec) )
}
