anEvent <- function( time, wave ) {
  #' @export
  if ( class(wave) == 'character' ) {
    wave <- csv2vec( wave )
  }
  event <- list( wave )
  names(event) <- as.character( format(time, scientific = FALSE) )
  return(event)  
}
