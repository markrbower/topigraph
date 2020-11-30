IDv <- function( g, target, attrval='name' ) {
  #' @export
  # IDv returns a list containing the index of each 'target' value in the graph, 'g'.
  # To test whether ANY of the 'name' values was NOT in the graph, use 'any(sapply(idx,function(x) length(x))==0)'.
  # For example, see 'notInGraph.R'.
  
  library(igraph)
  
  graph_values <- as.numeric( vertex_attr(g,attrval) )
  gidx <- unname( sapply( target, function(x) which( graph_values == x ) ) )
  if ( length(gidx) != length(target) ) {
    print( "Error in IDv. Length of target: ", length(target), "\tlength of idx: ", length(idx) )
  }
  return( gidx )
}
