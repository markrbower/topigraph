graphReconstructBuffer <- function( conn, P_table, subject, channel, seizureUsed ) {
  #' @export
  #' 
  #
  
  source('~/Dropbox/Documents/Concepts/2020_08_10_topigraph/topigraph/Analysis/topigraph/R/csv2vec.R')
  
  options(stringsAsFactors = FALSE);
  
  reconstruct <- function( clusterid ) {
    query <- paste0( "select time, waveform, incident, weights from ", P_table, " where" )
    query <- paste0( query, paste0(" subject=\'", subject, "\' AND " ) )
    query <- paste0( query, paste0(" channel=\'", channel, "\' AND " ) )
    query <- paste0( query, paste0(" clusterid=\'", clusterid, "\' AND " ) )
    query <- paste0( query, paste0(" seizureUsed=", seizureUsed, ";" ) )
    rs <- DBI::dbGetQuery( conn, query )
    
    grph <<- igraph::graph(edges=NULL,n=NULL,directed=FALSE) %>%
      igraph::add_vertices( nrow(rs), name=rs$time, membership=clusterid, waveform=rs$waveform )
    
    # Use 'incident' and 'weights' to remake the links?
    edz_vec <- csv2vec( rs$incident )
    edz <- IDv( grph, csv2vec( paste0( paste0( edz_vec, collapse=paste0(',',rs$time,',')), ',', rs$time ) ) )
#    edz <- matrix( edz, ncol=2, byrow=TRUE )
#    edz <- cbind( edz, csv2vec( rs$weights ) )
    grph <<- igraph::add.edges( grph, edz, csv2vec(rs$weights) )
    
    grph <<- igraph::simplify(grph)
  }

  export <- function() {
    return( grph )
  }
  
  obj <- list(reconstruct=reconstruct,export=export)
  class(obj) <- c('graphReconstructBuffer')
  return( obj )
}




