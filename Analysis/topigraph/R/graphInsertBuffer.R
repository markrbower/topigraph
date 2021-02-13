graphInsertBuffer <- function( parameters, cw, cc, ed, gf, blackout, dib_=NULL, state_=NULL, dbName_=NULL, P_table_=NULL ) {
  #' @export
  #' 
  #
  library( igraph )
#  library( Rfast )
  
#  source('~/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/Analysis/NPO/R/aLink.R')
#  source('~/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/Analysis/NPO/R/anEvent.R')
#  source('~/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/Analysis/NPO/R/databaseInsertBuffer.R')
#  load(file='~/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/Analysis/NPO/R/databaseInsertBuffer.Rcmp')
  source('~/Dropbox/Documents/Concepts/2020_08_10_topigraph/topigraph/Analysis/topigraph/R/csv2vec.R')
#  load(file='~/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/Analysis/NPO/R/csv2vec.Rcmp')
#  source('~/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/Analysis/NPO/R/IDv.R')
#  load(file='~/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/Analysis/NPO/R/IDv.Rcmp')
  
  options(stringsAsFactors = FALSE);

# Parse inputs
  if ( "computation_mask" %in% names(parameters) ) {
    mask <- parameters$computation_mask # Try to make this the accepted format
  } else {
    mask <- parameters$computationMask
  }
  dib <- dib_
  if ( is.null(dib) ) { # ... then a size is needed before the graph is dumped.
    dib <- topconnect::databaseInsertBuffer( topconnect::db('testProject'), 'test', c('name','gender','birth'), 3 )
  }
  if ( is.null(dbName_) ) { # ... then that's a problem
    print( "Please supply a database connection" )
    return(-1)
  } else {
    dbName <- dbName_
  }
  state <- state_

  correlationWindow <- cw
  CW <- cw
  storeInGraphWindow <- 1*cw
  persistInDbWindow <- 2*cw

  cc_th <- cc
  ed_th <- ed
  countDoneWith <- 0
  graph_filename <- gf
  
  grph <- graph(edges=NULL,n=NULL,directed=FALSE)

  initialize <- function() {
    # A named list: time=waveform
    events <<- list()
    links <<- list()
  }
  
  insert <- function( event ) {
    # 1. Add to "events" and "links"
    # You want each new event
    # and events that link to it, on the list or the graph.

    # Compute statistics on the center event
    t_event <- as.numeric( names(event) )
    wavevec_event <- unname( unlist( event ) )
    energy_event <- sqrt( sum( wavevec_event[mask] * wavevec_event[mask] ) )

    # node names should be unique
    if ( !( names(event) %in% names(events) ) ) {
#      str_incident_list <- list()
#      str_incident_graph <- list()
#      str_weights_list <- list()
#      str_weights_graph <- list()
      # Form links with other events in the list.
      if ( length(events) > 0 ) {
        # Link to other events in the list.
        te <- unlist( sapply( as.numeric( names(events)), function(x) t_event - x ) )
        possible_links_events <- which( te > blackout & te < CW )
        # link to events
        if ( length(possible_links_events) > 0 ) { # then there is some link to make
          # Compute, filter and link
          all_times <- as.numeric( names( events[possible_links_events] ) )
          wavevec <- t( sapply( events[possible_links_events], function(x) t(unname(as.numeric(unlist(x)))) ) )
          cc <- sapply( seq(1,nrow(wavevec)), function(x) cor( wavevec_event[mask], wavevec[x,mask] ) )
          weights <- sapply( seq(1,length(cc)), function(x) ((1+cc[x])/2) )
          ed <- sapply( seq(1,nrow(wavevec)), function(x) energy_event / sqrt(sum(wavevec[x,mask]*wavevec[x,mask])) )
          idxs <- which( (cc>cc_th | cc<(-1*cc_th)) & ed<ed_th & ed>(1/ed_th) ) # these are idx's in 'possible_links'
          if ( length(idxs) > 0 ) {
            # Add the links
#            str_incident_list <- paste0( as.character(sapply( idxs, function(x) all_times[x])), collapse="," )
#            str_weights_list <- paste0( as.character(sapply( idxs, function(x) weights[x])), collapse="," )
            new_links <- lapply( idxs, function(x) aLink( as.character(t_event), as.character(all_times[x]), weights[x]) )
            links <<- append( links, unlist(new_links) )
          } # length(idxs) > 0, which means there are valid indices to which to link.
        } # length(idxs) > 0, which means there are valid indices to which to link.
      } # length(events)>0, so the events list has elements to consider
      
      # link to graph
      # Form links with events in the graph
      tg <- V(grph)$name[ as.numeric(V(grph)$name) > (t_event-CW) & as.numeric(V(grph)$name) < (t_event-blackout) ]
      if ( length(tg) > 0 ) { # then there is some link to make
        # Compute, filter and link
        wavevec <- sapply( seq(1,length(tg)), function(x) csv2vec(igraph::get.vertex.attribute( grph, 'waveform', IDv(grph,tg[x]))))
#        print( paste0( "nrow of wavevec: ", ncol(wavevec)))
        cc <- sapply( seq(1,ncol(wavevec)), function(x) cor( wavevec_event[mask], wavevec[mask,x] ) )
        weights <- sapply( seq(1,length(cc)), function(x) ((1+cc[x])/2) )
        ed <- sapply( seq(1,ncol(wavevec)), function(x) energy_event / sqrt(sum(wavevec[mask,x]*wavevec[mask,x])) )
        idxs <- which( (cc>cc_th | cc<(-1*cc_th)) & ed<ed_th & ed>(1/ed_th) ) # these are idx's in 'possible_links'
        if ( length(idxs) > 0 ) {
          # Add the links
          weights <- sapply( idxs, function(x) ((1+cc[x])/2) )
#          str_incident_graph <- paste0( as.character(sapply( idxs, function(x) tg[x])), collapse="," )
#          str_weights_graph <- paste0( as.character(sapply( idxs, function(x) weights[x])), collapse="," )
          new_links <- lapply( idxs, function(x) aLink( as.character(t_event), as.character(tg[x]),((1+cc[x])/2)))
          links <<- append( links, unlist(new_links) )
        } # length(idxs) > 0, which means there are valid indices to which to link.
      } # elements with delay > blackout

#      str_incident <- list()
#      if ( length(str_incident_list)>0 ) {
#        str_incident <- append( str_incident, str_incident_list )
#      }
#      if ( length(str_incident_graph)>0 ) {
#        str_incident <- append( str_incident, str_incident_graph )
#      }
#      attr( event, 'incident' ) <- str_incident
#
#      str_weights <- list()
#      if ( length(str_weights_list)>0 ) {
#        str_weights <- append( str_weights, str_weights_list )
#      }
#      if ( length(str_weights_graph)>0 ) {
#        str_weights <- append( str_weights, str_weights_graph )
#      }
#      attr( event, 'weights' ) <- str_weights
      
      # Add new nodes to eventList.
      events <<- append( events, event )
      rm(wavevec_event)
      rm(energy_event)
      
      # 2. When first and last are separated by CW, add nodes and links to the graph.
      # Realize that some events may be duplicates, out of order and/or already be in the graph.
      nE <- length(events)
      delta <- as.numeric(names(events[nE])) - as.numeric(names(events[1]))
      #      print( paste0( "delta: ", delta ) )
      if ( delta > CW ) {
        addEventsAndLinksToGraph( events[order(as.numeric(unlist(names(events))),decreasing=FALSE)] )
      }
      
      # 3. Find nodes that now have a complete CW, both before and after, and compute membership.
      # This is nodes with no membership value that are more than one CW away from the newest addition.
      idx_MMB <- which( V(grph)$name<(t_event-CW) & is.na(V(grph)$membership) )
      if ( length(idx_MMB) > 0 ) {
        computeMembership( idx_MMB )
      }

      # 4. Compute how many nodes will fall off the graph, into the "persist bucket" ("PB"). Call that number "nPB".
      tPersist <- t_event - 3 * correlationWindow
      idx_PB <- which(V(grph)$name<tPersist)
#      print( paste0( "idx_PB: ", length(idx_PB ) ) )
      if ( length(idx_PB) >= dib$updateNumber() ) {
        transferFromGraphToDIB( idx_PB )
      }
      
    } # name is unique
  }
  
  addEventsAndLinksToGraph <- function( events_tmp ) {
    # Add new nodes to the graph and then add the links
    grph_ <- graph(edges=NULL,n=NULL,directed=FALSE) %>%
      add_vertices( length(events_tmp), name=names(events_tmp), membership=NA, waveform=sapply(unname(events_tmp),function(x) paste0(x,collapse=',')), incident=NA, weights=NA )
    # Merge
    # From: https://stackoverflow.com/questions/48000074/combining-merging-two-graphs-in-igraph    
    attrs <- rbind(as_data_frame(grph, "vertices"), as_data_frame(grph_, "vertices")) %>% unique()
    el <- rbind(as_data_frame(grph), as_data_frame(grph_))
    grph <<- graph_from_data_frame(el, directed = FALSE, vertices = attrs)
    grph_ <- NULL
    # Add links
    edge_list <- paste0( names(links), collapse=',')
    if ( nchar(edge_list) > 0 ) {
      edge_vec <- csv2vec( edge_list )
      if ( length(edge_vec) %% 2 != 0 ) {
        print( "Error 1" )
        print( edge_vec )
      }
      id <- IDv( grph, edge_vec )
      #    print( id )
      if ( length(id) %% 2 != 0 ) {
        print( "Error 2" )
        print( id )
      }
      tryCatch({
        #      print( paste0( id ) )
        if ( (length(edge_vec) %% 2 == 0) & (( length(id) %% 2 == 0 )) ) {
          grph <<- igraph::add_edges( grph, id, weight=as.vector(unlist(links)) )
        }
        #      print( paste0( links ) )
        #      grph <<- igraph::set_edge_attr( grph, 'weight', index=E(grph), value=as.vector(unlist(links)) )
      })
    }
    # clear lists
    initialize()
  }
  
  computeMembership <- function( idx_computeMembership ) {
    for ( idx in idx_computeMembership ) {
      tCN <- as.numeric( get.vertex.attribute(grph,'name',idx) )
      # Check that membership has not already been computed
      if ( is.na(get.vertex.attribute( grph, 'membership', idx )) ) {
        #    	create a sub-graph of nodes within the CW of CN
        sub_grph <- igraph::induced_subgraph(grph, ( as.numeric(V(grph)$name)>=(tCN-CW) & as.numeric(V(grph)$name)<=(tCN+CW)),impl="auto")
        #    	compute communities
        sub_cliques <- cluster_louvain( sub_grph, weight=igraph::get.edge.attribute(sub_grph, 'weight' ) )
        # sub_cliques_i <- cluster_infomap( sub_grph, e.weights=igraph::get.edge.attribute(grph, 'weight' ) )
        # sub_cliques_w <- cluster_walktrap( sub_grph, weights=igraph::get.edge.attribute(grph, 'weight' ) 
        # sub_cliques_f <- cluster_fast_greedy( sub_grph, weights=igraph::get.edge.attribute(grph, 'weight' ), membership = TRUE )
        sub_cliques_membership <- membership( sub_cliques )
        clusterid <- sub_cliques_membership[as.numeric(names(sub_cliques_membership))==tCN]
        member_idx <- which( sub_cliques_membership == clusterid )
        grph_clique <- igraph::induced_subgraph(sub_grph, member_idx )
        
        # needed: incident, weights
        edz <- incident( grph_clique, IDv(grph_clique, tCN) )
        enz <- ends( grph_clique, edz )
        weights <- edz$weight
        mat <- cbind( enz, weights )
        keep_idx <- which( as.numeric(mat[,2]) == tCN )
        str_incident <- paste0( mat[keep_idx,1], collapse=',' )
        str_weights <- paste0( round(as.numeric(mat[keep_idx,3]),4), collapse=',' )
        grph <<- igraph::set.vertex.attribute(grph,'incident',IDv(grph,tCN),value=str_incident)
        grph <<- igraph::set.vertex.attribute(grph,'weights',IDv(grph,tCN),value=str_weights)
        
        #    	count votes of nodes with same membership. If 0, use current. If tie, take lowest ID
        clique_memberships <- unlist( get.vertex.attribute(grph_clique,name='membership',index=V(grph_clique)) )
        validids <- seq( 1, length( V(grph_clique) ) )
        if ( !( is.null(clique_memberships) ) & (length( which(!is.na(clique_memberships)) )>0 ) ) { # at lest some of the vertices have memberships. Use these.
          validids <- validids[ which( !is.na(clique_memberships) ) ]
          clusterid <- as.numeric(names(sort(table(clique_memberships[validids],useNA="no"),decreasing=TRUE)[1]))
        } else { # find the next, available clusterid from the database and grph.
          clusterid <- 0
          # Check the databasea
          query <- paste0( "select max(clusterid) as max_database from P;")
          conn <- topconnect::db( dbName )
          rs <- DBI::dbGetQuery( conn, query )
          if ( nrow(rs) > 0 & !is.na(rs$max_database) ) {
            clusterid <- rs$max_database
          }
          DBI::dbDisconnect( conn )
          # Check the graph
          grph_memberships <- unlist( get.vertex.attribute(grph,name='membership',index=V(grph)) )
          if ( !is.null(grph_memberships) & (length( which(!is.na(grph_memberships)) )>0 ) ) {
            max_graph <- max( grph_memberships, na.rm=TRUE )
          } else {
            max_graph <- -1
          }
          clusterid <- max( clusterid, max_graph ) + 1
        }
        grph <<- set.vertex.attribute( grph, 'membership', idx, clusterid )
      }
    }
  }
  
  transferFromGraphToDIB <- function( idx_PersistBucket ) {
    #    	start with  earliest, unassigned node, the Center Node (CN), and move forward until within CW of last node
    for ( id_remove in idx_PersistBucket ) {
      tCN <- as.numeric( str_incident <- igraph::get.vertex.attribute(grph,'name',id_remove) )
      clusterid <- as.numeric( str_incident <- igraph::get.vertex.attribute(grph,'membership',id_remove) )
      # Things needed:
      # waveform, peak, energy
      waveform <- igraph::get.vertex.attribute( grph, 'waveform', id_remove )
      wavevec <- csv2vec( waveform )
      energy <- sqrt( sum( wavevec * wavevec ) )
      peak <- ifelse( abs(max(wavevec)) > abs(min(wavevec)), max(wavevec), min(wavevec) )
      str_incident <- igraph::get.vertex.attribute(grph,'incident',id_remove)
      str_weights <- igraph::get.vertex.attribute(grph,'weights',id_remove)
      
      dib$insert( list(subject=state$subject, UUID=state$session, channel=state$channel, seizureUsed=state$seizureUsed, time=tCN, waveform=waveform, peak=peak, energy=energy, clusterid=clusterid, incident=str_incident, weights=str_weights ) )
    }
    # drop all nodes at once. Clear all memory for dropped nodes and links.
    grph <<- igraph::induced_subgraph(grph, setdiff( V(grph), idx_PersistBucket ),impl="auto" )
  }
  
  flush <- function() {
    # Differs from the 'insert' function by:
    # 1) removing the insert function
    # 2) reversing order of adding nodes to DIB and flusing events + links.

    # flush the events and links lists
    addEventsAndLinksToGraph( events[order(as.numeric(unlist(names(events))),decreasing=FALSE)] )
    
    # Add vertices to the DIB.
    idx_PB <- seq( 1, length(V(grph)) )
    computeMembership( idx_PB )
    transferFromGraphToDIB( idx_PB )
    
    # flush the DIB
    dib$flush()
    
  }
  
  findClusterid <- function( g, t ) {
    # Assume all vertices have the same clique number as 't'.
    # Find those vertices that are before 't',
    # find their clusterid in the db,
    # then find the clusterid that accounts for the most vertices.
    clusterid <- NA
    ancestors <- V(g)$name[ V(g)$name < t ]
    time_string <- paste0( ancestors, collapse=' OR time=')
    query <- paste0( 'select clusterid from ', P_table, ' where time=', time_string )
    conn <- topconnect::db( dbName )
    rs <- DBI::dbGetQuery( conn, query )
    if ( nrow(rs) > 0 ) {
      clusterid <- as.numeric(names(sort(table(rs$clusterid),decreasing=TRUE)[1]))
    }
    if ( is.na(clusterid) ) { # Assign this community the next, new ID
      query <- paste0( "select max(clusterid) as max_id from ",  P_table, ";" )
      rs <- DBI::dbGetQuery( conn, query )
      clusterid <- as.numeric( rs$max_id ) + 1
    }
    DBI::dbDisconnect( conn )
    return( clusterid )
  }


  loadListsFromGraph <- function( grph ) {
    # Load events from the graph into the events list
    tmp_waveforms <- igraph::get.vertex.attribute( grph, 'waveform' )
    tmp_names <- igraph::get.vertex.attribute(grph,'name')
    events <<- list()
    # events <<- mapply( function(x,y) append( events, anEvent(x,y) ), tmp_names, tmp_waveforms ) # doesn't work why?
    for ( idx in seq(1,length(tmp_names) ) ) {
      events <<- append( events, anEvent(tmp_names[idx],tmp_waveforms[idx]) )
    }
#    event_ <- anEvent( utc, wvfrm )
#    events <<- append( events, event )

    # Load links from the graph into the links list
    weights <- as.list( igraph::get.edge.attribute( grph, 'weight' ) )
    endz <- ends( grph, E(grph), names=TRUE )
    links <<- list()
    n1 <- igraph::get.vertex.attribute( grph, 'name', endz[,1])
    n2 <- igraph::get.vertex.attribute( grph, 'name', endz[,2])
    # links <<- mapply( function(x,y,z) append( links, aLink( x, y, z ) ), n1, n2, weights ) # Doesn't work why?
    for ( idx in seq(1,length(n1)) ) {
      links <<- append( links, aLink( n1[idx], n2[idx], weights[idx] ) )
    }
  }
  
  max_graph_time <- function() {
    if ( gsize(grph) == 0 ) {
      value <- 0
    } else {
      value <-  max( as.numeric( igraph::get.vertex.attribute(grph,'name') ) )
    }
    return( value )
  }
  
  toString <- function() {
    print( events )
    print( links )
  }
  
  getEvents <- function() {
    return( events )
  }
  
  getLinks <- function() {
    return( links )
  }
  
  export <- function() {
    return( grph )
  }
  
  # This part of object creation is placed at the end so that the function 'loadListsFromGraph()' has been defined and can be found.
  if ( file.exists(file=graph_filename) ) {
    print( paste0( "Reading graph: ", graph_filename ) )
    grph <- read_graph( file=graph_filename, format='graphml' )
    print( "Reading graph - done" )
    #
    # Update the graphInsertBuffer to reflect this graph
    #
    loadListsFromGraph( grph )
    print( 'Done')
    #
  } else {
    grph <- graph(edges=NULL,n=NULL,directed=FALSE)
  }
  
  obj <- list(initialize=initialize,insert=insert,loadListsFromGraph=loadListsFromGraph,getEvents=getEvents,getLinks=getLinks,flush=flush,max_graph_time=max_graph_time,toString=toString,export=export)
  class(obj) <- c('graphInsertBuffer')
  initialize()
  return( obj )
}




