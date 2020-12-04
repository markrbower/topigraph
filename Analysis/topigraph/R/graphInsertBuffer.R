graphInsertBuffer <- function( parameters, cw, cc, ed, gf, blackout, dib_=NULL, state=NULL ) {
  #' @export
  #' 
  # Keep a list of events and links. Note those that have slipped beyond the correlation window.
  # When enough have, move "slipped" events to nodes and their links to edges.
  # Find nodes that have slipped beyond the persist-limit, compute membership and persist nodes and membership.
  #
  # Arguments:
  # grph:
  # limit:
  #
  # Lists contain events and links.
  # Graphs contain nodes and edges.
  #
  
  # Test:
#  e1 <- anEvent( 123, c(7,10,9))
#  e2 <- anEvent( 234, c(10,12,11))
#  e3 <- anEvent( 345, c(9,12,10))
#  e4 <- anEvent( 456, c(4,0,2))
#  e5 <- anEvent( 567, c(5,2,4))
#  parameters <- list(computationMask=c(1,2,3))
#  gib <- graphInsertBuffer( parameters, 200, .9, 1.5, 'tmp.xml', 20 ); gib$insert(e1); gib$insert(e2); gib$insert(e3); gib$insert(e4); gib$insert(e5)
#  grph <- gib$makeGraph()
  # print( grph )
  # plot( grph )
  # subgrph <- gib$processSubgraph(0,400)
  # plot( subgrph )
  # grph <- gib$makeGraph()
  # plot( grph )

  library( igraph )
#  library( Rfast )
  
#  source('~/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/Analysis/NPO/R/aLink.R')
#  source('~/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/Analysis/NPO/R/anEvent.R')
#  source('~/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/Analysis/NPO/R/databaseInsertBuffer.R')
#  load(file='~/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/Analysis/NPO/R/databaseInsertBuffer.Rcmp')
#  source('~/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/Analysis/NPO/R/csv2vec.R')
#  load(file='~/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/Analysis/NPO/R/csv2vec.Rcmp')
#  source('~/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/Analysis/NPO/R/IDv.R')
#  load(file='~/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/Analysis/NPO/R/IDv.Rcmp')
  
  options(stringsAsFactors = FALSE);
  
  correlationWindow <- cw
  storeInGraphWindow <- 1*cw
  persistInDbWindow <- 2*cw

  cc_th <- cc
  ed_th <- ed
  countDoneWith <- 0
  graph_filename <- gf
  
#  print( graph_filename )
  
  if ( "computation_mask" %in% names(parameters) ) {
    mask <- parameters$computation_mask # Try to make this the accepted format
  } else {
    mask <- parameters$computationMask
  }
  # How to find the name of "P_table"?!

  # Handle databaseInsertBuffer, updateNumber or default graph size.
  dib <- dib_
  if ( is.null(dib) ) { # ... then a size is needed before the graph is dumped.
    dib <- topconnect::databaseInsertBuffer( topconnect::db('testProject'), 'test', c('name','gender','birth'), 3 )
  }
  
  initialize <- function() {
    # A named list: time=waveform
    events <<- list()
    links <<- list()
  }    

#  insert <- function( event, utc, state, dib ) { # event is a list of time and waveform-as-string
  insert <- function( event ) { # event is a list of time and waveform-as-string
    # Can I set this up so that 'dib' is optional?
    
    # node names should be unique
    if ( !( names(event) %in% names(events) ) ) {
      # Unpack existing events
      t_event <- as.numeric( names(event) )
      if ( length(events) > 0 ) {
        td <- unlist( sapply( as.numeric( names(events)), function(x) t_event - x ) )
        if (max(td) > blackout ) { # then there is some link to make
          # print( "In" )
          wavevec_event <- unname( unlist( event ) )
          energy_event <- sqrt( sum( wavevec_event[mask] * wavevec_event[mask] ) )
        
          # Find events within the correlation window
          linkto_idx <- which( td > blackout & td < correlationWindow )
          if ( length(linkto_idx) > 0 ) {
            # Compute, filter and link
            all_times <- as.numeric( names( events[linkto_idx] ) )
            wavevec <- t( sapply( events[linkto_idx], function(x) t(unname(as.numeric(unlist(x)))) ) )
            cc <- sapply( seq(1,nrow(wavevec)), function(x) cor( wavevec_event[mask], wavevec[x,mask] ) )
            ed <- sapply( seq(1,nrow(wavevec)), function(x) energy_event / sqrt(sum(wavevec[x,mask]*wavevec[x,mask])) )
            idxs <- which( (cc>cc_th | cc<(-1*cc_th)) & ed<ed_th & ed>(1/ed_th) )
            if ( length(idxs) > 0 ) {
              new_links <- lapply( idxs, function(x) aLink( as.character(t_event), as.character(all_times[x]), ((1+cc[x])/2) ) )
  #            for ( idx in idxs ) {
  #              al <- aLink( as.character(t_event), as.character(all_times[idx]), ((1+cc[idx])/2) )
  #              links <<- append( links, al )
  #            }
              links <<- append( links, unlist(new_links) )
              edge_list <- paste0( names(links), collapse=',')
              edge_vec <- csv2vec( edge_list )
              if ( length(edge_vec) %% 2 != 0 ) {
                print( "Error on insert 1" )
                print( edge_vec )
              }
              if ( length(links)%%100 == 0 ) {
                print( paste0( "Nlinks: ", length(links) ) )
              }
            }
          }
        } # max(td) > blackout
      } # comparison to existing events
      # Append new event
      events <<- append( events, event )
      if ( length(events)%%100 == 0 ) {
        print( paste0( "Nevents: ", length(events) ) )
      }
      
      # Determine if an "update" is needed.
      update( as.numeric( names(event) ) )
    }
    # print( "Leaving insert" )
  }

  eventList <- function() {
    return( events )
  }
  
  linkList <- function() {
    return( links )
  }
  
  makeGraph <- function( tmp_storeInGraphWindow, tmp_persistInDbWindow ) {
    tmp1 <- storeInGraphWindow
    storeInGraphWindow <<- tmp_storeInGraphWindow
    tmp2 <- persistInDbWindow
    persistInDbWindow <<- tmp_persistInDbWindow
    appendToGraph()
    storeInGraphWindow <<- tmp1
    persistInDbWindow <<- tmp2
  }
  
  appendToGraph <- function() { # Add new events and links from the lists to the graph
    #
    # Only append events and create links for latencies greater than one correlation window.
    #
    t0 <- max(as.numeric(names(events)))
    t_graphWindow <- t0 - storeInGraphWindow
    t_storeWindow <- t0 - persistInDbWindow
    # Events to store in graph
    event_times <- as.numeric( names(events) )
    keep_events <- events[which( event_times >= t_graphWindow )]
    events_ <- events[ which( event_times < t_graphWindow )]

    # Which links to keep?
    # If the times are stored [larger,smaller], then all of the aLink entries that follow
    # the first aLink whose first time will be in the graph.
    firstTimes <- unname( sapply( names(links), function(x) as.numeric( unlist( str_split(x,',' ) ) )[1] ) )
    keep_links <- links[which(firstTimes >= t_graphWindow )]
    links_ <- links[which(firstTimes < t_graphWindow )]

    if ( length(links_) > 0 ) {
      #    print( paste0( 'Making a graph with ', length(events), ' nodes and ', length(links), ' edges.' ) )
      grph_ <- graph(edges=NULL,n=NULL,directed=FALSE)
      grph_ <- igraph::add_vertices( grph_, length(events_))
      grph_ <- igraph::set_vertex_attr( grph_, 'name', V(grph_), names(events_) )
      grph_ <- igraph::set_vertex_attr( grph_, 'waveform', V(grph_), sapply( unname(events_), function(x) paste0(x,collapse=',')) )
      
      #grph <<- grph + grph_
      # From: https://stackoverflow.com/questions/48000074/combining-merging-two-graphs-in-igraph    
      attrs <- rbind(as_data_frame(grph, "vertices"), as_data_frame(grph_, "vertices")) %>% unique()
      el <- rbind(as_data_frame(grph), as_data_frame(grph_))
      new_g <- graph_from_data_frame(el, directed = FALSE, vertices = attrs)
      grph <<- new_g
  
      edge_list <- paste0( names(links_), collapse=',')
      edge_vec <- csv2vec( edge_list )
      if ( length(edge_vec) %% 2 != 0 ) {
        print( "Error 1" )
        print( edge_vec )
      }
      id <- unlist( IDv( grph, edge_vec, 'name' ) )
      if ( length(id) %% 2 != 0 ) {
        print( "Error 2" )
        print( id )
      }
      tryCatch({
        grph <<- igraph::add_edges( grph, id )
        grph <<- igraph::set_edge_attr( grph, 'weight', index=E(grph), value=as.vector(unlist(links_)) )
      })
      
      events <<- keep_events
      links <<- keep_links
      return( 1 )
    } else {
      return( 0 )
    }
  }
  
  processSubgraph <- function( Nlimit, append=TRUE ) {
    # What happens when an event moves out of the storage window?
    
    if ( append == TRUE ) {
      graphChanged <- appendToGraph()
    }
    
    if ( !graphChanged ) {
      return(0)
    }
    
    gr_labels <- as.numeric( unlist(igraph::get.vertex.attribute( grph, 'name' ) ) )
    persist_idx <- which( gr_labels <= Nlimit )
    persist_labels <- gr_labels[persist_idx]

    # Find communities
    gr_cliques <- cluster_louvain( grph, weight=igraph::get.edge.attribute(grph, 'weight' ) )
#    gr_cliques_i <- cluster_infomap( grph, e.weights=igraph::get.edge.attribute(grph, 'weight' ) )
#    gr_cliques_w <- cluster_walktrap( grph, weights=igraph::get.edge.attribute(grph, 'weight' ) )
#    gr_cliques_f <- cluster_fast_greedy( grph, weights=igraph::get.edge.attribute(grph, 'weight' ), membership = TRUE )

    # Check if these times are already persisted. If so, then update membership IDs.
    # 5 steps: query existing times, match old cliques to new, fill in non-matching cliques, re-map existing cliques, persist.
    #
    # New idea: 1. Add the new member and drop too-distal members
    #           2. Persist if there are enough in the DIB (DIB handles that internally)
    #           3. Check communities after each addition and just change the label of the newbie.
    #
    # 1. query existing times
    #
    # If "dib" is defined, then that should be used here.
    # Otherwise, you run out of database connections.
    #
    if ( !( exists("conn") && !is.null(conn) ) ) {
      conn <- topconnect::db('testProject')
    }
    if ( !( exists("P_table") && !is.null(P_table) ) ) {
      P_table <- "P"
    }
    time_string <- paste0( gr_labels, collapse=' OR time=')
    query <- paste0( 'select time,clusterid from ', P_table, ' where time=', time_string, ';' )
    db_rs <- DBI::dbGetQuery( conn, query )

    # Persist these memberships, parameters and ancestors.
    # Check that the migration of old group numbers into new is correct.
    #
    gr_cliques_unique_id <- unique( gr_cliques$membership )
    gr_nonmatching_cliques <- list()
    lookup_table <- vector( mode="integer", length=max(gr_cliques_unique_id) )
    for ( gr_id in gr_cliques_unique_id ) {
      # 2. match old cliques to new
      gr_labels_subset <- gr_labels[ which( gr_cliques$membership == gr_id ) ]
      db_matching_idx <- which( db_rs$time %in% gr_labels_subset )
      if ( length(db_matching_idx>0) ) {
        db_table <- table( db_rs$clusterid[db_matching_idx] )
        max_db_table_idx <- which( db_table == max(db_table) )
        lookup_table[gr_id] <- names(db_table[max_db_table_idx[1]])
      } else {
        gr_nonmatching_cliques <- append( gr_nonmatching_cliques, gr_id )
      }
    }
    gr_nonmatching_cliques <- unlist( gr_nonmatching_cliques )
    # 3. fill in non-matching cliques
    if ( length(gr_nonmatching_cliques) > 0 ) {
      if ( exists("state") ) {
        query <- paste0( "select max(clusterid) as M from ", P_table, " where subject=\'", state$subject, "\' and channel=\'", state$channel, "\' and seizureUsed=", state$seizureUsed, ";" )
      } else {
        query <- paste0( "select max(clusterid) as M from ", P_table, ";" )
      }
      print( query )
      rs <- DBI::dbGetQuery( conn, query )
      if ( !is.na(rs$M) ) {
        M <- rs$M
      } else {
        M <- 0
      }
      for ( gr_id in gr_nonmatching_cliques ) {
        M <- M + 1
        lookup_table[gr_id] <- M
      }
    }
    # 4. re-map existing cliques
    query <- paste0( 'insert into ', P_table, ' ')
    for ( gr_id in gr_cliques_unique_id ) {
      # This assumes 'ncnames' have same order as 'gr_cliques$membership'
      # Is this always true?
      gr_labels_subset <- gr_labels[ which( gr_cliques$membership == gr_id ) ]
      gr_labels_subset <- gr_labels_subset[ which( gr_labels_subset < Nlimit ) ]
      for ( gr_label in gr_labels_subset ) {
        # Need waveform, energy, peak and ancestors
        idv <- IDv( grph, gr_label, 'name' )
        waveform <- igraph::get.vertex.attribute( grph, 'waveform', idv )
        wavevec <- csv2vec( waveform )
        energy <- sqrt( sum( wavevec * wavevec ) )
        peak <- ifelse( abs(max(wavevec)) > abs(min(wavevec)), max(wavevec), min(wavevec) )
        ends_on <- incident( grph, idv )
        if (length(ends_on) > 0 ) {
          edge_weights <- unlist( igraph::get.edge.attribute( grph, 'weight', ends_on ) )
#          names(edge_weights) <- edges_on
#          weights_on <- sort( edge_weights, decreasing=TRUE )
#          Nedges_on <- min( 10, length(edges_on) )
#          ends_on <- ends( grph, names(weights_on[1:Nedges_on]) )
#          ancestors <- ends_on[,2]
          enz <- ends( grph, ends_on )
          vec_incident <- apply( enz, 1, function(x) setdiff(x,gr_label) )
          str_incident <- paste0( igraph::get.vertex.attribute( grph, "name", vec_incident), collapse="," )
#          print( str_incident )
          str_weights <- paste0( edge_weights, collapse="," )
          if ( !is.null(dib) ) {
            if ( exists("state") ) {
              dib$insert( list(subject=state$subject, UUID=state$session, channel=state$channel, seizureUsed=state$seizureUsed, time=as.numeric(gr_label), waveform=waveform, peak=peak, energy=energy, clusterid=lookup_table[gr_id], incident=str_incident, weights=str_weights ) )
            } else {
              dib$insert( list(subject="000", channel="000", seizureUsed=0, time=as.numeric(gr_label), waveform=waveform, peak=peak, energy=energy, clusterid=lookup_table[gr_id], incident=str_incident, weights=str_weights ) )
            }              
          }
        }
      }
    }

    # find the remaining subgraph and get it's events and links
    # keep out to two correlation windows of graph
    countDoneWith <<- countDoneWith + length( persist_idx )

    # Delete removed nodes, don't keep nodes. No need to update 'events' or 'links', because these aren't in the graph, yet.
    #
    before <- length( E(grph) )
    remgrph <- igraph::delete_vertices( grph, persist_idx )
    after <- length( E(remgrph) )
    print( paste0( "before: ", before, "\tafter: ", after ) )

    # This is a mistake! The [events,links] data structure holds only unadded data!
    # The function 'appendToGraph' manages the lists, moves them into a graph and clears them.
#    tmp_events <<- igraph::get.vertex.attribute( remgrph, 'waveform' )
#    events <<- lapply( tmp_events, function(x) csv2vec(x) )
#    names(events) <<- igraph::get.vertex.attribute(remgrph,'name')
#    m <- state$seizureUsed - min( as.numeric( names(events) ) )
#    M <- state$seizureUsed - max( as.numeric( names(events) ) )
#    links <<- as.list( igraph::get.edge.attribute( remgrph, 'weight' ) )
#    endz <- ends( remgrph, E(remgrph), names=TRUE )
#    ts <- names(events)
#    names(links) <<- mapply( function(x,y) paste0(x,',',y), ts[endz[,1]], ts[endz[,2]] )
#    initialize() # This is old. From back when all vertices were entered into the graph.

    # In case of crash, save graph to disk here.
    # Why aren't the vertex waveforms and edge weights being saved?!
    # print( state$graph_filename )
    # print( paste0( "Waveform: ", igraph::get.vertex.attribute( remgrph, 'waveform', 1 ) ) )
    # print( paste0( "Weight  : ", igraph::get.edge.attribute( remgrph, 'weight', 1 ) ) )
    #print( paste0( "Writing graph to: ", graph_filename ) )
    write_graph( remgrph, file=graph_filename, 'graphml' )
    #print( "Writing graph - done" )
    # print( "Done writing" )

    # Return
#    rm( grph )
#    gc()
    grph <<- remgrph
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
  
  sizeWithinWindow <- function( Nstart, Nstop ) {
    labels <- names( events )
    idx <- which( labels >= Nstart  & labels <= Nstop )
    return( length(idx) )
  }
  
  sizeLessThan <- function( Nlimit ) {
    labels <- as.numeric( names( events ) )
    gr_labels <- igraph::get.vertex.attribute( grph, 'name' )
#    print( paste0( min( labels-Nlimit ) ) )
    targets <- union( labels[labels < Nlimit], gr_labels[gr_labels < Nlimit] )
    print( min(labels-Nlimit) )
    return( length(targets) )
  }
  
  size <- function() {
    # print( "In size" )
    labels <- names( events )
    return( length(labels) )
  }
  
  flush <- function() {
    # Process all nodes
    grph <- makeGraph()
    return( grph )
  }
  
  max_graph_time <- function() {
    if ( gsize(grph) == 0 ) {
      value <- 0
    } else {
      value <-  max( as.numeric( igraph::get.vertex.attribute(grph,'name') ) )
    }
    return( value )
  }
  
  update <- function( utc ) {
    # Handles the databaseInsertBuffer
    # How to make the "3*..." more visible? How to make the whole "plan" more cohesive?
    N <- sizeLessThan( (utc-persistInDbWindow)  )
    L <- length( links )
#    print( paste0( utc, ' ', N, ' ', L ) )
    if ( !is.null(dib) ) {
      if ( N >= dib$updateNumber() ) {
        processSubgraph( (utc-persistInDbWindow), TRUE )
      }
    } else {
      processSubgraph( (utc-persistInDbWindow), TRUE )
    }
  }
  
  persistGraph <- function( state ) { # Send the entire, current graph to the database.
    max_event_time <- max( as.numeric( names(events) ) )  # This will not work. How to make it work?
    makeGraph( -1, -1 ) # This runs 'appendtoGraph' with -1 latency to get all events.
    state <- processSubgraph( max_event_time+1, state, FALSE ) # 'append' is FALSE, beacuse 'makeGraph' appended, already.
    return( state )
  }
  
  toString <- function() {
    print( events )
    print( links )
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
  
  obj <- list(initialize=initialize,insert=insert,events=eventList,links=linkList,makeGraph=makeGraph,appendToGraph=appendToGraph,processSubgraph=processSubgraph,loadListsFromGraph=loadListsFromGraph,sizeWithinWindow=sizeWithinWindow,sizeLessThan=sizeLessThan,size=size,flush=flush,max_graph_time=max_graph_time,update=update,persistGraph=persistGraph,toString=toString,export=export)
  class(obj) <- c('graphInsertBuffer')
  initialize()
  return( obj )
}
