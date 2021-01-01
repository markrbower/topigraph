test_that("creating anEvent works",{
  e1 <- anEvent( 123, 10 )
  expect_equal( names(e1), "123" )
  v <- unname(unlist(e1["123"]))
  expect_equal( v, 10 )
  e2 <- anEvent( 234, 20 )
  events_ <- list( e1, e2 )
  expect_equal( length(events_), 2 )
})

test_that("adding anEvent to a graph works",{
  e1 <- anEvent( 123, 10 )
  e2 <- anEvent( 234, 20 )
  events_ <- list( e1, e2 )
  g <- igraph::graph.empty()
  g <- igraph::add_vertices( g, length(events_) )
  expect_equal( igraph::gorder(g), 2 )
})

test_that("creating aLink works",{
  k1 <- aLink( 123, 234, .4 )
  expect_equal( names(k1), "123,234" )
})

test_that("adding aLink to a graph works",{
  e1 <- anEvent( 123, 10 )
  e2 <- anEvent( 234, 20 )
  events_ <- list( e1, e2 )
  k1 <- aLink( 123, 234, .4 )
  links_ <- list( k1 )
  g <- igraph::graph.empty()
  g <- igraph::add_vertices( g, length(events_) )
  g <- igraph::set_vertex_attr( g, 'name', igraph::V(g), names(unlist(events_)) )
  g <- igraph::add.edges( g, c(1,2) )
  expect_equal( igraph::ecount(g), 1 )
})

test_that("IDv works",{
  e1 <- anEvent( 123, 10 )
  e2 <- anEvent( 234, 20 )
  events_ <- list( e1, e2 )
  g <- igraph::graph.empty()
  g <- igraph::add_vertices( g, length(events_) )
  g <- igraph::set_vertex_attr( g, 'name', igraph::V(g), names(unlist(events_)) )
  v1 <- IDv(g,"123")  
  expect_equal( v1, 1 )
  v2 <- IDv(g,"234")  
  expect_equal( v2, 2 )
})

test_that("IDv for attributes works",{
  e1 <- anEvent( 123, 10 )
  e2 <- anEvent( 234, 20 )
  events_ <- list( e1, e2 )
  g <- igraph::graph.empty()
  g <- igraph::add_vertices( g, length(events_) )
  g <- igraph::set_vertex_attr( g, 'name', igraph::V(g), names(unlist(events_)) )
  g <- igraph::set_vertex_attr( g, 'value', V(g), sapply( unname(events_), function(x) paste0(x,collapse=',')) )
  attr <- igraph::get.vertex.attribute( g, 'value' )
  expect_equal( attr[2], "20" )
})

test_that("graphInsertBuffer insert works",{
  e1 <- anEvent( 123, c(7,10,9))
  parameters <- list(computationMask=c(1,2,3))
  conn <- topconnect::db("testProject")
  if ( file.exists('tmp.xml' ) ) {
    file.remove( 'tmp.xml' )
  }
  gib <- graphInsertBuffer( parameters, 200, .9, 1.5, 'tmp.xml', 2, dib_=NULL,state=NULL, conn=conn )
  gib$insert(e1)
  e <- gib$getEvents()
  expect_equal( length(e), 1)
})

test_that("adding events to the graph works",{
  if ( file.exists('tmp.xml' ) ) {
    file.remove( 'tmp.xml' )
  }
  parameters <- list(computationMask=c(1,2,3))
  fields <- c("subject","UUID","channel","seizureUsed","time","waveform","clusterid","peak", "energy", "incident", "weights" )
  dib <- topconnect::databaseInsertBuffer(conn,"P",fields,limit=5,updates=NULL)
  state <- list(subject='bb8', session='02563349-e614-4a87-8d25-242496dcab8d', channel="A1", seizureUsed=1000 )
  conn <- topconnect::db("testProject")
  gib <- graphInsertBuffer( parameters, cw=200, cc=.8, ed=1.5, gf='tmp.xml', blackout=2, dib_=dib,state=state, conn=conn )
  
  e1 <- anEvent( 121, c(7,10,9) )
  e2 <- anEvent( 232, c(11,12,13) )
  e3 <- anEvent( 243, c(11,12,14) )
  gib$insert(e1);gib$insert(e2);gib$insert(e3)
  E <- gib$getEvents()
  expect_equal( length(E), 3 )
  L <- gib$getLinks()
  expect_equal( length(L), 1 )
  grph <- gib$export()
  expect_equal( length(V(grph)), 0 )
  expect_equal( length(E(grph)), 0 )
  e4 <- anEvent( 304, c(7,10,9) )
  e5 <- anEvent( 365, c(11,12,11) )
  e6 <- anEvent( 386, c(11,14,13) )
  gib$insert(e4);gib$insert(e5);gib$insert(e6)
  e7 <- anEvent( 407, c(7,10,10) )
  e8 <- anEvent( 468, c(11,11,13) )
  e9 <- anEvent( 489, c(11,10,15) )
  gib$insert(e7);gib$insert(e8);gib$insert(e9)
  E <- gib$getEvents()
  expect_equal( length(E), 4 )
  L <- gib$getLinks()
  expect_equal( length(L), 5 )
  grph <- gib$export()
  expect_equal( length(V(grph)), 5 )
  expect_equal( length(E(grph)), 2 )
})

test_that("adding elements to database works",{
  if ( file.exists('tmp.xml' ) ) {
    file.remove( 'tmp.xml' )
  }
  conn <- topconnect::db("testProject")
  query <- 'truncate P;'
  rs <- DBI::dbGetQuery( conn, query )
  
  parameters <- list(computationMask=c(1,2,3))
  fields <- c("subject","UUID","channel","seizureUsed","time","waveform","clusterid","peak", "energy", "incident", "weights" )
  dib <- topconnect::databaseInsertBuffer(conn,"P",fields,limit=2,updates=NULL)
  state <- list(subject='bb8', session='02563349-e614-4a87-8d25-242496dcab8d', channel="A1", seizureUsed=1000 )
  gib <- graphInsertBuffer( parameters, cw=200, cc=.8, ed=1.5, gf='tmp.xml', blackout=2, dib_=dib,state=state, conn=conn )
  
  e1 <- anEvent( 121, c(7,10,9) )
  e2 <- anEvent( 232, c(11,12,13) )
  e3 <- anEvent( 243, c(11,12,14) )
  gib$insert(e1);gib$insert(e2);gib$insert(e3)
  e4 <- anEvent( 304, c(7,10,9) )
  e5 <- anEvent( 365, c(11,12,11) )
  e6 <- anEvent( 386, c(11,14,13) )
  gib$insert(e4);gib$insert(e5);gib$insert(e6)
  e7 <- anEvent( 407, c(7,10,10) )
  e8 <- anEvent( 468, c(11,11,13) )
  e9 <- anEvent( 839, c(11,10,15) ) # Store/drop e1 & e2. Should activate database.
  gib$insert(e7);gib$insert(e8)
  gib$insert(e9)
  query <- 'select * from P;'
  rs <- DBI::dbGetQuery( conn, query )
  expect_equal( nrow(rs), 2 )
  e10 <- anEvent( 940, c(11,10,17) )
  gib$insert(e10)
  query <- 'select * from P;'
  rs <- DBI::dbGetQuery( conn, query )
  expect_equal( nrow(rs), 4 )
})

test_that("flushing the database works",{
  if ( file.exists('tmp.xml' ) ) {
    file.remove( 'tmp.xml' )
  }
  conn <- topconnect::db("testProject")
  query <- 'truncate P;'
  rs <- DBI::dbGetQuery( conn, query )
  
  parameters <- list(computationMask=c(1,2,3))
  fields <- c("subject","UUID","channel","seizureUsed","time","waveform","clusterid","peak", "energy", "incident", "weights" )
  dib <- topconnect::databaseInsertBuffer(conn,"P",fields,limit=2,updates=NULL)
  state <- list(subject='bb8', session='02563349-e614-4a87-8d25-242496dcab8d', channel="A1", seizureUsed=1000 )
  gib <- graphInsertBuffer( parameters, cw=200, cc=.8, ed=1.5, gf='tmp.xml', blackout=2, dib_=dib,state=state, conn=conn )
  
  e1 <- anEvent( 121, c(7,10,9) )
  e2 <- anEvent( 232, c(11,12,13) )
  e3 <- anEvent( 243, c(11,12,14) )
  gib$insert(e1);gib$insert(e2);gib$insert(e3)
  e4 <- anEvent( 304, c(7,10,9) )
  e5 <- anEvent( 365, c(11,12,11) )
  e6 <- anEvent( 386, c(11,14,13) )
  gib$insert(e4);gib$insert(e5);gib$insert(e6)
  e7 <- anEvent( 407, c(7,10,10) )
  e8 <- anEvent( 468, c(11,11,13) )
  e9 <- anEvent( 839, c(11,10,15) ) # Store/drop e1 & e2. Should activate database.
  gib$insert(e7);gib$insert(e8)
  gib$insert(e9)
  query <- 'select * from P;'
  rs <- DBI::dbGetQuery( conn, query )
  expect_equal( nrow(rs), 2 )
  e10 <- anEvent( 940, c(11,10,17) )
  gib$insert(e10)
  rs <- DBI::dbGetQuery( conn, query )
  expect_equal( nrow(rs), 4 )
  
  gib$flush()
  grph <- gib$export()
  expect_equal( length(V(grph)), 0 )
  
  rs <- DBI::dbGetQuery( conn, query )
  expect_equal( nrow(rs), 10 )
  
})







