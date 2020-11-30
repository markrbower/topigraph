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
  gib <- graphInsertBuffer( parameters, 200, .9, 1.5, 'tmp.xml', 2 )
  gib$insert(e1)
  # What to test that insert ran?
  eList <- gib$events()
  expect_equal( length(eList), 1)
})

test_that("graphInsertBuffer update works",{
  e1 <- anEvent( 123, c(7,10,9))
  e2 <- anEvent( 234, c(10,12,11))
  e3 <- anEvent( 345, c(9,12,10))
  e4 <- anEvent( 456, c(9,13,10))
  e5 <- anEvent( 5567, c(9,12,11)) # Big time diff to push others off graph.
  parameters <- list(computationMask=c(1,2,3))
  gib <- graphInsertBuffer( parameters, 250, .9, 1.5, 'tmp.xml', 2 );
  gib$insert(e1); gib$insert(e2); gib$insert(e3); gib$insert(e4); gib$insert(e5)
  # e5 should now be in the graph and off the events list  
  eList <- gib$events()
  expect_equal( length(eList), 1)
})

test_that("graphInsertBuffer append works",{
  e1 <- anEvent( 123, c(7,10,9))
  e2 <- anEvent( 234, c(10,12,11))
  e3 <- anEvent( 345, c(9,12,10))
  e4 <- anEvent( 456, c(9,13,10))
  e5 <- anEvent( 5567, c(9,12,11)) # Big time diff to push others off graph.
  parameters <- list(computationMask=c(1,2,3))
  gib <- graphInsertBuffer( parameters, 250, .9, 1.5, 'tmp.xml', 2 );
  gib$insert(e1); gib$insert(e2); gib$insert(e3); gib$insert(e4); gib$insert(e5)
  # e1 should now be in the graph and off the events list
  e6 <- anEvent( 5667, c(8,1,9) )
  gib$insert( e6 )
  eList <- gib$events()
  expect_equal( length(eList), 2)
})

