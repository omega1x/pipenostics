library(igraph)
library(pipenostics)

g <- graph_from_data_frame(m325testbench[, c("sender", "acceptor")])
x <- V(g)
checkmate::assert_true(is_dag(g))
n <- vapply(
  x, function(id) (length(subcomponent(g, id, "out")) - 1)/gsize(g), double(1)
)



