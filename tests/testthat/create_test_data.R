library(tidyverse)
library(here)

extdata_dir <- file.path(here(), 'extdata')
if (!file.exists(extdata_dir)) {
  dir.create(extdata_dir)
}

# make an initial mini network
set.seed(253)
num_nodes <- 30
tmp_node_data <- data.frame(
  node_idx = 0:(num_nodes - 1),
  id = 0:(num_nodes - 1),
  gene_id = sprintf('ENSTEST%011d', seq_len(num_nodes)),
  gene_name = sprintf('gene-%d', seq_len(num_nodes)),
  cluster_id = c(1, rep(2, 5), rep(3, 10), rep(4, 7), rep(5, 7))
)

write_csv(tmp_node_data, file = file.path(extdata_dir, 'nodes-test-mini.csv'))

add_edges <- function(edges, source, target, weights) {
  if (length(target) != length(source) | length(weights) != length(source)) {
    stop("Weights is not the correct length!")
  }
  if (is.null(edges)) {
    starting_edge_id <- 0
  } else {
    starting_edge_id <- max(edges$edge_id) + 1
  }

  edges_tmp <- data.frame(
    edge_idx = seq(starting_edge_id, starting_edge_id + length(weights) - 1),
    source = source,
    target = target,
    weight = weights
  )
  return(rbind(edges, edges_tmp))
}

current_cluster <- 1
node_id <- filter(tmp_node_data, cluster_id == current_cluster) |>
  head(1) |>
  pull(node_idx)
target_nodes <- filter(tmp_node_data, cluster_id %in% 2:4) |>
  group_by(cluster_id) |>
  slice_head(n = 1) |>
  pull(node_idx)
tmp_edge_data <- add_edges(NULL, rep(node_id, length(target_nodes)),
                           target_nodes, runif(length(target_nodes)))

current_cluster <- 2
current_nodes <- filter(tmp_node_data, cluster_id == current_cluster)
source_nodes <- current_nodes$node_idx[ c(rep(1, 3), rep(2, 3), rep(5, 2)) ]
target_nodes <- current_nodes$node_idx[ c(2, 3, 4, 3, 4, 5, 3, 4) ]
tmp_edge_data <- add_edges(tmp_edge_data, source_nodes, target_nodes,
                           runif(length(source_nodes), min = 0.7))

current_cluster <- 3
current_nodes <- filter(tmp_node_data, cluster_id == current_cluster)
source_nodes <- current_nodes$node_idx[c(rep(1, 3), rep(c(2, 3, 4), each = 2))]
target_nodes <- current_nodes$node_idx[c(2:length(current_nodes$node_idx))]
tmp_edge_data <- add_edges(tmp_edge_data, source_nodes, target_nodes,
                           runif(length(source_nodes), min = 0.5))

current_cluster <- 4
current_nodes <- filter(tmp_node_data, cluster_id == current_cluster)
# 16 * 6
# 17 * 5
# 18 * 4
# 19 * 3
# 20 * 2
# 21 * 1

n <- nrow(current_nodes)
source_nodes <- integer(length = n * (n - 1)/2 )
target_nodes <- integer(length = n * (n - 1)/2 )
i <- 1
for (idx in seq_len(n - 1)) {
  k = n - idx
  source_nodes[i:(i + k - 1)] <- rep(current_nodes$node_idx[idx], k)
  target_nodes[i:(i + k - 1)] <- current_nodes$node_idx[c((idx + 1):length(current_nodes$node_idx))]
  i <- i + k
}
tmp_edge_data <- add_edges(tmp_edge_data, source_nodes, target_nodes,
                           runif(length(source_nodes), min = 0.8))

# remove self edges
tmp_edge_data <- tmp_edge_data[ tmp_edge_data$source != tmp_edge_data$target, ]
# remove duplicate edges
tmp_edge_data <- tmp_edge_data[ rownames(unique(tmp_edge_data[, c('source', 'target')])), ]

write_csv(tmp_edge_data, file = file.path(extdata_dir, 'edges-test-mini.csv'))

set.seed(652)
num_nodes <- 1000
tmp_node_data <- data.frame(
  node_idx = 0:(num_nodes - 1),
  id = 0:(num_nodes - 1),
  gene_id = sprintf('ENSTEST%011d', seq_len(num_nodes)),
  gene_name = sprintf('gene-%d', seq_len(num_nodes)),
  cluster_id = sample(0:19, num_nodes, replace = TRUE))

write_csv(tmp_node_data, file = file.path(extdata_dir, 'nodes-test.csv'))

num_edges <- 5000
tmp_edge_data <- data.frame(
  edge_idx = 0:(num_edges - 1),
  source = sample(tmp_node_data$id, num_edges, replace = TRUE),
  target = sample(tmp_node_data$id, num_edges, replace = TRUE),
  weight = runif(num_edges, min = 0.6, max = 1))
# remove self edges
tmp_edge_data <- tmp_edge_data[ tmp_edge_data$source != tmp_edge_data$target, ]
# remove duplicate edges
tmp_edge_data <- tmp_edge_data[ rownames(unique(tmp_edge_data[, c('source', 'target')])), ]

write_csv(tmp_edge_data, file = file.path(extdata_dir, 'edges-test.csv'))
