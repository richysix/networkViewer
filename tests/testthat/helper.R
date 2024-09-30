load_info <- function() {
  return(
    list(
      test_node_file = file.path("", "Users", "rjw26", "checkouts", "networkViewer", "extdata", "nodes-test-mini.csv"),
      test_edge_file = file.path("", "Users", "rjw26", "checkouts", "networkViewer", "extdata", "edges-test-mini.csv"),
      expected_node_col_types = cols(
        "node_idx" = readr::col_integer(),
        "id" = readr::col_integer(),
        "gene_id" = readr::col_character(),
        "gene_name" = readr::col_character(),
        "cluster_id" = readr::col_integer()
      ),
      expected_edge_col_types = cols(
        "edge_idx" = readr::col_integer(),
        "source" = readr::col_integer(),
        "target" = readr::col_integer(),
        "weight" = readr::col_double()
      ),
      mini_nodes = read_rds(test_path("fixtures", "nodes-mini.rds")),
      mini_edges = read_rds(test_path("fixtures", "edges-mini.rds")),
      no_idx_file = test_path("fixtures", "nodes-no-idx.csv"),
      no_source_file = test_path("fixtures", "edges-no-source.csv"),
      no_target_file = test_path("fixtures", "edges-no-source-target.csv")
    )
  )
}
