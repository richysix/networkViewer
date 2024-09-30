test_that("set_col_types works", {
  test_file <- file.path("", "Users", "rjw26", "checkouts", "networkViewer", "extdata", "nodes-test-mini.csv")
  expected <- cols(
    "node_idx" = readr::col_integer(),
    "id" = readr::col_integer(),
    "gene_id" = readr::col_character(),
    "gene_name" = readr::col_character(),
    "cluster_id" = readr::col_integer()
  )
  expect_equal(set_col_types(test_file, readr_func = readr::read_csv), expected)

  test_file <- file.path("", "Users", "rjw26", "checkouts", "networkViewer", "extdata", "edges-test-mini.csv")
  expected <- cols(
    "edge_idx" = readr::col_integer(),
    "source" = readr::col_integer(),
    "target" = readr::col_integer(),
    "weight" = readr::col_double()
  )
  expect_equal(set_col_types(test_file, readr_func = readr::read_csv), expected)
})

test_that("load_data works", {
  test_file <- file.path("", "Users", "rjw26", "checkouts", "networkViewer", "extdata", "nodes-test-mini.csv")
  expect_equal(load_data(test_file), read_csv(test_file, col_types = "iicci"))

  test_file <- file.path("", "Users", "rjw26", "checkouts", "networkViewer", "extdata", "edges-test-mini.csv")
  expect_equal(load_data(test_file), read_csv(test_file, col_types = "iiid"))
})
