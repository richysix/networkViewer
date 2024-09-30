test_that("set_col_types works", {
  test_info <- load_info()
  expect_equal(set_col_types(test_info$test_node_file,
                             readr_func = readr::read_csv),
               test_info$expected_node_col_types)

  expect_equal(set_col_types(test_info$test_edge_file,
                             readr_func = readr::read_csv),
               test_info$expected_edge_col_types)
})

test_that("load_data works", {
  test_info <- load_info()
  expect_equal(load_data(test_info$test_node_file),
               read_csv(test_info$test_node_file, col_types = "iicci"))

  expect_equal(load_data(test_info$test_edge_file),
               read_csv(test_info$test_edge_file, col_types = "iiid"))
})

test_that("load_node_data works", {
  test_info <- load_info()
  expect_equal(load_node_data(test_info$test_node_file),
               test_info$mini_nodes)
  err <- rlang::catch_cnd(load_node_data(test_info$no_idx_file))
  expect_s3_class(err, "error_missing_column")
  expect_equal(err$message, "No column 'node_idx'! This is a required column.")
})

test_that("load_edge_data works", {
  test_info <- load_info()
  expect_equal(load_edge_data(test_info$test_edge_file),
               test_info$mini_edges)
  err <- rlang::catch_cnd(load_edge_data(test_info$no_source_file))
  expect_s3_class(err, "error_missing_column")
  expect_equal(err$message, "Missing columns: source")

  err <- rlang::catch_cnd(load_edge_data(test_info$no_target_file))
  expect_s3_class(err, "error_missing_column")
  expect_equal(err$message, "Missing columns: source, target")
})
