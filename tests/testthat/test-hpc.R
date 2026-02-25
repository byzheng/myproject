test_that("create_hpc_sentinel creates valid sentinel file", {
    skip_if_not_installed("jsonlite")
    
    temp_dir <- tempdir()
    sentinel_path <- file.path(temp_dir, "test_sentinel.json")
    input1 <- tempfile(tmpdir = temp_dir, fileext = ".rds")
    input2 <- tempfile(tmpdir = temp_dir, fileext = ".csv")
    
    # Create mock input files
    saveRDS(mtcars, input1)
    write.csv(iris, input2, row.names = FALSE)
    
    # Create sentinel
    result <- create_hpc_sentinel(
        sentinel_path = sentinel_path,
        input_files = c(input1, input2),
        metadata = list(job_id = "12345", nodes = 4)
    )
    
    expect_identical(result, sentinel_path)
    expect_true(file.exists(sentinel_path))
    
    # Read and validate contents
    sentinel_data <- jsonlite::read_json(sentinel_path)
    expect_true("completed_at" %in% names(sentinel_data))
    expect_true("input_files" %in% names(sentinel_data))
    expect_true("metadata" %in% names(sentinel_data))
    expect_equal(length(sentinel_data$input_files), 2)
    expect_equal(sentinel_data$metadata$job_id, "12345")
    expect_equal(sentinel_data$metadata$nodes, 4)
    
    # Clean up
    unlink(sentinel_path)
    unlink(c(input1, input2))
})

test_that("create_hpc_sentinel validates input_files", {
    temp_dir <- tempdir()
    sentinel_path <- file.path(temp_dir, "test_sentinel.json")
    
    expect_error(
        create_hpc_sentinel(sentinel_path, character(0)),
        "length.*> 0"
    )
    
    expect_error(
        create_hpc_sentinel(sentinel_path, 123),
        "is.character.*input_files"
    )
})

test_that("create_hpc_sentinel warns about missing input files", {
    skip_if_not_installed("jsonlite")
    
    temp_dir <- tempdir()
    sentinel_path <- file.path(temp_dir, "test_sentinel.json")
    nonexistent_file <- file.path(temp_dir, "does_not_exist.rds")
    
    expect_warning(
        create_hpc_sentinel(sentinel_path, nonexistent_file),
        "Input file not found"
    )
    
    unlink(sentinel_path)
})

test_that("check_hpc_sentinel validates existing sentinel", {
    skip_if_not_installed("jsonlite")
    
    temp_dir <- tempdir()
    sentinel_path <- file.path(temp_dir, "test_sentinel.json")
    input1 <- tempfile(tmpdir = temp_dir, fileext = ".rds")
    
    # Create mock input file
    saveRDS(mtcars, input1)
    
    # Create sentinel
    create_hpc_sentinel(sentinel_path, input1)
    
    # Check should pass
    result <- check_hpc_sentinel(sentinel_path, input1)
    expect_identical(result, sentinel_path)
    
    # Clean up
    unlink(sentinel_path)
    unlink(input1)
})

test_that("check_hpc_sentinel stops when sentinel missing", {
    skip_if_not_installed("jsonlite")
    
    temp_dir <- tempdir()
    sentinel_path <- file.path(temp_dir, "nonexistent_sentinel.json")
    input1 <- tempfile(tmpdir = temp_dir, fileext = ".rds")
    
    saveRDS(mtcars, input1)
    
    expect_error(
        check_hpc_sentinel(sentinel_path, input1, on_missing = "stop"),
        "HPC job not completed yet"
    )
    
    unlink(input1)
})

test_that("check_hpc_sentinel warns when sentinel missing and on_missing='warn'", {
    skip_if_not_installed("jsonlite")
    
    temp_dir <- tempdir()
    sentinel_path <- file.path(temp_dir, "nonexistent_sentinel.json")
    input1 <- tempfile(tmpdir = temp_dir, fileext = ".rds")
    
    saveRDS(mtcars, input1)
    
    expect_warning(
        result <- check_hpc_sentinel(sentinel_path, input1, on_missing = "warn"),
        "HPC job not completed yet"
    )
    expect_null(result)
    
    unlink(input1)
})

test_that("check_hpc_sentinel detects stale inputs", {
    skip_if_not_installed("jsonlite")
    
    temp_dir <- tempdir()
    sentinel_path <- file.path(temp_dir, "test_sentinel.json")
    input1 <- tempfile(tmpdir = temp_dir, fileext = ".rds")
    
    # Create input and sentinel
    saveRDS(mtcars, input1)
    create_hpc_sentinel(sentinel_path, input1)
    
    # Wait and modify input file (need >1 sec for staleness detection)
    Sys.sleep(1.5)
    saveRDS(iris, input1)
    
    expect_error(
        check_hpc_sentinel(sentinel_path, input1, on_stale = "stop"),
        "Input data changed after HPC job completed"
    )
    
    # Clean up
    unlink(sentinel_path)
    unlink(input1)
})

test_that("check_hpc_sentinel deletes stale sentinel when on_stale='delete'", {
    skip_if_not_installed("jsonlite")
    
    temp_dir <- tempdir()
    sentinel_path <- file.path(temp_dir, "test_sentinel.json")
    input1 <- tempfile(tmpdir = temp_dir, fileext = ".rds")
    
    # Create input and sentinel
    saveRDS(mtcars, input1)
    create_hpc_sentinel(sentinel_path, input1)
    
    # Wait and modify input file (need >1 sec for staleness detection)
    Sys.sleep(1.5)
    saveRDS(iris, input1)
    
    expect_error(
        check_hpc_sentinel(sentinel_path, input1, on_stale = "delete"),
        "Input data changed after HPC job completed"
    )
    
    # Sentinel should be deleted
    expect_false(file.exists(sentinel_path))
    
    # Clean up
    unlink(input1)
})

test_that("check_hpc_sentinel detects missing tracked inputs", {
    skip_if_not_installed("jsonlite")
    
    temp_dir <- tempdir()
    sentinel_path <- file.path(temp_dir, "test_sentinel.json")
    input1 <- tempfile(tmpdir = temp_dir, fileext = ".rds")
    input2 <- tempfile(tmpdir = temp_dir, fileext = ".csv")
    
    # Create only input1, not input2
    saveRDS(mtcars, input1)
    create_hpc_sentinel(sentinel_path, input1)
    
    # Now create input2 and check with both inputs
    write.csv(iris, input2, row.names = FALSE)
    
    expect_error(
        check_hpc_sentinel(sentinel_path, c(input1, input2), on_stale = "stop"),
        "HPC sentinel is incomplete"
    )
    
    # Clean up
    unlink(sentinel_path)
    unlink(c(input1, input2))
})

test_that("check_hpc_sentinel handles corrupted sentinel", {
    skip_if_not_installed("jsonlite")
    
    temp_dir <- tempdir()
    sentinel_path <- file.path(temp_dir, "test_sentinel.json")
    input1 <- tempfile(tmpdir = temp_dir, fileext = ".rds")
    
    # Create corrupted JSON file
    writeLines("not valid json {{{", sentinel_path)
    saveRDS(mtcars, input1)
    
    expect_error(
        check_hpc_sentinel(sentinel_path, input1),
        "Sentinel file corrupted"
    )
    
    # Sentinel should be deleted
    expect_false(file.exists(sentinel_path))
    
    # Clean up
    unlink(input1)
})

test_that("check_hpc_sentinel validates input_files parameter", {
    temp_dir <- tempdir()
    sentinel_path <- file.path(temp_dir, "test_sentinel.json")
    
    expect_error(
        check_hpc_sentinel(sentinel_path, character(0)),
        "length.*> 0"
    )
    
    expect_error(
        check_hpc_sentinel(sentinel_path, 123),
        "is.character.*input_files"
    )
})

test_that("get_hpc_sentinel_metadata retrieves metadata", {
    skip_if_not_installed("jsonlite")
    
    temp_dir <- tempdir()
    sentinel_path <- file.path(temp_dir, "test_sentinel.json")
    input1 <- tempfile(tmpdir = temp_dir, fileext = ".rds")
    
    # Create input and sentinel
    saveRDS(mtcars, input1)
    create_hpc_sentinel(
        sentinel_path, 
        input1,
        metadata = list(job_id = "99999", cluster = "hpc01")
    )
    
    # Get metadata
    meta <- get_hpc_sentinel_metadata(sentinel_path)
    
    expect_true("completed_at" %in% names(meta))
    expect_true("input_files" %in% names(meta))
    expect_true("metadata" %in% names(meta))
    expect_equal(meta$metadata$job_id, "99999")
    expect_equal(meta$metadata$cluster, "hpc01")
    
    # Clean up
    unlink(sentinel_path)
    unlink(input1)
})

test_that("get_hpc_sentinel_metadata stops when file missing", {
    temp_dir <- tempdir()
    sentinel_path <- file.path(temp_dir, "nonexistent_sentinel.json")
    
    expect_error(
        get_hpc_sentinel_metadata(sentinel_path),
        "Sentinel file not found"
    )
})
