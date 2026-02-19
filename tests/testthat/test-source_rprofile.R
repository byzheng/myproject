test_that("source_rprofile sources the first .Rprofile found and stops", {
    temp_dir <- tempfile()
    dir.create(temp_dir)
    on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

    child_dir <- file.path(temp_dir, "a")
    deep_dir <- file.path(child_dir, "b")
    dir.create(deep_dir, recursive = TRUE)

    marker <- normalizePath(file.path(temp_dir, "marker.txt"), winslash = "/", mustWork = FALSE)

    child_profile <- normalizePath(file.path(child_dir, ".Rprofile"), winslash = "/", mustWork = FALSE)
    root_profile <- file.path(temp_dir, ".Rprofile")

    writeLines(sprintf("write('child', file = '%s', append = TRUE)", marker), child_profile)
    writeLines(sprintf("write('root', file = '%s', append = TRUE)", marker), root_profile)

    result <- source_rprofile(start = deep_dir, stop_at = temp_dir)

    expect_equal(
        normalizePath(result, winslash = "/", mustWork = TRUE),
        normalizePath(child_profile, winslash = "/", mustWork = TRUE)
    )
    expect_true(file.exists(marker))
    expect_equal(readLines(marker), "child")
})

test_that("source_rprofile does not search above stop_at", {
    temp_dir <- tempfile()
    dir.create(temp_dir)
    on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

    stop_dir <- file.path(temp_dir, "stop")
    deep_dir <- file.path(stop_dir, "deep")
    dir.create(deep_dir, recursive = TRUE)

    writeLines("write('root', file = tempfile())", file.path(temp_dir, ".Rprofile"))

    result <- source_rprofile(start = deep_dir, stop_at = stop_dir)

    expect_null(result)
})

test_that("source_rprofile returns NULL when no .Rprofile is found", {
    temp_dir <- tempfile()
    dir.create(temp_dir)
    on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

    deep_dir <- file.path(temp_dir, "x", "y")
    dir.create(deep_dir, recursive = TRUE)

    result <- source_rprofile(start = deep_dir, stop_at = temp_dir)

    expect_null(result)
})
