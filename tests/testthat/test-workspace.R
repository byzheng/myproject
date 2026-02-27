test_that("find_ws locates workspace root from current directory", {
    old <- getwd()
    on.exit(setwd(old), add = TRUE)

    root <- file.path(tempdir(), paste0("workspace-test-", as.integer(stats::runif(1, 1, 1e9))))
    dir.create(root, recursive = TRUE, showWarnings = FALSE)
    file.create(file.path(root, ".workspace"))

    setwd(root)

    expect_equal(
        find_ws(),
        normalizePath(root, mustWork = FALSE, winslash = "/")
    )
})

test_that("find_ws locates workspace root from nested directory", {
    old <- getwd()
    on.exit(setwd(old), add = TRUE)

    root <- file.path(tempdir(), paste0("workspace-test-", as.integer(stats::runif(1, 1, 1e9))))
    dir.create(root, recursive = TRUE, showWarnings = FALSE)
    file.create(file.path(root, ".workspace"))

    nested <- file.path(root, "deep", "nested", "path")
    dir.create(nested, recursive = TRUE, showWarnings = FALSE)

    setwd(nested)

    expect_equal(
        find_ws(),
        normalizePath(root, mustWork = FALSE, winslash = "/")
    )
})

test_that("find_ws accepts path argument", {
    old <- getwd()
    on.exit(setwd(old), add = TRUE)

    root <- file.path(tempdir(), paste0("workspace-test-", as.integer(stats::runif(1, 1, 1e9))))
    dir.create(root, recursive = TRUE, showWarnings = FALSE)
    file.create(file.path(root, ".workspace"))

    nested <- file.path(root, "some", "nested", "dir")
    dir.create(nested, recursive = TRUE, showWarnings = FALSE)

    # Don't change directory, pass path argument
    expect_equal(
        find_ws(path = nested),
        normalizePath(root, mustWork = FALSE, winslash = "/")
    )
})

test_that("path_ws builds path relative to workspace root", {
    old <- getwd()
    on.exit(setwd(old), add = TRUE)

    root <- file.path(tempdir(), paste0("workspace-test-", as.integer(stats::runif(1, 1, 1e9))))
    dir.create(root, recursive = TRUE, showWarnings = FALSE)
    file.create(file.path(root, ".workspace"))

    dir.create(file.path(root, "data"), recursive = TRUE, showWarnings = FALSE)
    file.create(file.path(root, "data", "input.csv"))

    setwd(root)

    expect_equal(
        path_ws("data", "input.csv"),
        file.path("data", "input.csv")
    )
})

test_that("path_ws resolves from nested directory", {
    old <- getwd()
    on.exit(setwd(old), add = TRUE)

    root <- file.path(tempdir(), paste0("workspace-test-", as.integer(stats::runif(1, 1, 1e9))))
    dir.create(root, recursive = TRUE, showWarnings = FALSE)
    file.create(file.path(root, ".workspace"))

    dir.create(file.path(root, "data"), recursive = TRUE, showWarnings = FALSE)
    nested <- file.path(root, "projects", "A")
    dir.create(nested, recursive = TRUE, showWarnings = FALSE)

    setwd(nested)

    expect_equal(
        path_ws("data", "input.csv"),
        file.path("..", "..", "data", "input.csv")
    )
})

test_that("path_ws returns relative path when possible", {
    old <- getwd()
    on.exit(setwd(old), add = TRUE)

    root <- file.path(tempdir(), paste0("workspace-test-", as.integer(stats::runif(1, 1, 1e9))))
    dir.create(root, recursive = TRUE, showWarnings = FALSE)
    file.create(file.path(root, ".workspace"))

    subdir <- file.path(root, "subdir")
    dir.create(subdir, recursive = TRUE, showWarnings = FALSE)

    setwd(subdir)

    result <- path_ws("data")
    # Should be a relative path, not absolute
    expect_false(grepl("^[A-Z]:", result) || grepl("^/", result))
})

