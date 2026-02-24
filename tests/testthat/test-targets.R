test_that("get_targets loads and combines targets objects", {
    skip_if_not_installed("targets")

    temp_dir <- tempfile()
    dir.create(temp_dir)
    on.exit(unlink(temp_dir, recursive = TRUE))

    script_dir <- file.path(temp_dir, "script")
    dir.create(script_dir, recursive = TRUE)

    writeLines("targets_alpha <- targets::tar_target(alpha, 1)", file.path(script_dir, "_targets_alpha.R"))

    nested_dir <- file.path(script_dir, "nested")
    dir.create(nested_dir, recursive = TRUE)
    writeLines("targets_beta <- targets::tar_target(beta, 2)", file.path(nested_dir, "_targets_beta.R"))

    result <- get_targets(path = script_dir)

    expect_type(result, "list")
    expect_length(result, 2)
    expect_true(all(vapply(result, inherits, logical(1), what = "tar_target")))
})

test_that("get_targets handles bare single tar_target calls", {
    skip_if_not_installed("targets")

    temp_dir <- tempfile()
    dir.create(temp_dir)
    on.exit(unlink(temp_dir, recursive = TRUE))

    script_dir <- file.path(temp_dir, "script")
    dir.create(script_dir, recursive = TRUE)

    writeLines("targets::tar_target(alpha, 1)", file.path(script_dir, "_targets_bare_item.R"))

    result <- get_targets(path = script_dir)

    expect_type(result, "list")
    expect_length(result, 1)
    expect_true(all(vapply(result, inherits, logical(1), what = "tar_target")))
})

test_that("get_targets handles bare list of tar_targets calls", {
    skip_if_not_installed("targets")

    temp_dir <- tempfile()
    dir.create(temp_dir)
    on.exit(unlink(temp_dir, recursive = TRUE))

    script_dir <- file.path(temp_dir, "script")
    dir.create(script_dir, recursive = TRUE)

    writeLines("list(targets::tar_target(beta, 1))", file.path(script_dir, "_targets_bare_list.R"))

    result <- get_targets(path = script_dir)

    expect_type(result, "list")
    expect_length(result, 1)
    expect_true(all(vapply(result, inherits, logical(1), what = "tar_target")))
})

test_that("get_targets combines bare single and bare list tar_targets", {
    skip_if_not_installed("targets")

    temp_dir <- tempfile()
    dir.create(temp_dir)
    on.exit(unlink(temp_dir, recursive = TRUE))

    script_dir <- file.path(temp_dir, "script")
    dir.create(script_dir, recursive = TRUE)

    writeLines("targets::tar_target(alpha, 1)", file.path(script_dir, "_targets_bare_item.R"))
    writeLines("list(targets::tar_target(beta, 1))", file.path(script_dir, "_targets_bare_list.R"))

    result <- get_targets(path = script_dir)

    expect_type(result, "list")
    expect_length(result, 2)
    expect_true(all(vapply(result, inherits, logical(1), what = "tar_target")))
})





test_that("get_targets respects file pattern", {
    skip_if_not_installed("targets")

    temp_dir <- tempfile()
    dir.create(temp_dir)
    on.exit(unlink(temp_dir, recursive = TRUE))

    script_dir <- file.path(temp_dir, "script")
    dir.create(script_dir, recursive = TRUE)

    writeLines("targets_keep <- targets::tar_target(keep, TRUE)", file.path(script_dir, "_targets_keep.R"))
    writeLines("targets_skip <- targets::tar_target(skip, TRUE)", file.path(script_dir, "not_targets.R"))

    result <- get_targets(path = script_dir)

    expect_length(result, 1)
    expect_true(inherits(result[[1]], "tar_target"))
})

test_that("get_targets returns NULL when no targets objects found", {
    temp_dir <- tempfile()
    dir.create(temp_dir)
    on.exit(unlink(temp_dir, recursive = TRUE))

    script_dir <- file.path(temp_dir, "script")
    dir.create(script_dir, recursive = TRUE)

    writeLines("x <- 1", file.path(script_dir, "_targets_empty.R"))

    expect_null(get_targets(path = script_dir))
})

test_that("get_targets validates inputs", {
    expect_error(get_targets(path = 1), class = "simpleError")
    expect_error(get_targets(pattern = c("a", "b")), class = "simpleError")
})

test_that("get_targets reports source file when sourcing fails", {
    temp_dir <- tempfile()
    dir.create(temp_dir)
    on.exit(unlink(temp_dir, recursive = TRUE))

    script_dir <- file.path(temp_dir, "script")
    dir.create(script_dir, recursive = TRUE)

    bad_file <- file.path(script_dir, "_targets_broken.R")
    writeLines("stop('boom during source')", bad_file)

    expect_error(
        get_targets(path = script_dir),
        "Failed to source target file"
    )
    expect_error(
        get_targets(path = script_dir),
        bad_file,
        fixed = TRUE
    )
})

test_that("get_targets errors for non tar_target objects", {
    temp_dir <- tempfile()
    dir.create(temp_dir)
    on.exit(unlink(temp_dir, recursive = TRUE))

    script_dir <- file.path(temp_dir, "script")
    dir.create(script_dir, recursive = TRUE)

    writeLines("targets_bad <- list(value = 1)", file.path(script_dir, "_targets_bad.R"))

    expect_error(
        get_targets(path = script_dir),
        "not all elements inherit from 'tar_target'"
    )
})

test_that("get_targets errors for list with non tar_target elements", {
    skip_if_not_installed("targets")

    temp_dir <- tempfile()
    dir.create(temp_dir)
    on.exit(unlink(temp_dir, recursive = TRUE))

    script_dir <- file.path(temp_dir, "script")
    dir.create(script_dir, recursive = TRUE)

    writeLines(
        "targets_mixed <- list(targets::tar_target(alpha, 1), 'not a target')",
        file.path(script_dir, "_targets_mixed.R")
    )

    expect_error(
        get_targets(path = script_dir),
        "not all elements inherit from 'tar_target'"
    )
})

test_that("get_targets errors for duplicate internal target names", {
    skip_if_not_installed("targets")

    temp_dir <- tempfile()
    dir.create(temp_dir)
    on.exit(unlink(temp_dir, recursive = TRUE))

    script_dir <- file.path(temp_dir, "script")
    source_dir <- file.path(temp_dir, "source")
    dir.create(script_dir, recursive = TRUE)
    dir.create(source_dir, recursive = TRUE)

    writeLines(
        "targets_one <- targets::tar_target(shared_name, 1)",
        file.path(script_dir, "_targets_one.R")
    )
    writeLines(
        "targets_two <- targets::tar_target(shared_name, 2)",
        file.path(source_dir, "_targets_two.R")
    )

    expect_error(
        get_targets(path = c(script_dir, source_dir)),
        "Duplicate internal target names"
    )
})

test_that("get_targets handles list of tar_target objects from single file", {
    skip_if_not_installed("targets")

    temp_dir <- tempfile()
    dir.create(temp_dir)
    on.exit(unlink(temp_dir, recursive = TRUE))

    script_dir <- file.path(temp_dir, "script")
    dir.create(script_dir, recursive = TRUE)

    writeLines(
        "targets_multi <- list(targets::tar_target(alpha, 1), targets::tar_target(beta, 2))",
        file.path(script_dir, "_targets_multi.R")
    )

    result <- get_targets(path = script_dir)

    expect_type(result, "list")
    expect_length(result, 2)
    expect_true(all(vapply(result, inherits, logical(1), what = "tar_target")))
})
