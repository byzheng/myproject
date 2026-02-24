test_that("tar_or_run returns fallback when target read fails", {
    skip_if_not_installed("targets")

    fallback_value <- data.frame(x = 1:5)
    result <- tar_or_run(nonexistent_target, fallback_value, quiet = TRUE)

    expect_identical(result, fallback_value)
})

test_that("tar_or_run validates target argument", {
    expect_error(
        tar_or_run("string_target", 1 + 1),
        "must be an unquoted name"
    )
})

test_that("tar_or_run requires target argument", {
    expect_error(
        tar_or_run(, 1 + 1),
        "must be provided"
    )
})

test_that("tar_or_run evaluates fallback expression when target fails", {
    skip_if_not_installed("targets")

    fallback_result <- 42
    result <- tar_or_run(nonexistent_target, fallback_result, quiet = TRUE)

    expect_equal(result, fallback_result)
})

test_that("tar_or_run messages when target read fails", {
    skip_if_not_installed("targets")

    expect_message(
        tar_or_run(nonexistent_target, 1, quiet = FALSE),
        "Target read failed"
    )
    expect_message(
        tar_or_run(nonexistent_target, 1, quiet = FALSE),
        "Running fallback expression"
    )
})

test_that("tar_or_run suppresses messages with quiet = TRUE", {
    skip_if_not_installed("targets")

    expect_no_message(
        tar_or_run(nonexistent_target, 1, quiet = TRUE)
    )
})



test_that("tar_or_run reads target correctly", {
    skip_if_not_installed("targets")

    # Create a temporary directory for targets project
    temp_dir <- tempdir()
    old_wd <- getwd()
    setwd(temp_dir)
    on.exit(setwd(old_wd))

    # Create a simple _targets.R file
    targets_script <- "
    list(
        targets::tar_target(test_target_1, 123),
        targets::tar_target(test_target_2, data.frame(a = 1:3, b = 4:6))
    )
    "
    writeLines(targets_script, "_targets.R")

    # Build the targets
    targets::tar_make()

    # Test that tar_or_run reads the target correctly
    expect_no_message(
        result <- tar_or_run(test_target_1)
    )
    expect_equal(result, 123)
    expect_no_message(
        result_df <- tar_or_run(test_target_2)
    )
    expect_equal(result_df, data.frame(a = 1:3, b = 4:6))
})


