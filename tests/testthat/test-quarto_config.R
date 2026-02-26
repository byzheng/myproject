test_that("list_quarto_render_files resolves render patterns from _quarto.yml", {
    root <- file.path(tempdir(), paste0("quarto-list-", as.integer(Sys.time()), "-", sample.int(1e6, 1)))
    dir.create(root, recursive = TRUE)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)

    dir.create(file.path(root, "script"), recursive = TRUE)
    dir.create(file.path(root, "script", "nested"), recursive = TRUE)
    dir.create(file.path(root, "story"), recursive = TRUE)
    dir.create(file.path(root, "story", "nested"), recursive = TRUE)

    writeLines(c(
        "project:",
        "  type: website",
        "  render:",
        "    - \"index.qmd\"",
        "    - \"pipeline_targets.qmd\"",
        "    - \"{script,story}/*.qmd\"",
        "    - \"{script,story}/**/*.{qmd,R,r,Rmd,rmd}\""
    ), file.path(root, "_quarto.yml"))

    files_to_create <- c(
        "index.qmd",
        "pipeline_targets.qmd",
        "script/a.qmd",
        "story/b.qmd",
        "script/nested/c.R",
        "story/nested/d.rmd",
        "script/nested/e.txt"
    )
    for (f in files_to_create) {
        writeLines("x", file.path(root, f))
    }

    result <- list_quarto_render_files(root_dir = root)

    expect_setequal(result, c(
        "index.qmd",
        "pipeline_targets.qmd",
        "script/a.qmd",
        "story/b.qmd",
        "script/nested/c.R",
        "story/nested/d.rmd"
    ))
})

test_that("list_quarto_render_hashes returns named hashes and updates on content change", {
    root <- file.path(tempdir(), paste0("quarto-hash-", as.integer(Sys.time()), "-", sample.int(1e6, 1)))
    dir.create(root, recursive = TRUE)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)

    dir.create(file.path(root, "script"), recursive = TRUE)
    writeLines(c(
        "project:",
        "  render:",
        "    - \"index.qmd\"",
        "    - \"script/*.qmd\""
    ), file.path(root, "_quarto.yml"))

    writeLines("initial", file.path(root, "index.qmd"))
    writeLines("initial", file.path(root, "script", "a.qmd"))

    hashes_before <- list_quarto_render_hashes(root_dir = root)
    expect_setequal(names(hashes_before), c("index.qmd", "script/a.qmd"))
    expect_true(all(nzchar(unname(hashes_before))))

    writeLines("updated", file.path(root, "script", "a.qmd"))

    hashes_after <- list_quarto_render_hashes(root_dir = root)
    expect_identical(hashes_before[["index.qmd"]], hashes_after[["index.qmd"]])
    expect_false(identical(hashes_before[["script/a.qmd"]], hashes_after[["script/a.qmd"]]))
})

test_that("list_quarto_render_hashes supports target triggering scenarios", {
    root <- file.path(tempdir(), paste0("quarto-target-hash-", as.integer(Sys.time()), "-", sample.int(1e6, 1)))
    dir.create(root, recursive = TRUE)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)

    dir.create(file.path(root, "script"), recursive = TRUE)
    writeLines(c(
        "project:",
        "  render:",
        "    - \"index.qmd\"",
        "    - \"script/*.qmd\""
    ), file.path(root, "_quarto.yml"))

    writeLines("index", file.path(root, "index.qmd"))
    writeLines("a-v1", file.path(root, "script", "a.qmd"))

    hashes_1 <- list_quarto_render_hashes(root_dir = root)

    # no file changes
    hashes_2 <- list_quarto_render_hashes(root_dir = root)
    expect_identical(hashes_1, hashes_2)

    # change file content
    writeLines("a-v2", file.path(root, "script", "a.qmd"))
    hashes_3 <- list_quarto_render_hashes(root_dir = root)
    expect_false(identical(hashes_2, hashes_3))
    expect_identical(hashes_2[["index.qmd"]], hashes_3[["index.qmd"]])
    expect_false(identical(hashes_2[["script/a.qmd"]], hashes_3[["script/a.qmd"]]))

    # delete a file
    unlink(file.path(root, "script", "a.qmd"))
    hashes_4 <- list_quarto_render_hashes(root_dir = root)
    expect_false(identical(hashes_3, hashes_4))
    expect_setequal(names(hashes_4), "index.qmd")

    # add new file
    writeLines("b-v1", file.path(root, "script", "b.qmd"))
    hashes_5 <- list_quarto_render_hashes(root_dir = root)
    expect_false(identical(hashes_4, hashes_5))
    expect_setequal(names(hashes_5), c("index.qmd", "script/b.qmd"))
})
