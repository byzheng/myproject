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
        "render:",
        "  - \"index.qmd\"",
        "  - \"pipeline_targets.qmd\"",
        "  - \"{script,story}/*.qmd\"",
        "  - \"{script,story}/**/*.{qmd,R,r,Rmd,rmd}\""
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

test_that("render_modified_quarto returns only changed files in dry run", {
    root <- file.path(tempdir(), paste0("quarto-modified-", as.integer(Sys.time()), "-", sample.int(1e6, 1)))
    dir.create(root, recursive = TRUE)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)

    dir.create(file.path(root, "script"), recursive = TRUE)
    writeLines(c(
        "render:",
        "  - \"index.qmd\"",
        "  - \"script/*.qmd\""
    ), file.path(root, "_quarto.yml"))

    writeLines("initial", file.path(root, "index.qmd"))
    writeLines("initial", file.path(root, "script", "a.qmd"))

    hashes <- as.character(tools::md5sum(c(
        file.path(root, "index.qmd"),
        file.path(root, "script", "a.qmd")
    )))
    names(hashes) <- c("index.qmd", "script/a.qmd")

    cache_path <- file.path(root, ".quarto", "render-hashes.json")
    dir.create(dirname(cache_path), recursive = TRUE)
    jsonlite::write_json(as.list(hashes), cache_path, auto_unbox = TRUE, pretty = TRUE)

    unchanged <- render_modified_quarto(root_dir = root, dry_run = TRUE)
    expect_length(unchanged, 0)

    writeLines("updated", file.path(root, "script", "a.qmd"))

    changed <- render_modified_quarto(root_dir = root, dry_run = TRUE)
    expect_equal(changed, "script/a.qmd")
})
