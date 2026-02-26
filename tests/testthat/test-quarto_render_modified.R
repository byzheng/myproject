test_that("render_modified_quarto works end-to-end", {
    skip_if_not_installed("quarto")
    skip_if_not_installed("yaml")

    root <- file.path(tempdir(), paste0("quarto-render-modified-", as.integer(Sys.time()), "-", sample.int(1e6, 1)))
    dir.create(root, recursive = TRUE)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    oldwd <- setwd(root)
    on.exit(setwd(oldwd), add = TRUE)
    # Setup project
    writeLines(c(
        "project:",
        "  type: website",
        "  render:",
        "    - 'index.qmd'",
        "    - 'script/*.qmd'"
    ), file.path(root, "_quarto.yml"))
    dir.create(file.path(root, "script"), recursive = TRUE)

    # Initial files
    writeLines("# Index", file.path(root, "index.qmd"))
    writeLines("# A", file.path(root, "script", "a.qmd"))

    # Initial run
    out1 <- render_modified_quarto(root_dir = root, dry_run = FALSE)
    expect_true(all(file.exists(file.path(root, "_site", c("index.html", "script/a.html")))))
    expect_setequal(out1, c("index.qmd", "script/a.qmd"))

    # Rerun without changes
    out2 <- render_modified_quarto(root_dir = root, dry_run = FALSE)
    expect_length(out2, 0)

    # Change file content
    writeLines("# A updated", file.path(root, "script", "a.qmd"))
    out3 <- render_modified_quarto(root_dir = root, dry_run = FALSE)
    expect_true(file.exists(file.path(root, "_site", "script/a.html")))
    expect_equal(out3[1], "script/a.qmd")

    # Add new file
    writeLines("# B", file.path(root, "script", "b.qmd"))
    out4 <- render_modified_quarto(root_dir = root, dry_run = FALSE)
    expect_true(file.exists(file.path(root, "_site", "script/b.html")))
    expect_equal(out4[1], "script/b.qmd")

    # Delete a file
    unlink(file.path(root, "script", "a.qmd"))
    out5 <- render_modified_quarto(root_dir = root, dry_run = FALSE)
    expect_false("script/a.qmd" %in% out5)
    expect_true(file.exists(file.path(root, "_site", "script/b.html")))
})
