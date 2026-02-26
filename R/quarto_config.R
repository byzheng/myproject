

#' List files targeted by `_quarto.yml` `render:` patterns
#'
#' Reads the `render:` section from a Quarto project file and resolves it to a
#' concrete file list.
#'
#' @param quarto_yml Character scalar. Path to `_quarto.yml`, relative to
#'   `root_dir` or absolute.
#' @param root_dir Character scalar. Project root used to resolve relative
#'   paths. Defaults to `here::here()`.
#'
#' @return Character vector of relative file paths matched by `render:`
#'   patterns.
#' @export
#' @examples
#' \dontrun{
#' list_quarto_render_files()
#' }
list_quarto_render_files <- function(quarto_yml = "_quarto.yml", root_dir = here::here()) {
    stopifnot(is.character(quarto_yml), length(quarto_yml) == 1)
    stopifnot(is.character(root_dir), length(root_dir) == 1)

    quarto_path <- if (grepl("^/|^[A-Z]:", quarto_yml)) {
        quarto_yml
    } else {
        file.path(root_dir, quarto_yml)
    }

    if (!file.exists(quarto_path)) {
        stop("Quarto config not found: ", quarto_path, call. = FALSE)
    }

    patterns <- parse_quarto_render_patterns(quarto_path)
    if (length(patterns) == 0) {
        return(character(0))
    }
    root_dir <- normalizePath(root_dir, winslash = "/")
    all_files <- c()
    for (i in seq_along(patterns)) {
        files_i <- bracer::glob(file.path(root_dir, patterns[i]), engine = "r")
        all_files <- c(all_files, files_i)
    }
    all_files <- make_relative(all_files, root_dir)
    # Exclude files starting with _ or . (Quarto ignores these)
    all_files <- all_files[!grepl("^[_\\.]", basename(all_files))]
    sort(unique(all_files))
}


#' List content hashes for files targeted by `_quarto.yml` `render:` patterns
#'
#' Resolves `_quarto.yml` `render:` patterns to files and returns a named
#' character vector of MD5 hashes, where names are relative file paths.
#'
#' @param quarto_yml Character scalar. Path to `_quarto.yml`, relative to
#'   `root_dir` or absolute.
#' @param root_dir Character scalar. Project root used to resolve relative
#'   paths. Defaults to `here::here()`.
#'
#' @return Named character vector of MD5 hashes. Names are relative file paths.
#' @export
#' @examples
#' \dontrun{
#' list_quarto_render_hashes()
#' }
list_quarto_render_hashes <- function(quarto_yml = "_quarto.yml", root_dir = here::here()) {
    stopifnot(is.character(root_dir), length(root_dir) == 1)

    files <- list_quarto_render_files(quarto_yml = quarto_yml, root_dir = root_dir)
    if (length(files) == 0) {
        return(stats::setNames(character(0), character(0)))
    }

    root_dir <- normalizePath(root_dir, winslash = "/", mustWork = TRUE)
    abs_files <- file.path(root_dir, files)
    hashes <- as.character(tools::md5sum(abs_files))
    names(hashes) <- files
    hashes
}


#' Render only modified Quarto files
#'
#' Uses content hashes for files matched by `_quarto.yml` `render:` patterns and
#' renders only files whose content changed since the last successful run.
#'
#' @param quarto_yml Character scalar. Path to `_quarto.yml`, relative to
#'   `root_dir` or absolute.
#' @param root_dir Character scalar. Project root used to resolve relative
#'   paths. Defaults to `here::here()`.
#' @param cache_file Character scalar. Path to JSON hash cache file, relative to
#'   `root_dir` or absolute.
#' @param dry_run Logical scalar. If `TRUE`, returns files that would be
#'   rendered without invoking Quarto.
#' @param force Logical scalar. If `TRUE`, renders all matched files.
#'
#' @return Character vector of relative files selected for rendering.
#' @export
#' @examples
#' \dontrun{
#' render_modified_quarto()
#' render_modified_quarto(dry_run = TRUE)
#' }
render_modified_quarto <- function(
    quarto_yml = "_quarto.yml",
    root_dir = here::here(),
    cache_file = ".quarto/render-hashes.json",
    dry_run = FALSE,
    force = FALSE
) {
    stopifnot(is.logical(dry_run), length(dry_run) == 1)
    stopifnot(is.logical(force), length(force) == 1)
    stopifnot(is.character(cache_file), length(cache_file) == 1)

    hashes <- list_quarto_render_hashes(quarto_yml = quarto_yml, root_dir = root_dir)
    files <- names(hashes)
    if (length(files) == 0) {
        message("No files matched `_quarto.yml` render patterns.")
        return(invisible(character(0)))
    }

    cache_path <- if (grepl("^/|^[A-Z]:", cache_file)) {
        cache_file
    } else {
        file.path(root_dir, cache_file)
    }

    old_hashes <- character(0)
    if (file.exists(cache_path)) {
        cached <- jsonlite::read_json(cache_path, simplifyVector = TRUE)
        if (is.list(cached) && !is.null(names(cached))) {
            old_hashes <- unlist(cached, use.names = TRUE)
        } else if (is.atomic(cached) && !is.null(names(cached))) {
            old_hashes <- cached
        }
    }

    # Determine changed files
    # Also include files whose output HTML does not exist
    html_paths <- file.path(root_dir, "_site", tools::file_path_sans_ext(files))
    html_paths <- paste0(html_paths, ".html") |> 
        make_relative(root_dir)
    changed <- if (force) {
        files
    } else {
        old_vals <- old_hashes[files]
        changed <- files[is.na(old_vals) | old_vals != hashes[files]]    
        missing_html <- files[!file.exists(html_paths)]
        changed <- unique(c(changed, missing_html))
        changed
    }

    # Clean up HTML files for deleted source files
    existing_html_files <- list.files(file.path(root_dir, "_site"), 
        pattern = "\\.html$", 
        recursive = TRUE, 
        full.names = TRUE)
    if (length(existing_html_files) > 0) {
        existing_html_files <- make_relative(existing_html_files, root_dir)
        deleted_files <- existing_html_files[!(existing_html_files %in% html_paths)]
        if (length(deleted_files) > 0) {
            message("Removing HTML files for deleted source files: ", length(deleted_files))
            message("Deleted HTML files: ", paste(deleted_files, collapse = ", "))
            to_remove <- deleted_files[file.exists(deleted_files)]
            if (length(to_remove) > 0) {
                unlink(to_remove)
            }
        }
    }
    
    

    if (length(changed) == 0) {
        message("No modified render files detected.")
        return(invisible(changed))
    }

    if (dry_run) {
        message("Dry run: ", length(changed), " file(s) would be rendered.")
        return(invisible(changed))
    }
    if (force) {
        quarto::quarto_render()
    } else {
        orig_yml <- file.path(root_dir, quarto_yml)
        backup_yml <- paste0(orig_yml, ".bak-", as.integer(Sys.time()), "-", sample.int(1e6, 1))
        file.copy(orig_yml, backup_yml, overwrite = TRUE)
        on.exit({
            file.copy(backup_yml, orig_yml, overwrite = TRUE)
            unlink(backup_yml)
        }, add = TRUE)

        # Ensure index.qmd is last if present
        idx <- which(changed == "index.qmd")
        if (length(idx) > 0) {
            changed <- c(changed[-idx], changed[idx])
        } else {
            if (file.exists(file.path(root_dir, "index.qmd"))) {
                changed <- c(changed, "index.qmd")
            }
        }

        config <- yaml::read_yaml(orig_yml)
        config$project$render <- changed
        yaml::write_yaml(
            config,
            orig_yml,
            handlers = list(logical = yaml::verbatim_logical)
        )
        tryCatch({
            quarto::quarto_render(quarto_args = c("--no-clean"))
        }, error = function(e) {
            stop("Quarto render failed: ", conditionMessage(e), call. = FALSE)
        })
    }
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(as.list(hashes), cache_path, auto_unbox = TRUE, pretty = TRUE)

    message("Rendered ", length(changed), " modified file(s).")
    invisible(changed)
}


parse_quarto_render_patterns <- function(quarto_path) {
    config <- yaml::read_yaml(quarto_path)
    render <- NULL
    if (!is.null(config$project) && !is.null(config$project$render)) {
        render <- config$project$render
    } else if (!is.null(config$render)) {
        render <- config$render
    }
    if (is.null(render)) {
        return(character(0))
    }
    if (is.character(render)) {
        return(render[nzchar(render)])
    }
    if (is.list(render)) {
        values <- unlist(render, recursive = TRUE, use.names = FALSE)
        values <- values[is.character(values)]
        values <- as.character(values)
        return(values[nzchar(values)])
    }
    character(0)
}


# copy_quarto_site <- function(
#     source_dir = "_site",
#     target_dir,
#     overwrite = TRUE,
#     create_parent = TRUE,
#     verbose = TRUE
# ) {
#     # ---- checks ----
#     if (missing(target_dir) || is.null(target_dir)) {
#         stop("`target_dir` must be provided.")
#     }

#     if (!dir.exists(source_dir)) {
#         stop(sprintf("Source directory does not exist: %s", source_dir))
#     }

#     # normalize paths
#     source_dir <- fs::path_norm(source_dir)
#     target_dir <- fs::path_norm(target_dir)

#     # prevent dangerous copy (copying into itself)
#     if (fs::path_has_parent(target_dir, source_dir)) {
#         stop("Target directory cannot be inside source directory.")
#     }

#     # create parent directory if needed
#     parent_dir <- fs::path_dir(target_dir)
#     if (!dir.exists(parent_dir)) {
#         if (create_parent) {
#             if (verbose) message("Creating parent directory: ", parent_dir)
#             fs::dir_create(parent_dir, recurse = TRUE)
#         } else {
#             stop("Parent directory does not exist: ", parent_dir)
#         }
#     }

#     # remove existing target
#     if (dir.exists(target_dir)) {
#         if (overwrite) {
#             if (verbose) message("Removing existing target: ", target_dir)
#             fs::dir_delete(target_dir)
#         } else {
#             stop("Target already exists. Set `overwrite = TRUE` to replace it.")
#         }
#     }

#     # ---- copy ----
#     if (verbose) {
#         message("Copying Quarto site:")
#         message("  from: ", source_dir)
#         message("  to:   ", target_dir)
#     }

#     fs::dir_copy(source_dir, target_dir)

#     if (verbose) message("Done ✔")

#     return(invisible(target_dir))
# }
