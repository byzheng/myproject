

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
    sort(unique(all_files))
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
#' @param quarto_bin Character scalar. Quarto executable name or path.
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
    quarto_bin = "quarto",
    dry_run = FALSE,
    force = FALSE
) {
    stopifnot(is.logical(dry_run), length(dry_run) == 1)
    stopifnot(is.logical(force), length(force) == 1)
    stopifnot(is.character(cache_file), length(cache_file) == 1)
    stopifnot(is.character(quarto_bin), length(quarto_bin) == 1)

    files <- list_quarto_render_files(quarto_yml = quarto_yml, root_dir = root_dir)
    if (length(files) == 0) {
        message("No files matched `_quarto.yml` render patterns.")
        return(invisible(character(0)))
    }

    abs_files <- file.path(root_dir, files)
    hashes <- as.character(tools::md5sum(abs_files))
    names(hashes) <- files

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

    changed <- if (force) {
        files
    } else {
        old_vals <- old_hashes[files]
        files[is.na(old_vals) | old_vals != hashes[files]]
    }

    if (length(changed) == 0) {
        message("No modified render files detected.")
        return(invisible(changed))
    }

    if (dry_run) {
        message("Dry run: ", length(changed), " file(s) would be rendered.")
        return(invisible(changed))
    }
    changed_full_path <- file.path(root_dir, changed)
    changed_full_path <- paste(shQuote(changed_full_path), collapse = " ")
    cmd_out <- system2(quarto_bin, c("render", changed_full_path), stdout = TRUE, stderr = TRUE)
    status <- attr(cmd_out, "status")
    if (is.null(status)) {
        status <- 0
    }
    if (!identical(status, 0L)) {
        stop(
            "Quarto render failed with status ", status, "\n",
            paste(cmd_out, collapse = "\n"),
            call. = FALSE
        )
    }

    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(as.list(hashes), cache_path, auto_unbox = TRUE, pretty = TRUE)

    message("Rendered ", length(changed), " modified file(s).")
    invisible(changed)
}


parse_quarto_render_patterns <- function(quarto_path) {
    config <- yaml::read_yaml(quarto_path)
    if (is.null(config) || is.null(config$render)) {
        return(character(0))
    }

    render <- config$render

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


strip_outer_quotes <- function(x) {
    x <- trimws(x)
    x <- sub("^\"(.*)\"$", "\\1", x)
    x <- sub("^'(.*)'$", "\\1", x)
    x
}


expand_brace_patterns <- function(pattern) {
    open_pos <- regexpr("\\{", pattern, perl = TRUE)[1]
    if (open_pos < 1) {
        return(pattern)
    }

    rest <- substr(pattern, open_pos + 1, nchar(pattern))
    close_rel <- regexpr("\\}", rest, perl = TRUE)[1]
    if (close_rel < 1) {
        return(pattern)
    }

    close_pos <- open_pos + close_rel
    prefix <- substr(pattern, 1, open_pos - 1)
    inner <- substr(pattern, open_pos + 1, close_pos - 1)
    suffix <- substr(pattern, close_pos + 1, nchar(pattern))

    choices <- strsplit(inner, ",", fixed = TRUE)[[1]]
    expanded <- unlist(lapply(choices, function(choice) {
        expand_brace_patterns(paste0(prefix, choice, suffix))
    }), use.names = FALSE)

    unique(expanded)
}


glob_to_regex <- function(pattern) {
    if (!nzchar(pattern)) {
        return("^$")
    }

    chars <- strsplit(pattern, "", fixed = TRUE)[[1]]
    out <- character(0)
    i <- 1L

    while (i <= length(chars)) {
        ch <- chars[i]
        if (ch == "*") {
            if (i < length(chars) && chars[i + 1L] == "*") {
                out <- c(out, ".*")
                i <- i + 2L
            } else {
                out <- c(out, "[^/]*")
                i <- i + 1L
            }
        } else if (ch == "?") {
            out <- c(out, "[^/]")
            i <- i + 1L
        } else if (ch %in% c(".", "^", "$", "|", "(", ")", "[", "]", "+", "\\")) {
            out <- c(out, paste0("\\\\", ch))
            i <- i + 1L
        } else {
            out <- c(out, ch)
            i <- i + 1L
        }
    }

    paste0("^", paste(out, collapse = ""), "$")
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
