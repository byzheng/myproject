
#' Copies the contents of a Quarto site's output directory (default '_site') to a specified target directory.
#' Handles overwriting, parent directory creation, and prevents copying into itself.
#'
#' @param source_dir Character scalar. Source directory to copy from (default '_site').
#' @param target_dir Character scalar. Target directory to copy to. Must be provided.
#' @param overwrite Logical scalar. If TRUE, overwrites existing target directory (default TRUE).
#' @param create_parent Logical scalar. If TRUE, creates parent directory if it does not exist (default TRUE).
#' @param verbose Logical scalar. If TRUE, prints progress messages (default TRUE).
#' @return Invisibly returns the target directory path.
#' @export
#' @examples
#' \dontrun{
#' copy_quarto_site(source_dir = "_site", target_dir = "docs")
#' }
copy_quarto_site <- function(
    source_dir = "_site",
    target_dir,
    overwrite = TRUE,
    create_parent = TRUE,
    verbose = TRUE
) {
    # ---- checks ----
    if (missing(target_dir) || is.null(target_dir)) {
        stop("`target_dir` must be provided.")
    }

    if (!dir.exists(source_dir)) {
        stop(sprintf("Source directory does not exist: %s", source_dir))
    }

    # normalize paths
    source_dir <- fs::path_norm(source_dir)
    target_dir <- fs::path_norm(target_dir)

    # prevent dangerous copy (copying into itself)
    if (fs::path_has_parent(target_dir, source_dir)) {
        stop("Target directory cannot be inside source directory.")
    }

    # create parent directory if needed
    parent_dir <- fs::path_dir(target_dir)
    if (!dir.exists(parent_dir)) {
        if (create_parent) {
            if (verbose) message("Creating parent directory: ", parent_dir)
            fs::dir_create(parent_dir, recurse = TRUE)
        } else {
            stop("Parent directory does not exist: ", parent_dir)
        }
    }

    # remove existing target
    if (dir.exists(target_dir)) {
        if (overwrite) {
            if (verbose) message("Removing existing target: ", target_dir)
            fs::dir_delete(target_dir)
        } else {
            stop("Target already exists. Set `overwrite = TRUE` to replace it.")
        }
    }

    # ---- copy ----
    if (verbose) {
        message("Copying Quarto site:")
        message("  from: ", source_dir)
        message("  to:   ", target_dir)
    }

    fs::dir_copy(source_dir, target_dir)

    if (verbose) message("Done ✔")

    return(invisible(target_dir))
}
