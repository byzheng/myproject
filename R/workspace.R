#' Build a path relative to the workspace root
#'
#' Resolves path components against the workspace root discovered by
#' [find_ws()].
#'
#' @param ... Path components passed to [base::file.path()].
#'
#' @return A path string relative to [base::getwd()] when possible.
#' @export
path_ws <- function(...) {
    root_dir <- find_ws()
    target_path <- normalizePath(file.path(root_dir, ...), mustWork = FALSE)
    return(make_relative(target_path, start = getwd()))
}


#' Find the workspace root directory
#'
#' Locates the workspace root by looking for a `.workspace` file.
#'
#' @param path Starting path to search from. Defaults to current directory.
#'
#' @return The workspace root directory path.
#' @export
find_ws <- function(path = ".") {
    rprojroot::find_root(rprojroot::has_file(".workspace"), path = path)
}
