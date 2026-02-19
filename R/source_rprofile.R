#' Source the nearest `.Rprofile` from a directory tree
#'
#' Starting from `start`, this function searches for a `.Rprofile` file in the
#' current directory and then each parent directory until `stop_at` (inclusive)
#' or the filesystem root is reached. The first `.Rprofile` found is sourced and
#' the search stops immediately.
#'
#' @param start Character scalar. Directory where the search begins.
#'   Defaults to [base::getwd()].
#' @param stop_at Character scalar. Directory at which upward searching stops.
#'   Defaults to `"/"`.
#'
#' @return Invisibly returns the path to the sourced `.Rprofile` file, or
#'   `NULL` if no `.Rprofile` is found.
#' @export 
source_rprofile <- function(start = getwd(), stop_at = "/") {
    start <- normalizePath(start, winslash = "/", mustWork = TRUE)
    stop_at <- normalizePath(stop_at, winslash = "/", mustWork = FALSE)
    
    current <- start
    while(TRUE) {
        rprofile_path <- file.path(current, ".Rprofile")
        if (file.exists(rprofile_path)) {
        message("Sourcing .Rprofile: ", rprofile_path)
        source(rprofile_path, local = TRUE)
        return(invisible(rprofile_path))  # stop after first
        }
        
        if (current == stop_at || dirname(current) == current) break
        current <- dirname(current)
    }
    
    invisible(NULL)
}