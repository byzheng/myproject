
#' Read data from a file path
#'
#' Reads `.rds` or `.csv` files using a path relative to the project root.
#' Optionally converts the result to a tibble when the `tibble` package is
#' available.
#'
#' @param path Character scalar. Relative path to the data file.
#' @param as_tibble Logical scalar. Whether to coerce output to tibble when
#'   possible. Defaults to `TRUE`.
#' @param ... Additional arguments passed to [base::readRDS()] or
#'   [utils::read.csv()].
#'
#' @return A data object read from disk, optionally converted to a tibble.
#' @export
#' @examples
#' \dontrun{
#' read_data("data/example.rds")
#' read_data("data/example.csv", stringsAsFactors = FALSE)
#' }
read_data <- function(path, as_tibble = TRUE, ...) {
    stopifnot(is.character(path), length(path) == 1)
    path <- here::here(path)
    stopifnot(file.exists(path))
    stopifnot(is.logical(as_tibble), length(as_tibble) == 1)

    ext <- tolower(tools::file_ext(path))

    data <- switch(ext,
        "rds" = readRDS(path, ...),
        "csv" = utils::read.csv(path, ...),
        stop("Unsupported file type: ", ext)
    )

    if (as_tibble && requireNamespace("tibble", quietly = TRUE)) {
        data <- tibble::as_tibble(data)
    }

    data
}
