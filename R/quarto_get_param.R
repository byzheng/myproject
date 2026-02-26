#' Get a Quarto parameter value
#'
#' Returns a value from `params` when available, otherwise evaluates an
#' optional default function.
#'
#' @param name Character scalar. Name of the parameter to retrieve.
#' @param default_fun Optional function used to compute a default value when
#'   the parameter is missing or `NULL`.
#' @param ... Additional arguments passed to `default_fun`.
#'
#' @return The parameter value if found and not `NULL`; otherwise the return
#'   value from `default_fun(...)` when provided; otherwise `NULL`.
#' @export
#' @examples
#' get_param("missing")
#' get_param("x", function() 42)
get_param <- function(name, default_fun = NULL, ...) {
    stopifnot(is.character(name), length(name) == 1)
    stopifnot(is.null(default_fun) || is.function(default_fun))
    if (exists("params", inherits = TRUE)) {
        params_obj <- get("params", inherits = TRUE)
        if (!is.null(params_obj[[name]])) {
            return(params_obj[[name]])
        }
    }
    if (!is.null(default_fun)) {
        return(default_fun(...))
    }

    NULL
}

