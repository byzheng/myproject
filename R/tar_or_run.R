
#' Try to Read a targets Target, or Run a Fallback Expression
#'
#' Attempts to read a computed target from a targets pipeline using
#' `targets::tar_read()`. If the target cannot be read (e.g., it hasn't been
#' computed yet), runs a fallback expression instead. Useful in interactive
#' workflows where a target may not always be available.
#'
#' @param target Unquoted name of the target to read (not a string).
#' @param expr Expression to evaluate if target read fails. Will be evaluated
#'   lazily only if needed.
#' @param quiet Logical. If `FALSE` (default), print messages explaining why
#'   the target read failed and that the fallback is running. If `TRUE`,
#'   suppress these messages.
#' @param ... Additional arguments passed to `targets::tar_read()`, such as `branches` or `store`. This allows for more flexible target reading, such as reading specific branches of a dynamic target or from a non-default store.
#'
#' @return The value of the read target if successful, otherwise the result of
#'   evaluating `expr`.
#' @export
#' @examples
#' \dontrun{
#' # Try to read a computed target, fall back to a simple value
#' tar_or_run(my_data, data.frame(x = 1:5))
#'
#' # Suppress messages when using interactively
#' tar_or_run(results, compute_results(), quiet = TRUE)
#' }
tar_or_run <- function(target, expr, quiet = FALSE, ...) {
    target_quo <- rlang::enquo(target)
    if (rlang::quo_is_missing(target_quo)) {
        stop("`target` must be provided")
    }

    if (!rlang::is_symbol(rlang::quo_get_expr(target_quo))) {
        stop("`target` must be an unquoted name (not a string)")
    }

    target_name <- rlang::as_name(target_quo)
    dots <- rlang::list2(...)
    read_call <- rlang::call2(
        targets::tar_read,
        rlang::sym(target_name),
        !!!dots
    )

    tryCatch(
        rlang::eval_bare(read_call),
        error = function(e) {
            if (!quiet) {
                message("Target read failed: ", conditionMessage(e))
                message("Running fallback expression instead.")
            }
            force(expr)
        }
    )
}
