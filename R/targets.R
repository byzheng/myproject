#' Load and Combine targets Objects
#'
#' Recursively finds and sources target definition scripts, then collects all
#' objects in scope with names beginning with `_targets_` and combines them into
#' a single vector or list using `c()`.
#'
#' @param path Character vector of directories to search for target scripts.
#'   Defaults to `c("script", "source", "story")`.
#' @param pattern Character scalar regular expression used to match target
#'   script file names. Defaults to `"^_targets_.*\\.R$"`.
#'
#' @return A combined object created from all objects whose names match
#'   `"^targets_"` after sourcing matched files, plus any tar_target objects
#'   created as the final expression in sourced files (even if not assigned).
#' @details
#' All matched `targets_*` objects must be either:
#' \itemize{
#'   \item A single object inheriting from class `"tar_target"`, OR
#'   \item A list where all elements inherit from class `"tar_target"`
#' }
#' Alternatively, a file can contain a bare tar_target call (e.g., 
#' `targets::tar_target(alpha, 1)`) as its final expression, which will be
#' captured and named after its internal target name.
#' The function validates this after each sourced file and errors with file
#' context if any matched object does not meet these criteria. It also errors on
#' duplicate internal target names. If sourcing a matched file fails, the
#' function rethrows with the source file path.
#' @export
#' @examples
#' \dontrun{
#' # Single tar_target per variable
#' dir.create("script")
#' writeLines(
#'   "targets_alpha <- targets::tar_target(alpha, 1)",
#'   "script/_targets_alpha.R"
#' )
#' 
#' # List of tar_targets per variable
#' writeLines(
#'   "targets_multi <- list(targets::tar_target(beta, 2), targets::tar_target(gamma, 3))",
#'   "script/_targets_multi.R"
#' )
#' 
#' get_targets(path = "script")
#' }
get_targets <- function(
    path = c("script", "source", "story"),
    pattern = "^_targets_.*\\.R$") {
    stopifnot(is.character(path), length(path) > 0)
    stopifnot(is.character(pattern), length(pattern) == 1)

    target_env <- new.env(parent = baseenv())

    target_files <- sort(
        list.files(
            path = path,
            pattern = pattern,
            recursive = TRUE,
            full.names = TRUE
        )
    )
    if (length(target_files) == 0) {
        message("No target files found in specified paths.")
        return(NULL)
    }
    for (target_file in target_files) {
        file_env <- new.env(parent = target_env)
        source_result <- tryCatch(
            {
                source(target_file, local = file_env)
            },
            error = function(error_condition) {
                error_condition
            }
        )

        if (inherits(source_result, "error")) {
            stop(
                sprintf(
                    "Failed to source target file %s: %s",
                    target_file,
                    conditionMessage(source_result)
                ),
                call. = FALSE
            )
        }

        # Capture the last evaluated expression from source()
        # source() returns a list with "value" element containing the last expression
        last_value <- source_result$value
        
        file_object_names <- ls(envir = file_env)
        file_target_names <- sort(ls(envir = file_env, pattern = "^targets_"))

        # Check if last expression is a bare tar_target or list of tar_targets
        # Only treat as bare if there are NO assigned targets_* variables
        bare_tar_target <- NULL
        if (length(file_target_names) == 0) {
            is_bare_single <- inherits(last_value, "tar_target")
            is_bare_list <- is.list(last_value) && length(last_value) > 0 && 
                            all(vapply(last_value, inherits, logical(1), what = "tar_target"))
            
            if ((is_bare_single || is_bare_list)) {
                # This is a bare tar_target/list returned from source (not from an assignment)
                bare_tar_target <- last_value
            }
        }

        if (length(file_target_names) == 0 && is.null(bare_tar_target)) {
            list2env(mget(file_object_names, envir = file_env, inherits = FALSE), envir = target_env)
            next
        }

        existing_target_names <- ls(envir = target_env, pattern = "^targets_")

        file_target_objects <- mget(
            file_target_names,
            envir = file_env,
            inherits = FALSE
        )
        
        # Add bare tar_target if found
        if (!is.null(bare_tar_target)) {
            if (inherits(bare_tar_target, "tar_target")) {
                # Name it after its internal target name, prefixed with "targets_"
                obj_name <- paste0("targets_", bare_tar_target$name)
            } else if (is.list(bare_tar_target)) {
                # Use a generated name combining all internal target names
                obj_name <- paste0("targets_", paste(
                    vapply(bare_tar_target, function(x) x$name, character(1)),
                    collapse = "_"
                ))
            }
            file_target_objects[[obj_name]] <- bare_tar_target
            file_target_names <- c(file_target_names, obj_name)
        }
        
        # Validate: each object must be a tar_target OR a list of tar_targets
        for (obj_name in names(file_target_objects)) {
            obj <- file_target_objects[[obj_name]]
            
            if (inherits(obj, "tar_target")) {
                # Single tar_target: valid
                next
            } else if (is.list(obj)) {
                # List: check all elements are tar_targets
                if (length(obj) == 0) {
                    stop(
                        sprintf(
                            "Empty list after sourcing %s: %s",
                            target_file,
                            obj_name
                        ),
                        call. = FALSE
                    )
                }
                all_tar_targets <- all(vapply(obj, inherits, logical(1), what = "tar_target"))
                if (!all_tar_targets) {
                    stop(
                        sprintf(
                            paste0(
                                "Object %s in %s is a list but not all elements ",
                                "inherit from 'tar_target'"
                            ),
                            obj_name,
                            target_file
                        ),
                        call. = FALSE
                    )
                }
            } else {
                # Neither tar_target nor list: invalid
                stop(
                    sprintf(
                        paste0(
                            "Object %s in %s must be a tar_target or a list of tar_targets"
                        ),
                        obj_name,
                        target_file
                    ),
                    call. = FALSE
                )
            }
        }

        existing_target_objects <- mget(
            existing_target_names,
            envir = target_env,
            inherits = FALSE,
            ifnotfound = list(NULL)
        )
        existing_target_objects <- existing_target_objects[!vapply(existing_target_objects, is.null, logical(1))]

        # Extract internal names from existing objects (handle both single and list)
        existing_internal_names <- character(0)
        for (obj in existing_target_objects) {
            if (inherits(obj, "tar_target")) {
                existing_internal_names <- c(existing_internal_names, obj$name)
            } else if (is.list(obj)) {
                existing_internal_names <- c(existing_internal_names, 
                    vapply(obj, function(x) x$name, character(1)))
            }
        }
        
        # Extract internal names from file objects (handle both single and list)
        file_internal_names <- character(0)
        for (obj in file_target_objects) {
            if (inherits(obj, "tar_target")) {
                file_internal_names <- c(file_internal_names, obj$name)
            } else if (is.list(obj)) {
                file_internal_names <- c(file_internal_names,
                    vapply(obj, function(x) x$name, character(1)))
            }
        }

        duplicated_internal_names <- intersect(file_internal_names, existing_internal_names)
        if (length(duplicated_internal_names) > 0) {
            stop(
                sprintf(
                    paste0(
                        "Duplicate internal target names after sourcing %s: %s"
                    ),
                    target_file,
                    paste(duplicated_internal_names, collapse = ", ")
                ),
                call. = FALSE
            )
        }

        list2env(file_target_objects, envir = target_env)
        
        # Also copy any other non-targets_ objects from the file to maintain state
        # (for cases where one targets_* object references another variable)
        other_object_names <- setdiff(file_object_names, file_target_names)
        if (length(other_object_names) > 0) {
            list2env(mget(other_object_names, envir = file_env, inherits = FALSE), envir = target_env)
        }
    }

    target_object_names <- sort(ls(envir = target_env, pattern = "^targets_"))

    if (length(target_object_names) == 0) {
        return(NULL)
    }

    target_objects <- mget(target_object_names, envir = target_env, inherits = FALSE)

    do.call(c, target_objects)
}