#' Create External Sentinel File with Input Tracking
#'
#' @param sentinel_path Path to sentinel file, either relative to project root or absolute
#' @param input_files Character vector of input file paths, either relative to project root or absolute
#' @param metadata Optional list of additional metadata to store
#'
#' @details
#' Tracks external dependencies (HPC jobs, shell scripts, database exports, etc.) using sentinel files.
#' All paths are resolved relative to the project root using `here::here()`. Absolute paths 
#' are automatically converted to relative paths for storage, ensuring sentinels work consistently 
#' across platforms (Windows/Linux/Mac) and different working directories.
#' 
#' All timestamps are stored in UTC to ensure consistency when sentinels are shared across 
#' machines in different timezones (e.g., Sydney to Brisbane, or across international systems).
#' 
#' Examples of valid calls:
#' - Relative paths: `create_external_sentinel(".external/job.json", c("data/raw.csv", "params.json"))`
#' - Absolute paths: `create_external_sentinel("/full/path/.external/job.json", c("/full/path/data/raw.csv"))`
#' - Mixed: `create_external_sentinel("./external/job.json", "/full/path/data/raw.csv")`
#'
#' Creates a JSON sentinel file that records:
#' - Completion timestamp (UTC)
#' - Input files (stored as relative paths) and their modification times (UTC)
#' - Optional custom metadata
#'
#' @examples
#' \dontrun{
#' # Using relative paths (recommended)
#' create_external_sentinel(
#'   sentinel_path = ".external/job_complete.json",
#'   input_files = c("data/raw.csv", "params.json"),
#'   metadata = list(job_id = "12345", nodes = 4)
#' )
#' }
#' @export 
create_external_sentinel <- function(sentinel_path, input_files, metadata = list()) {
    stopifnot(is.character(input_files), length(input_files) > 0)
    
    # Resolve paths relative to project root (suppress warnings for non-existent paths)
    sentinel_abs <- suppressWarnings(normalizePath(suppressWarnings(here::here(sentinel_path)), winslash = "/"))
    input_abs <- suppressWarnings(normalizePath(suppressWarnings(here::here(input_files)), winslash = "/"))
    project_root <- suppressWarnings(normalizePath(suppressWarnings(here::here()), winslash = "/"))
    
    # Convert absolute paths back to relative for storage (portability)
    sentinel_rel <- if (grepl("^/|^[A-Z]:", sentinel_path)) {
        make_relative(sentinel_abs, project_root)
    } else {
        sentinel_path
    }
    
    input_rel <- if (all(grepl("^/|^[A-Z]:", input_files))) {
        sapply(input_abs, make_relative, project_root, USE.NAMES = FALSE)
    } else {
        input_files
    }
    
    # Get modification times for all input files (convert to UTC for cross-timezone consistency)
    input_mtimes <- vapply(input_abs, function(f) {
        if (!file.exists(f)) {
            warning("Input file not found: ", f)
            return(NA_character_)
        }
        mtime_utc <- as.POSIXct(file.info(f)$mtime, tz = "UTC")
        as.character(mtime_utc)
    }, character(1))
    
    # Build sentinel metadata using relative paths for portability
    sentinel_data <- list(
        completed_at = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
        input_files = as.list(stats::setNames(input_mtimes, input_rel)),
        metadata = metadata
    )
    
    # Write as JSON
    dir.create(dirname(sentinel_abs), recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(sentinel_data, sentinel_abs, 
                         pretty = TRUE, auto_unbox = TRUE)
    
    message("Created sentinel: ", sentinel_rel)
    message("  Completed: ", sentinel_data$completed_at)
    message("  Tracked ", length(input_rel), " input file(s)")
    
    invisible(sentinel_rel)
}


#' Check External Sentinel Staleness for targets
#'
#' @param sentinel_path Path to sentinel file, either relative to project root or absolute
#' @param input_files Character vector of input file paths, either relative to project root or absolute
#' @param on_missing Action when sentinel missing: "stop" (default) or "warn"
#' @param on_stale Action when sentinel stale: "stop" (default), "warn", or "delete"
#'
#' @return Relative path to sentinel (invisibly) if valid, otherwise stops with informative error
#'
#' @details
#' All paths are resolved relative to the project root using `here::here()`. Absolute paths 
#' are automatically converted to relative paths for comparison with stored paths, ensuring
#' consistency across platforms and working directories.
#'
#' All timestamps are compared in UTC, ensuring correct staleness detection even when sentinels 
#' are shared across machines in different timezones.
#'
#' Validates sentinel by checking:
#' 1. Sentinel file exists
#' 2. Input files haven't changed since external process ran (compared in UTC)
#' 3. All expected input files are tracked in sentinel
#'
#' If validation fails, provides clear error message and optionally deletes stale sentinel.
#'
#' @examples
#' \dontrun{
#' # Using relative paths (recommended)
#' check_external_sentinel(
#'   sentinel_path = ".external/job_complete.json",
#'   input_files = c("data/raw.csv", "params.json")
#' )
#' }
#' @export
check_external_sentinel <- function(sentinel_path, 
                               input_files,
                               on_missing = c("stop", "warn"),
                               on_stale = c("stop", "warn", "delete")) {
    stopifnot(is.character(input_files), length(input_files) > 0)
    stopifnot(is.character(sentinel_path), length(sentinel_path) == 1)
    on_missing <- match.arg(on_missing)
    on_stale <- match.arg(on_stale)
    
    # Resolve paths relative to project root (suppress warnings for non-existent paths)
    sentinel_abs <- suppressWarnings(normalizePath(suppressWarnings(here::here(sentinel_path)), winslash = "/"))
    input_abs <- suppressWarnings(normalizePath(suppressWarnings(here::here(input_files)), winslash = "/"))
    project_root <- suppressWarnings(normalizePath(suppressWarnings(here::here()), winslash = "/"))
    
    # Convert absolute paths back to relative for error messages and return value
    sentinel_rel <- if (grepl("^/|^[A-Z]:", sentinel_path)) {
        make_relative(sentinel_abs, project_root)
    } else {
        sentinel_path
    }
    
    input_rel <- if (all(grepl("^/|^[A-Z]:", input_files))) {
        sapply(input_abs, make_relative, project_root, USE.NAMES = FALSE)
    } else {
        input_files
    }
    
    # Check if sentinel exists
    if (!file.exists(sentinel_abs)) {
        msg <- paste0(
            "External process not completed yet.\n",
            "Sentinel file not found: ", sentinel_rel, "\n",
            "Run external process to create it."
        )
        if (on_missing == "stop") stop(msg, call. = FALSE)
        warning(msg)
        return(NULL)
    }
    
    # Read sentinel metadata
    tryCatch({
        sentinel_data <- jsonlite::read_json(sentinel_abs)
    }, error = function(e) {
        unlink(sentinel_abs)
        stop(
            "Sentinel file corrupted: ", sentinel_rel, "\n",
            "Deleted. Re-run external process.\n",
            "Error: ", e$message,
            call. = FALSE
        )
    })
    
    # Validate structure
    if (is.null(sentinel_data$input_files)) {
        unlink(sentinel_abs)
        stop(
            "Sentinel file has invalid format (missing input_files).\n",
            "Deleted. Re-run external process.",
            call. = FALSE
        )
    }
    
    stored_files <- names(sentinel_data$input_files)
    
    # Check for missing inputs
    missing_inputs <- setdiff(input_rel, stored_files)
    if (length(missing_inputs) > 0) {
        if (on_stale == "delete") unlink(sentinel_abs)
        msg <- paste0(
            "External process sentinel is incomplete.\n",
            "Missing tracked inputs: ", paste(missing_inputs, collapse = ", "), "\n",
            if (on_stale == "delete") "Deleted sentinel. " else "",
            "Re-run external process."
        )
        if (on_stale == "stop" || on_stale == "delete") {
            stop(msg, call. = FALSE)
        } else {
            warning(msg)
        }
    }
    
    # Check for stale inputs (files modified after external process ran, comparing in UTC)
    stale_inputs <- character(0)
    for (i in seq_along(input_abs)) {
        input_file <- input_abs[i]
        input_name <- input_rel[i]
        
        if (!file.exists(input_file)) {
            stop("Input file not found: ", input_name, call. = FALSE)
        }
        
        current_mtime <- as.POSIXct(file.info(input_file)$mtime, tz = "UTC")
        stored_mtime <- as.POSIXct(sentinel_data$input_files[[input_name]], tz = "UTC")
        
        if (is.na(stored_mtime)) {
            stale_inputs <- c(stale_inputs, 
                            paste0(input_name, " (not tracked)"))
        } else if (difftime(current_mtime, stored_mtime, units = "secs") > 1) {
            # Use 1-second tolerance to handle floating-point precision and filesystem timestamp resolution
            stale_inputs <- c(stale_inputs, 
                            paste0(input_name, " (", current_mtime, " > ", stored_mtime, ")"))
        }
    }
    
    if (length(stale_inputs) > 0) {
        if (on_stale == "delete") unlink(sentinel_abs)
        msg <- paste0(
            "Input data changed after external process completed.\n",
            "Stale inputs:\n  ",
            paste(stale_inputs, collapse = "\n  "), "\n",
            if (on_stale == "delete") "Deleted sentinel. " else "",
            "Re-run external process."
        )
        if (on_stale == "stop" || on_stale == "delete") {
            stop(msg, call. = FALSE)
        } else {
            warning(msg)
        }
    }
    
    # All checks passed
    sentinel_rel
}


#' Get External Sentinel Metadata
#'
#' @param sentinel_path Path to sentinel file, either relative to project root or absolute
#' @return List with completion time, input files (as relative paths), and custom metadata
#'
#' @details
#' The sentinel path is resolved relative to the project root using `here::here()`. 
#' Absolute paths are automatically converted to relative paths.
#'
#' @examples
#' \dontrun{
#' meta <- get_external_sentinel_metadata(".external/job_complete.json")
#' cat("External process completed at:", meta$completed_at, "\n")
#' }
#' @export 
get_external_sentinel_metadata <- function(sentinel_path) {
    sentinel_abs <- suppressWarnings(here::here(sentinel_path))
    if (!file.exists(sentinel_abs)) {
        stop("Sentinel file not found: ", sentinel_path, call. = FALSE)
    }
    jsonlite::read_json(sentinel_abs)
}


