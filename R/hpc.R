#' Create HPC Sentinel File with Input Tracking
#'
#' @param sentinel_path Path to sentinel file to create
#' @param input_files Character vector of input file paths that HPC job depends on
#' @param metadata Optional list of additional metadata to store
#'
#' @details
#' Creates a JSON sentinel file that records:
#' - Completion timestamp
#' - Input files and their modification times
#' - Optional custom metadata
#'
#' @examples
#' \dontrun{
#' create_hpc_sentinel(
#'   sentinel_path = "output/.hpc_complete",
#'   input_files = c("output/data_cleaned.rds", "output/params.rds"),
#'   metadata = list(job_id = "12345", nodes = 4)
#' )
#' }
#' @export 
create_hpc_sentinel <- function(sentinel_path, input_files, metadata = list()) {
    stopifnot(is.character(input_files), length(input_files) > 0)
    
    # Get modification times for all input files
    input_mtimes <- vapply(input_files, function(f) {
        if (!file.exists(f)) {
            warning("Input file not found: ", f)
            return(NA_character_)
        }
        as.character(file.info(f)$mtime)
    }, character(1))
    
    # Build sentinel metadata
    sentinel_data <- list(
        completed_at = as.character(Sys.time()),
        input_files = as.list(stats::setNames(input_mtimes, input_files)),
        metadata = metadata
    )
    
    # Write as JSON
    dir.create(dirname(sentinel_path), recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(sentinel_data, sentinel_path, 
                         pretty = TRUE, auto_unbox = TRUE)
    
    message("Created sentinel: ", sentinel_path)
    message("  Completed: ", sentinel_data$completed_at)
    message("  Tracked ", length(input_files), " input file(s)")
    
    invisible(sentinel_path)
}


#' Check HPC Sentinel Staleness for targets
#'
#' @param sentinel_path Path to sentinel file
#' @param input_files Character vector of current input file paths
#' @param on_missing Action when sentinel missing: "stop" (default) or "warn"
#' @param on_stale Action when sentinel stale: "stop" (default), "warn", or "delete"
#'
#' @return Sentinel path if valid, otherwise stops with informative error
#'
#' @details
#' Validates sentinel by checking:
#' 1. Sentinel file exists
#' 2. Input files haven't changed since HPC job ran
#' 3. All expected input files are tracked in sentinel
#'
#' If validation fails, provides clear error message and optionally deletes stale sentinel.
#'
#' @examples
#' \dontrun{
#' # In targets file:
#' targets::tar_target(
#'   file_hpc_complete,
#'   {
#'     upstream_inputs <- c(file_cleaned_data, file_params)
#'     check_hpc_sentinel(
#'       sentinel_path = here::here("output/.hpc_complete"),
#'       input_files = upstream_inputs
#'     )
#'   },
#'   format = "file"
#' )
#' }
#' @export
check_hpc_sentinel <- function(sentinel_path, 
                               input_files,
                               on_missing = c("stop", "warn"),
                               on_stale = c("stop", "warn", "delete")) {
    stopifnot(is.character(input_files), length(input_files) > 0)
    stopifnot(is.character(sentinel_path), length(sentinel_path) == 1)
    on_missing <- match.arg(on_missing)
    on_stale <- match.arg(on_stale)
    
    # Check if sentinel exists
    if (!file.exists(sentinel_path)) {
        msg <- paste0(
            "HPC job not completed yet.\n",
            "Sentinel file not found: ", sentinel_path, "\n",
            "Run HPC script to create it."
        )
        if (on_missing == "stop") stop(msg, call. = FALSE)
        warning(msg)
        return(NULL)
    }
    
    # Read sentinel metadata
    tryCatch({
        sentinel_data <- jsonlite::read_json(sentinel_path)
    }, error = function(e) {
        unlink(sentinel_path)
        stop(
            "Sentinel file corrupted: ", sentinel_path, "\n",
            "Deleted. Re-run HPC job.\n",
            "Error: ", e$message,
            call. = FALSE
        )
    })
    
    # Validate structure
    if (is.null(sentinel_data$input_files)) {
        unlink(sentinel_path)
        stop(
            "Sentinel file has invalid format (missing input_files).\n",
            "Deleted. Re-run HPC job.",
            call. = FALSE
        )
    }
    
    stored_files <- names(sentinel_data$input_files)
    
    # Check for missing inputs
    missing_inputs <- setdiff(input_files, stored_files)
    if (length(missing_inputs) > 0) {
        if (on_stale == "delete") unlink(sentinel_path)
        msg <- paste0(
            "HPC sentinel is incomplete.\n",
            "Missing tracked inputs: ", paste(missing_inputs, collapse = ", "), "\n",
            if (on_stale == "delete") "Deleted sentinel. " else "",
            "Re-run HPC job."
        )
        if (on_stale == "stop" || on_stale == "delete") {
            stop(msg, call. = FALSE)
        } else {
            warning(msg)
        }
    }
    
    # Check for stale inputs (files modified after HPC ran)
    stale_inputs <- character(0)
    for (input_file in input_files) {
        if (!file.exists(input_file)) {
            stop("Input file not found: ", input_file, call. = FALSE)
        }
        
        current_mtime <- file.info(input_file)$mtime
        stored_mtime <- as.POSIXct(sentinel_data$input_files[[input_file]])
        
        if (is.na(stored_mtime)) {
            stale_inputs <- c(stale_inputs, 
                            paste0(input_file, " (not tracked)"))
        } else if (difftime(current_mtime, stored_mtime, units = "secs") > 1) {
            # Use 1-second tolerance to handle floating-point precision and filesystem timestamp resolution
            stale_inputs <- c(stale_inputs, 
                            paste0(input_file, " (", current_mtime, " > ", stored_mtime, ")"))
        }
    }
    
    if (length(stale_inputs) > 0) {
        if (on_stale == "delete") unlink(sentinel_path)
        msg <- paste0(
            "Input data changed after HPC job completed.\n",
            "Stale inputs:\n  ",
            paste(stale_inputs, collapse = "\n  "), "\n",
            if (on_stale == "delete") "Deleted sentinel. " else "",
            "Re-run HPC job."
        )
        if (on_stale == "stop" || on_stale == "delete") {
            stop(msg, call. = FALSE)
        } else {
            warning(msg)
        }
    }
    
    # All checks passed
    sentinel_path
}


#' Get Sentinel Metadata
#'
#' @param sentinel_path Path to sentinel file
#' @return List with completion time, input files, and custom metadata
#'
#' @examples
#' \dontrun{
#' meta <- get_hpc_sentinel_metadata("output/.hpc_complete")
#' cat("HPC completed at:", meta$completed_at, "\n")
#' }
#' @export 
get_hpc_sentinel_metadata <- function(sentinel_path) {
    if (!file.exists(sentinel_path)) {
        stop("Sentinel file not found: ", sentinel_path, call. = FALSE)
    }
    jsonlite::read_json(sentinel_path)
}
