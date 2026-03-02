
# Helper function to convert absolute path to relative
make_relative <- function(target, start) {
    stopifnot(is.character(target), length(target) > 0)
    stopifnot(is.character(start), length(start) == 1)
    start <- suppressWarnings(normalizePath(start, mustWork = FALSE, winslash = "/"))
    start_parts <- strsplit(start, "/", fixed = TRUE)[[1]]

    rel_paths <- c()
    for (i in seq_along(target)) {
        target_norm <- normalizePath(target[i], mustWork = FALSE, winslash = "/")    
        target_parts <- strsplit(target_norm, "/", fixed = TRUE)[[1]]
        
        if (length(target_parts) > 0 && length(start_parts) > 0 &&
            !identical(tolower(target_parts[[1]]), tolower(start_parts[[1]]))) {
            return(target_norm)
        }

        common <- 0L
        max_common <- min(length(target_parts), length(start_parts))
        while (common < max_common &&
            identical(tolower(target_parts[[common + 1L]]), tolower(start_parts[[common + 1L]]))) {
            common <- common + 1L
        }

        up <- if (common < length(start_parts)) rep("..", length(start_parts) - common) else character(0)
        down <- if (common < length(target_parts)) target_parts[(common + 1L):length(target_parts)] else character(0)
        rel <- c(up, down)

        if (length(rel) == 0) {
            rel <- "."
        }
        rel_paths <- c(rel_paths, do.call(file.path, as.list(rel)))
    }
    return(rel_paths)
}




# Helper: Read asset file as string
read_asset_file <- function(filename) {
    asset_path <- system.file("assets", filename, package = "myworkspace")
    if (asset_path == "") stop(paste("Asset not found:", filename))
    paste(readLines(asset_path, warn = FALSE), collapse = "\n")
}
