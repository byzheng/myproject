
# Helper function to convert absolute path to relative
make_relative <- function(abs_path, root_dir) {
    stopifnot(is.character(abs_path), length(abs_path) > 0)
    stopifnot(is.character(root_dir), length(root_dir) == 1)
    root_dir <- suppressWarnings(normalizePath(root_dir, winslash = "/"))
    
    rel_path <- c()
    for (i in seq_along(abs_path)) {
        abs_path_i <- suppressWarnings(normalizePath(abs_path[i], winslash = "/"))
    
        if (!grepl(paste0("^", gsub("\\\\", "\\\\\\\\", root_dir)), abs_path_i)) {
            return(abs_path_i) # Not under root, return as-is
        }
        
        # Remove root prefix and leading slash
        rel_path_i <- sub(paste0("^", gsub("\\\\", "\\\\\\\\", root_dir), "/?"), "", abs_path_i)
        
        rel_path[i] <- rel_path_i
        
    }
    return(rel_path)
}
