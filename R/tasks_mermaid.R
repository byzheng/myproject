#' Render targets pipeline as a vertical mermaid diagram for knitr
#'
#' This function generates a mermaid diagram from a targets pipeline, modifies the direction from left-to-right (LR) to top-to-bottom (TB), and returns it as knitr asis output for embedding in R Markdown or Quarto documents.
#'
#' @param ... Arguments passed to `targets::tar_mermaid()`.
#' @return A character string formatted for knitr asis output containing the mermaid diagram.
#' @examples
#' # In an R Markdown code chunk:
#' \dontrun{
#' knit_targets_mermaid(targets_only = TRUE)
#' }
#'
#' @export
knit_targets_mermaid <- function(...) {
    mermaid_code <- targets::tar_mermaid(...)
    pos <- grepl("direction LR", mermaid_code)
    mermaid_code[pos] <- gsub("direction LR", "direction TB", mermaid_code[pos])
    mermaid_code <- paste(mermaid_code, collapse = "\n")
    knitr::asis_output(
        paste0(
            "```{mermaid}\n",
            mermaid_code,
            "\n```"
        )
    )
}

