args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
    stop("Usage: Rscript render_rmd.R filename_without_ext")
}

library(rmarkdown)

render(
    input = paste0("notebooks/", args[1], ".Rmd"),
    output_dir = paste0(getwd(), "/pages/"),
    knit_root_dir = getwd()
)
