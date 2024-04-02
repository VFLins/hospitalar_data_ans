args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
    stop("Usage: Rscript render_rmd.R filename_without_ext")
}

library(rmarkdown)

if (tolower(args[1]) == "index") {
    output_dir = getwd()
} else {
    output_dir = paste0(getwd(), "/pages/")
}

render(
    input = paste0("notebooks/", args[1], ".Rmd"),
    output_dir = output_dir,
    knit_root_dir = getwd()
)
