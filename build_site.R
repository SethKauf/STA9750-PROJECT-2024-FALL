#!/usr/bin/env Rscript

# Check if Quarto is installed and render project
if(!require("quarto")) {
  install.packages("quarto")
}
library(quarto)

# Check Quarto installation
if(!quarto::quarto_binary_sitrep()) {
  stop("Something is wrong with your Quarto installation.")
}

# Render only files allowed by `.quartoignore`
quarto::quarto_render(".")

# Stage the rendered output (e.g., in `docs/`)
system("git add docs/*")

# Exit if not in RStudio
if(!any(grepl("rstudio", search()))) {
  q("no")
}
