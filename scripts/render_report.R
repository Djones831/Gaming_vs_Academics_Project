# Renders report/final_report.Rmd to report/final_report.html.
#
# Usage, from the project root:
#   Rscript scripts/render_report.R
#
# Knitting the Rmd directly from RStudio does the same thing. This script exists so
# the report can also be built from a terminal, where pandoc is usually not on PATH
# (RStudio ships its own copy and only exports it to its own R session).

# ---- Locate pandoc ----
# rmarkdown needs pandoc to turn the knitted markdown into HTML. Look in the places
# RStudio and Quarto install it before giving up.
if (!rmarkdown::pandoc_available()) {
  candidates <- c(
    Sys.getenv("RSTUDIO_PANDOC"),
    Sys.glob("/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/*"),
    Sys.glob("/Applications/RStudio.app/Contents/MacOS/quarto/bin/tools/*"),
    Sys.glob("/Applications/RStudio.app/Contents/MacOS/pandoc"),
    Sys.glob("/Applications/quarto/bin/tools/*"),
    "/usr/local/bin", "/opt/homebrew/bin",
    # Windows / Linux RStudio
    Sys.glob("C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools"),
    Sys.glob("/usr/lib/rstudio/resources/app/bin/quarto/bin/tools")
  )

  for (dir in candidates[nzchar(candidates)]) {
    if (file.exists(file.path(dir, "pandoc")) ||
        file.exists(file.path(dir, "pandoc.exe"))) {
      Sys.setenv(RSTUDIO_PANDOC = dir)
      break
    }
  }
}

if (!rmarkdown::pandoc_available()) {
  stop("pandoc not found. Either knit report/final_report.Rmd from RStudio, or ",
       "install pandoc (https://pandoc.org/installing.html) and rerun this script.",
       call. = FALSE)
}

cat("Using pandoc", as.character(rmarkdown::pandoc_version()),
    "from", rmarkdown::find_pandoc()$dir, "\n")

# ---- Render ----
# The Rmd sets its own root.dir to the project root, so relative paths like
# "data/cleaned_data.csv" resolve the same way regardless of where this is run from.
rmd <- "report/final_report.Rmd"
if (!file.exists(rmd)) {
  stop("Run this from the project root; could not find ", rmd, call. = FALSE)
}

out <- rmarkdown::render(
  rmd,
  output_file = "final_report.html",
  envir       = new.env(),   # keep the report's objects out of the caller's workspace
  quiet       = FALSE
)

cat("\nWrote", out, "\n")
