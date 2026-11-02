# Install the GitHub package, then launch with codebookr::run_app().
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}
remotes::install_github("kimcourage/codebook-r")
