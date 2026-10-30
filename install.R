# Run once from the CodebookR folder: source("install.R")
local({
  packages <- c("shiny", "readr", "haven", "readxl", "pdftools", "jsonlite",
                "zip", "tidyr", "vctrs", "tidyselect", "ellmer")
  available <- function() vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  missing <- packages[!available()]
  if (length(missing)) {
    install.packages(missing, repos = "https://cloud.r-project.org")
  }
  missing <- packages[!available()]
  if (length(missing)) {
    stop("설치하지 못한 패키지: ", paste(missing, collapse = ", "),
         "\n위의 설치 메시지를 확인한 뒤 다시 실행해 주세요.", call. = FALSE)
  }
  message("준비되었습니다. 아래 명령으로 앱을 여세요:\n",
          'shiny::runApp(".", launch.browser = TRUE)')
})
