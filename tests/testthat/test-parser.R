testthat::test_that("Excel-style records keep row provenance and merged cell follow-ons", {
  x <- data.frame(V1 = c("기관 코드북", "변수명", "gender", "", "income"),
                  V2 = c("", "변수 설명", "성별", "", "월소득"),
                  V3 = c("", "값", "1", "9", "9999"),
                  V4 = c("", "값 설명", "남성", "무응답", "응답 거부"))
  out <- parse_tabular_codebook(x, "codebook.xlsx", sheet = "Vars")
  testthat::expect_true(out$detected)
  testthat::expect_equal(nrow(out$records), 3L)
  testthat::expect_equal(out$records$variable[[2]], "gender")
  testthat::expect_equal(out$records$source_loc[[2]], "Vars!row 4")
})

testthat::test_that("ambiguous sheets are retained as excerpts, not invented records", {
  x <- data.frame(A = c("제목", "기타", "값"), B = c("가", "나", "다"))
  out <- parse_tabular_codebook(x, "irregular.xlsx")
  testthat::expect_false(out$detected)
  testthat::expect_equal(nrow(out$records), 0L)
  testthat::expect_true(nchar(out$excerpt) > 0L)
})

testthat::test_that("PDF sample extracts line provenance", {
  pdf_file <- example_path("synthetic_codebook.pdf")
  if (!file.exists(pdf_file)) testthat::skip("No PDF fixture")
  out <- parse_pdf_codebook(pdf_file, "codebook.pdf")
  testthat::expect_true(out$detected)
  testthat::expect_true(any(out$records$variable == "gender"))
  testthat::expect_true(all(grepl("page", out$records$source_loc)))
})

testthat::test_that("unrecognized and truncated codebooks explain their limitations", {
  irregular <- parse_tabular_codebook(data.frame(a = c("title", "something")), "irregular.xlsx")
  testthat::expect_match(irregular$note, "헤더를 찾지", fixed = TRUE)
  x <- data.frame(a = c("Variable", "q1", "q2", "q3"), b = c("Value", "1", "2", "3"))
  limited <- parse_tabular_codebook(x, "book.csv", max_rows = 3L)
  testthat::expect_match(limited$note, "처음 3행", fixed = TRUE)
  testthat::expect_equal(nrow(limited$records), 2L)
  empty <- parse_tabular_codebook(data.frame(), "empty.xlsx")
  testthat::expect_match(empty$note, "빈 시트", fixed = TRUE)
})
