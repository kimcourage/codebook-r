write_test_csv <- function(text, encoding = "UTF-8") {
  path <- tempfile(fileext = ".csv")
  writeBin(charToRaw(iconv(text, from = "UTF-8", to = encoding)), path)
  path
}

testthat::test_that("CSV import preserves leading zeros, long identifiers and late mixed values", {
  path <- write_test_csv(paste0("id,long_id,value\n", paste(
    sprintf("%04d,9007199254740993,%s", 1:1002, c(rep("1", 1001), "refused")), collapse = "\n")))
  d <- read_dataset(path)
  testthat::expect_identical(d$id[[1]], "0001")
  testthat::expect_identical(d$long_id[[1]], "9007199254740993")
  testthat::expect_identical(d$value[[1002]], "refused")
  testthat::expect_equal(sum(is.na(d$value)), 0L)
})

testthat::test_that("Korean CSV encodings and explicit character columns are honored", {
  path <- write_test_csv("번호,응답\n123,동의\n456,거절\n", "CP949")
  d <- read_dataset(path, csv_encoding = "CP949", character_columns = "번호")
  testthat::expect_identical(d$번호, c("123", "456"))
  testthat::expect_identical(d$응답, c("동의", "거절"))
  testthat::expect_error(read_dataset(path, csv_encoding = "CP949", character_columns = "absent"), "변수명이 없습니다")
})

testthat::test_that("malformed CSV cannot silently produce a partial dataset", {
  path <- write_test_csv("id,q\n1,2,extra\n2,3\n")
  testthat::expect_error(suppressWarnings(read_dataset(path)), "CSV 구조 오류")
})

testthat::test_that("SPSS imports preserve value labels, variable labels and user missing codes", {
  path <- tempfile(fileext = ".sav")
  source <- data.frame(id = c("001", "002", "003"),
    q = haven::labelled_spss(c(1, 9, NA_real_), labels = c(동의 = 1, 무응답 = 9),
      na_values = 9, label = "문항 설명"))
  haven::write_sav(source, path)
  d <- read_dataset(path)
  testthat::expect_equal(as.numeric(d$q), c(1, 9, NA_real_))
  testthat::expect_equal(attr(d$q, "na_values"), 9)
  testthat::expect_identical(attr(d$q, "label"), "문항 설명")
  testthat::expect_equal(attr(d$q, "labels"), c(동의 = 1, 무응답 = 9))
  testthat::expect_identical(as.character(d$id), source$id)
  testthat::expect_identical(attr(d$id, "format.spss"), "A3")
  meta <- variable_metadata(d)
  testthat::expect_equal(meta$n_missing[meta$variable == "q"], 2L)
  testthat::expect_match(meta$value_labels[meta$variable == "q"], "무응답", fixed = TRUE)
})

testthat::test_that("Stata labels and tagged missing values survive import", {
  path <- tempfile(fileext = ".dta")
  x <- haven::labelled(c(1, haven::tagged_na("a"), NA_real_), labels = c(yes = 1), label = "Question")
  haven::write_dta(data.frame(q = x), path)
  d <- read_dataset(path)
  testthat::expect_equal(attr(d$q, "labels"), c(yes = 1))
  testthat::expect_identical(attr(d$q, "label"), "Question")
  testthat::expect_true(haven::is_tagged_na(d$q[[2]], "a"))
})

testthat::test_that("unsupported RDS columns and duplicate names fail clearly", {
  path <- tempfile(fileext = ".rds")
  saveRDS(data.frame(id = 1:2, nested = I(list(1:2, 3:4))), path)
  testthat::expect_error(read_dataset(path), "목록·행렬")
  d <- data.frame(a = 1, b = 2); names(d) <- c("q", "q")
  saveRDS(d, path)
  testthat::expect_error(read_dataset(path), "중복")
  testthat::expect_error(read_dataset(write_test_csv("q,q\n1,2\n")), "중복")
  testthat::expect_error(read_dataset(write_test_csv("q,\n1,2\n")), "비어")
})
