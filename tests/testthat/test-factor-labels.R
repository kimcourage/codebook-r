factor_test_rule <- function(variable = "gender") {
  data.frame(id = 1L, variable = variable, operation = "factor", values = "",
    minimum = NA_real_, maximum = NA_real_, reason = "범주형 분석", evidence = "book",
    confidence = "manual", source = "manual")
}

factor_example <- function() {
  data <- read_dataset(example_path("synthetic_survey.csv"))
  books <- read_codebooks(data.frame(name = "synthetic_codebook.xlsx",
    datapath = example_path("synthetic_codebook.xlsx")))
  mapping <- initial_mapping(data, books$records)
  list(data = data, books = books, mapping = mapping,
    rules = codebook_candidates(data, books, mapping))
}

testthat::test_that("measurement columns and continuation rows drive categorical proposals", {
  table <- rbind(c("Variable", "Description", "Value", "Value label", "Measurement", "Notes"),
    c("group", "집단", "1", "처치", "nominal", ""),
    c("", "", "2", "통제", "", ""),
    c("score", "점수", "99", "무응답", "numeric", ""))
  parsed <- parse_tabular_codebook(as.data.frame(table), "book.csv")
  testthat::expect_match(parsed$records$note[[2]], "nominal")
  d <- data.frame(group = c(1, 2), score = c(10, 99))
  cb <- list(records = parsed$records)
  r <- codebook_candidates(d, cb, initial_mapping(d, cb$records))
  testthat::expect_identical(r$variable[r$operation == "factor"], "group")
  testthat::expect_identical(levels(apply_approved_rules(d, r)$data$group), c("처치", "통제"))
})

testthat::test_that("the codebook example produces an analysable gender factor and numeric scores", {
  f <- factor_example()
  testthat::expect_identical(f$rules$variable[f$rules$operation == "factor"], "gender")
  result <- apply_approved_rules(f$data, f$rules)
  x <- result$data
  testthat::expect_true(is.factor(x$gender))
  testthat::expect_identical(levels(x$gender), c("남성", "여성"))
  testthat::expect_identical(as.character(x$gender), c("남성", "여성", NA, "남성", "여성", NA, "여성", "남성"))
  testthat::expect_true(is.numeric(x$income))
  testthat::expect_true(is.numeric(x$life_sat))
  testthat::expect_equal(sum(is.na(x$gender)), 2L)
  testthat::expect_equal(sum(is.na(x$income)), 2L)
  testthat::expect_equal(sum(is.na(x$life_sat)), 1L)
  design <- stats::model.matrix(~ gender, x)
  testthat::expect_identical(colnames(design), c("(Intercept)", "gender여성"))
  testthat::expect_equal(dim(design), c(6L, 2L))
  review <- codebook_comparison(x, f$books, f$mapping)
  testthat::expect_equal(review$undefined_codes[review$variable == "gender"], 0)
  testthat::expect_identical(f$data$gender, c(1, 2, 9, 1, 2, 9, 2, 1))
  testthat::expect_match(variable_metadata(x)$value_labels[2], "1=남성", fixed = TRUE)
  shown <- preview_label_changes(f$data, x, "gender")$table
  testthat::expect_identical(shown[[3]][shown[[1]] == "자료형"], "factor")
  testthat::expect_true("남성" %in% shown[[3]])
  testthat::expect_identical(sample_values(x)$gender, as.character(x$gender))
})

testthat::test_that("only explicit categorical instructions produce automatic factor proposals", {
  f <- factor_example()
  b <- f$books; b$records$note <- ""
  testthat::expect_false("factor" %in% codebook_candidates(f$data, b, f$mapping)$operation)
  for (note in c("numeric, not categorical", "범주형 아님", "수치형 점수")) {
    b$records$note[b$records$variable == "gender"] <- note
    testthat::expect_false("factor" %in% codebook_candidates(f$data, b, f$mapping)$operation)
  }
})

testthat::test_that("factor definitions are saved on manual and AI proposals independently of label selection", {
  f <- factor_example()
  for (origin in c("manual", "AI")) {
    r <- factor_test_rule(); r$source <- origin
    bound <- with_codebook_factor_labels(r, f$data, f$books, f$mapping)
    testthat::expect_identical(bound$schema_version, 2L)
    p <- rule_parameters(bound)
    testthat::expect_identical(p$labels, c("남성", "여성", "무응답"))
    x <- apply_approved_rules(f$data, bound)$data$gender
    testthat::expect_identical(levels(x), c("남성", "여성", "무응답"))
    testthat::expect_equal(sum(is.na(x)), 0L)
    testthat::expect_identical(attr(x, "label"), "성별")
  }
})

testthat::test_that("partial codebooks retain existing value labels and category conflicts are visible", {
  f <- factor_example()
  f$data$gender <- haven::labelled(f$data$gender, c(남성 = 1, 여성 = 2, 무응답 = 9))
  b <- f$books; b$records <- b$records[b$records$variable != "gender" | b$records$value_code == "9", ]
  bound <- with_codebook_factor_labels(factor_test_rule(), f$data, b, f$mapping)
  x <- apply_approved_rules(f$data, bound)$data$gender
  testthat::expect_true(all(c("남성", "여성") %in% levels(x)))
  b <- f$books; b$records$value_label[b$records$variable == "gender" & b$records$value_code %in% c("1", "2")] <- "같음"
  review <- codebook_comparison(f$data, b, f$mapping)
  testthat::expect_match(review$findings[review$variable == "gender"], "같은 범주명", fixed = TRUE)
})

testthat::test_that("labelled SPSS values and tagged missings convert without invented categories", {
  d <- data.frame(gender = haven::labelled_spss(c(1, 2, 9, NA),
    labels = c(남성 = 1, 여성 = 2, 무응답 = 9), na_values = 9, label = "성별"))
  x <- apply_approved_rules(d, factor_test_rule())$data$gender
  testthat::expect_identical(as.character(x), c("남성", "여성", NA, NA))
  testthat::expect_identical(levels(x), c("남성", "여성"))
  d <- data.frame(gender = haven::labelled(c(1, haven::tagged_na("a")),
    labels = c(남성 = 1, 무응답 = haven::tagged_na("a"))))
  x <- apply_approved_rules(d, factor_test_rule())$data$gender
  testthat::expect_identical(as.character(x), c("남성", NA))
  testthat::expect_identical(levels(x), "남성")
})

testthat::test_that("unlabelled values remain visible and duplicate category names cannot merge codes", {
  d <- data.frame(gender = haven::labelled(c(1, 2, 7, NA), c(남성 = 1, 여성 = 2)))
  x <- apply_approved_rules(d, factor_test_rule())
  testthat::expect_identical(as.character(x$data$gender), c("남성", "여성", "7", NA))
  testthat::expect_match(x$audit$note, "정의에 없는 코드 1개", fixed = TRUE)
  testthat::expect_equal(sum(is.na(x$data$gender)), 1L)
  attr(d$gender, "labels") <- stats::setNames(c(1, 2), c("같음", "같음"))
  testthat::expect_error(apply_approved_rules(d, factor_test_rule()), "범주명이 같습니다")
  d <- data.frame(gender = haven::labelled(c("01", "1", NA), c(선택A = "01", 선택B = "1")))
  x <- apply_approved_rules(d, factor_test_rule())$data$gender
  testthat::expect_identical(as.character(x), c("선택A", "선택B", NA))
  testthat::expect_identical(factor_source_codes(x), c("01", "1", NA))
})

testthat::test_that("factor codes stay available for codebook range and unknown-code checks", {
  f <- factor_example(); f$data$gender[[1]] <- 7
  f$books$records$minimum[f$books$records$variable == "gender"] <- "1"
  f$books$records$maximum[f$books$records$variable == "gender"] <- "2"
  x <- apply_approved_rules(f$data, f$rules)$data
  review <- codebook_comparison(x, f$books, f$mapping)
  row <- review[review$variable == "gender", ]
  testthat::expect_equal(row$undefined_codes, 1)
  testthat::expect_equal(row$out_of_range, 1)
  testthat::expect_false(grepl("숫자형이 아닙니다", row$findings))
})

testthat::test_that("standalone code reproduces factor levels and can be used in a model", {
  f <- factor_example(); expected <- apply_approved_rules(f$data, f$rules)
  root <- normalizePath(testthat::test_path("..", ".."))
  dir <- tempfile("factor-replay-"); dir.create(dir); on.exit(unlink(dir, recursive = TRUE))
  raw <- file.path(dir, "raw.csv"); write.csv(f$data, raw, row.names = FALSE)
  script <- file.path(dir, "preprocessing.R")
  writeLines(make_standalone_script(f$rules, root), script, useBytes = TRUE)
  output <- file.path(dir, "reproduced.rds")
  status <- system2(file.path(R.home("bin"), "Rscript"), shQuote(c("--vanilla", script, raw, output)), stdout = TRUE, stderr = TRUE)
  testthat::expect_null(attr(status, "status"), info = paste(status, collapse = "\n"))
  actual <- readRDS(output)
  testthat::expect_identical(actual, expected$data)
  testthat::expect_identical(colnames(stats::model.matrix(~ gender, actual)), c("(Intercept)", "gender여성"))
  testthat::expect_identical(read.csv(file.path(dir, "reproduced_audit.csv"))$details, expected$audit$details)
})

testthat::test_that("the Shiny example and manual editor use named factor levels", {
  f <- server_fixture()
  shiny::testServer(f$server, {
    session$flushReact()
    session$setInputs(load_example = 1L, review_variable = "gender")
    session$setInputs(approved_rules = rule_keys())
    testthat::expect_true(plan_preview()$ok, info = plan_preview()$error)
    testthat::expect_true(is.factor(plan_preview()$data$gender))
    testthat::expect_match(output$review_sample, "남성", fixed = TRUE)
    testthat::expect_match(output$review_metadata, "factor", fixed = TRUE)
    session$setInputs(run_button = 1L)
    testthat::expect_identical(levels(current_result()$applied$data$gender), c("남성", "여성"))
  })
})

testthat::test_that("the manual editor stores codebook names for a chosen factor conversion", {
  f <- server_fixture()
  shiny::testServer(f$server, {
    session$flushReact(); session$setInputs(load_example = 1L)
    session$setInputs(edit_rule = "new", manual_op = "factor", manual_var = "life_sat",
      manual_reason = "분석용 범주 지정", add_manual = 1L)
    r <- rules()[rules()$operation == "factor" & rules()$variable == "life_sat", ]
    testthat::expect_equal(nrow(r), 1L)
    testthat::expect_equal(r$schema_version, 2L)
    testthat::expect_true("만족" %in% rule_parameters(r)$labels)
    session$setInputs(approved_rules = rule_keys()[rules()$id == r$id])
    testthat::expect_true(is.factor(plan_preview()$data$life_sat))
  })
})

testthat::test_that("missing-code removal still refers to original codes after factor conversion", {
  d <- data.frame(gender = haven::labelled(c(1, 2, 9), c(남성 = 1, 여성 = 2, 무응답 = 9)))
  missing <- factor_test_rule(); missing$id <- 2L; missing$operation <- "set_na"; missing$values <- "9"
  x <- apply_approved_rules(d, rbind(factor_test_rule(), missing))$data$gender
  testthat::expect_identical(as.character(x), c("남성", "여성", NA))
  testthat::expect_identical(levels(x), c("남성", "여성"))
})
