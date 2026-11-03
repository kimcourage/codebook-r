bin_parameters <- function(...) {
  defaults <- list(input = "income", output = "income_group",
    bins = data.frame(lower = c("", "200", "400"), upper = c("200", "400", ""),
      lower_closed = TRUE, upper_closed = FALSE, label = c("낮음", "중간", "높음")),
    outside = "error", ordered = FALSE)
  utils::modifyList(defaults, list(...))
}
bin_rule <- function(p = bin_parameters(), id = 1L) new_research_rule("binning", p, "Researcher-specified ranges", id)

testthat::test_that("numeric ranges create labelled factors and preserve the source exactly", {
  d <- data.frame(income = c(-10, 0, 199.9, 200, 399.9, 400, 1000, NA_real_))
  attr(d$income, "label") <- "Income"
  x <- apply_approved_rules(d, bin_rule())
  testthat::expect_identical(x$data$income, d$income)
  testthat::expect_identical(as.character(x$data$income_group), c(rep("낮음", 3), rep("중간", 2), rep("높음", 2), NA_character_))
  testthat::expect_identical(levels(x$data$income_group), c("낮음", "중간", "높음"))
  testthat::expect_false(is.ordered(x$data$income_group))
  testthat::expect_equal(x$audit$n_affected, 7)
  testthat::expect_equal(x$audit$missing_before, 1)
  testthat::expect_equal(x$audit$missing_after, 1)
  details <- jsonlite::fromJSON(x$audit$details)
  testthat::expect_equal(details$bins$n, c(3, 2, 2))
  preview <- preview_rules(d, bin_rule())
  testthat::expect_identical(preview$data, x$data)
  testthat::expect_equal(preview$report$type_after, "factor")
  testthat::expect_true(is.ordered(apply_approved_rules(d, bin_rule(bin_parameters(ordered = TRUE)))$data$income_group))
  testthat::expect_identical(colnames(model.matrix(~ income_group, x$data)), c("(Intercept)", "income_group중간", "income_group높음"))
})

testthat::test_that("boundary inclusivity and uncovered values are explicit", {
  p <- bin_parameters()
  p$bins$lower_closed <- FALSE; p$bins$upper_closed <- TRUE
  d <- data.frame(income = c(199.9, 200, 200.1, 400, 400.1, NA))
  testthat::expect_identical(as.character(apply_approved_rules(d, bin_rule(p))$data$income_group), c("낮음", "낮음", "중간", "중간", "높음", NA_character_))
  p$bins$lower <- c("0", "200", "400"); p$bins$upper[3] <- "500"
  d <- data.frame(income = c(-1, 0, 200, 300, 400, 500, 501, NA))
  testthat::expect_error(apply_approved_rules(d, bin_rule(p)), "구간 밖 값이 3개")
  testthat::expect_false(preview_rules(d, bin_rule(p))$ok)
  p$outside <- "na"
  x <- apply_approved_rules(d, bin_rule(p))
  testthat::expect_equal(which(is.na(x$data$income_group)), c(1L, 2L, 7L, 8L))
  testthat::expect_identical(x$data$income, d$income)
  testthat::expect_equal(jsonlite::fromJSON(x$audit$details)$outside_n, 3)
  testthat::expect_equal(x$audit$missing_before, 1)
  testthat::expect_equal(x$audit$missing_after, 4)
  p <- bin_parameters(); p$bins$upper[1] <- "100"
  testthat::expect_error(apply_approved_rules(data.frame(income = 150), bin_rule(p)), "구간 밖")
  # Unobserved, configured categories remain levels for stable analysis coding.
  testthat::expect_identical(levels(apply_approved_rules(data.frame(income = NA_real_), bin_rule())$data$income_group), c("낮음", "중간", "높음"))
})

testthat::test_that("overlaps, empty intervals and invalid definitions cannot silently recode", {
  d <- data.frame(income = c(10, 250, 500))
  p <- bin_parameters(); p$bins$upper_closed[1] <- TRUE
  testthat::expect_error(apply_approved_rules(d, bin_rule(p)), "1번 구간과 2번 구간이 겹칩니다")
  p <- bin_parameters(); p$bins$upper[1] <- "300"
  testthat::expect_error(apply_approved_rules(d, bin_rule(p)), "겹칩니다")
  p <- bin_parameters(); p$bins$lower[2] <- "500"
  testthat::expect_error(apply_approved_rules(d, bin_rule(p)), "시작값")
  p <- bin_parameters(); p$bins$label[2] <- p$bins$label[1]
  testthat::expect_error(apply_approved_rules(d, bin_rule(p)), "범주명이 중복")
  p <- bin_parameters(); p$bins$label[2] <- "  "
  testthat::expect_error(apply_approved_rules(d, bin_rule(p)), "범주명")
  p <- bin_parameters(); p$bins$lower[2] <- "1+2"
  testthat::expect_error(apply_approved_rules(d, bin_rule(p)), "경계값은 숫자")
  p$bins$lower[2] <- "Inf"
  testthat::expect_error(apply_approved_rules(d, bin_rule(p)), "경계값은 숫자")
  p <- bin_parameters(); p$bins$lower_closed <- c(1, 1, 1)
  testthat::expect_error(apply_approved_rules(d, bin_rule(p)), "포함 여부")
  testthat::expect_error(apply_approved_rules(d, bin_rule(bin_parameters(output = "income"))), "이미 있는")
  testthat::expect_error(apply_approved_rules(transform(d, income = factor(income)), bin_rule()), "숫자 변수")
  testthat::expect_error(apply_approved_rules(transform(d, income = Inf), bin_rule()), "무한값")
  testthat::expect_error(apply_approved_rules(d, bin_rule(bin_parameters(outside = "ignore"))), "처리 방법")
  testthat::expect_error(apply_approved_rules(d, bin_rule(bin_parameters(ordered = NA))), "순서 설정")
})

testthat::test_that("SPSS and tagged missing values do not become ordinary categories", {
  d <- data.frame(income = haven::labelled_spss(c(100, 200, 9999, NA), na_values = 9999))
  x <- apply_approved_rules(d, bin_rule())
  testthat::expect_identical(x$data$income, d$income)
  testthat::expect_identical(as.character(x$data$income_group), c("낮음", "중간", NA_character_, NA_character_))
  d <- data.frame(income = haven::labelled(c(100, haven::tagged_na("a"))))
  testthat::expect_identical(as.character(apply_approved_rules(d, bin_rule())$data$income_group), c("낮음", NA_character_))
})

testthat::test_that("range definitions survive project files, RDS, and standalone replay", {
  d <- data.frame(income = c(199, 200, 400, NA))
  r <- bin_rule(bin_parameters(ordered = TRUE))
  dir <- tempfile(); dir.create(dir)
  raw <- file.path(dir, "raw.rds"); saveRDS(d, raw)
  file <- file.path(dir, "plan.json")
  manifest <- data.frame(kind = "data", name = "raw.rds", checksum = unname(tools::md5sum(raw)))
  write_project(file, manifest, list(), r, r$id, NULL, NULL, NULL)
  restored <- read_project(file, manifest, list())
  testthat::expect_identical(restored$rules$parameters, r$parameters)
  testthat::expect_equal(restored$additional_ids, r$id)
  script <- file.path(dir, "preprocessing.R")
  writeLines(make_standalone_script(r), script)
  result <- file.path(dir, "reproduced.rds")
  status <- system2(file.path(R.home("bin"), "Rscript"), shQuote(c("--vanilla", script, raw, result)), stdout = TRUE, stderr = TRUE)
  testthat::expect_null(attr(status, "status"), info = paste(status, collapse = "\n"))
  expected <- apply_approved_rules(d, r)
  testthat::expect_identical(readRDS(result), expected$data)
  audit <- read.csv(file.path(dir, "reproduced_audit.csv"))
  testthat::expect_identical(audit$details, expected$audit$details)
  candidate <- r; candidate$source <- "AI"
  testthat::expect_equal(nrow(append_rule_candidates(empty_rules(), candidate, d)), 0L)
})

testthat::test_that("binning is additional-only and editing invalidates dependent results", {
  f <- server_fixture()
  shiny::testServer(f$server, {
    ui <- capture_inputs(session); load_fixture(session, f)
    testthat::expect_false("binning" %in% unname(operation_choices(FALSE)))
    session$setInputs(manual_op = "binning", add_manual = 1L)
    testthat::expect_match(rule_feedback(), "추가 전처리")
    session$setInputs(approved_rules = ui$choices("approved_rules"), run_button = 1L)
    session$setInputs(start_additional = 1L)
    testthat::expect_identical(workspace_mode(), "additional")
    testthat::expect_match(ui$last("manual_op")$options, "binning")
    session$setInputs(edit_rule = "new", manual_op = "binning", bin_variable = "income", new_variable = "income_group",
      bin_lower_1 = "", bin_upper_1 = "200", bin_label_1 = "낮음", bin_lower_closed_1 = TRUE, bin_upper_closed_1 = FALSE,
      bin_lower_2 = "200", bin_upper_2 = "400", bin_label_2 = "중간", bin_lower_closed_2 = TRUE, bin_upper_closed_2 = FALSE,
      bin_lower_3 = "400", bin_upper_3 = "", bin_label_3 = "높음", bin_lower_closed_3 = TRUE, bin_upper_closed_3 = FALSE,
      bin_outside = "error", bin_ordered = FALSE, manual_reason = "Test ranges", add_manual = 2L)
    testthat::expect_equal(nrow(rules()), 2L)
    testthat::expect_equal(nrow(bin_preview()$step$details$bins), 3L)
    session$setInputs(approved_rules = ui$choices("approved_rules"), run_button = 2L)
    testthat::expect_identical(ui$last("review_variable")$value, "income")
    testthat::expect_true(is.factor(current_result()$applied$data$income_group))
    testthat::expect_false("income_group" %in% names(current_base()$applied$data))
    session$setInputs(manual_op = "center", scale_variable = "income", scale_scope = "group",
      scale_groups = "income_group", scale_reference = "all", new_variable = "centered", manual_reason = "Dependent", add_manual = 3L)
    session$setInputs(approved_rules = ui$choices("approved_rules"))
    keys <- rule_keys(); bin_key <- keys[rules()$operation == "binning"]
    session$setInputs(edit_rule = bin_key)
    testthat::expect_identical(bin_rows_state()$label, c("낮음", "중간", "높음"))
    testthat::expect_identical(ui$last("bin_variable")$value, "income")
    session$setInputs(manual_op = "binning", new_variable = "income_group", bin_label_1 = "저소득", add_manual = 4L)
    testthat::expect_null(current_result())
    testthat::expect_false(bin_key %in% rule_keys())
    testthat::expect_false(keys[rules()$operation == "center"] %in% rule_keys())
    testthat::expect_equal(f$calls$n, 0L)
  })
})

testthat::test_that("adding and removing interval rows preserves the entered definitions", {
  f <- server_fixture()
  shiny::testServer(f$server, {
    load_fixture(session, f)
    session$setInputs(bin_label_1 = "Keep", bin_lower_1 = "0", bin_upper_1 = "200", bin_add = 1L)
    testthat::expect_equal(nrow(bin_rows_state()), 4L)
    testthat::expect_equal(bin_rows_state()$label[1], "Keep")
    session$setInputs(bin_remove = 1L)
    testthat::expect_equal(nrow(bin_rows_state()), 3L)
    testthat::expect_equal(bin_rows_state()$lower[1], "0")
    testthat::expect_equal(bin_rows_state()$upper[1], "200")
  })
})
