review_book <- function() {
  r <- data.frame(variable = c("Q1", "Q1", "Q1", "unused"), label = c(rep("응답", 3), "사용하지 않은 변수"),
    value_code = c("1", "2", "9", ""), value_label = c("예", "아니오", "무응답", ""), note = "",
    minimum = c(rep("1", 3), ""), maximum = c(rep("2", 3), ""),
    source_file = "book.csv", source_loc = paste0("row ", 2:5), quote = "", origin = "codebook", stringsAsFactors = FALSE)
  list(records = r, summary = data.frame(file = "book.csv", part = "CSV", recognized = TRUE, variables = 2, notes = ""))
}

testthat::test_that("mapping requires explicit confirmation of approximate names and preserves exclusions", {
  d <- data.frame(q1 = c(1, 2, 9, 4), id = 1:4); cb <- review_book()
  m <- initial_mapping(d, cb$records)
  testthat::expect_equal(m$data_variable, c("", ""))
  testthat::expect_equal(m$suggestion, c("q1", ""))
  testthat::expect_equal(nrow(codebook_candidates(d, cb, m)), 0L)
  m$data_variable[[1]] <- "q1"
  testthat::expect_equal(initial_mapping(d, cb$records, m)$data_variable, c("q1", ""))
  t <- codebook_comparison(d, cb, m)
  testthat::expect_equal(t$undefined_codes[[1]], 1)
  testthat::expect_equal(t$out_of_range[[1]], 1)
  testthat::expect_match(t$findings[[2]], "정의가 없습니다")
  testthat::expect_equal(nrow(initial_mapping(d, empty_codebook_records())), 0)
})

testthat::test_that("codebook labels preserve values and explicit missing provenance survives execution", {
  d <- data.frame(Q1 = c(1L, 2L, 9L, 4L)); cb <- review_book(); m <- initial_mapping(d, cb$records)
  r <- codebook_candidates(d, cb, m)
  testthat::expect_equal(r$operation, c("set_na", "labels"))
  x <- apply_approved_rules(d, r)
  testthat::expect_equal(as.numeric(x$data$Q1), c(1, 2, NA, 4))
  testthat::expect_equal(attr(x$data$Q1, "labels"), c(예 = 1, 아니오 = 2, 무응답 = 9))
  testthat::expect_identical(d$Q1, c(1L, 2L, 9L, 4L))
  trace <- jsonlite::fromJSON(x$audit$details[[1]])
  testthat::expect_equal(trace$affected_rows$row, 3L)
  testthat::expect_equal(trace$affected_rows$code, "9")
  s <- data.frame(Q1 = haven::labelled_spss(c(1, 9), na_values = 9))
  y <- apply_approved_rules(s, r[r$operation == "labels", ])$data$Q1
  testthat::expect_equal(attr(y, "na_values"), 9)
  testthat::expect_equal(as.numeric(y), c(1, 9))
  testthat::expect_equal(sum(is.na(y)), 1)
})

testthat::test_that("contradictory definitions are reported without automatic replacement", {
  d <- data.frame(Q1 = c(1, 9)); cb <- review_book()
  duplicate <- cb$records[3, ]; duplicate$value_label <- "유효 응답"; duplicate$source_file <- "other.csv"
  cb$records <- rbind(cb$records, duplicate); m <- initial_mapping(d, cb$records)
  testthat::expect_equal(nrow(codebook_candidates(d, cb, m)), 0)
  testthat::expect_match(codebook_comparison(d, cb, m)$findings, "서로 다릅니다")
})

testthat::test_that("AI definitions require an exact source quote at the stated location", {
  cb <- review_book(); cb$source_lines <- data.frame(source_file = "book.csv", source_loc = "row 4", text = "Q1 | 응답 | 9 | 무응답")
  r <- cb$records[3, ]; r$quote <- "Q1 | 응답 | 9 | 무응답"
  testthat::expect_equal(validate_ai_definitions(r, cb)$origin, "AI")
  r$source_loc <- "row 3"
  testthat::expect_equal(nrow(validate_ai_definitions(r, cb)), 0)
  r$source_loc <- "row 4"; r$quote <- "Q1 | 응답 | 9 | 응답 거절"
  testthat::expect_equal(nrow(validate_ai_definitions(r, cb)), 0)
})

testthat::test_that("codebook bounds and source markers are read from tabular input", {
  path <- tempfile(fileext = ".csv")
  write.csv(data.frame(variable = "x", label = "연속 값", minimum = 1, maximum = 5), path, row.names = FALSE)
  cb <- read_codebooks(data.frame(name = "book.csv", datapath = path))
  testthat::expect_equal(cb$records$minimum, "1")
  testthat::expect_equal(cb$records$maximum, "5")
  testthat::expect_equal(cb$records$source_loc, "row 2")
  testthat::expect_match(cb$source_lines$text[[2]], "연속 값")
})

testthat::test_that("base result stays available through additions and is invalidated by core edits", {
  f <- server_fixture()
  shiny::testServer(f$server, {
    ui <- capture_inputs(session); load_fixture(session, f)
    base_key <- rule_keys(); session$setInputs(approved_rules = base_key, run_button = 1L)
    testthat::expect_false(is.null(current_base()))
    session$setInputs(start_additional = 1L)
    testthat::expect_equal(workspace_mode(), "additional")
    testthat::expect_length(ui$choices("approved_rules"), 0)
    testthat::expect_equal(selected_rules()$operation, "set_na")
    session$setInputs(edit_rule = "new", manual_op = "composite", composite_items = c("gender", "income"), composite_method = "mean", composite_min = 1, new_variable = "mean_score", manual_reason = "예제 평균", add_manual = 1L)
    testthat::expect_equal(length(additional_ids()), 1L)
    testthat::expect_false(is.null(current_base()))
    session$setInputs(approved_rules = tail(rule_keys(), 1), run_button = 2L)
    testthat::expect_equal(current_result()$applied$data$mean_score, c(50.5, 9999))
    testthat::expect_false("mean_score" %in% names(current_base()$applied$data))
    testthat::expect_true(file.exists(output$download_base))
    session$setInputs(edit_basic = 1L, approved_rules = base_key)
    session$setInputs(edit_rule = base_key, manual_op = "set_na", manual_var = "gender", manual_codes = "1,9", manual_reason = "검증 변경", add_manual = 2L)
    testthat::expect_null(current_base())
    session$setInputs(start_additional = 2L)
    testthat::expect_equal(workspace_mode(), "codebook")
  })
})

testthat::test_that("mapping and decisions invalidate results and exported context records unresolved issues", {
  f <- server_fixture()
  shiny::testServer(f$server, {
    ui <- capture_inputs(session); load_fixture(session, f)
    session$setInputs(approved_rules = rule_keys(), run_button = 1L)
    session$setInputs(review_variable = "income", review_decision = "보류", review_reason = "코드북에 정의 없음", save_decision = 1L)
    testthat::expect_null(current_base())
    session$setInputs(run_button = 2L)
    testthat::expect_equal(current_result()$review$comparison$decision, c("미검토", "보류"))
    old <- rule_keys()
    session$setInputs(mapping_variable = "gender", mapping_target = "income", save_mapping = 1L)
    testthat::expect_null(current_base())
    testthat::expect_equal(rules()$variable, "income")
    testthat::expect_false(any(old %in% rule_keys()))
    testthat::expect_length(approved_keys(), 0)
  })
})

testthat::test_that("no-change result requires a codebook and explicit action with nothing selected", {
  f <- server_fixture()
  shiny::testServer(f$server, {
    ui <- capture_inputs(session)
    session$setInputs(data_file = f$data_a, run_unchanged = 1L, run_button = 1L)
    testthat::expect_null(current_result())
    session$setInputs(book_files = f$book_a, approved_rules = character(), run_unchanged = 2L)
    testthat::expect_equal(current_base()$applied$data, dataset())
    testthat::expect_equal(nrow(current_result()$applied$audit), 0L)
    testthat::expect_true(file.exists(output$download_bundle))
    session$setInputs(approved_rules = rule_keys(), run_unchanged = 3L)
    testthat::expect_null(current_result())
    testthat::expect_match(execution_feedback(), "해제")
  })
})

testthat::test_that("portable plans restore decisions and additions without restoring approvals or credentials", {
  f <- server_fixture()
  shiny::testServer(f$server, {
    ui <- capture_inputs(session); load_fixture(session, f)
    session$setInputs(approved_rules = rule_keys(), run_button = 1L)
    session$setInputs(start_additional = 1L)
    session$setInputs(edit_rule = "new", manual_op = "factor", manual_var = "income", manual_reason = "범주 검사", add_manual = 1L)
    path <- output$save_project
    text <- paste(readLines(path), collapse = "\n")
    testthat::expect_false(grepl("synthetic-test-key|approved_keys|api_key|9999", text))
    before <- rules(); extras <- additional_ids()
    session$setInputs(open_project = data.frame(datapath = path, name = "plan.json"))
    testthat::expect_equal(rules(), before)
    testthat::expect_equal(additional_ids(), extras)
    testthat::expect_null(current_base())
    testthat::expect_length(approved_keys(), 0)
    testthat::expect_equal(workspace_mode(), "codebook")
    testthat::expect_length(ui$last("consent")$value, 0)
    session$setInputs(data_file = f$data_b)
    session$setInputs(open_project = data.frame(datapath = path, name = "again.json"))
    testthat::expect_match(project_feedback(), "파일 이름과 내용")
    testthat::expect_equal(nrow(rules()), 1)
  })
})

testthat::test_that("undo restores deleted work without reusing approval keys", {
  f <- server_fixture()
  shiny::testServer(f$server, {
    ui <- capture_inputs(session); load_fixture(session, f)
    old <- rule_keys(); session$setInputs(approved_rules = old, edit_rule = old, delete_rule = 1L)
    testthat::expect_equal(nrow(rules()), 0)
    session$setInputs(undo_rule = 1L)
    testthat::expect_equal(nrow(rules()), 1)
    testthat::expect_false(old %in% rule_keys())
    testthat::expect_length(approved_keys(), 0)
  })
})

testthat::test_that("standalone export reproduces codebook labels and missing provenance", {
  root <- normalizePath(testthat::test_path("..", "..")); d <- data.frame(Q1 = c(1, 9)); cb <- review_book(); m <- initial_mapping(d, cb$records)
  r <- codebook_candidates(d, cb, m); expected <- apply_approved_rules(d, r)
  dir <- tempfile("flow-replay-"); dir.create(dir); on.exit(unlink(dir, recursive = TRUE))
  saveRDS(d, file.path(dir, "original.rds"))
  writeLines(make_standalone_script(r, root), file.path(dir, "preprocessing.R"))
  result <- system2(file.path(R.home("bin"), "Rscript"), c(shQuote(file.path(dir, "preprocessing.R")), shQuote(file.path(dir, "original.rds")), shQuote(file.path(dir, "out.rds"))), stdout = TRUE, stderr = TRUE)
  testthat::expect_null(attr(result, "status"))
  testthat::expect_equal(readRDS(file.path(dir, "out.rds")), expected$data)
  testthat::expect_equal(read.csv(file.path(dir, "out_audit.csv"))$details, expected$audit$details)
})

testthat::test_that("numeric code formats agree while character identifiers stay literal", {
  cb <- review_book(); cb$records$value_code[3] <- "9.0"
  d <- data.frame(Q1 = c(1, 9)); m <- initial_mapping(d, cb$records)
  r <- codebook_candidates(d, cb, m)
  testthat::expect_equal(r$values[[1]], "9")
  testthat::expect_true(is.na(apply_approved_rules(d, r)$data$Q1[[2]]))
  testthat::expect_equal(codebook_comparison(d, cb, m)$undefined_codes, 0)
  cb$records$value_code[3] <- "09"
  chars <- data.frame(Q1 = c("1", "9"))
  testthat::expect_equal(codebook_comparison(chars, cb, m)$undefined_codes, 1)
})

testthat::test_that("AI extraction contract handles definitions plus rules and empty rejected results offline", {
  env <- new.env(parent = asNamespace("codebookr")); env$propose_ai_rules <- propose_ai_rules; environment(env$propose_ai_rules) <- env
  cb <- review_book(); cb$excerpts <- c("book.csv" = "[row 4] Q1 | 응답 | 9 | 무응답")
  cb$source_lines <- data.frame(source_file = "book.csv", source_loc = "row 4", text = "Q1 | 응답 | 9 | 무응답")
  defs <- cb$records[3, ]; defs$quote <- cb$source_lines$text
  response <- list(definitions = defs, rules = empty_rules())
  env$create_chat <- function(...) list(chat_structured = function(...) response)
  proposal <- env$propose_ai_rules(data.frame(Q1 = c(1, 9)), cb, "anthropic", "fake")
  testthat::expect_equal(nrow(proposal), 0)
  testthat::expect_equal(nrow(attr(proposal, "definitions")), 1)
  response$definitions$source_loc <- "page 100"
  rejected <- env$propose_ai_rules(data.frame(Q1 = c(1, 9)), cb, "anthropic", "fake")
  testthat::expect_equal(nrow(attr(rejected, "definitions")), 0)
  testthat::expect_equal(attr(rejected, "rejected_definitions"), 1)
})

testthat::test_that("codebook-labelled panel supports the full subsequent research workflow", {
  root <- normalizePath(testthat::test_path("..", ".."))
  d <- read_dataset(example_path("synthetic_panel.csv"))
  cb <- read_codebooks(data.frame(name = "synthetic_panel_codebook.csv", datapath = example_path("synthetic_panel_codebook.csv")))
  base <- codebook_candidates(d, cb, initial_mapping(d, cb$records))
  plan <- rbind(base, research_rule("long", long_parameters(), 7),
    research_rule("composite", composite_parameters(), 8),
    research_rule("center", scale_parameters(output = "centered", scope = "group", groups = "cohort"), 9),
    research_rule("standardize", scale_parameters(), 10),
    research_rule("wide", list(ids = "id", time = "occasion", values = c("q1", "q2", "score", "centered", "score_z"), separator = "_"), 11))
  x <- apply_approved_rules(d, plan)
  testthat::expect_equal(dim(x$data), c(4L, 12L))
  testthat::expect_equal(as.character(x$data$id), c("001", "002", "003", "004"))
  stats <- jsonlite::fromJSON(x$audit$details[[9]])$constants
  testthat::expect_equal(stats[[1]], c("A", "B"))
  testthat::expect_equal(stats$reference_n, c(3, 3))
})
