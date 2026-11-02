testthat::test_that("preview selection stays local and invalidates with its inputs", {
  f <- server_fixture()
  shiny::testServer(f$server, {
    ui <- capture_inputs(session); load_fixture(session, f)
    testthat::expect_equal(nrow(plan_preview()$report), 0L)
    session$setInputs(approved_rules = ui$choices("approved_rules"))
    testthat::expect_true(plan_preview()$ok)
    testthat::expect_equal(plan_preview()$report$n_affected, 1L)
    testthat::expect_equal(f$calls$n, 0L)
    testthat::expect_null(result())
    session$setInputs(book_files = f$book_b)
    testthat::expect_equal(nrow(plan_preview()$report), 0L)
    testthat::expect_null(current_result())
  })
})

testthat::test_that("editing clears only the changed rule selection and cannot apply a stale key", {
  f <- server_fixture()
  shiny::testServer(f$server, {
    ui <- capture_inputs(session); load_fixture(session, f)
    session$setInputs(manual_var = "income", manual_op = "set_na", manual_codes = "9999",
      manual_reason = "Synthetic missing code", add_manual = 1L)
    old_keys <- ui$choices("approved_rules")
    session$setInputs(approved_rules = old_keys, run_button = 1L)
    testthat::expect_false(is.null(current_result()))
    session$setInputs(edit_rule = old_keys[[1]], manual_var = "gender", manual_op = "set_na",
      manual_codes = "1", manual_reason = "Corrected synthetic rule", add_manual = 2L)
    testthat::expect_equal(rules()$id, c(1L, 2L))
    testthat::expect_equal(rules()$values, c("1", "9999"))
    testthat::expect_null(current_result())
    testthat::expect_equal(approved_keys(), old_keys[[2]])
    testthat::expect_false(old_keys[[1]] %in% rule_keys())
    session$setInputs(run_button = 2L)
    testthat::expect_equal(current_result()$approved$variable, "income")
    testthat::expect_equal(current_result()$applied$data$gender, c(1L, 9L))
    session$setInputs(approved_rules = ui$choices("approved_rules"), run_button = 3L)
    testthat::expect_true(is.na(current_result()$applied$data$gender[[1]]))
  })
})

testthat::test_that("stale editor save after a source replacement cannot edit a new same-ID rule", {
  f <- server_fixture()
  shiny::testServer(f$server, {
    ui <- capture_inputs(session); load_fixture(session, f)
    old <- ui$choices("approved_rules")[[1]]
    session$setInputs(edit_rule = old, manual_var = "income", manual_op = "set_na",
      manual_codes = "100", manual_reason = "Must not apply", book_files = f$book_b, add_manual = 1L)
    testthat::expect_equal(rules()$variable, "income")
    testthat::expect_equal(rules()$values, "9999")
    testthat::expect_match(output$rule_feedback, "다시 선택", fixed = TRUE)
    session$setInputs(delete_rule = 1L)
    testthat::expect_equal(nrow(rules()), 1L)
  })
})

testthat::test_that("deleting a selected rule clears results and its pending selection", {
  f <- server_fixture()
  shiny::testServer(f$server, {
    ui <- capture_inputs(session); load_fixture(session, f)
    old <- ui$choices("approved_rules")
    session$setInputs(approved_rules = old, run_button = 1L)
    session$setInputs(edit_rule = old, delete_rule = 1L)
    testthat::expect_equal(nrow(rules()), 0L)
    testthat::expect_length(approved_keys(), 0L)
    testthat::expect_null(current_result())
    testthat::expect_equal(nrow(plan_preview()$report), 0L)
    testthat::expect_equal(dataset()$gender, c(1L, 9L))
  })
})

testthat::test_that("order changes require new selection and preview catches ordering errors", {
  f <- server_fixture()
  shiny::testServer(f$server, {
    ui <- capture_inputs(session); load_fixture(session, f)
    session$setInputs(manual_var = "gender", manual_op = "reverse", manual_min = 1,
      manual_max = 2, manual_reason = "Synthetic order test", add_manual = 1L)
    keys <- ui$choices("approved_rules")
    session$setInputs(approved_rules = keys, run_button = 1L)
    testthat::expect_true(plan_preview()$ok)
    session$setInputs(edit_rule = keys[[2]], move_rule_up = 1L)
    testthat::expect_equal(rules()$id, c(2L, 1L))
    testthat::expect_length(approved_keys(), 0L)
    testthat::expect_null(current_result())
    session$setInputs(approved_rules = ui$choices("approved_rules"))
    testthat::expect_false(plan_preview()$ok)
    testthat::expect_match(plan_preview()$error, "범위", fixed = TRUE)
    session$setInputs(run_button = 2L)
    testthat::expect_null(current_result())
    testthat::expect_match(output$run_status, "실행을 중단", fixed = TRUE)
  })
})

testthat::test_that("import option changes invalidate approvals, consent and output immediately", {
  f <- server_fixture()
  shiny::testServer(f$server, {
    ui <- capture_inputs(session); load_fixture(session, f)
    session$setInputs(approved_rules = ui$choices("approved_rules"), consent = ui$choices("consent"), run_button = 1L)
    session$setInputs(csv_encoding = "CP949", run_button = 2L, ai_button = 1L)
    testthat::expect_null(current_result())
    testthat::expect_length(approved_keys(), 0L)
    testthat::expect_length(ui$last("consent")$value, 0L)
    testthat::expect_equal(f$calls$n, 0L)
    testthat::expect_equal(loaded_import_options()$csv_encoding, "CP949")
  })
})
