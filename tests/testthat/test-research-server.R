testthat::test_that("derived variables become selectable and dependent edits invalidate approval", {
  f <- server_fixture()
  shiny::testServer(f$server, {
    ui <- capture_inputs(session); load_fixture(session, f)
    session$setInputs(manual_op = "composite", composite_items = c("gender", "income"),
      composite_method = "mean", composite_min = 2, new_variable = "score",
      manual_reason = "Synthetic composite", edit_rule = "new", add_manual = 1L)
    session$setInputs(approved_rules = ui$choices("approved_rules"))
    testthat::expect_true("score" %in% names(editor_context()$data))
    session$setInputs(manual_op = "center", scale_variable = "score", new_variable = "centered",
      scale_scope = "all", scale_reference = "all", manual_reason = "Synthetic center", add_manual = 2L)
    keys <- ui$choices("approved_rules")
    session$setInputs(approved_rules = keys, run_button = 1L)
    testthat::expect_true(plan_preview()$ok)
    testthat::expect_true("centered" %in% names(current_result()$applied$data))
    session$setInputs(edit_rule = keys[[2]], manual_op = "composite", composite_method = "sum",
      new_variable = "score", manual_reason = "Change method", add_manual = 3L)
    testthat::expect_null(current_result())
    testthat::expect_identical(approved_keys(), keys[[1]])
    testthat::expect_false(any(keys[2:3] %in% rule_keys()))
    testthat::expect_equal(f$calls$n, 0L)
  })
})

testthat::test_that("removing or deselecting a producer blocks a dependent plan", {
  f <- server_fixture()
  shiny::testServer(f$server, {
    ui <- capture_inputs(session); load_fixture(session, f)
    session$setInputs(manual_op = "composite", composite_items = "income", composite_method = "sum",
      composite_min = 1, new_variable = "score", manual_reason = "Synthetic", edit_rule = "new", add_manual = 1L)
    session$setInputs(approved_rules = ui$choices("approved_rules"))
    session$setInputs(manual_op = "center", scale_variable = "score", new_variable = "centered",
      scale_scope = "all", scale_reference = "all", manual_reason = "Synthetic", add_manual = 2L)
    keys <- ui$choices("approved_rules")
    session$setInputs(approved_rules = keys[c(1, 3)], run_button = 1L)
    testthat::expect_false(plan_preview()$ok)
    testthat::expect_null(current_result())
    session$setInputs(approved_rules = keys, edit_rule = keys[[2]], delete_rule = 1L)
    testthat::expect_false(keys[[3]] %in% rule_keys())
    session$setInputs(approved_rules = ui$choices("approved_rules"), run_button = 2L)
    testthat::expect_false(plan_preview()$ok)
    testthat::expect_null(current_result())
  })
})

testthat::test_that("opening an existing subset operation restores its reference values", {
  f <- server_fixture()
  shiny::testServer(f$server, {
    ui <- capture_inputs(session); load_fixture(session, f)
    session$setInputs(reference_column = "income", reference_values = "100", edit_rule = "new")
    p <- scale_parameters(input = "income", output = "centered", reference = "subset",
      reference_column = "gender", reference_values = "9")
    set_rules(research_rule("center", p))
    session$setInputs(edit_rule = rule_keys()[[1]])
    testthat::expect_identical(ui$last("reference_column")$value, "gender")
    testthat::expect_identical(ui$last("reference_values")$value, "9")
    testthat::expect_identical(ui$last("new_variable")$value, "centered")
  })
})
