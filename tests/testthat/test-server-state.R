testthat::test_that("replacing a codebook rejects stale approvals and consent before browser reset", {
  f <- server_fixture()
  shiny::testServer(f$server, {
    ui <- capture_inputs(session)
    load_fixture(session, f)
    old_approval <- ui$choices("approved_rules")
    old_consent <- ui$choices("consent")
    session$setInputs(approved_rules = old_approval, consent = old_consent, run_button = 1L)
    testthat::expect_false(is.null(current_result()))
    zip_path <- output$download_bundle
    testthat::expect_true(file.exists(zip_path))
    testthat::expect_setequal(utils::unzip(zip_path, list = TRUE)$Name,
      c("cleaned_data.rds", "approved_rules.csv", "audit.csv", "before_metadata.csv",
        "after_metadata.csv", "preprocessing.R", "session_info.txt", "README.txt", "variable_mapping.csv", "codebook_definitions.csv", "codebook_review.csv", "codebook_documents.csv", "codebook_after_review.csv"))
    session$setInputs(book_files = f$book_b, run_button = 2L, ai_button = 1L)
    testthat::expect_equal(rules()$variable, "income")
    testthat::expect_length(ui$last("approved_rules")$value, 0L)
    testthat::expect_length(ui$last("consent")$value, 0L)
    testthat::expect_false(old_approval %in% ui$choices("approved_rules"))
    testthat::expect_false(old_consent %in% ui$choices("consent"))
    testthat::expect_null(current_result())
    testthat::expect_equal(f$calls$n, 0L)
    # A new deliberate selection authorizes only the replacement rule.
    session$setInputs(approved_rules = ui$choices("approved_rules"), run_button = 3L)
    testthat::expect_equal(current_result()$approved$variable, "income")
    testthat::expect_equal(sum(is.na(current_result()$applied$data$income)), 1L)
    testthat::expect_equal(sum(is.na(current_result()$applied$data$gender)), 0L)
    session$setInputs(consent = ui$choices("consent"), ai_button = 2L)
    testthat::expect_equal(f$calls$n, 1L)
  })
})

testthat::test_that("new data and identical-content reuploads require fresh approval and consent", {
  for (replacement in c("data_b", "data_same", "book_same")) {
    f <- server_fixture()
    shiny::testServer(f$server, {
      ui <- capture_inputs(session)
      load_fixture(session, f)
      old_approval <- ui$choices("approved_rules")
      old_consent <- ui$choices("consent")
      session$setInputs(approved_rules = old_approval, consent = old_consent, run_button = 1L)
      args <- list(run_button = 2L, ai_button = 1L)
      args[[if (replacement == "book_same") "book_files" else "data_file"]] <- f[[replacement]]
      do.call(session$setInputs, args)
      testthat::expect_null(current_result(), info = replacement)
      testthat::expect_length(ui$last("approved_rules")$value, 0L)
      testthat::expect_length(ui$last("consent")$value, 0L)
      testthat::expect_false(old_approval %in% ui$choices("approved_rules"))
      testthat::expect_equal(f$calls$n, 0L)
    })
  }
})

testthat::test_that("clearing every approval removes results and blocks execution and downloads", {
  for (empty in list(NULL, character())) {
    f <- server_fixture()
    shiny::testServer(f$server, {
      ui <- capture_inputs(session)
      load_fixture(session, f)
      session$setInputs(approved_rules = ui$choices("approved_rules"), run_button = 1L)
      testthat::expect_false(is.null(current_result()))
      session$setInputs(approved_rules = empty)
      testthat::expect_null(result())
      testthat::expect_null(current_result())
      testthat::expect_match(output$run_status, "실행 결과가 없습니다", fixed = TRUE)
      testthat::expect_error(output$download_bundle, class = "shiny.silent.error")
      session$setInputs(run_button = 2L)
      testthat::expect_null(current_result())
    })
  }
})

testthat::test_that("failed or removed uploads invalidate rules, results and consent", {
  for (kind in c("data_file", "book_files")) {
    for (replacement in list(NULL, data.frame(name = character(), datapath = character()),
                             data.frame(name = "broken.csv", datapath = "/nonexistent/codebookr-test.csv"))) {
      f <- server_fixture()
      shiny::testServer(f$server, {
        ui <- capture_inputs(session)
        load_fixture(session, f)
        session$setInputs(approved_rules = ui$choices("approved_rules"),
          consent = ui$choices("consent"), run_button = 1L)
        do.call(session$setInputs, stats::setNames(list(replacement), kind))
        testthat::expect_null(result())
        testthat::expect_null(current_result())
        testthat::expect_equal(nrow(rules()), 0L)
        testthat::expect_length(ui$last("consent")$value, 0L)
        testthat::expect_null(if (kind == "data_file") dataset() else codebooks())
      })
    }
  }
})

testthat::test_that("regenerating rules clears approvals even when their contents are identical", {
  f <- server_fixture()
  shiny::testServer(f$server, {
    ui <- capture_inputs(session)
    load_fixture(session, f)
    old_approval <- ui$choices("approved_rules")
    session$setInputs(approved_rules = old_approval, run_button = 1L)
    session$setInputs(refresh_heuristics = 1L)
    testthat::expect_null(current_result())
    testthat::expect_equal(ui$last("approved_rules")$value, old_approval)
    testthat::expect_true(old_approval %in% ui$choices("approved_rules"))
    session$setInputs(run_button = 2L)
    testthat::expect_false(is.null(current_result()))
  })
})

testthat::test_that("manual and AI additions retain only approvals for unchanged rules", {
  for (method in c("manual", "AI")) {
    f <- server_fixture()
    shiny::testServer(f$server, {
      ui <- capture_inputs(session)
      load_fixture(session, f)
      old_approval <- ui$choices("approved_rules")
      session$setInputs(approved_rules = old_approval, run_button = 1L)
      if (method == "manual") {
        session$setInputs(manual_var = "income", manual_op = "set_na", manual_codes = "9999",
          manual_reason = "Synthetic missing code", add_manual = 1L)
      } else {
        session$setInputs(consent = ui$choices("consent"), ai_button = 1L)
      }
      testthat::expect_null(current_result())
      testthat::expect_equal(nrow(rules()), 2L)
      testthat::expect_equal(as.character(ui$last("approved_rules")$value), old_approval)
      session$setInputs(run_button = 2L)
      testthat::expect_equal(nrow(current_result()$approved), 1L)
      testthat::expect_equal(current_result()$approved$operation, "set_na")
      # Selecting the new rule then invalidates the old result.
      session$setInputs(approved_rules = ui$choices("approved_rules"))
      testthat::expect_null(current_result())
      session$setInputs(run_button = 3L)
      testthat::expect_equal(nrow(current_result()$approved), 2L)
      # Selecting a subset also invalidates the result.
      session$setInputs(approved_rules = old_approval)
      testthat::expect_null(current_result())
    })
  }
})

testthat::test_that("changing a rule with the same displayed ID invalidates its approval", {
  f <- server_fixture()
  shiny::testServer(f$server, {
    ui <- capture_inputs(session)
    load_fixture(session, f)
    old_approval <- ui$choices("approved_rules")
    session$setInputs(approved_rules = old_approval, run_button = 1L)
    changed <- rules()
    changed$values <- "1"
    set_rules(changed)
    session$flushReact()
    testthat::expect_null(current_result())
    testthat::expect_length(ui$last("approved_rules")$value, 0L)
    session$setInputs(run_button = 2L)
    testthat::expect_null(current_result())
  })
})

testthat::test_that("provider and model changes invalidate consent without changing local approvals", {
  for (field in c("provider", "model")) {
    f <- server_fixture()
    shiny::testServer(f$server, {
      ui <- capture_inputs(session)
      load_fixture(session, f)
      session$setInputs(approved_rules = ui$choices("approved_rules"),
        consent = ui$choices("consent"), run_button = 1L)
      args <- list(ai_button = 1L)
      args[[field]] <- if (field == "provider") "anthropic" else "synthetic-model"
      do.call(session$setInputs, args)
      testthat::expect_equal(f$calls$n, 0L)
      testthat::expect_length(ui$last("consent")$value, 0L)
      testthat::expect_false(is.null(current_result()))
      session$setInputs(consent = ui$choices("consent"), ai_button = 2L)
      testthat::expect_equal(f$calls$n, 1L)
    })
  }
})

testthat::test_that("explicit consent withdrawal blocks subsequent AI requests", {
  f <- server_fixture()
  shiny::testServer(f$server, {
    ui <- capture_inputs(session)
    load_fixture(session, f)
    session$setInputs(consent = ui$choices("consent"), ai_button = 1L)
    testthat::expect_equal(f$calls$n, 1L)
    session$setInputs(consent = NULL, ai_button = 2L)
    testthat::expect_equal(f$calls$n, 1L)
  })
})

testthat::test_that("a result snapshot cannot be served against changed approvals", {
  f <- server_fixture()
  shiny::testServer(f$server, {
    ui <- capture_inputs(session)
    load_fixture(session, f)
    session$setInputs(approved_rules = ui$choices("approved_rules"), run_button = 1L)
    previous <- result()
    session$setInputs(approved_rules = NULL)
    result(previous)
    testthat::expect_null(current_result())
    testthat::expect_error(output$download_bundle, class = "shiny.silent.error")
  })
})

testthat::test_that("download button is offered only while a current result exists", {
  f <- server_fixture()
  shiny::testServer(f$server, {
    ui <- capture_inputs(session)
    load_fixture(session, f)
    download_visible <- function() grepl('id="download_bundle"',
      output$download_controls$html, fixed = TRUE)
    testthat::expect_false(download_visible())
    session$setInputs(approved_rules = ui$choices("approved_rules"), run_button = 1L)
    testthat::expect_true(download_visible())
    session$setInputs(approved_rules = NULL)
    testthat::expect_false(download_visible())
    session$setInputs(approved_rules = ui$choices("approved_rules"), run_button = 2L)
    testthat::expect_true(download_visible())
    session$setInputs(book_files = f$book_b)
    testthat::expect_false(download_visible())
  })
})

testthat::test_that("AI request status persists for success and duplicate-only responses", {
  f <- server_fixture()
  shiny::testServer(f$server, {
    ui <- capture_inputs(session)
    load_fixture(session, f)
    testthat::expect_identical(output$ai_status, "")
    session$setInputs(ai_button = 1L)
    testthat::expect_match(output$ai_status, "동의에 체크", fixed = TRUE)
    session$setInputs(consent = ui$choices("consent"), ai_button = 2L)
    testthat::expect_match(output$ai_status, "후보 1개 중 새 규칙 1개", fixed = TRUE)
    session$setInputs(ai_button = 3L)
    testthat::expect_match(output$ai_status, "후보 1개 중 새 규칙 0개", fixed = TRUE)
    session$setInputs(model = "another-model")
    testthat::expect_identical(output$ai_status, "")
  })
})

testthat::test_that("persistent AI failures redact the credential and allow a later retry", {
  f <- server_fixture()
  env <- environment(f$server)
  successful_request <- env$propose_ai_rules
  env$propose_ai_rules <- function(data, codebooks, provider, api_key, model) {
    stop(paste("Invalid credential", api_key), call. = FALSE)
  }
  shiny::testServer(f$server, {
    ui <- capture_inputs(session)
    load_fixture(session, f)
    session$setInputs(consent = ui$choices("consent"), ai_button = 1L)
    testthat::expect_match(output$ai_status, "AI 요청 실패", fixed = TRUE)
    testthat::expect_match(output$ai_status, "[REDACTED]", fixed = TRUE)
    testthat::expect_false(grepl("synthetic-test-key", output$ai_status, fixed = TRUE))
    env$propose_ai_rules <- successful_request
    session$setInputs(ai_button = 2L)
    testthat::expect_match(output$ai_status, "AI 응답 완료", fixed = TRUE)
  })
})
