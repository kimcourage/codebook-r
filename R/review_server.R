# Evaluated in the Shiny session environment by codebookr_server.
codebookr_review_bindings <- function() {
  review_table <- shiny::reactive({
    shiny::req(dataset(), codebooks())
    codebook_comparison(dataset(), codebooks(), mapping(), decisions())
  })
  invalidate_review <- function() {
    review_revision(review_revision() + 1L)
    base_result(NULL); result(NULL); execution_feedback("")
  }
  output$additional_mode <- shiny::renderText(workspace_mode())
  shiny::outputOptions(output, "additional_mode", suspendWhenHidden = FALSE)
  output$workspace_title <- shiny::renderText(if (workspace_mode() == "additional") "추가 전처리" else "코드북과 자료를 대조하세요")
  output$workspace_note <- shiny::renderText(if (workspace_mode() == "additional")
    "코드북 기준 결과에 필요한 작업을 더합니다. 앞선 기본 처리는 함께 재현되며, 기본 결과도 따로 받을 수 있습니다." else
    "변수별 원문과 실제 값을 확인하고 적용할 작업을 선택하세요. AI가 읽은 내용도 여기서 검토합니다.")
  output$execution_error <- shiny::renderText(execution_feedback())
  output$mapping_feedback <- shiny::renderText(mapping_feedback())
  output$project_feedback <- shiny::renderText(project_feedback())
  output$coverage <- shiny::renderText({
    if (is.null(dataset()) || is.null(codebooks())) return("원자료와 코드북을 모두 연결하면 변수별 점검을 시작합니다.")
    x <- review_table(); m <- mapping()
    sprintf("자료 변수 %d개 중 정의 연결 %d개. 확인사항이 있는 변수 %d개, 아직 검토하지 않은 변수 %d개. 자료에 연결하지 않은 코드북 변수 %d개.",
      nrow(x), sum(nzchar(x$codebook_variable)), sum(x$status == "확인 필요"), sum(x$decision == "미검토"), sum(!nzchar(m$data_variable)))
  })
  shiny::observe({
    m <- mapping(); old <- shiny::isolate(input$mapping_variable)
    choices <- if (!nrow(m)) character() else stats::setNames(m$codebook_variable, paste0(m$codebook_variable, " → ", ifelse(nzchar(m$data_variable), m$data_variable, "연결 안 됨")))
    shiny::updateSelectInput(session, "mapping_variable", choices = choices,
      selected = if (length(old) == 1L && old %in% m$codebook_variable) old else head(m$codebook_variable, 1L))
  })
  shiny::observe({
    m <- mapping(); i <- match(input$mapping_variable, m$codebook_variable)
    chosen <- if (length(i) == 1L && !is.na(i)) m$data_variable[[i]] else ""
    shiny::updateSelectInput(session, "mapping_target", choices = c("연결하지 않음" = "", names(dataset())), selected = chosen)
  })
  output$mapping_suggestion <- shiny::renderText({
    i <- match(input$mapping_variable, mapping()$codebook_variable)
    if (length(i) != 1L || is.na(i)) return("")
    s <- mapping()$suggestion[[i]]
    if (nzchar(s)) paste0("이름이 비슷한 변수: ", s, ". 뜻이 같은지 확인한 뒤 직접 연결하세요.") else "이름이 정확히 같은 변수만 처음에 연결합니다. 여러 정의를 한 변수에 연결하면 충돌 여부도 점검합니다."
  })
  shiny::observeEvent(input$save_mapping, {
    tryCatch({
      shiny::req(dataset(), codebooks())
      m <- mapping(); i <- match(input$mapping_variable, m$codebook_variable); target <- input$mapping_target
      if (length(i) != 1L || is.na(i) || length(target) != 1L || !target %in% c("", names(dataset()))) stop("코드북 변수와 연결할 자료 변수를 골라 주세요.", call. = FALSE)
      if (identical(m$data_variable[[i]], target)) return(invisible(NULL))
      m$data_variable[[i]] <- target; mapping(m)
      decisions(data.frame(variable = character(), decision = character(), reason = character()))
      invalidate_review(); undo_state(NULL)
      retained <- rules()[rules()$source != "codebook", , drop = FALSE]
      generated <- codebook_candidates(dataset(), codebooks(), m)
      updated <- append_rule_candidates(retained, generated, dataset())
      set_rules(updated, reset_approval = TRUE); reset_consent()
      mapping_feedback("변수 연결을 바꿨습니다. 코드북 후보를 갱신했으니 작업을 다시 선택하고 검토해 주세요.")
    }, error = function(e) mapping_feedback(error_message(e)))
  })
  output$mapping_table <- shiny::renderTable({
    m <- mapping(); names(m) <- c("코드북 변수", "자료 변수", "연결 후보"); m
  }, rownames = FALSE)
  shiny::observe({
    d <- dataset(); choices <- if (is.null(d)) character() else names(d)
    old <- shiny::isolate(input$review_variable)
    shiny::updateSelectInput(session, "review_variable", choices = choices,
      selected = if (length(old) == 1L && old %in% choices) old else head(choices, 1L))
    for (id in c("raw_columns", "result_columns")) {
      values <- if (id == "result_columns" && !is.null(current_result())) names(current_result()$applied$data) else choices
      selected <- intersect(shiny::isolate(input[[id]]), values)
      if (!length(selected)) selected <- head(values, 8L)
      if (id == "result_columns") selected <- head(unique(c(setdiff(values, choices), selected)), 10L)
      shiny::updateSelectInput(session, id, choices = values, selected = selected)
    }
  })
  output$raw_sample <- shiny::renderTable({shiny::req(dataset(), input$raw_columns); sample_values(dataset(), head(input$raw_columns, 10L))}, rownames = TRUE)
  output$review_findings <- shiny::renderText({
    x <- review_table(); x <- x[x$variable == input$review_variable, , drop = FALSE]
    if (!nrow(x)) return("")
    paste(x$label, if (nzchar(x$findings)) x$findings else "현재 점검 항목에서 불일치는 발견되지 않았습니다.",
      paste0("검토 기록: ", x$decision, if (nzchar(x$reason)) paste0(" — ", x$reason) else ""))
  })
  output$review_evidence <- shiny::renderTable({
    shiny::req(codebooks()); r <- mapped_codebook(codebooks(), mapping())$records
    r <- r[r$variable == input$review_variable, c("codebook_variable", "value_code", "value_label", "minimum", "maximum", "note", "source_file", "source_loc", "origin", "quote"), drop = FALSE]
    shown <- data.frame(코드 = r$value_code, 뜻 = r$value_label,
      지침 = paste(r$note, ifelse(nzchar(r$minimum) | nzchar(r$maximum), paste0("허용 범위: ", r$minimum, " ~ ", r$maximum), "")),
      근거 = paste0(r$source_file, " / ", r$source_loc, ifelse(r$origin == "AI", " (AI 해석)", "")), 원문 = r$quote, check.names = FALSE)
    head(shown, 100L)
  }, rownames = FALSE)
  output$review_frequency <- shiny::renderTable({shiny::req(dataset(), input$review_variable); value_frequency(dataset(), input$review_variable)}, rownames = FALSE)
  output$review_sample <- shiny::renderTable({
    shiny::req(dataset(), input$review_variable)
    p <- plan_preview(); v <- input$review_variable
    after <- p$data
    if (nrow(after) != nrow(dataset()) || any(selected_rules()$operation %in% c("long", "wide"))) return(sample_values(after, head(names(after), 10L)))
    out <- data.frame(행 = seq_len(min(12L, nrow(dataset()))), 이전 = head(raw_codes(dataset()[[v]]), 12L), check.names = FALSE)
    if (v %in% names(after)) out$이후 <- head(raw_codes(after[[v]]), 12L)
    for (nm in head(setdiff(names(after), names(dataset())), 4L)) out[[nm]] <- head(raw_codes(after[[nm]]), 12L)
    out
  }, rownames = FALSE)
  shiny::observeEvent(input$review_variable, {
    x <- decisions(); i <- match(input$review_variable, x$variable)
    shiny::updateSelectInput(session, "review_decision", selected = if (length(i) == 1L && !is.na(i)) x$decision[[i]] else "확인함")
    shiny::updateTextInput(session, "review_reason", value = if (length(i) == 1L && !is.na(i)) x$reason[[i]] else "")
  })
  shiny::observeEvent(input$save_decision, {
    tryCatch({
      shiny::req(dataset()); v <- input$review_variable
      if (length(v) != 1L || !v %in% names(dataset()) || !input$review_decision %in% c("확인함", "보류")) stop("검토할 변수를 선택해 주세요.", call. = FALSE)
      if (!nzchar(trimws(input$review_reason))) stop("확인하거나 보류한 이유를 적어 주세요.", call. = FALSE)
      d <- decisions(); d <- d[d$variable != v, , drop = FALSE]
      decisions(rbind(d, data.frame(variable = v, decision = input$review_decision, reason = trimws(input$review_reason))))
      invalidate_review(); rule_feedback("검토 기록을 저장했습니다. 결과를 다시 실행하면 기록이 함께 저장됩니다.")
    }, error = function(e) rule_feedback(error_message(e)))
  })
  output$review_overview <- shiny::renderTable({
    x <- review_table()[c("variable", "label", "status", "findings", "decision", "reason")]
    names(x) <- c("변수", "설명", "점검", "확인사항", "연구자 판단", "이유"); x
  }, rownames = FALSE)
  output$base_status <- shiny::renderText({
    x <- current_base()
    if (is.null(x)) return("코드북 기준 결과를 먼저 만들어 주세요.")
    sprintf("코드북 기준 결과: %d행, %d열. %d개 작업을 적용했습니다.", nrow(x$applied$data), ncol(x$applied$data), nrow(x$approved))
  })
  output$result_sample <- shiny::renderTable({shiny::req(current_result(), input$result_columns); sample_values(current_result()$applied$data, head(input$result_columns, 10L))}, rownames = TRUE)
  output$result_review <- shiny::renderText({
    x <- current_result(); shiny::req(x)
    t <- x$review$comparison
    sprintf("기본 처리 후 다시 점검한 확인사항은 %d개 변수입니다. 원자료 검토 기록에는 미검토 %d개 변수, 보류 %d개 변수가 남아 있습니다. 작업 실행이 전체 검토 완료를 뜻하지는 않습니다. 분기와 복잡한 점수 지침은 원문에서 별도로 확인하세요.", sum(x$review$after_comparison$status == "확인 필요"), sum(t$decision == "미검토"), sum(t$decision == "보류"))
  })
  output$post_review <- shiny::renderTable({
    x <- current_result(); shiny::req(x)
    t <- x$review$after_comparison[c("variable", "n_missing", "undefined_codes", "out_of_range", "findings", "decision")]
    names(t) <- c("변수", "결측 수", "목록에 없는 값", "범위 밖 값", "확인사항", "연구자 판단"); t
  }, rownames = FALSE)
  output$base_controls <- shiny::renderUI({
    if (is.null(current_base())) return(NULL)
    shiny::tagList(if (workspace_mode() == "additional" || is.null(current_result())) shiny::downloadButton("download_base", "기본 결과 ZIP"), shiny::actionButton("start_additional", "추가 전처리"), shiny::actionButton("edit_basic", "코드북 검토로 돌아가기"))
  })
  output$download_base <- shiny::downloadHandler(
    filename = function() sprintf("codebookr-basic-%s.zip", format(Sys.Date(), "%Y%m%d")),
    content = function(file) {
      x <- current_base(); shiny::req(x, dataset())
      export_bundle(file, x$applied, x$approved, metadata = variable_metadata(dataset()), source_name = data_name(), import_options = x$import_options, review = x$review)
    }, contentType = "application/zip")
  shiny::observeEvent(input$start_additional, {
    if (is.null(current_base())) {execution_feedback("코드북 기준 처리를 먼저 실행해 주세요."); return()}
    workspace_mode("additional"); set_rules(rules()); shiny::updateTabsetPanel(session, "main_tabs", selected = "codebook")
  })
  shiny::observeEvent(input$edit_basic, {
    saved <- current_base(); workspace_mode("codebook"); set_rules(rules())
    if (!is.null(saved)) shiny::updateCheckboxGroupInput(session, "approved_rules", selected = saved$approved_keys)
    shiny::updateTabsetPanel(session, "main_tabs", selected = "codebook")
  })
  shiny::observeEvent(input$run_unchanged, {
    shiny::req(dataset(), codebooks())
    if (workspace_mode() != "codebook" || length(approved_keys())) {execution_feedback("변경 없이 결과를 확인하려면 작업 선택을 모두 해제해 주세요."); return()}
    approved <- normalize_rules(empty_rules())
    x <- list(applied = apply_approved_rules(dataset(), approved), approved = approved,
      source_revision = source_revision(), rule_keys = rule_keys(), approved_keys = character(),
      import_options = loaded_import_options(), review_revision = review_revision(), review = review_context(dataset(), codebooks(), mapping(), decisions(), approved))
    result(x); base_result(x); execution_feedback(""); shiny::updateTabsetPanel(session, "main_tabs", selected = "execute")
  })
  shiny::observeEvent(input$undo_rule, {
    x <- undo_state()
    if (is.null(x)) {rule_feedback("되돌릴 작업이 없습니다."); return()}
    additional_ids(x$additional_ids); set_rules(x$rules, reset_approval = TRUE); undo_state(NULL)
    rule_feedback("직전 목록 변경을 되돌렸습니다. 적용할 작업을 다시 선택해 주세요.")
  })
  shiny::observeEvent(input$load_example, {
    reset_sources()
    files <- function(name) data.frame(name = name, datapath = file.path(codebookr_app_root, "examples", name), stringsAsFactors = FALSE)
    tryCatch({
      df <- files("synthetic_survey.csv"); bf <- files("synthetic_codebook.xlsx")
      d <- read_dataset(df$datapath, df$name); cb <- read_codebooks(bf)
      dataset(d); codebooks(cb); input_data_file(df); input_book_files(bf)
      loaded_import_options(normalize_import_options()); data_name(df$name)
      rebuild_rules(); data_feedback("가상 원자료와 코드북을 함께 불러왔습니다."); book_feedback("예제 코드북을 읽었습니다.")
    }, error = function(e) data_feedback(error_message(e)))
  })
  output$save_project <- shiny::downloadHandler(
    filename = function() paste0("codebookr-plan-", Sys.Date(), ".json"),
    content = function(file) {
      shiny::req(dataset(), codebooks())
      r <- normalize_codebook_records(codebooks()$records)
      write_project(file, source_manifest(input_data_file(), input_book_files()), loaded_import_options(), rules(), additional_ids(), mapping(), decisions(), r[r$origin == "AI", , drop = FALSE])
    }, contentType = "application/json")
  shiny::observeEvent(input$open_project, {
    tryCatch({
      if (is.null(dataset()) || is.null(codebooks())) stop("원자료와 코드북을 먼저 연결해 주세요.", call. = FALSE)
      x <- read_project(input$open_project$datapath[[1]], source_manifest(input_data_file(), input_book_files()), loaded_import_options())
      cb <- codebooks(); records <- normalize_codebook_records(cb$records); cb$records <- records[records$origin != "AI", , drop = FALSE]
      if (!is.null(x$ai_records) && nrow(x$ai_records)) {
        accepted <- validate_ai_definitions(x$ai_records, cb)
        if (nrow(accepted) != nrow(x$ai_records)) stop("저장된 AI 정의의 원문 근거를 확인하지 못했습니다.", call. = FALSE)
        cb$records <- unique(rbind(cb$records, accepted[names(cb$records)]))
      }
      m <- x$mapping
      if (is.null(m)) m <- data.frame(codebook_variable = character(), data_variable = character(), suggestion = character())
      if (is.null(m) || !all(c("codebook_variable", "data_variable", "suggestion") %in% names(m)) || anyNA(m) || anyDuplicated(m$codebook_variable) || any(!m$codebook_variable %in% cb$records$variable) || any(!m$data_variable %in% c("", names(dataset())))) stop("변수 연결 정보를 확인해 주세요.", call. = FALSE)
      d <- x$decisions
      if (is.null(d)) d <- data.frame(variable = character(), decision = character(), reason = character())
      if (!all(c("variable", "decision", "reason") %in% names(d)) || anyNA(d) || anyDuplicated(d$variable) || any(!d$variable %in% names(dataset())) || any(!d$decision %in% c("확인함", "보류")) || any(!nzchar(d$reason))) stop("검토 기록을 확인해 주세요.", call. = FALSE)
      codebooks(cb); mapping(initial_mapping(dataset(), cb$records, m)); decisions(d)
      additional_ids(as.integer(x$additional_ids)); workspace_mode("codebook"); undo_state(NULL)
      invalidate_review(); set_rules(x$rules, reset_approval = TRUE); reset_consent()
      project_feedback("작업을 불러왔습니다. 검토와 추가 작업은 복원했으며, 적용할 기본 작업을 다시 선택하고 실행해야 합니다.")
    }, error = function(e) project_feedback(paste0("작업을 읽지 못했습니다: ", error_message(e))))
  })
}
