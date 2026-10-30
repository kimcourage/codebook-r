# Shiny session state and handlers. Sourced separately for offline server tests.

codebookr_app_root <- normalizePath(".")

codebookr_server <- function(input, output, session) {
  dataset <- shiny::reactiveVal(NULL)
  codebooks <- shiny::reactiveVal(NULL)
  rules <- shiny::reactiveVal(empty_rules())
  result <- shiny::reactiveVal(NULL)
  ai_status <- shiny::reactiveVal("")
  data_feedback <- shiny::reactiveVal("")
  book_feedback <- shiny::reactiveVal("")
  rule_feedback <- shiny::reactiveVal("")
  execution_feedback <- shiny::reactiveVal("")
  loaded_import_options <- shiny::reactiveVal(list())
  data_name <- shiny::reactiveVal("")
  source_revision <- shiny::reactiveVal(0L)
  consent_revision <- shiny::reactiveVal(0L)
  rule_keys <- shiny::reactiveVal(character())
  next_rule_key <- 0L
  mapping <- shiny::reactiveVal(data.frame(codebook_variable = character(), data_variable = character(), suggestion = character()))
  decisions <- shiny::reactiveVal(data.frame(variable = character(), decision = character(), reason = character()))
  base_result <- shiny::reactiveVal(NULL)
  additional_ids <- shiny::reactiveVal(integer())
  workspace_mode <- shiny::reactiveVal("codebook")
  review_revision <- shiny::reactiveVal(0L)
  input_data_file <- shiny::reactiveVal(NULL)
  input_book_files <- shiny::reactiveVal(NULL)
  project_feedback <- shiny::reactiveVal("")
  mapping_feedback <- shiny::reactiveVal("")
  undo_state <- shiny::reactiveVal(NULL)
  current_base <- shiny::reactive({
    x <- base_result()
    if (is.null(x) || !identical(x$source_revision, source_revision()) ||
        !identical(x$review_revision, review_revision())) return(NULL)
    idx <- match(x$approved$id, rules()$id)
    if (anyNA(idx) || !identical(rule_keys()[idx], x$approved_keys)) return(NULL)
    x
  })
  active_rule_ids <- shiny::reactive({
    ids <- rules()$id
    if (workspace_mode() == "additional") ids[ids %in% additional_ids()] else ids[!ids %in% additional_ids()]
  })
  push_undo <- function() undo_state(list(rules = rules(), additional_ids = additional_ids()))

  # Browser updates are asynchronous. Old checkbox values must be invalid on
  # the server immediately, even before the browser receives the reset.
  consent_key <- shiny::reactive(paste0("consent_", consent_revision()))
  reset_consent <- function() {
    ai_status("")
    consent_revision(consent_revision() + 1L)
    shiny::updateCheckboxGroupInput(session, "consent",
      choices = stats::setNames(consent_key(),
        "전송 내용을 검토했으며 AI API 호출에 동의합니다."),
      selected = character())
  }

  set_rules <- function(proposed, reset_approval = FALSE) {
    proposed <- normalize_rules(proposed)
    # Core work precedes research additions in the executable plan.
    proposed <- proposed[order(proposed$id %in% additional_ids()), , drop = FALSE]
    previous <- normalize_rules(rules())
    old_contexts <- rule_contexts(previous); new_contexts <- rule_contexts(proposed)
    previous_keys <- rule_keys()
    keys <- vapply(seq_len(nrow(proposed)), function(i) {
      old <- match(proposed$id[[i]], previous$id)
      if (!reset_approval && !is.na(old) &&
          identical(new_contexts[[i]], old_contexts[[old]])) {
        return(previous_keys[[old]])
      }
      next_rule_key <<- next_rule_key + 1L
      paste0("rule_", next_rule_key)
    }, character(1))
    selected <- intersect(shiny::isolate(input$approved_rules), keys)
    choices <- stats::setNames(keys, vapply(seq_len(nrow(proposed)), function(i) {
      sprintf("%d. %s: %s", proposed$id[[i]], proposed$variable[[i]],
              substr(rule_description(proposed[i, , drop = FALSE]), 1L, 100L))
    }, character(1)))
    additional_ids(intersect(additional_ids(), proposed$id))
    rules(proposed)
    rule_keys(keys)
    result(NULL)
    execution_feedback("")
    rule_feedback("")
    visible <- if (workspace_mode() == "additional") proposed$id %in% additional_ids() else !proposed$id %in% additional_ids()
    shiny::updateCheckboxGroupInput(session, "approved_rules",
                                    choices = choices[visible], selected = intersect(selected, keys[visible]))
    editor <- shiny::isolate(input$edit_rule)
    shiny::updateSelectInput(session, "edit_rule", choices = c("새 작업" = "new", choices[visible]),
      selected = if (length(editor) == 1L && editor %in% keys[visible]) editor else "new")
  }

  reset_sources <- function() {
    source_revision(source_revision() + 1L)
    base_result(NULL); additional_ids(integer()); workspace_mode("codebook"); undo_state(NULL)
    mapping(data.frame(codebook_variable = character(), data_variable = character(), suggestion = character()))
    decisions(data.frame(variable = character(), decision = character(), reason = character()))
    review_revision(review_revision() + 1L)
    set_rules(empty_rules(), reset_approval = TRUE)
    reset_consent()
  }

  rebuild_rules <- function() {
    d <- dataset()
    cb <- codebooks()
    if (!is.null(d) && !is.null(cb)) mapping(initial_mapping(d, cb$records, mapping()))
    set_rules(if (!is.null(d) && !is.null(cb))
      codebook_candidates(d, cb, initial_mapping(d, cb$records, mapping())) else empty_rules(),
      reset_approval = TRUE)
  }

  approved_keys <- shiny::reactive({
    keys <- rule_keys()
    selected <- keys[keys %in% input$approved_rules & rules()$id %in% active_rule_ids()]
    if (workspace_mode() == "additional") {
      base <- current_base()
      if (is.null(base)) return(character())
      selected <- union(base$approved_keys, selected)
    }
    keys[keys %in% selected]
  })
  current_result <- shiny::reactive({
    x <- result()
    if (is.null(x) || !identical(x$source_revision, source_revision()) ||
        !identical(x$rule_keys, rule_keys()) || !identical(x$review_revision, review_revision()) ||
        !identical(x$approved_keys, approved_keys())) return(NULL)
    x
  })
  selected_rules <- shiny::reactive(rules()[rule_keys() %in% approved_keys(), , drop = FALSE])
  plan_preview <- shiny::reactive({
    d <- dataset()
    if (is.null(d)) return(list(ok = FALSE, error = "", report = data.frame()))
    preview_rules(d, selected_rules())
  })

  editor_context <- shiny::reactive({
    if (is.null(dataset())) return(list(data = NULL, error = "자료를 먼저 올려 주세요."))
    index <- match(input$edit_rule, rule_keys())
    before <- if (length(index) == 1L && !is.na(index)) seq_len(index - 1L) else seq_len(nrow(rules()))
    selected <- before[rule_keys()[before] %in% approved_keys()]
    p <- preview_rules(dataset(), rules()[selected, , drop = FALSE])
    list(data = p$data, error = p$error)
  })
  output$editor_context_status <- shiny::renderText({
    context <- editor_context()
    if (nzchar(context$error)) return(paste("앞선 작업을 확인해 주세요:", context$error))
    "선택한 앞선 작업의 결과를 사용합니다. 새 변수가 보이지 않으면 그 변수를 만드는 작업을 먼저 선택하세요."
  })
  shiny::observe({
    d <- editor_context()$data
    choices <- if (is.null(d)) character() else names(d)
    singles <- c("manual_var", "scale_variable", "reference_column", "wide_time")
    multiples <- c("composite_items", "scale_groups", "reshape_ids", "long_columns", "wide_values")
    for (id in c(singles, multiples)) {
      old <- shiny::isolate(input[[id]])
      selected <- intersect(old, choices)
      if (id %in% singles && !length(selected)) selected <- head(choices, 1L)
      shiny::updateSelectInput(session, id, choices = choices, selected = selected)
    }
  }, priority = 65)
  shiny::observe({
    d <- editor_context()$data; col <- input$reference_column
    choices <- if (is.null(d) || length(col) != 1L || !col %in% names(d)) character() else
      unique(as.character(d[[col]][!is.na(d[[col]])]))
    shiny::updateSelectInput(session, "reference_values", choices = choices,
      selected = intersect(shiny::isolate(input$reference_values), choices))
  }, priority = 65)

  error_message <- function(e, secret = "") {
    msg <- conditionMessage(e)
    if (nzchar(secret)) msg <- gsub(secret, "[REDACTED]", msg, fixed = TRUE)
    substr(msg, 1L, 450L)
  }
  warn <- function(e, secret = "") {
    shiny::showNotification(error_message(e, secret), type = "error", duration = 12)
  }

  shiny::observeEvent(list(input$data_file, input$csv_encoding, input$csv_character_columns), {
    reset_sources()
    dataset(NULL)
    data_feedback("")
    loaded_import_options(list())
    data_name("")
    shiny::updateSelectInput(session, "manual_var", choices = character())
    input_data_file(input$data_file)
    if (is.null(input$data_file) || !nrow(input$data_file)) return(invisible(NULL))
    tryCatch({
      file <- input$data_file
      options <- normalize_import_options(
        csv_encoding = if (is.null(input$csv_encoding)) "UTF-8" else input$csv_encoding,
        character_columns = if (is.null(input$csv_character_columns)) character() else split_codes(input$csv_character_columns))
      d <- do.call(read_dataset, c(list(path = file$datapath[[1]], original_name = file$name[[1]]), options))
      dataset(d)
      loaded_import_options(options)
      data_name(file$name[[1]])
      shiny::updateSelectInput(session, "manual_var", choices = names(d), selected = names(d)[[1]])
      rebuild_rules()
      data_feedback("자료를 읽었습니다. 변수 이름, 자료형과 결측 코드를 확인하세요.")
      shiny::showNotification(sprintf("원자료: %s행 × %s열", nrow(d), ncol(d)), type = "message")
    }, error = function(e) {
      dataset(NULL)
      data_feedback(paste0("자료 읽기 실패: ", error_message(e)))
      warn(e)
    })
  }, ignoreNULL = FALSE, priority = 100)

  shiny::observeEvent(input$book_files, {
    reset_sources()
    codebooks(NULL)
    book_feedback("")
    input_book_files(input$book_files)
    if (is.null(input$book_files) || !nrow(input$book_files)) return(invisible(NULL))
    tryCatch({
      cb <- read_codebooks(input$book_files)
      codebooks(cb)
      rebuild_rules()
      book_feedback(if (!nrow(cb$records))
        "변수 표를 자동으로 찾지 못했습니다. 발췌문을 확인하고 AI로 정의를 읽거나 직접 작업을 추가해 주세요." else
        "읽지 못한 부분이 있는지 확인한 뒤 코드북 검토로 넘어가세요.")
      shiny::showNotification(sprintf("코드북 %d개 파일을 읽었습니다.",
                               nrow(input$book_files)), type = "message")
    }, error = function(e) {
      codebooks(NULL)
      book_feedback(paste0("코드북 읽기 실패: ", error_message(e)))
      warn(e)
    })
  }, ignoreNULL = FALSE, priority = 100)

  # Consent covers the displayed payload and its destination/model.
  shiny::observeEvent(list(input$provider, input$model), {
    reset_consent()
  }, priority = 100)

  shiny::observeEvent(input$refresh_heuristics, {
    shiny::req(dataset(), codebooks())
    push_undo()
    proposed <- codebook_candidates(dataset(), codebooks(), mapping())
    set_rules(append_rule_candidates(rules(), proposed, dataset()))
    rule_feedback("새 코드북 후보를 추가했습니다. 기존 작업과 판단은 유지합니다.")
  }, priority = 75)

  shiny::observeEvent(input$edit_rule, {
    key <- input$edit_rule
    index <- match(key, rule_keys())
    if (identical(key, "new")) {
      shiny::updateSelectInput(session, "manual_op", selected = "set_na")
      shiny::updateTextInput(session, "manual_codes", value = "")
      shiny::updateNumericInput(session, "manual_min", value = 1)
      shiny::updateNumericInput(session, "manual_max", value = 5)
      shiny::updateTextInput(session, "manual_reason", value = "")
      shiny::updateTextInput(session, "new_variable", value = "")
    } else if (length(index) == 1L && !is.na(index)) {
      row <- rules()[index, , drop = FALSE]
      shiny::updateSelectInput(session, "manual_var", selected = row$variable[[1]])
      shiny::updateSelectInput(session, "manual_op", selected = row$operation[[1]])
      shiny::updateTextInput(session, "manual_codes", value = row$values[[1]])
      shiny::updateNumericInput(session, "manual_min", value = if (is.na(row$minimum[[1]])) 1 else row$minimum[[1]])
      shiny::updateNumericInput(session, "manual_max", value = if (is.na(row$maximum[[1]])) 5 else row$maximum[[1]])
      shiny::updateTextInput(session, "manual_reason", value = row$reason[[1]])
      p <- rule_parameters(row)
      if (row$operation %in% research_operations) {
        if (!is.null(p$output)) shiny::updateTextInput(session, "new_variable", value = p$output)
        values <- switch(row$operation,
          composite = list(composite_items = p$inputs, composite_method = p$method),
          center = , standardize = list(scale_variable = p$input, scale_scope = p$scope,
            scale_groups = p$groups, scale_reference = p$reference, reference_column = p$reference_column),
          long = list(reshape_ids = p$ids, long_columns = p$columns, long_layout = p$layout),
          wide = list(reshape_ids = p$ids, wide_time = p$time, wide_values = p$values))
        for (nm in names(values)) shiny::updateSelectInput(session, nm, selected = values[[nm]])
        if (row$operation == "composite") shiny::updateNumericInput(session, "composite_min", value = p$min_valid)
        if (row$operation %in% c("center", "standardize")) {
          if (!is.null(p$center)) shiny::updateNumericInput(session, "fixed_center", value = p$center)
          if (!is.null(p$scale)) shiny::updateNumericInput(session, "fixed_scale", value = p$scale)
          d <- editor_context()$data
          choices <- if (length(p$reference_column) == 1L && p$reference_column %in% names(d)) unique(as.character(d[[p$reference_column]][!is.na(d[[p$reference_column]])])) else character()
          shiny::updateSelectInput(session, "reference_values", choices = choices, selected = p$reference_values)
        }
        if (row$operation == "long") {
          for (nm in c("time", "value", "prefix", "separator")) shiny::updateTextInput(session, paste0("long_", nm), value = p[[nm]])
          shiny::updateCheckboxInput(session, "long_drop_na", value = p$drop_na)
        }
        if (row$operation == "wide") shiny::updateTextInput(session, "wide_separator", value = p$separator)
      }
    }
  }, priority = 60)

  shiny::observeEvent(input$add_manual, {
    shiny::req(dataset())
    tryCatch({
      op <- input$manual_op
      if (workspace_mode() == "additional" && is.null(current_base())) stop("코드북 기준 결과를 먼저 다시 확인하고 실행해 주세요.", call. = FALSE)
      if (op == "labels") stop("라벨 정의는 변수 연결과 코드북 원문에서 확인해 주세요. 제안을 제외하거나 코드북을 고친 뒤 다시 읽을 수 있습니다.", call. = FALSE)
      candidate <- if (op %in% research_operations) research_rule_from_inputs(input) else data.frame(
        id = 1L, variable = input$manual_var, operation = op,
        values = if (op == "set_na") trimws(input$manual_codes) else "",
        minimum = if (op == "reverse") input$manual_min else NA_real_,
        maximum = if (op == "reverse") input$manual_max else NA_real_,
        reason = input$manual_reason, evidence = "연구자 직접 지정",
        confidence = "manual", source = "manual", stringsAsFactors = FALSE
      )
      if (!nzchar(trimws(candidate$reason[[1]]))) stop("변경 근거를 입력해 주세요.", call. = FALSE)
      context <- editor_context()
      if (nzchar(context$error)) stop(paste("앞선 작업을 먼저 수정해 주세요:", context$error), call. = FALSE)
      check_rule(candidate, context$data)
      editor <- input$edit_rule
      if (length(editor) == 1L && !identical(editor, "new")) {
        index <- match(editor, rule_keys())
        if (is.na(index) || !rules()$id[[index]] %in% active_rule_ids()) stop("목록에서 기존 규칙을 다시 선택해 주세요.", call. = FALSE)
        push_undo()
        updated <- edit_rule_candidate(rules(), match(editor, rule_keys()), candidate, context$data)
        set_rules(updated)
        rule_feedback("작업을 수정했습니다. 이 작업과 영향을 받는 작업을 다시 선택하고 결과를 확인하세요.")
        return(invisible(NULL))
      }
      updated <- append_rule_candidates(rules(), candidate, context$data)
      if (nrow(updated) == nrow(rules())) {
        rule_feedback("동일한 규칙이 이미 존재합니다.")
        shiny::showNotification("동일한 규칙이 이미 존재합니다.", type = "warning")
      } else {
        push_undo()
        if (workspace_mode() == "additional") additional_ids(union(additional_ids(), setdiff(updated$id, rules()$id)))
        set_rules(updated)
        rule_feedback("작업을 추가했습니다. 목록에서 선택하면 결과를 미리 볼 수 있습니다.")
        shiny::showNotification("작업을 추가했습니다.")
      }
    }, error = function(e) {
      rule_feedback(paste0("작업을 저장하지 못했습니다: ", error_message(e)))
      warn(e)
    })
  })

  selected_editor_index <- function() {
    index <- match(input$edit_rule, rule_keys())
    if (length(index) != 1L || is.na(index)) stop("목록에서 기존 규칙을 다시 선택해 주세요.", call. = FALSE)
    if (!rules()$id[[index]] %in% active_rule_ids()) stop("현재 단계의 작업을 선택해 주세요.", call. = FALSE)
    index
  }
  shiny::observeEvent(input$delete_rule, {
    tryCatch({
      index <- selected_editor_index()
      push_undo()
      set_rules(rules()[-index, , drop = FALSE])
      rule_feedback("작업을 삭제했습니다. 원자료는 그대로입니다.")
    }, error = function(e) rule_feedback(error_message(e)))
  })
  move_rule <- function(direction) {
    tryCatch({
      index <- selected_editor_index()
      target <- index + direction
      if (target < 1L || target > nrow(rules()) || !rules()$id[[target]] %in% active_rule_ids()) return(invisible(NULL))
      push_undo()
      order <- seq_len(nrow(rules()))
      order[c(index, target)] <- order[c(target, index)]
      set_rules(rules()[order, , drop = FALSE], reset_approval = TRUE)
      rule_feedback("순서를 바꿨습니다. 작업을 다시 선택하고 결과를 확인하세요.")
    }, error = function(e) rule_feedback(error_message(e)))
  }
  shiny::observeEvent(input$move_rule_up, move_rule(-1L))
  shiny::observeEvent(input$move_rule_down, move_rule(1L))

  shiny::observeEvent(input$ai_button, {
    shiny::req(dataset(), codebooks())
    if (!identical(as.character(input$consent), consent_key())) {
      ai_status("코드북 전송 내용을 확인하고 동의에 체크해 주세요.")
      shiny::showNotification("코드북 전송 내용을 확인하고 동의에 체크해 주세요.",
                       type = "warning", duration = 8)
      return(invisible(NULL))
    }
    if (!nzchar(trimws(input$api_key))) {
      ai_status("본인의 API 키를 입력해 주세요.")
      shiny::showNotification("본인의 API 키를 입력해 주세요.", type = "warning")
      return(invisible(NULL))
    }
    tryCatch({
      ai_status("AI가 코드북을 해석하고 있습니다...")
      shiny::withProgress(message = "AI가 코드북을 해석하고 있습니다...", value = 0, {
        proposed <- propose_ai_rules(dataset(), codebooks(), input$provider,
                                     input$api_key, input$model)
        extracted <- attr(proposed, "definitions")
        rejected <- attr(proposed, "rejected_definitions")
        if (!is.null(extracted) && nrow(extracted)) {
          cb <- codebooks(); extracted <- validate_ai_definitions(extracted, cb)
          cb$records <- unique(rbind(normalize_codebook_records(cb$records), extracted[names(normalize_codebook_records(cb$records))]))
          codebooks(cb); mapping(initial_mapping(dataset(), cb$records, mapping()))
          decisions(data.frame(variable = character(), decision = character(), reason = character()))
          review_revision(review_revision() + 1L); base_result(NULL)
          reset_consent()
        }
        push_undo()
        updated <- append_rule_candidates(rules(), codebook_candidates(dataset(), codebooks(), mapping()), dataset())
        updated <- append_rule_candidates(updated, proposed, dataset())
        n_added <- nrow(updated) - nrow(rules())
        if (n_added > 0L) set_rules(updated)
        ai_status(sprintf("AI 응답 완료: 후보 %d개 중 새 규칙 %d개 추가. 기존과 중복되거나 유효하지 않은 후보는 추가하지 않습니다. 새 규칙은 승인이 필요합니다.",
                          nrow(proposed), n_added))
        if (!is.null(extracted)) ai_status(paste(ai_status(), sprintf("원문 확인된 정의 %d개, 원문을 확인하지 못해 제외한 정의 %d개.", nrow(extracted), if (is.null(rejected)) 0L else rejected)))
        shiny::showNotification(ai_status(), duration = 10)
      })
    }, error = function(e) {
      message <- error_message(e, input$api_key)
      help <- if (grepl("401|authentication|invalid.*key", message, ignore.case = TRUE)) " API 키와 제공업체를 확인한 뒤 다시 요청해 주세요." else
        if (grepl("429|rate.limit", message, ignore.case = TRUE)) " 호출 한도와 잔액을 확인하고 잠시 후 재시도해 주세요." else
        if (grepl("404|model.*not", message, ignore.case = TRUE)) " 제공업체에서 이용 가능한 모델 ID를 확인해 주세요." else
        " 입력 내용과 연결 상태를 확인한 뒤 다시 요청할 수 있습니다."
      ai_status(paste0("AI 요청 실패: ", message, help))
      warn(e, input$api_key)
    })
  })

  output$ai_status <- shiny::renderText(ai_status())
  output$data_feedback <- shiny::renderText(data_feedback())
  output$book_feedback <- shiny::renderText(book_feedback())
  output$rule_feedback <- shiny::renderText(rule_feedback())
  preview_description <- shiny::reactive({
    count <- nrow(selected_rules())
    if (!count) return("실행할 작업을 선택하면 예상 결과가 나타납니다.")
    p <- plan_preview()
    if (!p$ok) return(paste0("먼저 이 부분을 수정해 주세요. ", p$error))
    sprintf("선택한 %d개 작업의 예상 결과입니다. 문제가 없으면 아래에서 적용하세요.", count)
  })
  output$preview_status <- shiny::renderText(preview_description())
  output$execution_plan_summary <- shiny::renderText(sub(
    "문제가 없으면 아래에서 적용하세요.",
    "아래 버튼을 누르면 선택한 작업을 승인하고 실행합니다.",
    preview_description(), fixed = TRUE))
  shiny::observeEvent(input$go_codebook, {
    if (is.null(dataset()) || is.null(codebooks())) { mapping_feedback("원자료와 코드북을 모두 연결해 주세요."); return() }
    workspace_mode("codebook"); set_rules(rules())
    shiny::updateTabsetPanel(session, "main_tabs", selected = "codebook")
  })
  shiny::observeEvent(input$go_plan, shiny::updateTabsetPanel(session, "main_tabs", selected = "codebook"))
  shiny::observeEvent(input$go_execute, shiny::updateTabsetPanel(session, "main_tabs", selected = "execute"))
  output$preview_table <- shiny::renderTable(preview_table_display(plan_preview()),
                                             striped = TRUE, rownames = FALSE)

  output$dataset_status <- shiny::renderText({
    d <- dataset()
    if (is.null(d)) return("원자료를 올려 주세요.")
    sprintf("%s (%s행, 변수 %s개)", data_name(), nrow(d), ncol(d))
  })
  output$metadata_table <- shiny::renderTable({
    shiny::req(dataset())
    head(metadata_display(dataset()), 120L)
  }, striped = TRUE, hover = TRUE, rownames = FALSE)

  output$book_summary <- shiny::renderTable({
    shiny::req(codebooks())
    x <- codebooks()$summary
    x$recognized <- ifelse(x$recognized, "인식됨", "직접 확인 필요")
    names(x) <- c("파일", "시트 또는 구간", "표 인식", "변수 수", "확인사항")
    x
  }, striped = TRUE, rownames = FALSE)
  output$book_excerpts <- shiny::renderText({
    shiny::req(codebooks())
    excerpts <- codebooks()$excerpts
    paste(vapply(names(excerpts), function(nm) paste0("[", nm, "]\n", excerpts[[nm]]), character(1)), collapse = "\n\n")
  })
  output$book_records <- shiny::renderTable({
    shiny::req(codebooks())
    x <- head(codebooks()$records, 35L)
    x <- x[c("variable", "label", "value_code", "value_label", "note", "source_file", "source_loc")]
    names(x) <- c("변수", "설명", "코드", "값 설명", "비고", "파일", "문서 위치")
    x
  }, striped = TRUE, rownames = FALSE)
  output$payload_preview <- shiny::renderText({
    shiny::req(dataset(), codebooks())
    make_ai_request_preview(dataset(), codebooks())
  })
  output$rules_table <- shiny::renderTable({
    r <- rules()
    if (!nrow(r)) return(NULL)
    rule_table_display(r, approved_keys(), rule_keys())
  }, striped = TRUE, rownames = FALSE)

  previously_checked <- character()
  shiny::observeEvent(input$approved_rules, {
    added <- setdiff(input$approved_rules, previously_checked)
    previously_checked <<- input$approved_rules
    if (length(added)) {
      i <- match(tail(added, 1L), rule_keys())
      if (!is.na(i) && rules()$variable[[i]] %in% names(dataset())) shiny::updateSelectInput(session, "review_variable", selected = rules()$variable[[i]])
    }
    result(NULL) # Prevent stale output after selection changes.
    execution_feedback("")
  }, ignoreNULL = FALSE, priority = 50)

  shiny::observeEvent(input$run_button, {
    shiny::req(dataset())
    tryCatch({
      if (is.null(codebooks())) stop("코드북을 먼저 연결해 주세요.", call. = FALSE)
      if (workspace_mode() == "additional" && is.null(current_base())) stop("코드북 기준 결과가 바뀌었습니다. 기본 처리를 먼저 다시 실행해 주세요.", call. = FALSE)
      approved <- selected_rules()
      if (!nrow(approved)) stop("실행할 작업을 하나 이상 선택해 주세요.", call. = FALSE)
      # Apply the displayed row order, independent of stable display IDs.
      applied <- apply_approved_rules(dataset(), approved)
      # Verify generated standalone script can be constructed before marking success.
      make_standalone_script(approved, import_options = loaded_import_options())
      result(list(applied = applied, approved = approved,
                  source_revision = source_revision(), rule_keys = rule_keys(),
                  approved_keys = approved_keys(), import_options = loaded_import_options(),
                  review_revision = review_revision(), review = review_context(dataset(), codebooks(), mapping(), decisions(),
                    approved[!approved$id %in% additional_ids(), , drop = FALSE],
                    after_data = if (workspace_mode() == "additional") current_base()$applied$data else applied$data)))
      if (workspace_mode() == "codebook") base_result(result())
      shiny::updateTabsetPanel(session, "main_tabs", selected = "execute")
      execution_feedback("")
      shiny::showNotification("승인한 전처리 규칙이 적용되었습니다.", type = "message")
    }, error = function(e) {
      result(NULL)
      execution_feedback(paste0("실행을 중단했습니다: ", error_message(e)))
      warn(e)
    })
  })

  output$run_status <- shiny::renderText({
    x <- current_result()
    if (is.null(x)) return(if (nzchar(execution_feedback())) execution_feedback() else "실행 결과가 없습니다. 작업을 선택하고 미리보기를 확인한 뒤 실행해 주세요.")
    sprintf("%d개 작업을 실행했습니다. 결과는 %d행, %d열입니다.",
            nrow(x$approved), nrow(x$applied$data), ncol(x$applied$data))
  })
  output$audit_table <- shiny::renderTable({
    shiny::req(current_result())
    x <- current_result()$applied$audit
    x <- x[c("id", "variable", "operation", "n_affected", "missing_before", "missing_after", "rows_before", "rows_after", "note")]
    x$operation <- operation_label(x$operation)
    names(x) <- c("번호", "변수", "작업", "적용 수", "결측 수 (전)", "결측 수 (후)", "행 수 (전)", "행 수 (후)", "계산 기준")
    x
  }, striped = TRUE, rownames = FALSE)
  output$after_metadata <- shiny::renderTable({
    shiny::req(current_result())
    head(metadata_display(current_result()$applied$data), 120L)
  }, striped = TRUE, rownames = FALSE)
  output$download_controls <- shiny::renderUI({
    if (is.null(current_result())) {
      return(shiny::tags$p(class = "hint",
        "규칙을 승인하고 실행하면 ZIP을 다운로드할 수 있습니다."))
    }
    shiny::downloadButton("download_bundle", if (workspace_mode() == "additional") "추가 작업 포함 ZIP" else "결과 다운로드 (ZIP)")
  })
  output$download_bundle <- shiny::downloadHandler(
    filename = function() sprintf("codebookr-results-%s.zip", format(Sys.Date(), "%Y%m%d")),
    content = function(file) {
      x <- current_result(); shiny::req(x, dataset())
      export_bundle(file, x$applied, x$approved,
                    metadata = variable_metadata(dataset()),
                    source_name = data_name(), import_options = x$import_options, review = x$review)
    },
    contentType = "application/zip"
  )
  eval(body(codebookr_review_bindings), envir = environment())
}

