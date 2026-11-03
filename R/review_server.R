# Evaluated in the Shiny session environment by codebookr_server.
codebookr_review_bindings <- quote({
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
  output$workspace_title <- shiny::renderText(if (workspace_mode() == "additional") "\ucd94\uac00 \uc804\ucc98\ub9ac" else "\ucf54\ub4dc\ubd81\uacfc \uc790\ub8cc\ub97c \ub300\uc870\ud558\uc138\uc694")
  output$workspace_note <- shiny::renderText(if (workspace_mode() == "additional")
    "\ucf54\ub4dc\ubd81 \uae30\uc900 \uacb0\uacfc\uc5d0 \ud544\uc694\ud55c \uc791\uc5c5\uc744 \ub354\ud569\ub2c8\ub2e4. \uc55e\uc120 \uae30\ubcf8 \ucc98\ub9ac\ub294 \ud568\uaed8 \uc7ac\ud604\ub418\uba70, \uae30\ubcf8 \uacb0\uacfc\ub3c4 \ub530\ub85c \ubc1b\uc744 \uc218 \uc788\uc2b5\ub2c8\ub2e4." else
    "\ubcc0\uc218\ubcc4 \uc6d0\ubb38\uacfc \uc2e4\uc81c \uac12\uc744 \ud655\uc778\ud558\uace0 \uc801\uc6a9\ud560 \uc791\uc5c5\uc744 \uc120\ud0dd\ud558\uc138\uc694. AI\uac00 \uc77d\uc740 \ub0b4\uc6a9\ub3c4 \uc5ec\uae30\uc11c \uac80\ud1a0\ud569\ub2c8\ub2e4.")
  output$execution_error <- shiny::renderText(execution_feedback())
  output$mapping_feedback <- shiny::renderText(mapping_feedback())
  output$project_feedback <- shiny::renderText(project_feedback())
  output$coverage <- shiny::renderText({
    if (is.null(dataset()) || is.null(codebooks())) return("\uc6d0\uc790\ub8cc\uc640 \ucf54\ub4dc\ubd81\uc744 \ubaa8\ub450 \uc5f0\uacb0\ud558\uba74 \ubcc0\uc218\ubcc4 \uc810\uac80\uc744 \uc2dc\uc791\ud569\ub2c8\ub2e4.")
    x <- review_table(); m <- mapping()
    sprintf("\uc790\ub8cc \ubcc0\uc218 %d\uac1c \uc911 \uc815\uc758 \uc5f0\uacb0 %d\uac1c. \ud655\uc778\uc0ac\ud56d\uc774 \uc788\ub294 \ubcc0\uc218 %d\uac1c, \uc544\uc9c1 \uac80\ud1a0\ud558\uc9c0 \uc54a\uc740 \ubcc0\uc218 %d\uac1c. \uc790\ub8cc\uc5d0 \uc5f0\uacb0\ud558\uc9c0 \uc54a\uc740 \ucf54\ub4dc\ubd81 \ubcc0\uc218 %d\uac1c.",
      nrow(x), sum(nzchar(x$codebook_variable)), sum(x$status == "\ud655\uc778 \ud544\uc694"), sum(x$decision == "\ubbf8\uac80\ud1a0"), sum(!nzchar(m$data_variable)))
  })
  shiny::observe({
    m <- mapping(); old <- shiny::isolate(input$mapping_variable)
    choices <- if (!nrow(m)) character() else stats::setNames(m$codebook_variable, paste0(m$codebook_variable, " \u2192 ", ifelse(nzchar(m$data_variable), m$data_variable, "\uc5f0\uacb0 \uc548 \ub428")))
    shiny::updateSelectInput(session, "mapping_variable", choices = choices,
      selected = if (length(old) == 1L && old %in% m$codebook_variable) old else head(m$codebook_variable, 1L))
  })
  shiny::observe({
    m <- mapping(); i <- match(input$mapping_variable, m$codebook_variable)
    chosen <- if (length(i) == 1L && !is.na(i)) m$data_variable[[i]] else ""
    shiny::updateSelectInput(session, "mapping_target", choices = c("\uc5f0\uacb0\ud558\uc9c0 \uc54a\uc74c" = "", names(dataset())), selected = chosen)
  })
  output$mapping_suggestion <- shiny::renderText({
    i <- match(input$mapping_variable, mapping()$codebook_variable)
    if (length(i) != 1L || is.na(i)) return("")
    s <- mapping()$suggestion[[i]]
    if (nzchar(s)) paste0("\uc774\ub984\uc774 \ube44\uc2b7\ud55c \ubcc0\uc218: ", s, ". \ub73b\uc774 \uac19\uc740\uc9c0 \ud655\uc778\ud55c \ub4a4 \uc9c1\uc811 \uc5f0\uacb0\ud558\uc138\uc694.") else "\uc774\ub984\uc774 \uc815\ud655\ud788 \uac19\uc740 \ubcc0\uc218\ub9cc \ucc98\uc74c\uc5d0 \uc5f0\uacb0\ud569\ub2c8\ub2e4. \uc5ec\ub7ec \uc815\uc758\ub97c \ud55c \ubcc0\uc218\uc5d0 \uc5f0\uacb0\ud558\uba74 \ucda9\ub3cc \uc5ec\ubd80\ub3c4 \uc810\uac80\ud569\ub2c8\ub2e4."
  })
  shiny::observeEvent(input$save_mapping, {
    tryCatch({
      shiny::req(dataset(), codebooks())
      m <- mapping(); i <- match(input$mapping_variable, m$codebook_variable); target <- input$mapping_target
      if (length(i) != 1L || is.na(i) || length(target) != 1L || !target %in% c("", names(dataset()))) stop("\ucf54\ub4dc\ubd81 \ubcc0\uc218\uc640 \uc5f0\uacb0\ud560 \uc790\ub8cc \ubcc0\uc218\ub97c \uace8\ub77c \uc8fc\uc138\uc694.", call. = FALSE)
      if (identical(m$data_variable[[i]], target)) return(invisible(NULL))
      m$data_variable[[i]] <- target; mapping(m)
      decisions(data.frame(variable = character(), decision = character(), reason = character()))
      invalidate_review(); undo_state(NULL)
      retained <- rules()[rules()$source != "codebook", , drop = FALSE]
      generated <- codebook_candidates(dataset(), codebooks(), m)
      updated <- append_rule_candidates(retained, generated, dataset())
      set_rules(updated, reset_approval = TRUE); reset_consent()
      mapping_feedback("\ubcc0\uc218 \uc5f0\uacb0\uc744 \ubc14\uafe8\uc2b5\ub2c8\ub2e4. \ucf54\ub4dc\ubd81 \ud6c4\ubcf4\ub97c \uac31\uc2e0\ud588\uc73c\ub2c8 \uc791\uc5c5\uc744 \ub2e4\uc2dc \uc120\ud0dd\ud558\uace0 \uac80\ud1a0\ud574 \uc8fc\uc138\uc694.")
    }, error = function(e) mapping_feedback(error_message(e)))
  })
  output$mapping_table <- shiny::renderTable({
    m <- mapping(); names(m) <- c("\ucf54\ub4dc\ubd81 \ubcc0\uc218", "\uc790\ub8cc \ubcc0\uc218", "\uc5f0\uacb0 \ud6c4\ubcf4"); m
  }, rownames = FALSE)
  shiny::observe({
    d <- dataset(); choices <- if (is.null(d)) character() else names(d)
    old <- shiny::isolate(input$review_variable)
    shiny::updateSelectInput(session, "review_variable", choices = choices,
      selected = if (length(old) == 1L && old %in% choices) old else head(choices, 1L))
  })
  shiny::observe({
    d <- dataset(); choices <- if (is.null(d)) character() else names(d)
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
    paste(x$label, if (nzchar(x$findings)) x$findings else "\ud604\uc7ac \uc810\uac80 \ud56d\ubaa9\uc5d0\uc11c \ubd88\uc77c\uce58\ub294 \ubc1c\uacac\ub418\uc9c0 \uc54a\uc558\uc2b5\ub2c8\ub2e4.",
      paste0("\uac80\ud1a0 \uae30\ub85d: ", x$decision, if (nzchar(x$reason)) paste0(" \u2014 ", x$reason) else ""))
  })
  output$review_evidence <- shiny::renderTable({
    shiny::req(codebooks()); r <- mapped_codebook(codebooks(), mapping())$records
    r <- r[r$variable == input$review_variable, c("codebook_variable", "value_code", "value_label", "minimum", "maximum", "note", "source_file", "source_loc", "origin", "quote"), drop = FALSE]
    shown <- data.frame("\ucf54\ub4dc" = r$value_code, "\ub73b" = r$value_label,
      "\uc9c0\uce68" = paste(r$note, ifelse(nzchar(r$minimum) | nzchar(r$maximum), paste0("\ud5c8\uc6a9 \ubc94\uc704: ", r$minimum, " ~ ", r$maximum), "")),
      "\uadfc\uac70" = paste0(r$source_file, " / ", r$source_loc, ifelse(r$origin == "AI", " (AI \ud574\uc11d)", "")), "\uc6d0\ubb38" = r$quote, check.names = FALSE)
    head(shown, 100L)
  }, rownames = FALSE)
  output$review_frequency <- shiny::renderTable({shiny::req(dataset(), input$review_variable); value_frequency(dataset(), input$review_variable)}, rownames = FALSE)
  output$review_sample <- shiny::renderTable({
    shiny::req(dataset(), input$review_variable)
    p <- plan_preview(); v <- input$review_variable
    after <- p$data
    if (nrow(after) != nrow(dataset()) || any(selected_rules()$operation %in% c("long", "wide"))) return(sample_values(after, head(names(after), 10L)))
    out <- data.frame("\ud589" = seq_len(min(12L, nrow(dataset()))), "\uc774\uc804" = head(raw_codes(dataset()[[v]]), 12L), check.names = FALSE)
    if (v %in% names(after)) out$"\uc774\ud6c4" <- head(raw_codes(after[[v]]), 12L)
    for (nm in head(setdiff(names(after), names(dataset())), 4L)) out[[nm]] <- head(raw_codes(after[[nm]]), 12L)
    out
  }, rownames = FALSE)
  shiny::observeEvent(input$review_variable, {
    x <- decisions(); i <- match(input$review_variable, x$variable)
    shiny::updateSelectInput(session, "review_decision", selected = if (length(i) == 1L && !is.na(i)) x$decision[[i]] else "\ud655\uc778\ud568")
    shiny::updateTextInput(session, "review_reason", value = if (length(i) == 1L && !is.na(i)) x$reason[[i]] else "")
  })
  shiny::observeEvent(input$save_decision, {
    tryCatch({
      shiny::req(dataset()); v <- input$review_variable
      if (length(v) != 1L || !v %in% names(dataset()) || !input$review_decision %in% c("\ud655\uc778\ud568", "\ubcf4\ub958")) stop("\uac80\ud1a0\ud560 \ubcc0\uc218\ub97c \uc120\ud0dd\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
      if (!nzchar(trimws(input$review_reason))) stop("\ud655\uc778\ud558\uac70\ub098 \ubcf4\ub958\ud55c \uc774\uc720\ub97c \uc801\uc5b4 \uc8fc\uc138\uc694.", call. = FALSE)
      d <- decisions(); d <- d[d$variable != v, , drop = FALSE]
      decisions(rbind(d, data.frame(variable = v, decision = input$review_decision, reason = trimws(input$review_reason))))
      invalidate_review(); rule_feedback("\uac80\ud1a0 \uae30\ub85d\uc744 \uc800\uc7a5\ud588\uc2b5\ub2c8\ub2e4. \uacb0\uacfc\ub97c \ub2e4\uc2dc \uc2e4\ud589\ud558\uba74 \uae30\ub85d\uc774 \ud568\uaed8 \uc800\uc7a5\ub429\ub2c8\ub2e4.")
    }, error = function(e) rule_feedback(error_message(e)))
  })
  output$review_overview <- shiny::renderTable({
    x <- review_table()[c("variable", "label", "status", "findings", "decision", "reason")]
    names(x) <- c("\ubcc0\uc218", "\uc124\uba85", "\uc810\uac80", "\ud655\uc778\uc0ac\ud56d", "\uc5f0\uad6c\uc790 \ud310\ub2e8", "\uc774\uc720"); x
  }, rownames = FALSE)
  output$base_status <- shiny::renderText({
    x <- current_base()
    if (is.null(x)) return("\ucf54\ub4dc\ubd81 \uae30\uc900 \uacb0\uacfc\ub97c \uba3c\uc800 \ub9cc\ub4e4\uc5b4 \uc8fc\uc138\uc694.")
    sprintf("\ucf54\ub4dc\ubd81 \uae30\uc900 \uacb0\uacfc: %d\ud589, %d\uc5f4. %d\uac1c \uc791\uc5c5\uc744 \uc801\uc6a9\ud588\uc2b5\ub2c8\ub2e4.", nrow(x$applied$data), ncol(x$applied$data), nrow(x$approved))
  })
  output$result_sample <- shiny::renderTable({shiny::req(current_result(), input$result_columns); sample_values(current_result()$applied$data, head(input$result_columns, 10L))}, rownames = TRUE)
  output$result_review <- shiny::renderText({
    x <- current_result(); shiny::req(x)
    t <- x$review$comparison
    sprintf("\uae30\ubcf8 \ucc98\ub9ac \ud6c4 \ub2e4\uc2dc \uc810\uac80\ud55c \ud655\uc778\uc0ac\ud56d\uc740 %d\uac1c \ubcc0\uc218\uc785\ub2c8\ub2e4. \uc6d0\uc790\ub8cc \uac80\ud1a0 \uae30\ub85d\uc5d0\ub294 \ubbf8\uac80\ud1a0 %d\uac1c \ubcc0\uc218, \ubcf4\ub958 %d\uac1c \ubcc0\uc218\uac00 \ub0a8\uc544 \uc788\uc2b5\ub2c8\ub2e4. \uc791\uc5c5 \uc2e4\ud589\uc774 \uc804\uccb4 \uac80\ud1a0 \uc644\ub8cc\ub97c \ub73b\ud558\uc9c0\ub294 \uc54a\uc2b5\ub2c8\ub2e4. \ubd84\uae30\uc640 \ubcf5\uc7a1\ud55c \uc810\uc218 \uc9c0\uce68\uc740 \uc6d0\ubb38\uc5d0\uc11c \ubcc4\ub3c4\ub85c \ud655\uc778\ud558\uc138\uc694.", sum(x$review$after_comparison$status == "\ud655\uc778 \ud544\uc694"), sum(t$decision == "\ubbf8\uac80\ud1a0"), sum(t$decision == "\ubcf4\ub958"))
  })
  output$post_review <- shiny::renderTable({
    x <- current_result(); shiny::req(x)
    t <- x$review$after_comparison[c("variable", "n_missing", "undefined_codes", "out_of_range", "findings", "decision")]
    names(t) <- c("\ubcc0\uc218", "\uacb0\uce21 \uc218", "\ubaa9\ub85d\uc5d0 \uc5c6\ub294 \uac12", "\ubc94\uc704 \ubc16 \uac12", "\ud655\uc778\uc0ac\ud56d", "\uc5f0\uad6c\uc790 \ud310\ub2e8"); t
  }, rownames = FALSE)
  output$base_controls <- shiny::renderUI({
    if (is.null(current_base())) return(NULL)
    shiny::tagList(if (workspace_mode() == "additional" || is.null(current_result())) shiny::downloadButton("download_base", "\uae30\ubcf8 \uacb0\uacfc ZIP"), shiny::actionButton("start_additional", "\ucd94\uac00 \uc804\ucc98\ub9ac"), shiny::actionButton("edit_basic", "\ucf54\ub4dc\ubd81 \uac80\ud1a0\ub85c \ub3cc\uc544\uac00\uae30"))
  })
  output$download_base <- shiny::downloadHandler(
    filename = function() sprintf("codebookr-basic-%s.zip", format(Sys.Date(), "%Y%m%d")),
    content = function(file) {
      x <- current_base(); shiny::req(x, dataset())
      export_bundle(file, x$applied, x$approved, metadata = variable_metadata(dataset()), source_name = data_name(), import_options = x$import_options, review = x$review)
    }, contentType = "application/zip")
  shiny::observeEvent(input$start_additional, {
    if (is.null(current_base())) {execution_feedback("\ucf54\ub4dc\ubd81 \uae30\uc900 \ucc98\ub9ac\ub97c \uba3c\uc800 \uc2e4\ud589\ud574 \uc8fc\uc138\uc694."); return()}
    workspace_mode("additional"); set_rules(rules()); shiny::updateTabsetPanel(session, "main_tabs", selected = "codebook")
  })
  shiny::observeEvent(input$edit_basic, {
    saved <- current_base(); workspace_mode("codebook"); set_rules(rules())
    if (!is.null(saved)) shiny::updateCheckboxGroupInput(session, "approved_rules", selected = saved$approved_keys)
    shiny::updateTabsetPanel(session, "main_tabs", selected = "codebook")
  })
  shiny::observeEvent(input$run_unchanged, {
    shiny::req(dataset(), codebooks())
    if (workspace_mode() != "codebook" || length(approved_keys())) {execution_feedback("\ubcc0\uacbd \uc5c6\uc774 \uacb0\uacfc\ub97c \ud655\uc778\ud558\ub824\uba74 \uc791\uc5c5 \uc120\ud0dd\uc744 \ubaa8\ub450 \ud574\uc81c\ud574 \uc8fc\uc138\uc694."); return()}
    approved <- normalize_rules(empty_rules())
    x <- list(applied = apply_approved_rules(dataset(), approved), approved = approved,
      source_revision = source_revision(), rule_keys = rule_keys(), approved_keys = character(),
      import_options = loaded_import_options(), review_revision = review_revision(), review = review_context(dataset(), codebooks(), mapping(), decisions(), approved))
    result(x); base_result(x); execution_feedback(""); shiny::updateTabsetPanel(session, "main_tabs", selected = "execute")
  })
  shiny::observeEvent(input$undo_rule, {
    x <- undo_state()
    if (is.null(x)) {rule_feedback("\ub418\ub3cc\ub9b4 \uc791\uc5c5\uc774 \uc5c6\uc2b5\ub2c8\ub2e4."); return()}
    additional_ids(x$additional_ids); set_rules(x$rules, reset_approval = TRUE); undo_state(NULL)
    rule_feedback("\uc9c1\uc804 \ubaa9\ub85d \ubcc0\uacbd\uc744 \ub418\ub3cc\ub838\uc2b5\ub2c8\ub2e4. \uc801\uc6a9\ud560 \uc791\uc5c5\uc744 \ub2e4\uc2dc \uc120\ud0dd\ud574 \uc8fc\uc138\uc694.")
  })
  shiny::observeEvent(input$load_example, {
    reset_sources()
    files <- function(name) data.frame(name = name, datapath = example_path(name), stringsAsFactors = FALSE)
    tryCatch({
      df <- files("synthetic_survey.csv"); bf <- files("synthetic_codebook.xlsx")
      d <- read_dataset(df$datapath, df$name); cb <- read_codebooks(bf)
      dataset(d); codebooks(cb); input_data_file(df); input_book_files(bf)
      loaded_import_options(normalize_import_options()); data_name(df$name)
      rebuild_rules(); data_feedback("\uac00\uc0c1 \uc6d0\uc790\ub8cc\uc640 \ucf54\ub4dc\ubd81\uc744 \ud568\uaed8 \ubd88\ub7ec\uc654\uc2b5\ub2c8\ub2e4."); book_feedback("\uc608\uc81c \ucf54\ub4dc\ubd81\uc744 \uc77d\uc5c8\uc2b5\ub2c8\ub2e4.")
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
      if (is.null(dataset()) || is.null(codebooks())) stop("\uc6d0\uc790\ub8cc\uc640 \ucf54\ub4dc\ubd81\uc744 \uba3c\uc800 \uc5f0\uacb0\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
      x <- read_project(input$open_project$datapath[[1]], source_manifest(input_data_file(), input_book_files()), loaded_import_options())
      cb <- codebooks(); records <- normalize_codebook_records(cb$records); cb$records <- records[records$origin != "AI", , drop = FALSE]
      if (!is.null(x$ai_records) && nrow(x$ai_records)) {
        accepted <- validate_ai_definitions(x$ai_records, cb)
        if (nrow(accepted) != nrow(x$ai_records)) stop("\uc800\uc7a5\ub41c AI \uc815\uc758\uc758 \uc6d0\ubb38 \uadfc\uac70\ub97c \ud655\uc778\ud558\uc9c0 \ubabb\ud588\uc2b5\ub2c8\ub2e4.", call. = FALSE)
        cb$records <- unique(rbind(cb$records, accepted[names(cb$records)]))
      }
      m <- x$mapping
      if (is.null(m)) m <- data.frame(codebook_variable = character(), data_variable = character(), suggestion = character())
      if (is.null(m) || !all(c("codebook_variable", "data_variable", "suggestion") %in% names(m)) || anyNA(m) || anyDuplicated(m$codebook_variable) || any(!m$codebook_variable %in% cb$records$variable) || any(!m$data_variable %in% c("", names(dataset())))) stop("\ubcc0\uc218 \uc5f0\uacb0 \uc815\ubcf4\ub97c \ud655\uc778\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
      d <- x$decisions
      if (is.null(d)) d <- data.frame(variable = character(), decision = character(), reason = character())
      if (!all(c("variable", "decision", "reason") %in% names(d)) || anyNA(d) || anyDuplicated(d$variable) || any(!d$variable %in% names(dataset())) || any(!d$decision %in% c("\ud655\uc778\ud568", "\ubcf4\ub958")) || any(!nzchar(d$reason))) stop("\uac80\ud1a0 \uae30\ub85d\uc744 \ud655\uc778\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
      codebooks(cb); mapping(initial_mapping(dataset(), cb$records, m)); decisions(d)
      additional_ids(as.integer(x$additional_ids)); workspace_mode("codebook"); undo_state(NULL)
      invalidate_review(); set_rules(x$rules, reset_approval = TRUE); reset_consent()
      project_feedback("\uc791\uc5c5\uc744 \ubd88\ub7ec\uc654\uc2b5\ub2c8\ub2e4. \uac80\ud1a0\uc640 \ucd94\uac00 \uc791\uc5c5\uc740 \ubcf5\uc6d0\ud588\uc73c\uba70, \uc801\uc6a9\ud560 \uae30\ubcf8 \uc791\uc5c5\uc744 \ub2e4\uc2dc \uc120\ud0dd\ud558\uace0 \uc2e4\ud589\ud574\uc57c \ud569\ub2c8\ub2e4.")
    }, error = function(e) project_feedback(paste0("\uc791\uc5c5\uc744 \uc77d\uc9c0 \ubabb\ud588\uc2b5\ub2c8\ub2e4: ", error_message(e))))
  })
})
