# Shiny session state and handlers. Sourced separately for offline server tests.


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
  bin_rows_state <- shiny::reactiveVal(empty_bin_rows())
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
        "\uc804\uc1a1 \ub0b4\uc6a9\uc744 \uac80\ud1a0\ud588\uc73c\uba70 AI API \ud638\ucd9c\uc5d0 \ub3d9\uc758\ud569\ub2c8\ub2e4."),
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
    shiny::updateSelectInput(session, "edit_rule", choices = c("\uc0c8 \uc791\uc5c5" = "new", choices[visible]),
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
    if (is.null(dataset())) return(list(data = NULL, error = "\uc790\ub8cc\ub97c \uba3c\uc800 \uc62c\ub824 \uc8fc\uc138\uc694."))
    index <- match(input$edit_rule, rule_keys())
    before <- if (length(index) == 1L && !is.na(index)) seq_len(index - 1L) else seq_len(nrow(rules()))
    selected <- before[rule_keys()[before] %in% approved_keys()]
    p <- preview_rules(dataset(), rules()[selected, , drop = FALSE])
    list(data = p$data, error = p$error)
  })
  output$editor_context_status <- shiny::renderText({
    context <- editor_context()
    if (nzchar(context$error)) return(paste("\uc55e\uc120 \uc791\uc5c5\uc744 \ud655\uc778\ud574 \uc8fc\uc138\uc694:", context$error))
    "\uc120\ud0dd\ud55c \uc55e\uc120 \uc791\uc5c5\uc758 \uacb0\uacfc\ub97c \uc0ac\uc6a9\ud569\ub2c8\ub2e4. \uc0c8 \ubcc0\uc218\uac00 \ubcf4\uc774\uc9c0 \uc54a\uc73c\uba74 \uadf8 \ubcc0\uc218\ub97c \ub9cc\ub4dc\ub294 \uc791\uc5c5\uc744 \uba3c\uc800 \uc120\ud0dd\ud558\uc138\uc694."
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
      data_feedback("\uc790\ub8cc\ub97c \uc77d\uc5c8\uc2b5\ub2c8\ub2e4. \ubcc0\uc218 \uc774\ub984, \uc790\ub8cc\ud615\uacfc \uacb0\uce21 \ucf54\ub4dc\ub97c \ud655\uc778\ud558\uc138\uc694.")
      shiny::showNotification(sprintf("\uc6d0\uc790\ub8cc: %s\ud589 \u00d7 %s\uc5f4", nrow(d), ncol(d)), type = "message")
    }, error = function(e) {
      dataset(NULL)
      data_feedback(paste0("\uc790\ub8cc \uc77d\uae30 \uc2e4\ud328: ", error_message(e)))
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
        "\ubcc0\uc218 \ud45c\ub97c \uc790\ub3d9\uc73c\ub85c \ucc3e\uc9c0 \ubabb\ud588\uc2b5\ub2c8\ub2e4. \ubc1c\ucdcc\ubb38\uc744 \ud655\uc778\ud558\uace0 AI\ub85c \uc815\uc758\ub97c \uc77d\uac70\ub098 \uc9c1\uc811 \uc791\uc5c5\uc744 \ucd94\uac00\ud574 \uc8fc\uc138\uc694." else
        "\uc77d\uc9c0 \ubabb\ud55c \ubd80\ubd84\uc774 \uc788\ub294\uc9c0 \ud655\uc778\ud55c \ub4a4 \ucf54\ub4dc\ubd81 \uac80\ud1a0\ub85c \ub118\uc5b4\uac00\uc138\uc694.")
      shiny::showNotification(sprintf("\ucf54\ub4dc\ubd81 %d\uac1c \ud30c\uc77c\uc744 \uc77d\uc5c8\uc2b5\ub2c8\ub2e4.",
                               nrow(input$book_files)), type = "message")
    }, error = function(e) {
      codebooks(NULL)
      book_feedback(paste0("\ucf54\ub4dc\ubd81 \uc77d\uae30 \uc2e4\ud328: ", error_message(e)))
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
    rule_feedback("\uc0c8 \ucf54\ub4dc\ubd81 \ud6c4\ubcf4\ub97c \ucd94\uac00\ud588\uc2b5\ub2c8\ub2e4. \uae30\uc874 \uc791\uc5c5\uacfc \ud310\ub2e8\uc740 \uc720\uc9c0\ud569\ub2c8\ub2e4.")
  }, priority = 75)

  shiny::observeEvent(input$edit_rule, {
    key <- input$edit_rule
    index <- match(key, rule_keys())
    if (identical(key, "new")) {
      bin_rows_state(empty_bin_rows())
      shiny::updateSelectInput(session, "bin_outside", selected = "error")
      shiny::updateCheckboxInput(session, "bin_ordered", value = FALSE)
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
        if (row$operation == "binning") {
          bin_rows_state(p$bins)
          shiny::updateCheckboxInput(session, "bin_ordered", value = p$ordered)
        }
        values <- switch(row$operation,
          binning = list(bin_variable = p$input, bin_outside = p$outside),
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
      if (op == "binning" && workspace_mode() != "additional") stop("\uac12 \uad6c\uac04\uc73c\ub85c \ubc94\uc8fc\ud615 \ub9cc\ub4e4\uae30\ub294 \uacb0\uacfc \ud654\uba74\uc758 \ucd94\uac00 \uc804\ucc98\ub9ac\uc5d0\uc11c \uc0ac\uc6a9\ud560 \uc218 \uc788\uc2b5\ub2c8\ub2e4.", call. = FALSE)
      if (workspace_mode() == "additional" && is.null(current_base())) stop("\ucf54\ub4dc\ubd81 \uae30\uc900 \uacb0\uacfc\ub97c \uba3c\uc800 \ub2e4\uc2dc \ud655\uc778\ud558\uace0 \uc2e4\ud589\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
      if (op == "labels") stop("\ub77c\ubca8 \uc815\uc758\ub294 \ubcc0\uc218 \uc5f0\uacb0\uacfc \ucf54\ub4dc\ubd81 \uc6d0\ubb38\uc5d0\uc11c \ud655\uc778\ud574 \uc8fc\uc138\uc694. \uc81c\uc548\uc744 \uc81c\uc678\ud558\uac70\ub098 \ucf54\ub4dc\ubd81\uc744 \uace0\uce5c \ub4a4 \ub2e4\uc2dc \uc77d\uc744 \uc218 \uc788\uc2b5\ub2c8\ub2e4.", call. = FALSE)
      candidate <- if (op %in% research_operations) research_rule_from_inputs(input, bin_rows_state()) else data.frame(
        id = 1L, variable = input$manual_var, operation = op,
        values = if (op == "set_na") trimws(input$manual_codes) else "",
        minimum = if (op == "reverse") input$manual_min else NA_real_,
        maximum = if (op == "reverse") input$manual_max else NA_real_,
        reason = input$manual_reason, evidence = "\uc5f0\uad6c\uc790 \uc9c1\uc811 \uc9c0\uc815",
        confidence = "manual", source = "manual", stringsAsFactors = FALSE
      )
      if (!nzchar(trimws(candidate$reason[[1]]))) stop("\ubcc0\uacbd \uadfc\uac70\ub97c \uc785\ub825\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
      context <- editor_context()
      if (op == "factor") candidate <- with_codebook_factor_labels(candidate, dataset(), codebooks(), mapping())
      if (nzchar(context$error)) stop(paste("\uc55e\uc120 \uc791\uc5c5\uc744 \uba3c\uc800 \uc218\uc815\ud574 \uc8fc\uc138\uc694:", context$error), call. = FALSE)
      check_rule(candidate, context$data)
      editor <- input$edit_rule
      if (length(editor) == 1L && !identical(editor, "new")) {
        index <- match(editor, rule_keys())
        if (is.na(index) || !rules()$id[[index]] %in% active_rule_ids()) stop("\ubaa9\ub85d\uc5d0\uc11c \uae30\uc874 \uaddc\uce59\uc744 \ub2e4\uc2dc \uc120\ud0dd\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
        push_undo()
        updated <- edit_rule_candidate(rules(), match(editor, rule_keys()), candidate, context$data)
        set_rules(updated)
        rule_feedback("\uc791\uc5c5\uc744 \uc218\uc815\ud588\uc2b5\ub2c8\ub2e4. \uc774 \uc791\uc5c5\uacfc \uc601\ud5a5\uc744 \ubc1b\ub294 \uc791\uc5c5\uc744 \ub2e4\uc2dc \uc120\ud0dd\ud558\uace0 \uacb0\uacfc\ub97c \ud655\uc778\ud558\uc138\uc694.")
        return(invisible(NULL))
      }
      updated <- append_rule_candidates(rules(), candidate, context$data)
      if (nrow(updated) == nrow(rules())) {
        rule_feedback("\ub3d9\uc77c\ud55c \uaddc\uce59\uc774 \uc774\ubbf8 \uc874\uc7ac\ud569\ub2c8\ub2e4.")
        shiny::showNotification("\ub3d9\uc77c\ud55c \uaddc\uce59\uc774 \uc774\ubbf8 \uc874\uc7ac\ud569\ub2c8\ub2e4.", type = "warning")
      } else {
        push_undo()
        if (workspace_mode() == "additional") additional_ids(union(additional_ids(), setdiff(updated$id, rules()$id)))
        set_rules(updated)
        rule_feedback("\uc791\uc5c5\uc744 \ucd94\uac00\ud588\uc2b5\ub2c8\ub2e4. \ubaa9\ub85d\uc5d0\uc11c \uc120\ud0dd\ud558\uba74 \uacb0\uacfc\ub97c \ubbf8\ub9ac \ubcfc \uc218 \uc788\uc2b5\ub2c8\ub2e4.")
        shiny::showNotification("\uc791\uc5c5\uc744 \ucd94\uac00\ud588\uc2b5\ub2c8\ub2e4.")
      }
    }, error = function(e) {
      rule_feedback(paste0("\uc791\uc5c5\uc744 \uc800\uc7a5\ud558\uc9c0 \ubabb\ud588\uc2b5\ub2c8\ub2e4: ", error_message(e)))
      warn(e)
    })
  })

  selected_editor_index <- function() {
    index <- match(input$edit_rule, rule_keys())
    if (length(index) != 1L || is.na(index)) stop("\ubaa9\ub85d\uc5d0\uc11c \uae30\uc874 \uaddc\uce59\uc744 \ub2e4\uc2dc \uc120\ud0dd\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
    if (!rules()$id[[index]] %in% active_rule_ids()) stop("\ud604\uc7ac \ub2e8\uacc4\uc758 \uc791\uc5c5\uc744 \uc120\ud0dd\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
    index
  }
  shiny::observeEvent(input$delete_rule, {
    tryCatch({
      index <- selected_editor_index()
      push_undo()
      set_rules(rules()[-index, , drop = FALSE])
      rule_feedback("\uc791\uc5c5\uc744 \uc0ad\uc81c\ud588\uc2b5\ub2c8\ub2e4. \uc6d0\uc790\ub8cc\ub294 \uadf8\ub300\ub85c\uc785\ub2c8\ub2e4.")
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
      rule_feedback("\uc21c\uc11c\ub97c \ubc14\uafe8\uc2b5\ub2c8\ub2e4. \uc791\uc5c5\uc744 \ub2e4\uc2dc \uc120\ud0dd\ud558\uace0 \uacb0\uacfc\ub97c \ud655\uc778\ud558\uc138\uc694.")
    }, error = function(e) rule_feedback(error_message(e)))
  }
  shiny::observeEvent(input$move_rule_up, move_rule(-1L))
  shiny::observeEvent(input$move_rule_down, move_rule(1L))

  shiny::observeEvent(input$ai_button, {
    shiny::req(dataset(), codebooks())
    if (!identical(as.character(input$consent), consent_key())) {
      ai_status("\ucf54\ub4dc\ubd81 \uc804\uc1a1 \ub0b4\uc6a9\uc744 \ud655\uc778\ud558\uace0 \ub3d9\uc758\uc5d0 \uccb4\ud06c\ud574 \uc8fc\uc138\uc694.")
      shiny::showNotification("\ucf54\ub4dc\ubd81 \uc804\uc1a1 \ub0b4\uc6a9\uc744 \ud655\uc778\ud558\uace0 \ub3d9\uc758\uc5d0 \uccb4\ud06c\ud574 \uc8fc\uc138\uc694.",
                       type = "warning", duration = 8)
      return(invisible(NULL))
    }
    if (!nzchar(trimws(input$api_key))) {
      ai_status("\ubcf8\uc778\uc758 API \ud0a4\ub97c \uc785\ub825\ud574 \uc8fc\uc138\uc694.")
      shiny::showNotification("\ubcf8\uc778\uc758 API \ud0a4\ub97c \uc785\ub825\ud574 \uc8fc\uc138\uc694.", type = "warning")
      return(invisible(NULL))
    }
    tryCatch({
      ai_status("AI\uac00 \ucf54\ub4dc\ubd81\uc744 \ud574\uc11d\ud558\uace0 \uc788\uc2b5\ub2c8\ub2e4...")
      shiny::withProgress(message = "AI\uac00 \ucf54\ub4dc\ubd81\uc744 \ud574\uc11d\ud558\uace0 \uc788\uc2b5\ub2c8\ub2e4...", value = 0, {
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
        proposed <- with_codebook_factor_labels(proposed, dataset(), codebooks(), mapping())
        updated <- append_rule_candidates(updated, proposed, dataset())
        n_added <- nrow(updated) - nrow(rules())
        if (n_added > 0L) set_rules(updated)
        ai_status(sprintf("AI \uc751\ub2f5 \uc644\ub8cc: \ud6c4\ubcf4 %d\uac1c \uc911 \uc0c8 \uaddc\uce59 %d\uac1c \ucd94\uac00. \uae30\uc874\uacfc \uc911\ubcf5\ub418\uac70\ub098 \uc720\ud6a8\ud558\uc9c0 \uc54a\uc740 \ud6c4\ubcf4\ub294 \ucd94\uac00\ud558\uc9c0 \uc54a\uc2b5\ub2c8\ub2e4. \uc0c8 \uaddc\uce59\uc740 \uc2b9\uc778\uc774 \ud544\uc694\ud569\ub2c8\ub2e4.",
                          nrow(proposed), n_added))
        if (!is.null(extracted)) ai_status(paste(ai_status(), sprintf("\uc6d0\ubb38 \ud655\uc778\ub41c \uc815\uc758 %d\uac1c, \uc6d0\ubb38\uc744 \ud655\uc778\ud558\uc9c0 \ubabb\ud574 \uc81c\uc678\ud55c \uc815\uc758 %d\uac1c.", nrow(extracted), if (is.null(rejected)) 0L else rejected)))
        shiny::showNotification(ai_status(), duration = 10)
      })
    }, error = function(e) {
      message <- error_message(e, input$api_key)
      help <- if (grepl("401|authentication|invalid.*key", message, ignore.case = TRUE)) " API \ud0a4\uc640 \uc81c\uacf5\uc5c5\uccb4\ub97c \ud655\uc778\ud55c \ub4a4 \ub2e4\uc2dc \uc694\uccad\ud574 \uc8fc\uc138\uc694." else
        if (grepl("429|rate.limit", message, ignore.case = TRUE)) " \ud638\ucd9c \ud55c\ub3c4\uc640 \uc794\uc561\uc744 \ud655\uc778\ud558\uace0 \uc7a0\uc2dc \ud6c4 \uc7ac\uc2dc\ub3c4\ud574 \uc8fc\uc138\uc694." else
        if (grepl("404|model.*not", message, ignore.case = TRUE)) " \uc81c\uacf5\uc5c5\uccb4\uc5d0\uc11c \uc774\uc6a9 \uac00\ub2a5\ud55c \ubaa8\ub378 ID\ub97c \ud655\uc778\ud574 \uc8fc\uc138\uc694." else
        " \uc785\ub825 \ub0b4\uc6a9\uacfc \uc5f0\uacb0 \uc0c1\ud0dc\ub97c \ud655\uc778\ud55c \ub4a4 \ub2e4\uc2dc \uc694\uccad\ud560 \uc218 \uc788\uc2b5\ub2c8\ub2e4."
      ai_status(paste0("AI \uc694\uccad \uc2e4\ud328: ", message, help))
      warn(e, input$api_key)
    })
  })

  output$ai_status <- shiny::renderText(ai_status())
  output$data_feedback <- shiny::renderText(data_feedback())
  output$book_feedback <- shiny::renderText(book_feedback())
  output$rule_feedback <- shiny::renderText(rule_feedback())
  preview_description <- shiny::reactive({
    count <- nrow(selected_rules())
    if (!count) return("\uc2e4\ud589\ud560 \uc791\uc5c5\uc744 \uc120\ud0dd\ud558\uba74 \uc608\uc0c1 \uacb0\uacfc\uac00 \ub098\ud0c0\ub0a9\ub2c8\ub2e4.")
    p <- plan_preview()
    if (!p$ok) return(paste0("\uba3c\uc800 \uc774 \ubd80\ubd84\uc744 \uc218\uc815\ud574 \uc8fc\uc138\uc694. ", p$error))
    sprintf("\uc120\ud0dd\ud55c %d\uac1c \uc791\uc5c5\uc758 \uc608\uc0c1 \uacb0\uacfc\uc785\ub2c8\ub2e4. \ubb38\uc81c\uac00 \uc5c6\uc73c\uba74 \uc544\ub798\uc5d0\uc11c \uc801\uc6a9\ud558\uc138\uc694.", count)
  })
  output$preview_status <- shiny::renderText(preview_description())
  output$execution_plan_summary <- shiny::renderText(sub(
    "\ubb38\uc81c\uac00 \uc5c6\uc73c\uba74 \uc544\ub798\uc5d0\uc11c \uc801\uc6a9\ud558\uc138\uc694.",
    "\uc544\ub798 \ubc84\ud2bc\uc744 \ub204\ub974\uba74 \uc120\ud0dd\ud55c \uc791\uc5c5\uc744 \uc2b9\uc778\ud558\uace0 \uc2e4\ud589\ud569\ub2c8\ub2e4.",
    preview_description(), fixed = TRUE))
  shiny::observeEvent(input$go_codebook, {
    if (is.null(dataset()) || is.null(codebooks())) { mapping_feedback("\uc6d0\uc790\ub8cc\uc640 \ucf54\ub4dc\ubd81\uc744 \ubaa8\ub450 \uc5f0\uacb0\ud574 \uc8fc\uc138\uc694."); return() }
    workspace_mode("codebook"); set_rules(rules())
    shiny::updateTabsetPanel(session, "main_tabs", selected = "codebook")
  })
  shiny::observeEvent(input$go_plan, shiny::updateTabsetPanel(session, "main_tabs", selected = "codebook"))
  shiny::observeEvent(input$go_execute, shiny::updateTabsetPanel(session, "main_tabs", selected = "execute"))
  output$preview_table <- shiny::renderTable(preview_table_display(plan_preview()),
                                             striped = TRUE, rownames = FALSE)

  output$dataset_status <- shiny::renderText({
    d <- dataset()
    if (is.null(d)) return("\uc6d0\uc790\ub8cc\ub97c \uc62c\ub824 \uc8fc\uc138\uc694.")
    sprintf("%s (%s\ud589, \ubcc0\uc218 %s\uac1c)", data_name(), nrow(d), ncol(d))
  })
  output$metadata_table <- shiny::renderTable({
    shiny::req(dataset())
    head(metadata_display(dataset()), 120L)
  }, striped = TRUE, hover = TRUE, rownames = FALSE)

  output$book_summary <- shiny::renderTable({
    shiny::req(codebooks())
    x <- codebooks()$summary
    x$recognized <- ifelse(x$recognized, "\uc778\uc2dd\ub428", "\uc9c1\uc811 \ud655\uc778 \ud544\uc694")
    names(x) <- c("\ud30c\uc77c", "\uc2dc\ud2b8 \ub610\ub294 \uad6c\uac04", "\ud45c \uc778\uc2dd", "\ubcc0\uc218 \uc218", "\ud655\uc778\uc0ac\ud56d")
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
    names(x) <- c("\ubcc0\uc218", "\uc124\uba85", "\ucf54\ub4dc", "\uac12 \uc124\uba85", "\ube44\uace0", "\ud30c\uc77c", "\ubb38\uc11c \uc704\uce58")
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
      if (!is.na(i)) {
        target <- rules()$variable[[i]]
        if (rules()$operation[[i]] == "binning") target <- rule_parameters(rules()[i, , drop = FALSE])$input
        if (target %in% names(dataset())) shiny::updateSelectInput(session, "review_variable", selected = target)
      }
    }
    result(NULL) # Prevent stale output after selection changes.
    execution_feedback("")
  }, ignoreNULL = FALSE, priority = 50)

  shiny::observeEvent(input$run_button, {
    shiny::req(dataset())
    tryCatch({
      if (is.null(codebooks())) stop("\ucf54\ub4dc\ubd81\uc744 \uba3c\uc800 \uc5f0\uacb0\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
      if (workspace_mode() == "additional" && is.null(current_base())) stop("\ucf54\ub4dc\ubd81 \uae30\uc900 \uacb0\uacfc\uac00 \ubc14\ub00c\uc5c8\uc2b5\ub2c8\ub2e4. \uae30\ubcf8 \ucc98\ub9ac\ub97c \uba3c\uc800 \ub2e4\uc2dc \uc2e4\ud589\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
      approved <- selected_rules()
      if (!nrow(approved)) stop("\uc2e4\ud589\ud560 \uc791\uc5c5\uc744 \ud558\ub098 \uc774\uc0c1 \uc120\ud0dd\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
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
      shiny::showNotification("\uc2b9\uc778\ud55c \uc804\ucc98\ub9ac \uaddc\uce59\uc774 \uc801\uc6a9\ub418\uc5c8\uc2b5\ub2c8\ub2e4.", type = "message")
    }, error = function(e) {
      result(NULL)
      execution_feedback(paste0("\uc2e4\ud589\uc744 \uc911\ub2e8\ud588\uc2b5\ub2c8\ub2e4: ", error_message(e)))
      warn(e)
    })
  })

  output$run_status <- shiny::renderText({
    x <- current_result()
    if (is.null(x)) return(if (nzchar(execution_feedback())) execution_feedback() else "\uc2e4\ud589 \uacb0\uacfc\uac00 \uc5c6\uc2b5\ub2c8\ub2e4. \uc791\uc5c5\uc744 \uc120\ud0dd\ud558\uace0 \ubbf8\ub9ac\ubcf4\uae30\ub97c \ud655\uc778\ud55c \ub4a4 \uc2e4\ud589\ud574 \uc8fc\uc138\uc694.")
    sprintf("%d\uac1c \uc791\uc5c5\uc744 \uc2e4\ud589\ud588\uc2b5\ub2c8\ub2e4. \uacb0\uacfc\ub294 %d\ud589, %d\uc5f4\uc785\ub2c8\ub2e4.",
            nrow(x$approved), nrow(x$applied$data), ncol(x$applied$data))
  })
  output$audit_table <- shiny::renderTable({
    shiny::req(current_result())
    x <- current_result()$applied$audit
    x <- x[c("id", "variable", "operation", "n_affected", "missing_before", "missing_after", "rows_before", "rows_after", "note")]
    x$operation <- operation_label(x$operation)
    names(x) <- c("\ubc88\ud638", "\ubcc0\uc218", "\uc791\uc5c5", "\uc801\uc6a9 \uc218", "\uacb0\uce21 \uc218 (\uc804)", "\uacb0\uce21 \uc218 (\ud6c4)", "\ud589 \uc218 (\uc804)", "\ud589 \uc218 (\ud6c4)", "\uacc4\uc0b0 \uae30\uc900")
    x
  }, striped = TRUE, rownames = FALSE)
  output$after_metadata <- shiny::renderTable({
    shiny::req(current_result())
    head(metadata_display(current_result()$applied$data), 120L)
  }, striped = TRUE, rownames = FALSE)
  output$download_controls <- shiny::renderUI({
    if (is.null(current_result())) {
      return(shiny::tags$p(class = "hint",
        "\uaddc\uce59\uc744 \uc2b9\uc778\ud558\uace0 \uc2e4\ud589\ud558\uba74 ZIP\uc744 \ub2e4\uc6b4\ub85c\ub4dc\ud560 \uc218 \uc788\uc2b5\ub2c8\ub2e4."))
    }
    shiny::downloadButton("download_bundle", if (workspace_mode() == "additional") "\ucd94\uac00 \uc791\uc5c5 \ud3ec\ud568 ZIP" else "\uacb0\uacfc \ub2e4\uc6b4\ub85c\ub4dc (ZIP)")
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
  eval(codebookr_review_bindings, envir = environment())
  eval(codebookr_binning_bindings, envir = environment())
  eval(codebookr_ui_bindings, envir = environment())
}

