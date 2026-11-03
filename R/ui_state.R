# Read-only presentation bindings. Never change approvals, source data or results here.
codebookr_ui_bindings <- quote({
  review_label_preview <- shiny::reactive({
    shiny::req(dataset(), input$review_variable)
    shiny::req(input$review_variable %in% names(dataset()))
    preview <- plan_preview()
    if (nzchar(preview$error)) return(list(table = NULL,
      note = "\uc791\uc5c5 \uc624\ub958\ub97c \uba3c\uc800 \ud574\uacb0\ud558\uc138\uc694. \ubcc0\uc218 \uc124\uba85\uacfc \uac12 \ub77c\ubca8\uc758 \ucd5c\uc885 \ubcc0\ud654\ub97c \uc544\uc9c1 \ud655\uc778\ud560 \uc218 \uc5c6\uc2b5\ub2c8\ub2e4."))
    preview_label_changes(dataset(), preview$data, input$review_variable)
  })
  output$review_metadata <- shiny::renderTable(review_label_preview()$table, rownames = FALSE)
  output$review_metadata_note <- shiny::renderText(review_label_preview()$note)
  ui_ready <- shiny::reactive(!is.null(dataset()) && !is.null(codebooks()))
  ui_flag <- function(value) if (isTRUE(value)) "yes" else "no"
  output$ui_has_data <- shiny::renderText(ui_flag(!is.null(dataset())))
  output$ui_has_books <- shiny::renderText(ui_flag(!is.null(codebooks())))
  output$ui_ready <- shiny::renderText(ui_flag(ui_ready()))
  output$ui_has_result <- shiny::renderText(ui_flag(!is.null(current_result())))
  output$ui_has_base <- shiny::renderText(ui_flag(!is.null(current_base())))
  output$ui_has_rules <- shiny::renderText(ui_flag(length(active_rule_ids()) > 0L))
  output$ui_has_selection <- shiny::renderText(ui_flag(length(approved_keys()) > 0L))
  for (id in c("ui_has_data", "ui_has_books", "ui_ready", "ui_has_result", "ui_has_base",
               "ui_has_rules", "ui_has_selection")) {
    shiny::outputOptions(output, id, suspendWhenHidden = FALSE)
  }
  output$ui_source_summary <- shiny::renderText({
    d <- dataset(); cb <- codebooks()
    source <- if (is.null(d)) "\uc6d0\uc790\ub8cc \ubbf8\uc5f0\uacb0" else
      sprintf("%s (%s\ud589, %s\uac1c \ubcc0\uc218)", data_name(), format(nrow(d), big.mark = ","), ncol(d))
    book <- if (is.null(cb)) "\ucf54\ub4dc\ubd81 \ubbf8\uc5f0\uacb0" else sprintf("\ucf54\ub4dc\ubd81 %d\uac1c \uc5f0\uacb0", nrow(input_book_files()))
    paste(source, book, sep = "  /  ")
  })
  output$ui_connection_hint <- shiny::renderText({
    if (is.null(dataset()) && is.null(codebooks())) "\uc6d0\uc790\ub8cc\uc640 \ucf54\ub4dc\ubd81\uc744 \uc120\ud0dd\ud558\uc138\uc694. \uc900\ube44\ub41c \ud30c\uc77c\uc774 \uc5c6\ub2e4\uba74 \uc608\uc81c\ub85c \uc2dc\uc791\ud560 \uc218 \uc788\uc2b5\ub2c8\ub2e4."
    else if (is.null(dataset())) "\ucf54\ub4dc\ubd81\uc774 \uc5f0\uacb0\ub418\uc5c8\uc2b5\ub2c8\ub2e4. \uc774\uc81c \uc6d0\uc790\ub8cc\ub97c \uc120\ud0dd\ud558\uc138\uc694."
    else if (is.null(codebooks())) "\uc6d0\uc790\ub8cc\uac00 \uc5f0\uacb0\ub418\uc5c8\uc2b5\ub2c8\ub2e4. \uc774\uc81c \ucf54\ub4dc\ubd81\uc744 \uc120\ud0dd\ud558\uc138\uc694."
    else "\uc790\ub8cc\uc640 \ucf54\ub4dc\ubd81\uc774 \uc900\ube44\ub418\uc5c8\uc2b5\ub2c8\ub2e4."
  })
  output$ui_selection_summary <- shiny::renderText({
    visible <- active_rule_ids()
    selected <- sum(selected_rules()$id %in% visible)
    if (workspace_mode() == "additional") {
      sprintf("\ucd94\uac00 \uc791\uc5c5 %d\uac1c \uc911 %d\uac1c \uc120\ud0dd, \uae30\ubcf8 \uc791\uc5c5 %d\uac1c \ud3ec\ud568",
        length(visible), selected, sum(!selected_rules()$id %in% visible))
    } else sprintf("%d\uac1c \uc791\uc5c5 \uc911 %d\uac1c \uc120\ud0dd", length(visible), selected)
  })
})
