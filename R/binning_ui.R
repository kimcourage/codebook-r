empty_bin_rows <- function(n = 3L) data.frame(lower = rep("", n), upper = rep("", n),
  lower_closed = rep(TRUE, n), upper_closed = rep(FALSE, n), label = rep("", n), stringsAsFactors = FALSE)

bin_rows_from_inputs <- function(input, fallback) {
  rows <- fallback
  for (i in seq_len(nrow(rows))) for (nm in names(rows)) {
    value <- input[[paste0("bin_", nm, "_", i)]]
    if (!is.null(value)) rows[[nm]][i] <- value
  }
  rows
}

bin_parameters_from_inputs <- function(input, rows) list(input = input$bin_variable,
  output = trimws(input$new_variable), bins = bin_rows_from_inputs(input, rows),
  outside = input$bin_outside, ordered = isTRUE(input$bin_ordered))

ui_binning_controls <- function() tagList(
  selectInput("bin_variable", "\uad6c\uac04\uc73c\ub85c \ub098\ub20c \uc218\uce58\ud615 \ubcc0\uc218", choices = character()),
  p(class = "hint", "\uac01 \uad6c\uac04\uc758 \ubc94\uc8fc\uba85\uacfc \ubc94\uc704\ub97c \uc785\ub825\ud558\uc138\uc694. \uc2dc\uc791\uac12 \ub610\ub294 \ub05d\uac12\uc744 \ube44\uc6b0\uba74 \ud574\ub2f9 \ubc29\ud5a5\uc5d0 \uc81c\ud55c\uc744 \ub450\uc9c0 \uc54a\uc2b5\ub2c8\ub2e4."),
  uiOutput("bin_rows"),
  div(class = "editor-actions", actionButton("bin_add", "\uad6c\uac04 \ucd94\uac00"), actionButton("bin_remove", "\ub9c8\uc9c0\ub9c9 \uad6c\uac04 \uc0ad\uc81c")),
  checkboxInput("bin_ordered", "\uc704\uc5d0\uc11c \uc544\ub798 \uc21c\uc11c\uac00 \uc788\ub294 \ubc94\uc8fc\ud615\uc73c\ub85c \ub9cc\ub4e4\uae30", FALSE),
  selectInput("bin_outside", "\uc5b4\ub290 \uad6c\uac04\uc5d0\ub3c4 \uc18d\ud558\uc9c0 \uc54a\ub294 \uac12", c("\uc801\uc6a9\uc744 \uc911\ub2e8\ud558\uace0 \ud655\uc778" = "error", "\uc0c8 \ubcc0\uc218\uc5d0\uc11c \uacb0\uce21\uac12\uc73c\ub85c \ucc98\ub9ac" = "na")),
  p(class = "hint", "\uc6d0\ub798 \uacb0\uce21\uac12\uc740 \uc0c8 \ubcc0\uc218\uc5d0\uc11c\ub3c4 \uacb0\uce21\uc73c\ub85c \uc720\uc9c0\ud569\ub2c8\ub2e4. \ubc94\uc8fc\uba85\uc740 \uc911\ubcf5\ud560 \uc218 \uc5c6\uc2b5\ub2c8\ub2e4."),
  h4("\uad6c\uac04\ubcc4 \uc608\uc0c1 \uac1c\uc218"), tableOutput("bin_preview"),
  p(class = "hint", role = "status", textOutput("bin_preview_note"))
)

codebookr_binning_bindings <- quote({
  output$bin_rows <- shiny::renderUI({
    rows <- bin_rows_state()
    tagList(lapply(seq_len(nrow(rows)), function(i) {
      id <- function(nm) paste0("bin_", nm, "_", i)
      tags$fieldset(class = "bin-range", tags$legend(paste("\uad6c\uac04", i)),
        textInput(id("label"), paste("\uad6c\uac04", i, "\ubc94\uc8fc\uba85"), rows$label[i]),
        fluidRow(column(6, textInput(id("lower"), paste("\uad6c\uac04", i, "\uc2dc\uc791\uac12"), rows$lower[i], placeholder = "\uc81c\ud55c \uc5c6\uc74c"),
          checkboxInput(id("lower_closed"), paste("\uad6c\uac04", i, "\uc2dc\uc791\uac12 \ud3ec\ud568"), rows$lower_closed[i])),
          column(6, textInput(id("upper"), paste("\uad6c\uac04", i, "\ub05d\uac12"), rows$upper[i], placeholder = "\uc81c\ud55c \uc5c6\uc74c"),
          checkboxInput(id("upper_closed"), paste("\uad6c\uac04", i, "\ub05d\uac12 \ud3ec\ud568"), rows$upper_closed[i]))))
    }))
  })
  shiny::observeEvent(input$bin_add, {
    rows <- bin_rows_from_inputs(input, bin_rows_state())
    if (nrow(rows) < 30L) bin_rows_state(rbind(rows, empty_bin_rows(1L)))
    else shiny::showNotification("\uad6c\uac04\uc740 \ucd5c\ub300 30\uac1c\uae4c\uc9c0 \uc9c0\uc815\ud560 \uc218 \uc788\uc2b5\ub2c8\ub2e4.", type = "warning")
  })
  shiny::observeEvent(input$bin_remove, {
    rows <- bin_rows_from_inputs(input, bin_rows_state())
    if (nrow(rows) > 1L) bin_rows_state(head(rows, -1L))
  })
  shiny::observe({
    d <- editor_context()$data
    choices <- names(d)[vapply(d, function(x) is.numeric(x) && !is.complex(x) && !inherits(x, c("Date", "POSIXt", "difftime")), logical(1))]
    selected <- intersect(shiny::isolate(input$bin_variable), choices)
    if (!length(selected)) selected <- head(choices, 1L)
    shiny::updateSelectInput(session, "bin_variable", choices = choices, selected = selected)
  }, priority = 65)
  shiny::observe({
    choices <- operation_choices(workspace_mode() == "additional")
    selected <- shiny::isolate(input$manual_op)
    if (length(selected) != 1L || !selected %in% unname(choices)) selected <- "set_na"
    shiny::updateSelectInput(session, "manual_op", choices = choices, selected = selected)
  }, priority = 70)
  bin_preview <- shiny::reactive({
    shiny::req(identical(input$manual_op, "binning"))
    tryCatch({
      context <- editor_context()
      if (nzchar(context$error)) stop(context$error, call. = FALSE)
      p <- bin_parameters_from_inputs(input, bin_rows_state())
      require_text(p$input, "\uc218\uce58\ud615 \ubcc0\uc218"); require_columns(context$data, p$input, "\uc218\uce58\ud615 \ubcc0\uc218")
      if (length(p$output) != 1L || !nzchar(p$output)) p$output <- "\uc0c8 \ubcc0\uc218"
      step <- binning_result(context$data[[p$input]], p, enforce = FALSE)
      list(step = step, error = "")
    }, error = function(e) list(step = NULL, error = conditionMessage(e)))
  })
  output$bin_preview <- shiny::renderTable({
    x <- bin_preview(); if (is.null(x$step)) return(NULL)
    b <- x$step$details$bins
    data.frame("\ubc94\uc8fc\uba85" = b$label, "\uad6c\uac04" = b$range, "\uac1c\uc218" = b$n, check.names = FALSE)
  }, rownames = FALSE)
  output$bin_preview_note <- shiny::renderText({
    x <- bin_preview(); if (nzchar(x$error)) return(x$error)
    d <- x$step$details
    paste0(sprintf("\uc6d0\ub798 \uacb0\uce21 %d\uac1c, \uad6c\uac04 \ubc16 \uac12 %d\uac1c.", d$original_missing_n, d$outside_n),
      if (d$outside_n > 0L && d$outside_policy == "error") " \uad6c\uac04 \ubc16 \uac12\uc774 \uc788\uc5b4 \uc800\uc7a5\uacfc \uc801\uc6a9\uc774 \uc911\ub2e8\ub429\ub2c8\ub2e4." else
      if (d$outside_n > 0L) " \uad6c\uac04 \ubc16 \uac12\uc740 \uc0c8 \ubcc0\uc218\uc5d0\uc11c \uacb0\uce21\uac12\uc73c\ub85c \ucc98\ub9ac\ub429\ub2c8\ub2e4." else "")
  })
})
