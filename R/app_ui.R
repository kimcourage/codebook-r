codebookr_ui <- function() {
ai_controls <- tagList(
  p(class = "hint", "AI\uac00 \ucf54\ub4dc\ubd81\uc758 \ubcc0\uc218 \uc815\uc758\uc640 \ucc98\ub9ac \uc9c0\uce68\uc744 \uc77d\uc2b5\ub2c8\ub2e4. \uc6d0\ubb38\uacfc \uc2e4\uc81c \uc790\ub8cc\ub97c \ub300\uc870\ud55c \ub4a4 \uc801\uc6a9\ud560 \ub0b4\uc6a9\uc744 \uc120\ud0dd\ud558\uc138\uc694."),
  fluidRow(
    column(4, selectInput("provider", "AI \uc81c\uacf5\uc5c5\uccb4", c("OpenAI" = "openai", "Anthropic" = "anthropic", "Google Gemini" = "gemini"))),
    column(4, passwordInput("api_key", "\ub0b4 API \ud0a4 (\uc800\uc7a5\ud558\uc9c0 \uc54a\uc74c)", "")),
    column(4, textInput("model", "\ubaa8\ub378 ID (\ube44\uc6b0\uba74 \uae30\ubcf8\uac12)", ""))
  ),
  tags$details(tags$summary("\uc678\ubd80\ub85c \ubcf4\ub0bc \ub0b4\uc6a9 \ud655\uc778"),
    p(class = "hint", "\ubcc0\uc218\uba85, \uc790\ub8cc\ud615, \ubcc0\uc218 \uc124\uba85\uacfc \ucf54\ub4dc\ubd81 \ubc1c\ucdcc\ubb38\uc744 \ubcf4\ub0c5\ub2c8\ub2e4. \uc751\ub2f5\uc790\ubcc4 \uc790\ub8cc\ub294 \ubcf4\ub0b4\uc9c0 \uc54a\uc2b5\ub2c8\ub2e4."),
    verbatimTextOutput("payload_preview")),
  div(class = "privacy-box", checkboxGroupInput("consent", NULL, choices = character())),
  actionButton("ai_button", "AI\ub85c \ucf54\ub4dc\ubd81 \uc77d\uae30", class = "btn-primary"),
  div(class = "feedback", role = "status", textOutput("ai_status"))
)

operation_editor <- tagList(
  selectInput("edit_rule", "\ud3b8\uc9d1\ud560 \uc791\uc5c5", choices = c("\uc0c8 \uc791\uc5c5" = "new")),
  p(class = "hint", textOutput("editor_context_status")),
  selectInput("manual_op", "\uc791\uc5c5 \uc885\ub958", c("\uacb0\uce21\uac12 \ucc98\ub9ac" = "set_na", "\uc5ed\ucf54\ub529" = "reverse",
    "\ubc94\uc8fc\ud615 \ubcc0\ud658" = "factor", "\ud569\uc131\ubcc0\uc218 \ub9cc\ub4e4\uae30" = "composite", "\uc911\uc2ec\ud654" = "center",
    "\ud45c\uc900\ud654 (z\uc810\uc218)" = "standardize", "wide \u2192 long" = "long", "long \u2192 wide" = "wide", "\ucf54\ub4dc\ubd81 \ub77c\ubca8 \ud655\uc778" = "labels")),
  conditionalPanel("['set_na','reverse','factor'].includes(input.manual_op)",
    selectInput("manual_var", "\ubcc0\uc218", choices = character())),
  conditionalPanel("input.manual_op === 'set_na'",
    textInput("manual_codes", "\uacb0\uce21\uc73c\ub85c \ubc14\uafc0 \ucf54\ub4dc", "", placeholder = "\uc608: 9, 99")),
  conditionalPanel("input.manual_op === 'reverse'", fluidRow(
    column(6, numericInput("manual_min", "\ucc99\ub3c4 \ucd5c\uc18c\uac12", 1)), column(6, numericInput("manual_max", "\ucc99\ub3c4 \ucd5c\ub300\uac12", 5)))),
  conditionalPanel("input.manual_op === 'factor'", p(class = "hint", "\uad00\uce21 \ucf54\ub4dc\ub97c \ubc94\uc8fc\ub85c \ubc14\uafc9\ub2c8\ub2e4. \ubc94\uc8fc \uc21c\uc11c\uc640 \uac12 \ub77c\ubca8\uc740 \uc9c0\uc815\ud558\uc9c0 \uc54a\uc2b5\ub2c8\ub2e4.")),
  conditionalPanel("input.manual_op === 'composite'",
    selectInput("composite_items", "\ud569\uce60 \ubb38\ud56d", choices = character(), multiple = TRUE),
    fluidRow(column(6, selectInput("composite_method", "\uacc4\uc0b0 \ubc29\ubc95", c("\ud3c9\uade0" = "mean", "\ud569\uacc4" = "sum"))),
      column(6, numericInput("composite_min", "\ucd5c\uc18c \uc751\ub2f5 \ubb38\ud56d \uc218", 1, min = 1, step = 1))),
    p(class = "hint", "\uc751\ub2f5\ud55c \ubb38\ud56d\ub9cc \uacc4\uc0b0\ud569\ub2c8\ub2e4. \ucd5c\uc18c \uc751\ub2f5 \uc218\uc5d0 \ubabb \ubbf8\uce58\uba74 \uacb0\uce21\uc73c\ub85c \ub0a8\uae41\ub2c8\ub2e4. \uc5ed\ubb38\ud56d\uc740 \uc55e\uc120 \uc791\uc5c5\uc5d0\uc11c \uc5ed\ucf54\ub529\ud558\uc138\uc694.")),
  conditionalPanel("['center','standardize'].includes(input.manual_op)",
    selectInput("scale_variable", "\uacc4\uc0b0\ud560 \ubcc0\uc218", choices = character()),
    selectInput("scale_scope", "\uacc4\uc0b0 \uae30\uc900", c("\uc804\uccb4 \uc790\ub8cc" = "all", "\uc9d1\ub2e8\ubcc4" = "group", "\uac1c\uc778\ubcc4" = "person")),
    conditionalPanel("input.scale_scope !== 'all'",
      selectInput("scale_groups", "\uc9d1\ub2e8 \ub610\ub294 \uac1c\uc778\uc744 \uad6c\ubd84\ud560 \ubcc0\uc218", choices = character(), multiple = TRUE)),
    selectInput("scale_reference", "\uae30\uc900\uac12 \uad6c\ud558\uae30", c("\ud604\uc7ac \uc790\ub8cc\uc5d0\uc11c \uacc4\uc0b0" = "all", "\ud2b9\uc815 \uc790\ub8cc\uc5d0\uc11c \uacc4\uc0b0" = "subset", "\uae30\uc900\uac12 \uc9c1\uc811 \uc785\ub825" = "fixed")),
    conditionalPanel("input.scale_reference === 'subset'",
      selectInput("reference_column", "\uae30\uc900 \uc790\ub8cc\ub97c \uace0\ub97c \ubcc0\uc218", choices = character()),
      selectInput("reference_values", "\ud3ec\ud568\ud560 \uac12", choices = character(), multiple = TRUE),
      p(class = "hint", "\uc120\ud0dd\ud55c \uc790\ub8cc\uc5d0\uc11c \uae30\uc900\uac12\uc744 \uad6c\ud55c \ub4a4 \uc804\uccb4 \ud589\uc5d0 \uc801\uc6a9\ud569\ub2c8\ub2e4. \uc9d1\ub2e8\ubcc4 \uacc4\uc0b0\uc5d0\uc11c\ub294 \uac01 \uc9d1\ub2e8 \uc548\uc5d0\uc11c \uae30\uc900 \uc790\ub8cc\ub97c \ucc3e\uc2b5\ub2c8\ub2e4.")),
    conditionalPanel("input.scale_reference === 'fixed'",
      numericInput("fixed_center", "\ube84 \uc911\uc2ec\uac12", 0),
      conditionalPanel("input.manual_op === 'standardize'", numericInput("fixed_scale", "\ub098\ub20c \ud45c\uc900\ud3b8\ucc28", 1, min = 0)),
      p(class = "hint", "\uc804\uccb4 \uc790\ub8cc\uc5d0 \uac19\uc740 \uae30\uc900\uac12\uc744 \uc801\uc6a9\ud569\ub2c8\ub2e4. \uc9d1\ub2e8\ubcc4\ub85c \ub2e4\ub978 \uac12\uc744 \uc9c1\uc811 \uc785\ub825\ud558\ub294 \ubc29\uc2dd\uc740 \uc9c0\uc6d0\ud558\uc9c0 \uc54a\uc2b5\ub2c8\ub2e4.")),
    p(class = "hint", "\uc911\uc2ec\ud654\ub294 \ud3c9\uade0\uc744 \ube8d\ub2c8\ub2e4. z\uc810\uc218\ub294 \ud3c9\uade0\uc744 \ube80 \ub4a4 \ud45c\ubcf8 \ud45c\uc900\ud3b8\ucc28\ub85c \ub098\ub215\ub2c8\ub2e4. \uacb0\uce21\uc740 \uc81c\uc678\ud558\uba70, \ud604\uc7ac \uc790\ub8cc\uc5d0\uc11c \uacc4\uc0b0\ud560 \ub54c \uac01 \ud589\uc740 \uac19\uc740 \ube44\uc911\uc744 \uac16\uc2b5\ub2c8\ub2e4.")),
  conditionalPanel("['composite','center','standardize'].includes(input.manual_op)",
    textInput("new_variable", "\uc0c8 \ubcc0\uc218 \uc774\ub984", "", placeholder = "\uc608: wellbeing_mean"),
    p(class = "hint", "\uc6d0\ub798 \ubcc0\uc218\ub294 \uc720\uc9c0\ud569\ub2c8\ub2e4. \uc774\ubbf8 \uc788\ub294 \ubcc0\uc218 \uc774\ub984\uc740 \uc4f8 \uc218 \uc5c6\uc2b5\ub2c8\ub2e4.")),
  conditionalPanel("['long','wide'].includes(input.manual_op)",
    selectInput("reshape_ids", "\ub300\uc0c1\uc744 \uad6c\ubd84\ud560 \uc2dd\ubcc4 \ubcc0\uc218", choices = character(), multiple = TRUE)),
  conditionalPanel("input.manual_op === 'long'",
    selectInput("long_columns", "\ud589\uc73c\ub85c \ubc14\uafc0 \ubc18\ubcf5 \uce21\uc815 \ubcc0\uc218", choices = character(), multiple = TRUE),
    selectInput("long_layout", "\ubcc0\uc218 \uc774\ub984 \uc77d\uae30", c("\ud55c \uac00\uc9c0 \uce21\uc815\uac12" = "single", "\uc5ec\ub7ec \uce21\uc815\uac12: \uc774\ub984 \ub05d\uc774 \uc2dc\uc810" = "multiple")),
    textInput("long_time", "\uc2dc\uc810\uc744 \ub2f4\uc744 \ubcc0\uc218 \uc774\ub984", "occasion"),
    conditionalPanel("input.long_layout === 'single'",
      textInput("long_value", "\uce21\uc815\uac12\uc744 \ub2f4\uc744 \ubcc0\uc218 \uc774\ub984", "score"),
      textInput("long_prefix", "\ubcc0\uc218 \uc774\ub984\uc5d0\uc11c \ube84 \uc811\ub450\uc5b4 (\uc120\ud0dd)", "", placeholder = "\uc608: score_"),
      p(class = "hint", "score_pre\uc640 score_post\uc5d0\uc11c score_\ub97c \ube7c\uba74 \uc2dc\uc810 \uac12\uc740 pre\uc640 post\uac00 \ub429\ub2c8\ub2e4.")),
    conditionalPanel("input.long_layout === 'multiple'",
      textInput("long_separator", "\uce21\uc815\uac12\uacfc \uc2dc\uc810 \uc0ac\uc774\uc758 \uad6c\ubd84\uc790", "_"),
      p(class = "hint", "\uc774\ub984\uc758 \ub9c8\uc9c0\ub9c9 \uad6c\ubd84\uc790\ub85c \ub098\ub215\ub2c8\ub2e4. mood_pre, mood_post, sleep_pre, sleep_post\ub294 mood\uc640 sleep \ub450 \uc5f4\ub85c \ubc14\ub01d\ub2c8\ub2e4.")),
    checkboxInput("long_drop_na", "\ubaa8\ub4e0 \uce21\uc815\uac12\uc774 \uacb0\uce21\uc778 \uc2dc\uc810\uc740 \uc81c\uc678", FALSE),
    p(class = "hint", "\uc120\ud0dd\ud558\uc9c0 \uc54a\uc740 \uc5f4\uc740 \uadf8\ub300\ub85c \ub530\ub77c\uac11\ub2c8\ub2e4. \uc2dd\ubcc4 \ubcc0\uc218\uc758 \uc870\ud569\uc740 \uc6d0\uc790\ub8cc\uc5d0\uc11c \ud589\ub9c8\ub2e4 \ub2ec\ub77c\uc57c \ud569\ub2c8\ub2e4.")),
  conditionalPanel("input.manual_op === 'wide'",
    selectInput("wide_time", "\uc2dc\uc810 \ub610\ub294 \ubc18\ubcf5 \ubc94\uc8fc", choices = character()),
    selectInput("wide_values", "\uc5f4\ub85c \ud3bc\uce60 \uce21\uc815\uac12", choices = character(), multiple = TRUE),
    textInput("wide_separator", "\uce21\uc815\uac12\uacfc \uc2dc\uc810 \uc0ac\uc774\uc758 \uad6c\ubd84\uc790", "_"),
    p(class = "hint", "score\uc758 pre\uc640 post \uac12\uc740 score_pre, score_post \uc5f4\ub85c \ub9cc\ub4ed\ub2c8\ub2e4. \uac19\uc740 \ub300\uc0c1\uc758 \uac19\uc740 \uc2dc\uc810\uc774 \uc911\ubcf5\ub418\uba74 \uc911\ub2e8\ud569\ub2c8\ub2e4. \ub098\uba38\uc9c0 \uc5f4\uc740 \ub300\uc0c1 \uc548\uc5d0\uc11c \uac12\uc774 \uac19\uc744 \ub54c\ub9cc \uc720\uc9c0\ud569\ub2c8\ub2e4.")),
  conditionalPanel("input.manual_op === 'labels'", p(class = "hint", "\ub77c\ubca8\uc740 \ucf54\ub4dc\ubd81 \uc6d0\ubb38\uc5d0\uc11c \ud655\uc778\ud569\ub2c8\ub2e4. \uc815\uc758\ub97c \ubc14\uafb8\ub824\uba74 \ucf54\ub4dc\ubd81\uc744 \uc218\uc815\ud55c \ub4a4 \ub2e4\uc2dc \uc5f0\uacb0\ud558\uc138\uc694.")),
  textInput("manual_reason", "\ubcc0\uacbd \uc774\uc720", "", placeholder = "\uc608: \ucc99\ub3c4 \uc9c0\uce68\uc5d0 \ub530\ub77c \uc138 \ubb38\ud56d\uc758 \ud3c9\uade0 \uacc4\uc0b0"),
  div(class = "editor-actions", actionButton("add_manual", "\uc791\uc5c5 \uc800\uc7a5", class = "btn-primary"),
    conditionalPanel("input.edit_rule !== 'new'", actionButton("delete_rule", "\uc0ad\uc81c"),
      actionButton("move_rule_up", "\uc704\ub85c"), actionButton("move_rule_down", "\uc544\ub798\ub85c"))),
  p(class = "hint", "\uc791\uc5c5\uc744 \uc218\uc815\ud558\uba74 \ud574\ub2f9 \uc791\uc5c5\uacfc \uadf8 \uacb0\uacfc\ub97c \uc0ac\uc6a9\ud558\ub294 \uc791\uc5c5\uc758 \uc120\ud0dd\uc774 \ud574\uc81c\ub429\ub2c8\ub2e4. \uc21c\uc11c\ub97c \ubc14\uafb8\uba74 \ubaa8\ub450 \ub2e4\uc2dc \uc120\ud0dd\ud574\uc57c \ud569\ub2c8\ub2e4.")
)

ui <- fluidPage(
  tags$head(tags$meta(charset = "UTF-8"), tags$title("CodebookR"),
    tags$script(HTML("$(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function(){ window.scrollTo(0, 0); });")),
    tags$style(HTML("
    body { background:#f5f7fa; color:#203047; font-family:-apple-system,BlinkMacSystemFont,'Noto Sans KR','Segoe UI',sans-serif; font-size:14px; }
    .container-fluid { max-width:1500px; } .app-hero { margin:26px 0 22px; }
    .app-hero h1 { font-size:28px; font-weight:700; margin:0 0 8px; } .app-hero p { color:#566579; }
    .nav-tabs > li > a { color:#314761; font-weight:600; padding:13px 22px; }
    .tab-content { background:white; border:1px solid #dfe6ef; border-top:none; padding:24px; min-height:550px; border-radius:0 0 10px 10px; }
    h3 { font-size:22px; margin-top:4px; } h4 { font-size:16px; font-weight:600; margin-top:22px; }
    .section-lead,.hint { color:#5b6a7d; line-height:1.7; } .hint { font-size:13px; }
    .feedback { margin:12px 0; line-height:1.7; color:#314761; }
    .privacy-box,.preview-box,.work-list { border:1px solid #dce5ef; background:#f7f9fc; border-radius:8px; padding:16px; margin:14px 0; }
    .work-list .checkbox { padding:8px 0; border-bottom:1px solid #e2e8f0; } .work-list h4 { margin-top:0; }
    .table-wrapper { max-height:350px; overflow:auto; border:1px solid #e5eaf1; border-radius:6px; margin:10px 0; }
    .table { font-size:13px; margin-bottom:0; } .table th { background:#eef3f8; white-space:nowrap; } .table td { vertical-align:top; }
    .btn-primary { background:#245e8a; border-color:#245e8a; } .btn { margin-right:7px; margin-bottom:6px; }
    details { border:1px solid #e0e6f0; padding:12px 14px; border-radius:8px; margin:14px 0; }
    summary { cursor:pointer; font-weight:600; color:#314761; } details[open] > summary { margin-bottom:14px; }
    .next-step { margin-top:22px; border-top:1px solid #e4e9ef; padding-top:18px; }
    pre { white-space:pre-wrap; overflow-wrap:anywhere; max-height:320px; overflow:auto; font-size:12px; }
    hr { border-color:#e4e9ef; } .shiny-notification { border-radius:8px; }
  "))),
  div(class = "app-hero", h1("CodebookR"), p("\ucf54\ub4dc\ubd81\uc5d0 \ub9de\ucdb0 \uc790\ub8cc\ub97c \uc815\ub9ac\ud558\uace0, \uc5f0\uad6c\uc5d0 \ud544\uc694\ud55c \uc804\ucc98\ub9ac\ub97c \uc774\uc5b4\uac00\uc138\uc694.")),
  div(style = "display:none", textOutput("additional_mode")),
  tabsetPanel(id = "main_tabs",
    tabPanel("\u2460 \uc790\ub8cc\uc640 \ucf54\ub4dc\ubd81", value = "data",
      h3("\uc6d0\uc790\ub8cc\uc640 \ucf54\ub4dc\ubd81\uc744 \uc5f0\uacb0\ud558\uc138\uc694"),
      p(class = "section-lead", "\ucf54\ub4dc\ubd81\uc744 \uae30\uc900\uc73c\ub85c \ubcc0\uc218\uc758 \ub73b, \uc751\ub2f5 \ucf54\ub4dc\uc640 \ucc98\ub9ac \uc9c0\uce68\uc744 \ud655\uc778\ud569\ub2c8\ub2e4."),
      fluidRow(
        column(6, fileInput("data_file", "\uc6d0\uc790\ub8cc", accept = c(".csv", ".sav", ".dta", ".rds"), buttonLabel = "\ud30c\uc77c \uc120\ud0dd", placeholder = "\uc6d0\uc790\ub8cc \ud30c\uc77c"),
          p(class = "hint", "CSV, SPSS, Stata, RDS. \ucd5c\ub300 100MB\uc785\ub2c8\ub2e4."),
          tags$details(tags$summary("CSV \uc77d\uae30 \uc635\uc158"),
            selectInput("csv_encoding", "\ubb38\uc790 \uc778\ucf54\ub529", c("UTF-8" = "UTF-8", "\ud55c\uad6d\uc5b4 Windows (CP949)" = "CP949", "EUC-KR" = "EUC-KR")),
            textInput("csv_character_columns", "\ubb38\uc790\ub85c \uc77d\uc744 \ubcc0\uc218\uba85", "", placeholder = "\uc608: respondent_id, region_code"),
            p(class = "hint", "\uc27c\ud45c\ub85c \uad6c\ubd84\ud569\ub2c8\ub2e4. \uc55e\uc790\ub9ac\uac00 0\uc774\uac70\ub098 16\uc790\ub9ac \uc774\uc0c1\uc778 \uc815\uc218 \ucf54\ub4dc\ub294 \uc790\ub3d9\uc73c\ub85c \ubb38\uc790\ub85c \uc77d\uc2b5\ub2c8\ub2e4. \ube48 \uce78\uacfc NA\ub294 \uacb0\uce21\uc73c\ub85c \uc77d\uc2b5\ub2c8\ub2e4. \uc77d\uae30 \uc635\uc158\uc744 \ubc14\uafb8\uba74 \uc791\uc5c5\uacfc \uacb0\uacfc\ub97c \ucd08\uae30\ud654\ud569\ub2c8\ub2e4."))),
        column(6, fileInput("book_files", "\ucf54\ub4dc\ubd81", accept = c(".pdf", ".xlsx", ".xls", ".csv"), multiple = TRUE, buttonLabel = "\ud30c\uc77c \uc120\ud0dd", placeholder = "\ucf54\ub4dc\ubd81 \ud30c\uc77c (\uc5ec\ub7ec \uac1c \uac00\ub2a5)"),
          p(class = "hint", "Excel, CSV\uc640 \ud14d\uc2a4\ud2b8\ud615 PDF\ub97c \uc77d\uc2b5\ub2c8\ub2e4. \uc2a4\uce94 \uc774\ubbf8\uc9c0\uc640 \ubcf5\uc7a1\ud55c \ubb38\uc11c\ub294 \uc77d\uae30 \uacb0\uacfc\ub97c \ud655\uc778\ud574 \uc8fc\uc138\uc694."),
          actionButton("load_example", "\uc608\uc81c \uc790\ub8cc\uc640 \ucf54\ub4dc\ubd81\uc73c\ub85c \uc0b4\ud3b4\ubcf4\uae30"))
      ),
      div(class = "feedback", textOutput("dataset_status"), textOutput("data_feedback"), textOutput("book_feedback")),
      selectInput("raw_columns", "\uc0b4\ud3b4\ubcfc \ubcc0\uc218", choices = character(), multiple = TRUE),
      div(class = "table-wrapper", tableOutput("raw_sample")), p(class = "hint", "\ucc98\uc74c 12\ud589, \uc120\ud0dd\ud55c \ubcc0\uc218 \uc911 \ucd5c\ub300 10\uac1c\ub97c \ud45c\uc2dc\ud569\ub2c8\ub2e4. \uacc4\uc0b0\uc740 \uc6d0\uc790\ub8cc\ub97c \ubcf4\uc874\ud55c \ucc44 \uc774 \ucef4\ud4e8\ud130\uc5d0\uc11c \ud569\ub2c8\ub2e4."),
      div(class = "preview-box", textOutput("coverage")),
      tags$details(tags$summary("\ubcc0\uc218 \uc5f0\uacb0 \ud655\uc778 \ub610\ub294 \ubcc0\uacbd"),
        fluidRow(column(5, selectInput("mapping_variable", "\ucf54\ub4dc\ubd81 \ubcc0\uc218", choices = character())), column(5, selectInput("mapping_target", "\uc5f0\uacb0\ud560 \uc790\ub8cc \ubcc0\uc218", choices = character()))),
        textOutput("mapping_suggestion"), actionButton("save_mapping", "\uc5f0\uacb0 \uc800\uc7a5"), div(class = "table-wrapper", tableOutput("mapping_table"))),
      div(class = "feedback", textOutput("mapping_feedback")),
      tags$details(tags$summary("\ucf54\ub4dc\ubd81 \uc77d\uae30 \uacb0\uacfc\uc640 \uc6d0\ubb38"),
        div(class = "table-wrapper", tableOutput("book_summary")),
        p(class = "hint", "\uc77d\uc9c0 \ubabb\ud55c \uad6c\uac04\uc774\ub098 \uc81c\ud55c\uc5d0 \uac78\ub9b0 \ubb38\uc11c\uac00 \uc788\ub294\uc9c0 \ud655\uc778\ud558\uc138\uc694. \ubc1c\ucdcc\ubb38\uc740 \ubb38\uc11c \uc804\uccb4\uac00 \uc544\ub2d9\ub2c8\ub2e4."),
        div(class = "table-wrapper", tableOutput("book_records")), p(class = "hint", "\ucc98\uc74c 35\uac1c \uc815\uc758\uc785\ub2c8\ub2e4."), verbatimTextOutput("book_excerpts")),
      tags$details(tags$summary("\uc790\ub8cc\ud615\uacfc \uae30\uc874 \ub77c\ubca8"), div(class = "table-wrapper", tableOutput("metadata_table")),
        p(class = "hint", "\ucd5c\ub300 120\uac1c \ubcc0\uc218. SPSS \uc9c0\uc815 \uacb0\uce21 \ucf54\ub4dc\ub294 \uc6d0\ub798 \ucf54\ub4dc\uc640 \uc18d\uc131\uc744 \uc720\uc9c0\ud558\uba70 \uacb0\uce21 \uc218\uc5d0 \ud3ec\ud568\ud569\ub2c8\ub2e4.")),
      tags$details(tags$summary("\uc791\uc5c5 \uc800\uc7a5\uacfc \ub2e4\uc2dc \uc5f4\uae30"),
        p(class = "hint", "\ubcc0\uc218 \uc5f0\uacb0, \uac80\ud1a0 \uae30\ub85d\uacfc \uc791\uc5c5 \uc124\uc815\uc744 \uc800\uc7a5\ud569\ub2c8\ub2e4. API \ud0a4\uc640 \uc751\ub2f5\uc790 \uc790\ub8cc\ub294 \ud3ec\ud568\ud558\uc9c0 \uc54a\uc2b5\ub2c8\ub2e4. \ub2e4\uc2dc \uc5f4 \ub54c \uac19\uc740 \uc6d0\uc790\ub8cc\uc640 \ucf54\ub4dc\ubd81\uc744 \uba3c\uc800 \uc5f0\uacb0\ud558\uace0 \uc801\uc6a9\ud560 \uc791\uc5c5\uc744 \ub2e4\uc2dc \uc120\ud0dd\ud558\uc138\uc694."),
        downloadButton("save_project", "\uc791\uc5c5 \ud30c\uc77c \uc800\uc7a5"),
        fileInput("open_project", "\uc800\uc7a5\ud55c \uc791\uc5c5 \uc5f4\uae30", accept = ".json", buttonLabel = "\ud30c\uc77c \uc120\ud0dd", placeholder = "CodebookR \uc791\uc5c5 \ud30c\uc77c"),
        div(class = "feedback", textOutput("project_feedback"))),
      div(class = "next-step", actionButton("go_codebook", "\ucf54\ub4dc\ubd81 \uac80\ud1a0", class = "btn-primary"))
    ),
    tabPanel("\u2461 \uac80\ud1a0", value = "codebook",
      h3(textOutput("workspace_title")), p(class = "section-lead", textOutput("workspace_note")),
      conditionalPanel("output.additional_mode === 'codebook'",
        tags$details(tags$summary("AI\ub85c \ucf54\ub4dc\ubd81 \uc77d\uae30"), ai_controls)),
      fluidRow(
        column(4, div(class = "work-list", h4("\uc801\uc6a9\ud560 \uc791\uc5c5"),
          p(class = "hint", "\uc704\uc5d0\uc11c \uc544\ub798\ub85c \uc801\uc6a9\ud569\ub2c8\ub2e4. \uc120\ud0dd\ud558\uba74 \uc624\ub978\ucabd\uc5d0\uc11c \uc608\uc0c1 \uacb0\uacfc\ub97c \ubcfc \uc218 \uc788\uc2b5\ub2c8\ub2e4."),
          checkboxGroupInput("approved_rules", NULL, choices = character()),
          conditionalPanel("output.additional_mode === 'codebook'", actionButton("refresh_heuristics", "\uc0c8 \ucf54\ub4dc\ubd81 \ud6c4\ubcf4 \ucc3e\uae30")),
          actionButton("undo_rule", "\uc9c1\uc804 \ubcc0\uacbd \ub418\ub3cc\ub9ac\uae30")),
          tags$details(tags$summary("\uc791\uc5c5 \ucd94\uac00 \ub610\ub294 \uc218\uc815"), operation_editor),
          div(class = "feedback", role = "status", textOutput("rule_feedback"))),
        column(8,
          selectInput("review_variable", "\uc0b4\ud3b4\ubcfc \ubcc0\uc218", choices = character()),
          div(class = "feedback", textOutput("review_findings")),
          h4("\ucf54\ub4dc\ubd81 \uadfc\uac70"), div(class = "table-wrapper", tableOutput("review_evidence")),
          p(class = "hint", "\uc815\uc758\ub294 \ucd5c\ub300 100\uac1c \ud45c\uc2dc\ud569\ub2c8\ub2e4. AI\uac00 \uc77d\uc740 \uc815\uc758\ub3c4 \uc6d0\ubb38\uacfc \ub73b\uc774 \ub9de\ub294\uc9c0 \ud655\uc778\ud558\uc138\uc694."),
          tags$details(tags$summary("\uc6d0\uc790\ub8cc \uac12\uacfc \ube48\ub3c4"), div(class = "table-wrapper", tableOutput("review_frequency")), p(class = "hint", "\ube48\ub3c4\uac00 \ub192\uc740 \uac12\ubd80\ud130 \ucd5c\ub300 25\uac1c \ud45c\uc2dc\ud569\ub2c8\ub2e4.")),
          div(class = "preview-box", h4("\uc801\uc6a9 \uc804\ud6c4 \ubbf8\ub9ac\ubcf4\uae30"), textOutput("preview_status"),
            div(class = "table-wrapper", tableOutput("review_sample")),
            p(class = "hint", "\ucc98\uc74c 12\ud589\uc785\ub2c8\ub2e4. \uc790\ub8cc \uad6c\uc870\ub97c \ubc14\uafb8\uba74 \uacb0\uacfc \ud45c\ub97c \ucd5c\ub300 10\uc5f4\uae4c\uc9c0 \ubcf4\uc5ec \uc90d\ub2c8\ub2e4.")),
          conditionalPanel("output.additional_mode === 'codebook'", tags$details(tags$summary("\ubcc0\uc218 \uac80\ud1a0 \uae30\ub85d \ub0a8\uae30\uae30"),
            selectInput("review_decision", "\ud310\ub2e8", c("\ud655\uc778\ud568", "\ubcf4\ub958")), textInput("review_reason", "\ud655\uc778 \ub610\ub294 \ubcf4\ub958\ud55c \uc774\uc720", ""), actionButton("save_decision", "\uac80\ud1a0 \uae30\ub85d \uc800\uc7a5")))
        )
      ),
      tags$details(tags$summary("\uc804\uccb4 \ubcc0\uc218\uc758 \ud655\uc778\uc0ac\ud56d"), div(class = "table-wrapper", tableOutput("review_overview"))),
      tags$details(tags$summary("\uc791\uc5c5\ubcc4 \uc608\uc0c1 \uc801\uc6a9 \uc218\uc640 \uacc4\uc0b0 \uae30\uc900"), div(class = "table-wrapper", tableOutput("preview_table"))),
      div(class = "next-step", textOutput("execution_plan_summary"),
        actionButton("run_button", "\uc120\ud0dd\ud55c \uc791\uc5c5 \uc801\uc6a9", class = "btn-primary"),
        conditionalPanel("output.additional_mode === 'codebook'", actionButton("run_unchanged", "\ubcc0\uacbd \uc5c6\uc774 \uacb0\uacfc \ud655\uc778")),
        div(class = "feedback", role = "status", textOutput("execution_error")))
    ),
    tabPanel("\u2462 \uacb0\uacfc", value = "execute",
      h3("\ucc98\ub9ac \uacb0\uacfc\ub97c \ud655\uc778\ud558\uc138\uc694"), div(class = "feedback", role = "status", textOutput("run_status")),
      uiOutput("download_controls"),
      selectInput("result_columns", "\uc0b4\ud3b4\ubcfc \uacb0\uacfc \ubcc0\uc218", choices = character(), multiple = TRUE),
      div(class = "table-wrapper", tableOutput("result_sample")), p(class = "hint", "\ucc98\uc74c 12\ud589, \ucd5c\ub300 10\uc5f4\uc744 \ud45c\uc2dc\ud569\ub2c8\ub2e4."),
      div(class = "preview-box", textOutput("result_review")),
      tags$details(tags$summary("\uae30\ubcf8 \ucc98\ub9ac \ud6c4 \ucf54\ub4dc\ubd81\uacfc \ub2e4\uc2dc \ub300\uc870\ud55c \uacb0\uacfc"), div(class = "table-wrapper", tableOutput("post_review"))),
      tags$details(tags$summary("\uc801\uc6a9 \uae30\ub85d\uacfc \uacb0\uacfc \uc790\ub8cc\ud615"), div(class = "table-wrapper", tableOutput("audit_table")), div(class = "table-wrapper", tableOutput("after_metadata"))),
      div(class = "next-step", textOutput("base_status"), uiOutput("base_controls")),
      p(class = "hint", "ZIP\uc5d0 \uacb0\uacfc \ub370\uc774\ud130, \uc801\uc6a9\ud55c \uc791\uc5c5, \ucf54\ub4dc\ubd81 \uadfc\uac70\uc640 \uac80\ud1a0 \uae30\ub85d, \uc6d0\uc790\ub8cc\ubd80\ud130 \ub2e4\uc2dc \uc2e4\ud589\ud560 R \ucf54\ub4dc\ub97c \ub2f4\uc2b5\ub2c8\ub2e4. \uc791\uc5c5\uc744 \ubcf4\ub958\ud588\uac70\ub098 \uc77d\uc9c0 \ubabb\ud55c \uc9c0\uce68\uc774 \uc788\uc73c\uba74 \ubd84\uc11d \uc804\uc5d0 \ud655\uc778\ud558\uc138\uc694.")
    )
  ),
  tags$footer(class = "hint", style = "margin:20px 4px 30px;", "CodebookR")
)

  ui
}
