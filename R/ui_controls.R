operation_choices <- function(additional = FALSE) {
  choices <- c("\uacb0\uce21\uac12 \ucc98\ub9ac" = "set_na", "\uc5ed\ucf54\ub529" = "reverse",
    "\ubc94\uc8fc\ud615 \ubcc0\ud658" = "factor", "\ud569\uc131\ubcc0\uc218 \ub9cc\ub4e4\uae30" = "composite", "\uc911\uc2ec\ud654" = "center",
    "\ud45c\uc900\ud654 (z\uc810\uc218)" = "standardize", "wide \u2192 long" = "long", "long \u2192 wide" = "wide", "\ucf54\ub4dc\ubd81 \ub77c\ubca8 \ud655\uc778" = "labels"
  )
  if (additional) choices <- c(choices, "\uac12 \uad6c\uac04\uc73c\ub85c \ubc94\uc8fc\ud615 \ub9cc\ub4e4\uae30" = "binning")
  choices
}

# Existing input contracts are preserved here; layout lives in app_ui.R.
ui_ai_controls <- function() tagList(
  p(class = "hint", "AI\uac00 \ucf54\ub4dc\ubd81\uc758 \ubcc0\uc218 \uc815\uc758\uc640 \ucc98\ub9ac \uc9c0\uce68\uc744 \uc77d\uc2b5\ub2c8\ub2e4. \uc6d0\ubb38\uacfc \uc2e4\uc81c \uc790\ub8cc\ub97c \ub300\uc870\ud55c \ub4a4 \uc801\uc6a9\ud560 \ub0b4\uc6a9\uc744 \uc120\ud0dd\ud558\uc138\uc694."),
  fluidRow(
    column(12, selectInput("provider", "AI \uc81c\uacf5\uc5c5\uccb4", c("OpenAI" = "openai", "Anthropic" = "anthropic", "Google Gemini" = "gemini"))),
    column(12, passwordInput("api_key", "\ub0b4 API \ud0a4 (\uc800\uc7a5\ud558\uc9c0 \uc54a\uc74c)", "")),
    column(12, textInput("model", "\ubaa8\ub378 ID (\ube44\uc6b0\uba74 \uae30\ubcf8\uac12)", ""))
  ),
  tags$details(tags$summary("\uc678\ubd80\ub85c \ubcf4\ub0bc \ub0b4\uc6a9 \ud655\uc778"),
    p(class = "hint", "\ubcc0\uc218\uba85, \uc790\ub8cc\ud615, \ubcc0\uc218 \uc124\uba85\uacfc \ucf54\ub4dc\ubd81 \ubc1c\ucdcc\ubb38\uc744 \ubcf4\ub0c5\ub2c8\ub2e4. \uc751\ub2f5\uc790\ubcc4 \uc790\ub8cc\ub294 \ubcf4\ub0b4\uc9c0 \uc54a\uc2b5\ub2c8\ub2e4."),
    verbatimTextOutput("payload_preview")),
  div(class = "privacy-box", checkboxGroupInput("consent", NULL, choices = character())),
  actionButton("ai_button", "AI\ub85c \ucf54\ub4dc\ubd81 \uc77d\uae30", class = "btn-primary"),
  div(class = "feedback", role = "status", textOutput("ai_status"))
)

ui_operation_editor <- function() tagList(
  selectInput("edit_rule", "\ud3b8\uc9d1\ud560 \uc791\uc5c5", choices = c("\uc0c8 \uc791\uc5c5" = "new")),
  p(class = "hint", textOutput("editor_context_status")),
  selectInput("manual_op", "\uc791\uc5c5 \uc885\ub958", operation_choices()),
  conditionalPanel("['set_na','reverse','factor'].includes(input.manual_op)",
    selectInput("manual_var", "\ubcc0\uc218", choices = character())),
  conditionalPanel("input.manual_op === 'set_na'",
    textInput("manual_codes", "\uacb0\uce21\uc73c\ub85c \ubc14\uafc0 \ucf54\ub4dc", "", placeholder = "\uc608: 9, 99")),
  conditionalPanel("input.manual_op === 'reverse'", fluidRow(
    column(6, numericInput("manual_min", "\ucc99\ub3c4 \ucd5c\uc18c\uac12", 1)), column(6, numericInput("manual_max", "\ucc99\ub3c4 \ucd5c\ub300\uac12", 5)))),
  conditionalPanel("input.manual_op === 'factor'", p(class = "hint", "\ucf54\ub4dc\ubd81\uc758 \uc751\ub2f5\uba85\uc744 \ubc94\uc8fc\uba85\uc73c\ub85c \uc0ac\uc6a9\ud574 factor\ub85c \ubc14\uafc9\ub2c8\ub2e4. \uc608: 1 \u2192 \ub0a8\uc131, 2 \u2192 \uc5ec\uc131. \ub77c\ubca8\uc774 \uc5c6\ub294 \uac12\uc740 \ucf54\ub4dc\ub97c \ubc94\uc8fc\uba85\uc73c\ub85c \uc720\uc9c0\ud569\ub2c8\ub2e4. \ubc94\uc8fc \uac04 \uc21c\uc11c\ub294 \uc9c0\uc815\ud558\uc9c0 \uc54a\uc2b5\ub2c8\ub2e4.")),
  conditionalPanel("input.manual_op === 'binning'", ui_binning_controls()),
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
  conditionalPanel("['composite','center','standardize','binning'].includes(input.manual_op)",
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
