# Run from the repository root with shiny::runApp('.').
options(shiny.maxRequestSize = 100 * 1024^2)
required <- c("shiny", "readr", "haven", "readxl", "pdftools", "jsonlite", "zip", "tidyr", "vctrs", "tidyselect")
missing_packages <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) stop("필수 패키지를 설치하세요: ", paste(missing_packages, collapse = ", "))
for (file in c("data_io.R", "codebook_parser.R", "codebook_review.R", "transformations.R", "rules.R", "llm.R", "export.R", "project_io.R", "review_server.R")) {
  source(file.path("R", file), encoding = "UTF-8")
}
library(shiny)

ai_controls <- tagList(
  p(class = "hint", "AI가 코드북의 변수 정의와 처리 지침을 읽습니다. 원문과 실제 자료를 대조한 뒤 적용할 내용을 선택하세요."),
  fluidRow(
    column(4, selectInput("provider", "AI 제공업체", c("OpenAI" = "openai", "Anthropic" = "anthropic", "Google Gemini" = "gemini"))),
    column(4, passwordInput("api_key", "내 API 키 (저장하지 않음)", "")),
    column(4, textInput("model", "모델 ID (비우면 기본값)", ""))
  ),
  tags$details(tags$summary("외부로 보낼 내용 확인"),
    p(class = "hint", "변수명, 자료형, 변수 설명과 코드북 발췌문을 보냅니다. 응답자별 자료는 보내지 않습니다."),
    verbatimTextOutput("payload_preview")),
  div(class = "privacy-box", checkboxGroupInput("consent", NULL, choices = character())),
  actionButton("ai_button", "AI로 코드북 읽기", class = "btn-primary"),
  div(class = "feedback", role = "status", textOutput("ai_status"))
)

operation_editor <- tagList(
  selectInput("edit_rule", "편집할 작업", choices = c("새 작업" = "new")),
  p(class = "hint", textOutput("editor_context_status")),
  selectInput("manual_op", "작업 종류", c("결측값 처리" = "set_na", "역코딩" = "reverse",
    "범주형 변환" = "factor", "합성변수 만들기" = "composite", "중심화" = "center",
    "표준화 (z점수)" = "standardize", "wide → long" = "long", "long → wide" = "wide", "코드북 라벨 확인" = "labels")),
  conditionalPanel("['set_na','reverse','factor'].includes(input.manual_op)",
    selectInput("manual_var", "변수", choices = character())),
  conditionalPanel("input.manual_op === 'set_na'",
    textInput("manual_codes", "결측으로 바꿀 코드", "", placeholder = "예: 9, 99")),
  conditionalPanel("input.manual_op === 'reverse'", fluidRow(
    column(6, numericInput("manual_min", "척도 최소값", 1)), column(6, numericInput("manual_max", "척도 최대값", 5)))),
  conditionalPanel("input.manual_op === 'factor'", p(class = "hint", "관측 코드를 범주로 바꿉니다. 범주 순서와 값 라벨은 지정하지 않습니다.")),
  conditionalPanel("input.manual_op === 'composite'",
    selectInput("composite_items", "합칠 문항", choices = character(), multiple = TRUE),
    fluidRow(column(6, selectInput("composite_method", "계산 방법", c("평균" = "mean", "합계" = "sum"))),
      column(6, numericInput("composite_min", "최소 응답 문항 수", 1, min = 1, step = 1))),
    p(class = "hint", "응답한 문항만 계산합니다. 최소 응답 수에 못 미치면 결측으로 남깁니다. 역문항은 앞선 작업에서 역코딩하세요.")),
  conditionalPanel("['center','standardize'].includes(input.manual_op)",
    selectInput("scale_variable", "계산할 변수", choices = character()),
    selectInput("scale_scope", "계산 기준", c("전체 자료" = "all", "집단별" = "group", "개인별" = "person")),
    conditionalPanel("input.scale_scope !== 'all'",
      selectInput("scale_groups", "집단 또는 개인을 구분할 변수", choices = character(), multiple = TRUE)),
    selectInput("scale_reference", "기준값 구하기", c("현재 자료에서 계산" = "all", "특정 자료에서 계산" = "subset", "기준값 직접 입력" = "fixed")),
    conditionalPanel("input.scale_reference === 'subset'",
      selectInput("reference_column", "기준 자료를 고를 변수", choices = character()),
      selectInput("reference_values", "포함할 값", choices = character(), multiple = TRUE),
      p(class = "hint", "선택한 자료에서 기준값을 구한 뒤 전체 행에 적용합니다. 집단별 계산에서는 각 집단 안에서 기준 자료를 찾습니다.")),
    conditionalPanel("input.scale_reference === 'fixed'",
      numericInput("fixed_center", "뺄 중심값", 0),
      conditionalPanel("input.manual_op === 'standardize'", numericInput("fixed_scale", "나눌 표준편차", 1, min = 0)),
      p(class = "hint", "전체 자료에 같은 기준값을 적용합니다. 집단별로 다른 값을 직접 입력하는 방식은 지원하지 않습니다.")),
    p(class = "hint", "중심화는 평균을 뺍니다. z점수는 평균을 뺀 뒤 표본 표준편차로 나눕니다. 결측은 제외하며, 현재 자료에서 계산할 때 각 행은 같은 비중을 갖습니다.")),
  conditionalPanel("['composite','center','standardize'].includes(input.manual_op)",
    textInput("new_variable", "새 변수 이름", "", placeholder = "예: wellbeing_mean"),
    p(class = "hint", "원래 변수는 유지합니다. 이미 있는 변수 이름은 쓸 수 없습니다.")),
  conditionalPanel("['long','wide'].includes(input.manual_op)",
    selectInput("reshape_ids", "대상을 구분할 식별 변수", choices = character(), multiple = TRUE)),
  conditionalPanel("input.manual_op === 'long'",
    selectInput("long_columns", "행으로 바꿀 반복 측정 변수", choices = character(), multiple = TRUE),
    selectInput("long_layout", "변수 이름 읽기", c("한 가지 측정값" = "single", "여러 측정값: 이름 끝이 시점" = "multiple")),
    textInput("long_time", "시점을 담을 변수 이름", "occasion"),
    conditionalPanel("input.long_layout === 'single'",
      textInput("long_value", "측정값을 담을 변수 이름", "score"),
      textInput("long_prefix", "변수 이름에서 뺄 접두어 (선택)", "", placeholder = "예: score_"),
      p(class = "hint", "score_pre와 score_post에서 score_를 빼면 시점 값은 pre와 post가 됩니다.")),
    conditionalPanel("input.long_layout === 'multiple'",
      textInput("long_separator", "측정값과 시점 사이의 구분자", "_"),
      p(class = "hint", "이름의 마지막 구분자로 나눕니다. mood_pre, mood_post, sleep_pre, sleep_post는 mood와 sleep 두 열로 바뀝니다.")),
    checkboxInput("long_drop_na", "모든 측정값이 결측인 시점은 제외", FALSE),
    p(class = "hint", "선택하지 않은 열은 그대로 따라갑니다. 식별 변수의 조합은 원자료에서 행마다 달라야 합니다.")),
  conditionalPanel("input.manual_op === 'wide'",
    selectInput("wide_time", "시점 또는 반복 범주", choices = character()),
    selectInput("wide_values", "열로 펼칠 측정값", choices = character(), multiple = TRUE),
    textInput("wide_separator", "측정값과 시점 사이의 구분자", "_"),
    p(class = "hint", "score의 pre와 post 값은 score_pre, score_post 열로 만듭니다. 같은 대상의 같은 시점이 중복되면 중단합니다. 나머지 열은 대상 안에서 값이 같을 때만 유지합니다.")),
  conditionalPanel("input.manual_op === 'labels'", p(class = "hint", "라벨은 코드북 원문에서 확인합니다. 정의를 바꾸려면 코드북을 수정한 뒤 다시 연결하세요.")),
  textInput("manual_reason", "변경 이유", "", placeholder = "예: 척도 지침에 따라 세 문항의 평균 계산"),
  div(class = "editor-actions", actionButton("add_manual", "작업 저장", class = "btn-primary"),
    conditionalPanel("input.edit_rule !== 'new'", actionButton("delete_rule", "삭제"),
      actionButton("move_rule_up", "위로"), actionButton("move_rule_down", "아래로"))),
  p(class = "hint", "작업을 수정하면 해당 작업과 그 결과를 사용하는 작업의 선택이 해제됩니다. 순서를 바꾸면 모두 다시 선택해야 합니다.")
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
  div(class = "app-hero", h1("CodebookR"), p("코드북에 맞춰 자료를 정리하고, 연구에 필요한 전처리를 이어가세요.")),
  div(style = "display:none", textOutput("additional_mode")),
  tabsetPanel(id = "main_tabs",
    tabPanel("① 자료와 코드북", value = "data",
      h3("원자료와 코드북을 연결하세요"),
      p(class = "section-lead", "코드북을 기준으로 변수의 뜻, 응답 코드와 처리 지침을 확인합니다."),
      fluidRow(
        column(6, fileInput("data_file", "원자료", accept = c(".csv", ".sav", ".dta", ".rds"), buttonLabel = "파일 선택", placeholder = "원자료 파일"),
          p(class = "hint", "CSV, SPSS, Stata, RDS. 최대 100MB입니다."),
          tags$details(tags$summary("CSV 읽기 옵션"),
            selectInput("csv_encoding", "문자 인코딩", c("UTF-8" = "UTF-8", "한국어 Windows (CP949)" = "CP949", "EUC-KR" = "EUC-KR")),
            textInput("csv_character_columns", "문자로 읽을 변수명", "", placeholder = "예: respondent_id, region_code"),
            p(class = "hint", "쉼표로 구분합니다. 앞자리가 0이거나 16자리 이상인 정수 코드는 자동으로 문자로 읽습니다. 빈 칸과 NA는 결측으로 읽습니다. 읽기 옵션을 바꾸면 작업과 결과를 초기화합니다."))),
        column(6, fileInput("book_files", "코드북", accept = c(".pdf", ".xlsx", ".xls", ".csv"), multiple = TRUE, buttonLabel = "파일 선택", placeholder = "코드북 파일 (여러 개 가능)"),
          p(class = "hint", "Excel, CSV와 텍스트형 PDF를 읽습니다. 스캔 이미지와 복잡한 문서는 읽기 결과를 확인해 주세요."),
          actionButton("load_example", "예제 자료와 코드북으로 살펴보기"))
      ),
      div(class = "feedback", textOutput("dataset_status"), textOutput("data_feedback"), textOutput("book_feedback")),
      selectInput("raw_columns", "살펴볼 변수", choices = character(), multiple = TRUE),
      div(class = "table-wrapper", tableOutput("raw_sample")), p(class = "hint", "처음 12행, 선택한 변수 중 최대 10개를 표시합니다. 계산은 원자료를 보존한 채 이 컴퓨터에서 합니다."),
      div(class = "preview-box", textOutput("coverage")),
      tags$details(tags$summary("변수 연결 확인 또는 변경"),
        fluidRow(column(5, selectInput("mapping_variable", "코드북 변수", choices = character())), column(5, selectInput("mapping_target", "연결할 자료 변수", choices = character()))),
        textOutput("mapping_suggestion"), actionButton("save_mapping", "연결 저장"), div(class = "table-wrapper", tableOutput("mapping_table"))),
      div(class = "feedback", textOutput("mapping_feedback")),
      tags$details(tags$summary("코드북 읽기 결과와 원문"),
        div(class = "table-wrapper", tableOutput("book_summary")),
        p(class = "hint", "읽지 못한 구간이나 제한에 걸린 문서가 있는지 확인하세요. 발췌문은 문서 전체가 아닙니다."),
        div(class = "table-wrapper", tableOutput("book_records")), p(class = "hint", "처음 35개 정의입니다."), verbatimTextOutput("book_excerpts")),
      tags$details(tags$summary("자료형과 기존 라벨"), div(class = "table-wrapper", tableOutput("metadata_table")),
        p(class = "hint", "최대 120개 변수. SPSS 지정 결측 코드는 원래 코드와 속성을 유지하며 결측 수에 포함합니다.")),
      tags$details(tags$summary("작업 저장과 다시 열기"),
        p(class = "hint", "변수 연결, 검토 기록과 작업 설정을 저장합니다. API 키와 응답자 자료는 포함하지 않습니다. 다시 열 때 같은 원자료와 코드북을 먼저 연결하고 적용할 작업을 다시 선택하세요."),
        downloadButton("save_project", "작업 파일 저장"),
        fileInput("open_project", "저장한 작업 열기", accept = ".json", buttonLabel = "파일 선택", placeholder = "CodebookR 작업 파일"),
        div(class = "feedback", textOutput("project_feedback"))),
      div(class = "next-step", actionButton("go_codebook", "코드북 검토", class = "btn-primary"))
    ),
    tabPanel("② 검토", value = "codebook",
      h3(textOutput("workspace_title")), p(class = "section-lead", textOutput("workspace_note")),
      conditionalPanel("output.additional_mode === 'codebook'",
        tags$details(tags$summary("AI로 코드북 읽기"), ai_controls)),
      fluidRow(
        column(4, div(class = "work-list", h4("적용할 작업"),
          p(class = "hint", "위에서 아래로 적용합니다. 선택하면 오른쪽에서 예상 결과를 볼 수 있습니다."),
          checkboxGroupInput("approved_rules", NULL, choices = character()),
          conditionalPanel("output.additional_mode === 'codebook'", actionButton("refresh_heuristics", "새 코드북 후보 찾기")),
          actionButton("undo_rule", "직전 변경 되돌리기")),
          tags$details(tags$summary("작업 추가 또는 수정"), operation_editor),
          div(class = "feedback", role = "status", textOutput("rule_feedback"))),
        column(8,
          selectInput("review_variable", "살펴볼 변수", choices = character()),
          div(class = "feedback", textOutput("review_findings")),
          h4("코드북 근거"), div(class = "table-wrapper", tableOutput("review_evidence")),
          p(class = "hint", "정의는 최대 100개 표시합니다. AI가 읽은 정의도 원문과 뜻이 맞는지 확인하세요."),
          tags$details(tags$summary("원자료 값과 빈도"), div(class = "table-wrapper", tableOutput("review_frequency")), p(class = "hint", "빈도가 높은 값부터 최대 25개 표시합니다.")),
          div(class = "preview-box", h4("적용 전후 미리보기"), textOutput("preview_status"),
            div(class = "table-wrapper", tableOutput("review_sample")),
            p(class = "hint", "처음 12행입니다. 자료 구조를 바꾸면 결과 표를 최대 10열까지 보여 줍니다.")),
          conditionalPanel("output.additional_mode === 'codebook'", tags$details(tags$summary("변수 검토 기록 남기기"),
            selectInput("review_decision", "판단", c("확인함", "보류")), textInput("review_reason", "확인 또는 보류한 이유", ""), actionButton("save_decision", "검토 기록 저장")))
        )
      ),
      tags$details(tags$summary("전체 변수의 확인사항"), div(class = "table-wrapper", tableOutput("review_overview"))),
      tags$details(tags$summary("작업별 예상 적용 수와 계산 기준"), div(class = "table-wrapper", tableOutput("preview_table"))),
      div(class = "next-step", textOutput("execution_plan_summary"),
        actionButton("run_button", "선택한 작업 적용", class = "btn-primary"),
        conditionalPanel("output.additional_mode === 'codebook'", actionButton("run_unchanged", "변경 없이 결과 확인")),
        div(class = "feedback", role = "status", textOutput("execution_error")))
    ),
    tabPanel("③ 결과", value = "execute",
      h3("처리 결과를 확인하세요"), div(class = "feedback", role = "status", textOutput("run_status")),
      uiOutput("download_controls"),
      selectInput("result_columns", "살펴볼 결과 변수", choices = character(), multiple = TRUE),
      div(class = "table-wrapper", tableOutput("result_sample")), p(class = "hint", "처음 12행, 최대 10열을 표시합니다."),
      div(class = "preview-box", textOutput("result_review")),
      tags$details(tags$summary("기본 처리 후 코드북과 다시 대조한 결과"), div(class = "table-wrapper", tableOutput("post_review"))),
      tags$details(tags$summary("적용 기록과 결과 자료형"), div(class = "table-wrapper", tableOutput("audit_table")), div(class = "table-wrapper", tableOutput("after_metadata"))),
      div(class = "next-step", textOutput("base_status"), uiOutput("base_controls")),
      p(class = "hint", "ZIP에 결과 데이터, 적용한 작업, 코드북 근거와 검토 기록, 원자료부터 다시 실행할 R 코드를 담습니다. 작업을 보류했거나 읽지 못한 지침이 있으면 분석 전에 확인하세요.")
    )
  ),
  tags$footer(class = "hint", style = "margin:20px 4px 30px;", "CodebookR")
)
source(file.path("R", "server.R"), encoding = "UTF-8")
shinyApp(ui = ui, server = codebookr_server)
