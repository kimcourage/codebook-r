# Restricted transformation engine. No eval(parse()), system(), or AI-written code.

empty_rules <- function() {
  data.frame(
    id = integer(), variable = character(), operation = character(),
    values = character(), minimum = numeric(), maximum = numeric(),
    reason = character(), evidence = character(), confidence = character(),
    source = character(), stringsAsFactors = FALSE
  )
}

split_codes <- function(text) {
  if (is.na(text) || !nzchar(trimws(text))) return(character())
  codes <- unique(trimws(strsplit(as.character(text), ",", fixed = TRUE)[[1]]))
  codes[nzchar(codes)]
}

match_variable <- function(candidate, data_names) {
  if (candidate %in% data_names) return(candidate)
  matches <- which(tolower(candidate) == tolower(data_names))
  if (length(matches) == 1L) return(data_names[[matches]])
  NA_character_
}

check_rule <- function(rule, data) {
  if (!is.data.frame(data)) stop("data.frame이 아닙니다.", call. = FALSE)
  rule <- normalize_rules(rule)
  rule_parameters(rule)
  if (rule$operation[[1]] %in% research_operations) return(check_research_rule(rule, data))
  nm <- as.character(rule$variable[[1]])
  op <- as.character(rule$operation[[1]])
  if (is.na(nm) || !(nm %in% names(data))) {
    stop("데이터에 존재하지 않는 변수입니다.", call. = FALSE)
  }
  if (!op %in% c("set_na", "reverse", "factor")) {
    stop("허용되지 않은 전처리 작업입니다.", call. = FALSE)
  }
  if (op == "set_na") {
    codes <- split_codes(rule$values[[1]])
    if (!length(codes) || length(codes) > 30L || any(nchar(codes) > 80L) ||
        any(grepl("[\r\n]", codes))) {
      stop("결측값 코드는 쉼표로 구분한 1~30개 값이어야 합니다.", call. = FALSE)
    }
  }
  if (op == "reverse") {
    x <- data[[nm]]
    min_value <- suppressWarnings(as.numeric(rule$minimum[[1]]))
    max_value <- suppressWarnings(as.numeric(rule$maximum[[1]]))
    if (!(is.numeric(x) || is.integer(x)) || inherits(x, "Date") ||
        inherits(x, "POSIXt") || !is.finite(min_value) ||
        !is.finite(max_value) || min_value >= max_value) {
      stop("역코딩에는 숫자 변수와 유효한 최소/최대값이 필요합니다.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

apply_approved_rules <- function(data, rules) {
  if (!is.data.frame(data)) stop("data.frame이 아닙니다.", call. = FALSE)
  cleaned <- data  # Copy on modify; original object is not overwritten.
  audit <- list()
  if (!nrow(rules)) {
    return(list(data = cleaned, audit = data.frame(
      id = integer(), variable = character(), operation = character(),
      n_affected = integer(), missing_before = integer(), missing_after = integer(),
      rows_before = integer(), rows_after = integer(), columns_before = integer(), columns_after = integer(), details = character(), note = character(),
      stringsAsFactors = FALSE
    )))
  }
  for (i in seq_len(nrow(rules))) {
    rule <- rules[i, , drop = FALSE]
    check_rule(rule, cleaned)
    nm <- rule$variable[[1]]
    op <- rule$operation[[1]]
    x <- cleaned[[nm]]
    variable_label <- attr(x, "label", exact = TRUE)
    before <- sum(is.na(x))
    n_affected <- 0L
    details <- "{}"; note <- ""
    rows_before <- nrow(cleaned); columns_before <- ncol(cleaned)
    if (op %in% research_operations) {
      step <- apply_research_rule(cleaned, rule)
      cleaned <- step$data; n_affected <- step$n_affected; before <- step$missing_before
      after <- step$missing_after; details <- step$details; note <- step$note
    } else if (op == "set_na") {
      codes <- split_codes(rule$values[[1]])
      # SPSS user-missing codes are preserved at import; an explicit rule can
      # normalize them to ordinary NA even though is.na(labelled_spss) is TRUE.
      raw <- if (inherits(x, "haven_labelled")) unclass(x) else x
      hit <- !is.na(raw) & (as.character(raw) %in% codes)
      n_affected <- sum(hit)
      details <- encode_parameters(list(original_codes = as.data.frame(table(as.character(raw[hit]))), source_variable = nm, affected_rows = data.frame(row = which(hit), code = as.character(raw[hit]), stringsAsFactors = FALSE)))
      x[hit] <- NA
    } else if (op == "reverse") {
      min_value <- as.numeric(rule$minimum[[1]])
      max_value <- as.numeric(rule$maximum[[1]])
      values <- as.numeric(x)
      if (any(values[!is.na(values)] < min_value |
              values[!is.na(values)] > max_value)) {
        stop(sprintf("%s의 값이 역코딩 범위 [%g, %g]를 벗어납니다. 결측 처리 규칙을 먼저 적용하거나 범위를 수정하세요.",
                     nm, min_value, max_value), call. = FALSE)
      }
      n_affected <- sum(!is.na(values))
      x <- ifelse(is.na(values), NA_real_, min_value + max_value - values)
    } else if (op == "factor") {
      n_affected <- sum(!is.na(x))
      # In MVP this preserves observed codes, NOT codebook value labels.
      values <- as.character(x)
      values[is.na(x)] <- NA_character_
      x <- factor(values, exclude = NA)
    }
    if (!op %in% research_operations) {
      if (!is.null(variable_label)) attr(x, "label") <- variable_label
      cleaned[[nm]] <- x
      after <- sum(is.na(x))
    }
    audit[[length(audit) + 1L]] <- data.frame(
      id = as.integer(rule$id[[1]]), variable = nm, operation = op,
      n_affected = as.integer(n_affected), missing_before = before,
      missing_after = after, rows_before = rows_before, rows_after = nrow(cleaned),
      columns_before = columns_before, columns_after = ncol(cleaned),
      details = details, note = note, stringsAsFactors = FALSE
    )
  }
  list(data = cleaned, audit = do.call(rbind, audit))
}

# Exact codebook evidence only: a low-confidence suggestion requiring approval.
heuristic_missing_rules <- function(records, data) {
  out <- empty_rules()
  if (is.null(records) || !nrow(records) || !ncol(data)) return(out)
  keyword <- "무응답|응답.?거절|응답.?거부|잘.?모름|모르겠|비해당|결측|미응답|해당.?없음|missing|refus|don't know|do not know|not applicable|no response|n/a"
  hit <- grepl(keyword, records$value_label, ignore.case = TRUE, perl = TRUE) |
    grepl(keyword, records$note, ignore.case = TRUE, perl = TRUE)
  negation <- grepl("결측.?아님|결측.?값.?아님|not missing|valid code|missing.?아님",
                    paste(records$value_label, records$note),
                    ignore.case = TRUE, perl = TRUE)
  hit <- hit & !negation
  eligible <- records[hit & nzchar(records$value_code), , drop = FALSE]
  if (!nrow(eligible)) return(out)
  eligible <- eligible[grepl("^[[:alnum:]_.-]{1,40}$", eligible$value_code), , drop = FALSE]
  if (!nrow(eligible)) return(out)
  eligible$matched <- vapply(eligible$variable, match_variable,
                             character(1), data_names = names(data))
  eligible <- eligible[!is.na(eligible$matched), , drop = FALSE]
  if (!nrow(eligible)) return(out)
  for (v in unique(eligible$matched)) {
    rows <- eligible[eligible$matched == v, , drop = FALSE]
    codes <- unique(rows$value_code)
    out <- rbind(out, data.frame(
      id = nrow(out) + 1L, variable = v, operation = "set_na",
      values = paste(codes, collapse = ","), minimum = NA_real_,
      maximum = NA_real_, reason = "코드북의 명시적 결측/무응답 범주 (확인 필요)",
      evidence = paste(unique(paste(rows$source_file, rows$source_loc,
                                    rows$value_code, rows$value_label, sep = " / ")),
                       collapse = "; "),
      confidence = "medium", source = "codebook", stringsAsFactors = FALSE
    ))
  }
  out
}

append_rule_candidates <- function(existing, candidates, data) {
  existing <- normalize_rules(existing)
  if (is.null(candidates) || !nrow(candidates)) return(existing)
  if (!all(names(empty_rules())[-1] %in% names(candidates))) stop("규칙 제안의 필수 필드가 누락되었습니다.", call. = FALSE)
  candidates <- normalize_rules(candidates)
  for (i in seq_len(nrow(candidates))) {
    row <- candidates[i, names(existing), drop = FALSE]
    if (row$operation %in% research_operations && row$source == "AI") next
    if (!row$operation %in% research_operations) {
      matched <- match_variable(as.character(row$variable[[1]]), names(data))
      if (is.na(matched)) next
      row$variable <- matched
    }
    row$id <- if (nrow(existing)) max(existing$id) + 1L else 1L
    fields <- c("variable", "operation", "values", "minimum", "maximum", "schema_version", "parameters")
    signature <- function(x) encode_parameters(as.list(x[1, fields, drop = FALSE]))
    if (nrow(existing) && any(vapply(seq_len(nrow(existing)), function(j) identical(signature(existing[j, , drop = FALSE]), signature(row)), logical(1)))) next
    valid <- tryCatch({check_rule(row, data); TRUE}, error = function(e) FALSE)
    if (valid) existing <- rbind(existing, row)
  }
  rownames(existing) <- NULL
  existing
}

operation_label <- function(op) {
  labels <- c(set_na = "결측값 처리", reverse = "역코딩", factor = "범주형 변환",
    composite = "합성변수", center = "중심화", standardize = "표준화", long = "wide → long", wide = "long → wide", labels = "변수 설명과 값 라벨")
  unname(labels[op])
}

rule_description <- function(rule) {
  op <- rule$operation[[1]]
  if (op %in% research_operations) {
    p <- rule_parameters(rule)
    if (op == "labels") return(if (length(p$codes)) paste0("변수 설명과 값 라벨 ", length(p$codes), "개 적용") else "변수 설명 적용")
    if (op == "composite") return(sprintf("%s의 %s → %s (최소 %d문항)", paste(p$inputs, collapse = ", "), ifelse(p$method == "mean", "평균", "합계"), p$output, p$min_valid))
    if (op %in% c("center", "standardize")) return(paste0(p$input, " → ", p$output, " (", if (p$scope == "all") "전체" else paste(p$groups, collapse = ", "), " 기준)"))
    return(if (op == "long") paste0(paste(p$columns, collapse = ", "), "를 행으로, 시점: ", p$time) else paste0(paste(p$values, collapse = ", "), "를 ", p$time, "별 열로"))
  }
  if (op == "set_na") return(paste0(rule$values[[1]], " → 결측값"))
  if (op == "reverse") return(sprintf("%g–%g 범위에서 방향 반전", rule$minimum[[1]], rule$maximum[[1]]))
  "관측 코드를 범주로 변환"
}

rule_table_display <- function(rules, selected_keys = character(), keys = character()) {
  if (!nrow(rules)) return(NULL)
  data.frame(
    순서 = seq_len(nrow(rules)), 번호 = rules$id, 변수 = rules$variable,
    작업 = operation_label(rules$operation),
    변경 = vapply(seq_len(nrow(rules)), function(i) rule_description(rules[i, , drop = FALSE]), character(1)),
    선택 = ifelse(keys %in% selected_keys, "실행 대상", "미선택"),
    이유 = rules$reason, 근거 = rules$evidence,
    제안자 = ifelse(rules$source == "AI", "AI", ifelse(rules$source == "codebook", "코드북", "연구자")),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

# Preview uses the execution engine, in the same order, on a copy of the data.
# A failure blocks subsequent estimates instead of showing a partial plan as safe.
preview_rules <- function(data, rules) {
  current <- data
  rows <- list()
  failed <- ""
  for (i in seq_len(nrow(rules))) {
    rule <- rules[i, , drop = FALSE]
    nm <- rule$variable[[1]]
    before <- if (nm %in% names(current)) current[[nm]] else NULL
    row <- data.frame(id = rule$id, variable = nm, operation = rule$operation,
      n_affected = NA_integer_, missing_before = if (is.null(before)) NA_integer_ else sum(is.na(before)),
      missing_after = NA_integer_, rows_before = nrow(current), rows_after = NA_integer_, columns_before = ncol(current), columns_after = NA_integer_, details = "{}",
      type_before = if (rule$operation %in% c("long", "wide")) "표" else if (is.null(before)) "새 변수" else paste(class(before), collapse = "/"),
      type_after = "", status = "", note = "", stringsAsFactors = FALSE)
    if (nzchar(failed)) {
      row$status <- "대기"
      row$note <- "앞선 규칙의 오류를 먼저 수정해 주세요."
    } else {
      step <- tryCatch(apply_approved_rules(current, rule), error = function(e) e)
      if (inherits(step, "error")) {
        failed <- sprintf("#%s %s: %s", rule$id[[1]], nm, conditionMessage(step))
        row$status <- "수정 필요"
        row$note <- conditionMessage(step)
      } else {
        row$n_affected <- step$audit$n_affected
        row$missing_after <- step$audit$missing_after
        row$type_after <- if (rule$operation %in% c("long", "wide")) "표" else paste(class(step$data[[nm]]), collapse = "/")
        row$rows_after <- nrow(step$data); row$columns_after <- ncol(step$data)
        row$missing_before <- step$audit$missing_before
        row$details <- step$audit$details
        row$status <- "실행 가능"
        notices <- step$audit$note[nzchar(step$audit$note)]
        if (row$n_affected == 0L) notices <- c(notices, "적용 대상이 없습니다. 변수와 코드 값을 확인해 주세요.")
        if (rule$operation[[1]] == "factor") notices <- c(notices, "순서 없는 범주형입니다. 코드북 값 라벨은 자동 적용하지 않습니다.")
        if (rule$operation[[1]] %in% c("reverse", "factor") && length(attr(before, "labels", exact = TRUE))) {
          notices <- c(notices, "기존 값 라벨은 제거됩니다. 변수 설명은 유지합니다.")
        }
        row$note <- paste(notices, collapse = " ")
        current <- step$data
      }
    }
    rows[[i]] <- row
  }
  list(ok = nrow(rules) > 0L && !nzchar(failed), error = failed,
       report = if (length(rows)) do.call(rbind, rows) else data.frame(), data = current)
}

preview_table_display <- function(preview) {
  x <- preview$report
  if (!nrow(x)) return(NULL)
  data.frame(번호 = x$id, 변수 = x$variable, 작업 = operation_label(x$operation),
    `적용 대상 수` = x$n_affected,
    `결측 수 (전 → 후)` = paste(x$missing_before, "→", x$missing_after),
    `자료형 (전 → 후)` = paste(x$type_before, "→", x$type_after),
    `행 수` = paste(x$rows_before, "→", x$rows_after),
    `열 수` = paste(x$columns_before, "→", x$columns_after),
    상태 = x$status, 확인사항 = x$note, check.names = FALSE)
}

edit_rule_candidate <- function(existing, index, candidate, data) {
  if (length(index) != 1L || is.na(index) || !index %in% seq_len(nrow(existing))) {
    stop("수정할 규칙이 바뀌었습니다. 목록에서 다시 선택해 주세요.", call. = FALSE)
  }
  existing <- normalize_rules(existing); candidate <- normalize_rules(candidate)
  check_rule(candidate, data)
  others <- existing[-index, , drop = FALSE]
  if (nrow(append_rule_candidates(others, candidate, data)) == nrow(others)) {
    stop("동일한 작업이 이미 있습니다. 기존 규칙을 선택해 주세요.", call. = FALSE)
  }
  original <- existing[index, , drop = FALSE]
  candidate$id <- original$id
  candidate$source <- "manual"
  candidate$confidence <- "manual"
  candidate$evidence <- substr(paste0("수정 전: ", rule_description(original),
    " / 원래 근거: ", original$evidence[[1]]), 1L, 2000L)
  existing[index, ] <- candidate[, names(existing), drop = FALSE]
  existing
}
