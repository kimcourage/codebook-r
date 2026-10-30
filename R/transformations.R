# Versioned, structured research operations. Parameters are data, never R code.
research_operations <- c("composite", "center", "standardize", "long", "wide", "labels")

normalize_rules <- function(rules) {
  if (!"schema_version" %in% names(rules)) rules$schema_version <- rep(1L, nrow(rules))
  if (!"parameters" %in% names(rules)) rules$parameters <- rep("{}", nrow(rules))
  rules
}

encode_parameters <- function(x) {
  # JSON records contain the underlying group codes. Label metadata stays on
  # the dataset; jsonlite cannot safely serialize every labelled vector class.
  json_values <- function(value) {
    if (inherits(value, "haven_labelled")) {
      value <- unclass(value); attributes(value) <- NULL
    } else if (is.data.frame(value)) {
      value[] <- lapply(value, json_values)
    } else if (is.list(value)) value <- lapply(value, json_values)
    value
  }
  as.character(jsonlite::toJSON(json_values(x), auto_unbox = TRUE,
    null = "null", na = "null", digits = NA, dataframe = "rows"))
}

rule_parameters <- function(rule) {
  rule <- normalize_rules(rule)
  if (length(rule$schema_version) != 1L || is.na(rule$schema_version) ||
      !rule$schema_version %in% c(1L, 2L)) stop("지원하지 않는 작업 형식입니다.", call. = FALSE)
  if (!rule$operation[[1]] %in% research_operations) return(list())
  if (rule$schema_version != 2L) stop("새 작업의 설정 형식이 맞지 않습니다.", call. = FALSE)
  p <- tryCatch(jsonlite::fromJSON(rule$parameters[[1]], simplifyVector = TRUE), error = function(e) NULL)
  if (!is.list(p)) stop("작업 설정을 읽을 수 없습니다.", call. = FALSE)
  p
}

new_research_rule <- function(operation, parameters, reason, id = 1L) {
  target <- if (operation %in% c("long", "wide")) "자료 전체" else parameters$output
  data.frame(id = as.integer(id), variable = target, operation = operation, values = "",
    minimum = NA_real_, maximum = NA_real_, reason = reason, evidence = "연구자 지정",
    confidence = "manual", source = "manual", schema_version = 2L,
    parameters = encode_parameters(parameters), stringsAsFactors = FALSE)
}

require_text <- function(x, label) {
  if (length(x) != 1L || !is.character(x) || is.na(x) || !nzchar(trimws(x)) || grepl("[\r\n\t]", x)) {
    stop(paste0(label, "을(를) 입력해 주세요."), call. = FALSE)
  }
  x
}
require_columns <- function(data, columns, label, empty = FALSE) {
  if (is.null(columns)) columns <- character()
  if (!is.character(columns) || anyNA(columns) || anyDuplicated(columns) ||
      (!empty && !length(columns)) || any(!columns %in% names(data))) {
    stop(paste0(label, ": 앞선 작업 결과에 있는 변수를 선택해 주세요."), call. = FALSE)
  }
  columns
}
new_column <- function(data, name) {
  require_text(name, "새 변수 이름")
  if (name %in% names(data)) stop(paste0("이미 있는 변수 이름입니다: ", name), call. = FALSE)
}
plain_numeric <- function(x, name) {
  if (!is.numeric(x) || is.complex(x) || inherits(x, c("Date", "POSIXt", "difftime"))) {
    stop(paste0(name, ": 숫자 변수가 필요합니다."), call. = FALSE)
  }
  values <- as.numeric(x)
  values[is.na(x)] <- NA_real_ # Includes labelled SPSS user missings.
  if (any(!is.na(values) & !is.finite(values))) stop(paste0(name, ": 무한값이 있습니다."), call. = FALSE)
  values
}
check_keys <- function(data, columns, unique = FALSE) {
  require_columns(data, columns, "식별 변수")
  if (any(vapply(data[columns], function(x) any(is.na(x)) || any(trimws(as.character(x)) == "", na.rm = TRUE), logical(1)))) {
    stop("식별 변수나 시점에 결측값 또는 빈 값이 있습니다.", call. = FALSE)
  }
  if (unique && anyDuplicated(data[columns])) stop("식별 변수와 시점의 조합이 중복됩니다. 중복을 먼저 확인해 주세요. 자동으로 합치지 않습니다.", call. = FALSE)
}

check_reshape_size <- function(rows, columns) {
  if (as.double(rows) * as.double(columns) > 5e6) {
    stop("변환 결과가 500만 셀을 넘습니다. 선택한 시점과 변수를 줄여 주세요.", call. = FALSE)
  }
}

long_mapping <- function(data, p) {
  columns <- require_columns(data, p$columns, "반복 측정 변수")
  require_text(p$time, "시점 변수 이름")
  if (identical(p$layout, "single")) {
    require_text(p$value, "측정값 변수 이름")
    times <- columns
    if (!is.null(p$prefix) && nzchar(p$prefix)) {
      if (any(!startsWith(columns, p$prefix))) stop("선택한 변수 이름이 모두 지정한 접두어로 시작해야 합니다.", call. = FALSE)
      times <- substring(columns, nchar(p$prefix) + 1L)
    }
    measures <- rep(p$value, length(columns))
  } else if (identical(p$layout, "multiple")) {
    require_text(p$separator, "이름 구분자")
    pieces <- strsplit(columns, p$separator, fixed = TRUE)
    if (any(lengths(pieces) < 2L)) stop("변수 이름을 측정값과 시점으로 나눌 수 없습니다. 구분자를 확인해 주세요.", call. = FALSE)
    times <- vapply(pieces, tail, character(1), n = 1L)
    measures <- vapply(pieces, function(x) paste(head(x, -1L), collapse = p$separator), character(1))
  } else stop("long 변환의 이름 읽기 방식을 선택해 주세요.", call. = FALSE)
  if (any(!nzchar(times)) || any(!nzchar(measures))) stop("측정값 이름이나 시점 이름이 비어 있습니다.", call. = FALSE)
  mapping <- data.frame(.name = columns, .value = measures, stringsAsFactors = FALSE)
  mapping[[p$time]] <- times
  if (anyDuplicated(mapping[c(".value", p$time)])) stop("같은 측정값과 시점으로 연결되는 변수가 둘 이상입니다.", call. = FALSE)
  reserved <- setdiff(names(data), columns)
  if (p$time %in% c(".name", ".value") || p$time %in% c(reserved, measures) || any(measures %in% reserved)) {
    stop("변환 후 변수 이름이 기존 변수 또는 시점 변수와 겹칩니다.", call. = FALSE)
  }
  # Combining differently labelled waves would silently change the interpretation.
  for (measure in unique(measures)) {
    attrs <- lapply(data[columns[measures == measure]], function(x) attributes(x)[c("labels", "na_values", "na_range", "label")])
    if (length(attrs) > 1L && !all(vapply(attrs, identical, logical(1), attrs[[1]]))) {
      stop(paste0(measure, ": 시점별 값 라벨이나 결측 정의가 다릅니다. 먼저 일치시켜 주세요."), call. = FALSE)
    }
  }
  mapping
}

check_research_rule <- function(rule, data) {
  p <- rule_parameters(rule); op <- rule$operation[[1]]
  if (op %in% c("composite", "center", "standardize")) {
    new_column(data, p$output)
    if (!identical(rule$variable[[1]], p$output)) stop("작업의 출력 변수와 설정이 일치하지 않습니다.", call. = FALSE)
  }
  if (op == "labels") {
    require_text(p$input, "변수"); require_columns(data, p$input, "변수")
    if (!identical(rule$variable[[1]], p$input) || !identical(p$output, p$input)) stop("라벨의 대상 변수가 다릅니다.", call. = FALSE)
    x <- data[[p$input]]
    if (!(is.numeric(x) || is.character(x)) || is.complex(x) || inherits(x, c("Date", "POSIXt", "difftime"))) stop("숫자 또는 문자 변수에 라벨을 적용할 수 있습니다.", call. = FALSE)
    if (length(p$codes) != length(p$labels) || anyNA(p$codes) || anyNA(p$labels) || any(!nzchar(p$labels)) || length(p$codes) > 5000L) stop("값 라벨 정의를 확인해 주세요.", call. = FALSE)
    codes <- if (is.numeric(x)) suppressWarnings(as.numeric(p$codes)) else as.character(p$codes)
    if (anyNA(codes) || anyDuplicated(codes) || (is.numeric(codes) && any(!is.finite(codes)))) stop("라벨 코드가 중복되거나 변수의 자료형과 맞지 않습니다.", call. = FALSE)
    if (length(p$label) != 1L || is.na(p$label)) stop("변수 설명을 확인해 주세요.", call. = FALSE)
  } else if (op == "composite") {
    cols <- require_columns(data, p$inputs, "합칠 문항")
    if (!p$method %in% c("mean", "sum")) stop("합계 또는 평균을 선택해 주세요.", call. = FALSE)
    if (length(p$min_valid) != 1L || !is.numeric(p$min_valid) || !is.finite(p$min_valid) ||
        p$min_valid != floor(p$min_valid) || p$min_valid < 1L || p$min_valid > length(cols)) {
      stop("최소 응답 수는 1 이상, 선택한 문항 수 이하여야 합니다.", call. = FALSE)
    }
    invisible(lapply(cols, function(nm) plain_numeric(data[[nm]], nm)))
  } else if (op %in% c("center", "standardize")) {
    require_columns(data, p$input, "계산할 변수"); require_text(p$input, "계산할 변수")
    plain_numeric(data[[p$input]], p$input)
    if (!p$scope %in% c("all", "group", "person")) stop("계산 기준을 선택해 주세요.", call. = FALSE)
    if (p$scope != "all") check_keys(data, p$groups)
    if (p$scope == "all" && length(p$groups)) stop("전체 자료 기준에는 묶음 변수를 지정할 수 없습니다.", call. = FALSE)
    if (!p$reference %in% c("all", "subset", "fixed")) stop("기준값 계산 방식을 선택해 주세요.", call. = FALSE)
    if (p$reference == "subset") {
      require_text(p$reference_column, "기준 자료를 고를 변수")
      require_columns(data, p$reference_column, "기준 자료를 고를 변수")
      if (!length(p$reference_values) || anyNA(p$reference_values)) stop("기준 자료에 포함할 값을 선택해 주세요.", call. = FALSE)
    }
    if (p$reference == "fixed") {
      if (p$scope != "all") stop("기준값 직접 입력은 전체 자료에 같은 값을 적용할 때 사용할 수 있습니다.", call. = FALSE)
      if (length(p$center) != 1L || !is.numeric(p$center) || !is.finite(p$center)) stop("유효한 중심값을 입력해 주세요.", call. = FALSE)
      if (op == "standardize" && (length(p$scale) != 1L || !is.numeric(p$scale) || !is.finite(p$scale) || p$scale <= 0)) stop("표준편차는 0보다 커야 합니다.", call. = FALSE)
    }
  } else if (op == "long") {
    check_keys(data, p$ids, unique = TRUE)
    if (length(intersect(p$ids, p$columns))) stop("식별 변수는 반복 측정 변수로 선택할 수 없습니다.", call. = FALSE)
    if (!is.logical(p$drop_na) || length(p$drop_na) != 1L || is.na(p$drop_na)) stop("결측 시점 처리 방식을 선택해 주세요.", call. = FALSE)
    long_mapping(data, p)
  } else if (op == "wide") {
    require_text(p$time, "시점 변수"); require_text(p$separator, "이름 구분자")
    require_columns(data, p$values, "펼칠 측정값")
    if (anyDuplicated(c(p$ids, p$time, p$values))) stop("식별 변수, 시점, 측정값은 서로 다른 변수를 골라 주세요.", call. = FALSE)
    check_keys(data, c(p$ids, p$time), unique = TRUE)
    require_columns(data, p$ids, "식별 변수")
  }
  if (!nrow(data)) stop("계산할 행이 없습니다.", call. = FALSE)
  invisible(TRUE)
}

apply_research_rule <- function(data, rule) {
  check_research_rule(rule, data)
  p <- rule_parameters(rule); op <- rule$operation[[1]]; out <- data
  details <- list(); before <- 0L; note <- ""
  if (op == "labels") {
    x <- data[[p$input]]; before <- sum(is.na(x)); after <- before
    codes <- if (is.numeric(x)) as.numeric(p$codes) else as.character(p$codes)
    labels <- stats::setNames(codes, as.character(p$labels))
    if (inherits(x, "haven_labelled")) {
      if (length(labels)) attr(x, "labels") <- labels
      if (nzchar(p$label)) attr(x, "label") <- p$label
    } else {
      x <- haven::labelled(x, labels = if (length(labels)) labels else NULL,
        label = if (nzchar(p$label)) p$label else attr(x, "label", exact = TRUE))
    }
    out[[p$input]] <- x; affected <- length(x)
    details <- list(codes = p$codes, labels = p$labels, label = p$label)
    note <- sprintf("값은 유지하고 변수 설명과 값 라벨 %d개를 적용합니다.", length(labels))
  } else if (op == "composite") {
    m <- do.call(cbind, lapply(p$inputs, function(nm) plain_numeric(data[[nm]], nm)))
    valid <- rowSums(!is.na(m)); y <- rowSums(m, na.rm = TRUE)
    if (p$method == "mean") y <- y / pmax(valid, 1L)
    y[valid < p$min_valid] <- NA_real_
    if (any(!is.na(y) & !is.finite(y))) stop("계산 결과가 너무 큽니다. 입력값을 확인해 주세요.", call. = FALSE)
    out[[p$output]] <- y
    details <- list(valid_item_counts = as.data.frame(table(valid)), min_valid = p$min_valid,
      scored = sum(!is.na(y)), unscored = sum(is.na(y)))
    note <- sprintf("%d문항 중 %d개 이상 응답: %d행 계산, %d행 결측", length(p$inputs), p$min_valid, sum(!is.na(y)), sum(is.na(y)))
    affected <- sum(!is.na(y)); after <- sum(is.na(y))
  } else if (op %in% c("center", "standardize")) {
    x <- plain_numeric(data[[p$input]], p$input); before <- sum(is.na(x))
    grouping <- if (p$scope == "all") rep(1L, nrow(data)) else as.integer(vctrs::vec_group_id(data[p$groups]))
    reference <- rep(TRUE, nrow(data))
    if (p$reference == "subset") reference <- !is.na(data[[p$reference_column]]) & as.character(data[[p$reference_column]]) %in% as.character(p$reference_values)
    y <- rep(NA_real_, length(x)); stats <- list()
    for (rows in split(seq_along(grouping), grouping)) {
      g <- grouping[[rows[[1]]]]
      ref <- rows[reference[rows] & !is.na(x[rows])]
      if (p$reference == "fixed") { center <- p$center; scale <- if (op == "standardize") p$scale else 1 } else {
        if (!length(ref)) stop(sprintf("묶음 %d에 기준값을 계산할 유효 응답이 없습니다.", g), call. = FALSE)
        center <- mean(x[ref]); scale <- if (op == "standardize") stats::sd(x[ref]) else 1
        if (!is.finite(center) || !is.finite(scale) || scale <= 0) stop(sprintf("묶음 %d의 표준편차를 계산할 수 없습니다. 유효 응답 2개 이상과 서로 다른 값이 필요합니다.", g), call. = FALSE)
      }
      y[rows] <- (x[rows] - center) / scale
      group <- if (p$scope == "all") data.frame(group = "전체") else data[rows[[1]], p$groups, drop = FALSE]
      names(group) <- paste0("기준_", names(group))
      group$reference_n <- if (p$reference == "fixed") NA_integer_ else length(ref)
      group$center <- center; group$scale <- scale; stats[[length(stats) + 1L]] <- group
    }
    if (any(!is.na(y) & !is.finite(y))) stop("계산 결과가 너무 큽니다. 기준값을 확인해 주세요.", call. = FALSE)
    out[[p$output]] <- y; table <- do.call(rbind, stats); rownames(table) <- NULL
    details <- list(reference = p$reference, scope = p$scope, constants = table,
      sd_definition = if (op == "standardize" && p$reference != "fixed") "sample n-1" else "specified or unused")
    note <- paste(vapply(seq_len(min(nrow(table), 6L)), function(i) sprintf("%s: n=%s, 중심=%g%s",
      paste(as.character(table[i, seq_len(ncol(table) - 3L), drop = TRUE]), collapse = "/"),
      ifelse(is.na(table$reference_n[[i]]), "직접 입력", table$reference_n[[i]]), table$center[[i]],
      if (op == "standardize") sprintf(", 표준편차=%g", table$scale[[i]]) else ""), character(1)), collapse = "; ")
    if (nrow(table) > 6L) note <- paste0(note, "; 나머지 기준값은 다운로드한 적용 기록에서 확인할 수 있습니다.")
    affected <- sum(!is.na(y)); after <- sum(is.na(y))
  } else if (op == "long") {
    mapping <- long_mapping(data, p)
    check_reshape_size(as.double(nrow(data)) * length(unique(mapping[[p$time]])),
      length(setdiff(names(data), p$columns)) + 1L + length(unique(mapping$.value)))
    out <- as.data.frame(tidyr::pivot_longer_spec(data, mapping, names_repair = "check_unique", values_drop_na = p$drop_na))
    check_keys(out, c(p$ids, p$time), unique = TRUE)
    before <- sum(vapply(data[p$columns], function(x) sum(is.na(x)), integer(1)))
    after <- sum(vapply(out[unique(mapping$.value)], function(x) sum(is.na(x)), integer(1)))
    details <- list(mapping = mapping, ids = p$ids, dropped_rows = nrow(data) * length(unique(mapping[[p$time]])) - nrow(out))
    note <- paste0(nrow(data), "행 → ", nrow(out), "행. ", paste(p$ids, collapse = ", "), "와 ", p$time, " 조합 확인. ",
      if (p$drop_na) paste0("모든 측정값이 결측인 시점 ", details$dropped_rows, "행 제외.") else "결측 시점 유지.",
      " 연결: ", paste(head(paste0(mapping$.name, " → ", mapping$.value, " [", mapping[[p$time]], "]"), 8L), collapse = "; "))
    affected <- nrow(out)
  } else {
    ids <- p$ids; time <- p$time; values <- p$values
    groups <- as.integer(vctrs::vec_group_id(data[ids]))
    extra <- setdiff(names(data), c(ids, time, values))
    for (nm in extra) {
      if (nrow(unique(data[c(ids, nm)])) > length(unique(groups))) stop(paste0(nm, ": 같은 대상 안에서 값이 다릅니다. 펼칠 측정값에 포함하거나 식별 변수를 확인해 주세요."), call. = FALSE)
    }
    times <- unique(data[[time]])
    check_reshape_size(length(unique(groups)), length(ids) + length(extra) + as.double(length(values)) * length(times))
    mapping <- data.frame(.name = unlist(lapply(values, function(v) paste0(v, p$separator, as.character(times)))),
      .value = rep(values, each = length(times)), stringsAsFactors = FALSE)
    mapping[[time]] <- rep(times, times = length(values))
    if (anyDuplicated(mapping$.name) || any(mapping$.name %in% c(ids, extra))) stop("펼친 변수 이름이 중복되거나 기존 변수와 겹칩니다. 이름 구분자나 시점 값을 바꿔 주세요.", call. = FALSE)
    out <- as.data.frame(tidyr::pivot_wider_spec(data[c(ids, time, values)], mapping,
      id_cols = tidyselect::all_of(ids), names_repair = "check_unique"))
    if (length(extra)) {
      constants <- data[!duplicated(groups), c(ids, extra), drop = FALSE]
      idx <- vctrs::vec_match(out[ids], constants[ids])
      out[extra] <- constants[idx, extra, drop = FALSE]
    }
    before <- sum(vapply(data[values], function(x) sum(is.na(x)), integer(1)))
    after <- sum(vapply(out[mapping$.name], function(x) sum(is.na(x)), integer(1)))
    details <- list(mapping = mapping, ids = ids, preserved_columns = extra,
      absent_combinations = nrow(out) * length(times) - nrow(data))
    note <- sprintf("%d행 → %d행. 중복 조합 없음. 없는 시점 조합 %d개는 결측으로 채웠습니다.", nrow(data), nrow(out), details$absent_combinations)
    affected <- nrow(out)
  }
  list(data = out, n_affected = affected, missing_before = before, missing_after = after,
       details = encode_parameters(details), note = note)
}

rule_io <- function(rule) {
  op <- rule$operation[[1]]
  if (!op %in% research_operations) return(list(reads = rule$variable, writes = rule$variable, all = FALSE))
  p <- rule_parameters(rule)
  if (op %in% c("long", "wide")) return(list(reads = character(), writes = character(), all = TRUE))
  if (op == "labels") return(list(reads = p$input, writes = p$input, all = FALSE))
  list(reads = if (op == "composite") p$inputs else unique(c(p$input, p$groups,
    if (p$reference == "subset") p$reference_column)), writes = p$output, all = FALSE)
}

rule_contexts <- function(rules) {
  rules <- normalize_rules(rules); signatures <- character(nrow(rules)); ios <- list(); ancestors <- list()
  for (i in seq_len(nrow(rules))) {
    io <- rule_io(rules[i, , drop = FALSE]); prev <- seq_len(i - 1L)
    dependencies <- prev[vapply(prev, function(j) io$all || ios[[j]]$all || length(intersect(io$reads, ios[[j]]$writes)) > 0L, logical(1))]
    ancestors[[i]] <- sort(unique(c(dependencies, unlist(ancestors[dependencies]))))
    signatures[[i]] <- encode_parameters(rules[c(ancestors[[i]], i), , drop = FALSE])
    ios[[i]] <- io
  }
  signatures
}

research_rule_from_inputs <- function(input) {
  op <- input$manual_op
  p <- switch(op,
    composite = list(inputs = as.character(input$composite_items), output = trimws(input$new_variable),
      method = input$composite_method, min_valid = input$composite_min),
    center = , standardize = list(input = input$scale_variable, output = trimws(input$new_variable),
      scope = input$scale_scope, groups = if (identical(input$scale_scope, "all")) character() else as.character(input$scale_groups),
      reference = input$scale_reference, reference_column = input$reference_column,
      reference_values = as.character(input$reference_values), center = input$fixed_center, scale = input$fixed_scale),
    long = list(ids = as.character(input$reshape_ids), columns = as.character(input$long_columns),
      layout = input$long_layout, time = trimws(input$long_time), value = trimws(input$long_value),
      prefix = input$long_prefix, separator = input$long_separator, drop_na = isTRUE(input$long_drop_na)),
    wide = list(ids = as.character(input$reshape_ids), time = input$wide_time,
      values = as.character(input$wide_values), separator = input$wide_separator))
  new_research_rule(op, p, input$manual_reason)
}
