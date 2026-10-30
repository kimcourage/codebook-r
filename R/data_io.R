# Data I/O and metadata. No API requests are performed by this module.

normalize_import_options <- function(csv_encoding = "UTF-8", character_columns = character()) {
  if (!csv_encoding %in% c("UTF-8", "CP949", "EUC-KR")) {
    stop("CSV 문자 인코딩은 UTF-8, CP949, EUC-KR 중에서 선택해 주세요.", call. = FALSE)
  }
  character_columns <- unique(trimws(as.character(character_columns)))
  list(csv_encoding = csv_encoding, character_columns = character_columns[nzchar(character_columns)])
}

read_dataset <- function(path, original_name = basename(path), csv_encoding = "UTF-8",
                         character_columns = character()) {
  if (!file.exists(path)) stop("데이터 파일을 찾을 수 없습니다.", call. = FALSE)
  options <- normalize_import_options(csv_encoding, character_columns)
  ext <- tolower(tools::file_ext(original_name))
  read_csv_safely <- function() {
    # Read strings first: guessing a sample can turn late mixed values into NA,
    # and numeric conversion can irreversibly round long identifiers.
    raw <- readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()),
      locale = readr::locale(encoding = options$csv_encoding), name_repair = "minimal",
      show_col_types = FALSE, progress = FALSE)
    problems <- readr::problems(raw)
    if (nrow(problems)) stop(sprintf(
      "CSV 구조 오류 %d건이 있습니다. 따옴표, 열 개수와 인코딩을 확인한 뒤 다시 읽어 주세요.",
      nrow(problems)), call. = FALSE)
    if (anyNA(names(raw)) || any(!nzchar(names(raw))) || anyDuplicated(names(raw))) {
      stop("변수명이 비어 있거나 중복되어 있습니다. 원자료에서 수정해 주세요.", call. = FALSE)
    }
    strings <- c(names(raw), unlist(raw, use.names = FALSE))
    if (any(!is.na(strings) & is.na(iconv(strings, from = "UTF-8", to = "UTF-8", sub = NA)))) {
      stop("CSV 한글 인코딩을 읽지 못했습니다. 읽기 옵션에서 CP949 또는 EUC-KR을 선택해 주세요.", call. = FALSE)
    }
    absent <- setdiff(options$character_columns, names(raw))
    if (length(absent)) stop(paste("문자로 읽을 변수명이 없습니다:", paste(absent, collapse = ", ")),
                            call. = FALSE)
    protected <- names(raw)[vapply(raw, function(x) any(
      grepl("^[+-]?0[0-9]+$|^[+-]?[0-9]{16,}$", x), na.rm = TRUE), logical(1))]
    spec <- readr::cols(.default = readr::col_guess())
    for (nm in union(protected, options$character_columns)) spec$cols[[nm]] <- readr::col_character()
    as.data.frame(readr::type_convert(raw, col_types = spec,
      locale = readr::locale(encoding = "UTF-8")))
  }
  data <- switch(
    ext,
    csv = read_csv_safely(),
    sav = as.data.frame(haven::read_sav(path, user_na = TRUE, .name_repair = "minimal")),
    dta = as.data.frame(haven::read_dta(path, .name_repair = "minimal")),
    rds = readRDS(path),
    stop(sprintf("지원하지 않는 데이터 형식: .%s", ext), call. = FALSE)
  )
  if (!is.data.frame(data)) stop("RDS 파일에는 data.frame이 들어 있어야 합니다.", call. = FALSE)
  data <- as.data.frame(data)  # Avoid by-reference semantics from e.g. data.table RDS.
  if (!ncol(data)) stop("열이 없는 데이터셋입니다.", call. = FALSE)
  nms <- names(data)
  if (anyNA(nms) || any(!nzchar(nms)) || anyDuplicated(nms)) {
    stop("변수명이 비어 있거나 중복되어 있습니다. 원자료에서 수정해 주세요.", call. = FALSE)
  }
  unsupported <- nms[vapply(data, function(x) !is.atomic(x) || !is.null(dim(x)), logical(1))]
  if (length(unsupported)) stop(paste("목록·행렬 형태의 열은 지원하지 않습니다:",
                                      paste(unsupported, collapse = ", ")), call. = FALSE)
  data
}

variable_metadata <- function(data) {
  stopifnot(is.data.frame(data))
  if (!ncol(data)) return(data.frame())
  n <- nrow(data)
  do.call(rbind, lapply(names(data), function(nm) {
    x <- data[[nm]]
    lab <- attr(x, "label", exact = TRUE)
    if (is.null(lab) || length(lab) != 1L) lab <- ""
    labels <- attr(x, "labels", exact = TRUE)
    value_labels <- if (length(labels)) paste(head(paste0(unname(labels), "=", names(labels)), 20L), collapse = "; ") else ""
    if (length(labels) > 20L) value_labels <- paste0(value_labels, "; … (총 ", length(labels), "개)")
    user_missing <- c(attr(x, "na_values", exact = TRUE), attr(x, "na_range", exact = TRUE))
    data.frame(
      variable = nm,
      class = paste(class(x), collapse = "/"),
      label = as.character(lab),
      value_labels = value_labels,
      declared_missing = paste(user_missing, collapse = ", "),
      n_missing = sum(is.na(x)),
      pct_missing = if (n) round(100 * mean(is.na(x)), 1) else 0,
      stringsAsFactors = FALSE
    )
  }))
}

metadata_display <- function(data) {
  x <- variable_metadata(data)
  names(x) <- c("변수", "자료형", "변수 설명", "값 라벨 (최대 20개)", "지정 결측값", "결측 수", "결측률 (%)")
  x
}

# Safe metadata for the LLM: no row-level records and no respondent values.
metadata_for_ai <- function(data, max_variables = 200L) {
  meta <- variable_metadata(data)
  meta <- head(meta[c("variable", "class", "label")], max_variables)
  meta[] <- lapply(meta, function(x) substr(as.character(x), 1L, 200L))
  meta
}
