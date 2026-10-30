# Portable local plan files contain definitions and decisions, never credentials or raw rows.
source_manifest <- function(data_file, book_files) {
  file_row <- function(file, kind) {
    if (is.null(file) || !nrow(file)) return(NULL)
    data.frame(kind = kind, name = basename(file$name), checksum = unname(tools::md5sum(file$datapath)), stringsAsFactors = FALSE)
  }
  rbind(file_row(data_file, "data"), file_row(book_files, "codebook"))
}

write_project <- function(path, manifest, options, rules, additional_ids, mapping, decisions, ai_records) {
  doc <- list(format = "CodebookR-plan", version = 1L, sources = manifest,
    import_options = options, rules = normalize_rules(rules), additional_ids = additional_ids,
    mapping = mapping, decisions = decisions, ai_records = ai_records)
  writeLines(jsonlite::toJSON(doc, auto_unbox = TRUE, dataframe = "rows", na = "null", null = "null", digits = NA, pretty = TRUE), path, useBytes = TRUE)
}

read_project <- function(path, manifest, options) {
  if (file.info(path)$size > 20 * 1024^2) stop("작업 파일은 20MB까지 읽을 수 있습니다.", call. = FALSE)
  x <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  if (!identical(x$format, "CodebookR-plan") || !identical(x$version, 1L)) stop("지원하지 않는 작업 파일입니다.", call. = FALSE)
  signature <- function(m) {
    if (!is.data.frame(m) || !all(c("kind", "name", "checksum") %in% names(m)) || anyNA(m)) return("")
    paste(sort(paste(m$kind, m$name, m$checksum, sep = "|")), collapse = "\n")
  }
  if (!nzchar(signature(manifest)) || !identical(signature(x$sources), signature(manifest))) stop("저장할 때 사용한 원자료와 코드북을 먼저 연결해 주세요. 파일 이름과 내용이 모두 같아야 합니다.", call. = FALSE)
  restored_options <- do.call(normalize_import_options, x$import_options)
  if (!identical(restored_options, do.call(normalize_import_options, options))) stop("CSV 읽기 옵션이 다릅니다. 저장할 때의 인코딩과 문자 열을 먼저 지정해 주세요.", call. = FALSE)
  if (is.list(x$rules) && !length(x$rules)) x$rules <- normalize_rules(empty_rules())
  if (!is.data.frame(x$rules) || !all(names(empty_rules()) %in% names(x$rules)) || anyDuplicated(x$rules$id) || anyNA(x$rules$id) || any(x$rules$id < 1)) stop("작업 목록 형식이 올바르지 않습니다.", call. = FALSE)
  for (nm in c("values", "reason", "evidence", "confidence", "source")) x$rules[[nm]][is.na(x$rules[[nm]])] <- ""
  x$rules$minimum <- as.numeric(x$rules$minimum)
  x$rules$maximum <- as.numeric(x$rules$maximum)
  x$rules <- normalize_rules(x$rules)
  if (any(!x$rules$operation %in% c("set_na", "reverse", "factor", research_operations))) stop("지원하지 않는 작업이 있습니다.", call. = FALSE)
  invisible(lapply(seq_len(nrow(x$rules)), function(i) rule_parameters(x$rules[i, , drop = FALSE])))
  if (is.null(x$additional_ids)) x$additional_ids <- integer()
  if (anyNA(x$additional_ids) || any(!x$additional_ids %in% x$rules$id)) stop("추가 작업 구분을 읽을 수 없습니다.", call. = FALSE)
  for (nm in c("mapping", "decisions", "ai_records")) {
    if (is.list(x[[nm]]) && !length(x[[nm]])) x[[nm]] <- NULL
    if (!is.null(x[[nm]]) && !is.data.frame(x[[nm]])) stop("작업 파일의 검토 정보가 올바르지 않습니다.", call. = FALSE)
  }
  x
}
