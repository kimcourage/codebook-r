# Codebook import. Source locations are preserved for human review.

empty_codebook_records <- function() {
  data.frame(
    variable = character(), label = character(), value_code = character(),
    value_label = character(), note = character(), source_file = character(),
    source_loc = character(), stringsAsFactors = FALSE
  )
}

compact_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(gsub("[\r\n\t]+", " ", x))
}

classify_header <- function(x) {
  x <- tolower(gsub("[[:space:]_.()/-]", "", trimws(x), perl = TRUE))
  if (grepl("^(변수명|변수코드|변수id|variable|varname|변수번호|문항코드|질문코드)$", x)) return("variable")
  if (grepl("^(변수설명|변수라벨|변수레이블|문항명|문항내용|질문내용|질문|설문문항|variablelabel|question|description|questiondescription|label)$", x)) return("label")
  if (grepl("^(값|코드값|응답코드|응답값|값코드|value|valuecode|code)$", x)) return("value_code")
  if (grepl("^(값설명|값라벨|값레이블|응답내용|응답범주|응답라벨|보기|범주명|범주설명|valuelabel|codelabel|category)$", x)) return("value_label")
  if (grepl("^(비고|주석|참고|note|notes|결측처리|결측코드)$", x)) return("note")
  if (grepl("^(최소|최소값|하한|minimum|min|lower)$", x)) return("minimum")
  if (grepl("^(최대|최대값|상한|maximum|max|upper)$", x)) return("maximum")
  ""
}

parse_tabular_codebook <- function(table, source_file, sheet = "", max_rows = 5000L) {
  out <- empty_codebook_records()
  if (is.null(table) || !nrow(table) || !ncol(table)) {
    return(list(records = out, excerpt = "", detected = FALSE, note = "빈 시트입니다."))
  }
  raw <- as.data.frame(table, stringsAsFactors = FALSE)
  limits <- character()
  if (nrow(raw) > max_rows) limits <- c(limits, sprintf("처음 %d행만 검사했습니다.", max_rows))
  if (ncol(raw) > 35L) limits <- c(limits, "처음 35열만 검사했습니다.")
  note <- paste(limits, collapse = " ")
  raw[] <- lapply(raw, compact_text)
  raw <- head(raw, max_rows)
  raw <- raw[, seq_len(min(ncol(raw), 35L)), drop = FALSE]
  lines <- apply(raw, 1L, function(row) paste(row[nzchar(row)], collapse = " | "))
  source_lines <- data.frame(source_file = source_file, source_loc = if (nzchar(sheet)) sprintf("%s!row %d", sheet, seq_along(lines)) else sprintf("row %d", seq_along(lines)), text = lines, stringsAsFactors = FALSE)
  excerpt <- paste(head(paste0("[", source_lines$source_loc[nzchar(lines)], "] ", lines[nzchar(lines)]), 100), collapse = "\n")

  # Locate header row by semantic coverage, not assumed first row.
  candidates <- seq_len(min(nrow(raw), 25L))
  headers <- lapply(candidates, function(i) vapply(unlist(raw[i, , drop = TRUE], use.names = FALSE),
                                                    classify_header, character(1)))
  scores <- vapply(headers, function(h) {
    2L * as.integer("variable" %in% h) + length(unique(h[nzchar(h)]))
  }, integer(1))
  if (!length(scores) || max(scores) < 3L ||
      !("variable" %in% headers[[which.max(scores)]])) {
    return(list(records = out, excerpt = excerpt, source_lines = source_lines, detected = FALSE,
      note = paste("첫 25행에서 변수명 헤더를 찾지 못했습니다. 발췌문을 확인하거나 수동 규칙을 추가해 주세요.", note)))
  }
  header_row <- candidates[which.max(scores)]
  h <- headers[[which.max(scores)]]
  positions <- setNames(rep(NA_integer_, 7L),
                        c("variable", "label", "value_code", "value_label", "note", "minimum", "maximum"))
  for (nm in names(positions)) {
    matches <- which(h == nm)
    if (length(matches)) positions[[nm]] <- matches[[1]]
  }
  val <- function(i, name) {
    col <- positions[[name]]
    if (is.na(col)) "" else raw[[col]][i]
  }
  previous_var <- ""
  previous_label <- ""
  if (header_row >= nrow(raw)) {
    return(list(records = out, excerpt = excerpt, source_lines = source_lines, detected = TRUE, note = paste("헤더 뒤에 변수 기록이 없습니다.", note)))
  }
  rows <- lapply(seq.int(header_row + 1L, nrow(raw)), function(i) {
    v <- val(i, "variable")
    lab <- val(i, "label")
    code <- val(i, "value_code")
    code_lab <- val(i, "value_label")
    note <- val(i, "note")
    # Some worksheets use merged cells: carry the previous variable only
    # when this row explicitly contains a response code.
    if (!nzchar(v) && nzchar(code)) {
      v <- previous_var
      if (!nzchar(lab)) lab <- previous_label
    }
    if (nzchar(val(i, "variable"))) {
      previous_var <<- v
      previous_label <<- lab
    }
    if (!nzchar(v)) return(NULL)
    data.frame(
      variable = v, label = lab, value_code = code,
      value_label = code_lab, note = note, source_file = source_file,
      minimum = val(i, "minimum"), maximum = val(i, "maximum"), quote = source_lines$text[[i]], origin = "codebook",
      source_loc = if (nzchar(sheet)) sprintf("%s!row %d", sheet, i) else sprintf("row %d", i),
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows)) out <- do.call(rbind, rows)
  list(records = out, excerpt = excerpt, source_lines = source_lines, detected = TRUE, note = note)
}

parse_pdf_codebook <- function(path, source_file, max_pages = 80L) {
  pages <- pdftools::pdf_text(path)
  if (!length(pages)) stop("PDF에서 텍스트를 찾지 못했습니다.", call. = FALSE)
  note <- if (length(pages) > max_pages) sprintf("처음 %d페이지만 검사했습니다.", max_pages) else ""
  pages <- head(pages, max_pages)
  if (sum(nchar(pages)) < 30L) {
    stop("PDF의 텍스트를 추출할 수 없습니다. 스캔 PDF는 현재 OCR을 지원하지 않습니다.", call. = FALSE)
  }
  all_lines <- character(); source_lines <- list()
  out <- empty_codebook_records()
  for (p in seq_along(pages)) {
    lines <- trimws(strsplit(pages[[p]], "\n", fixed = TRUE)[[1]])
    lines <- lines[nzchar(lines)]
    all_lines <- c(all_lines, sprintf("[page %d] %s", p, lines))
    if (length(lines)) source_lines[[length(source_lines)+1L]] <- data.frame(source_file = source_file, source_loc = sprintf("page %d", p), text = lines, stringsAsFactors = FALSE)
    # Conservative offline parser: explicitly delimited rows only.
    for (line in lines) {
      parts <- trimws(strsplit(line, "|", fixed = TRUE)[[1]])
      if (length(parts) < 4L) next
      v <- parts[[1]]
      if (classify_header(v) == "variable" && classify_header(parts[[3]]) == "value_code") next
      if (!grepl("^[A-Za-z][A-Za-z0-9_.]{0,79}$", v)) next
      out <- rbind(out, data.frame(
        variable = v, label = parts[[2]], value_code = parts[[3]],
        value_label = parts[[4]], note = "", source_file = source_file,
        source_loc = sprintf("page %d", p), stringsAsFactors = FALSE
      ))
    }
  }
  list(records = out, source_lines = if (length(source_lines)) do.call(rbind, source_lines) else data.frame(),
       excerpt = paste(head(all_lines, 180L), collapse = "\n"),
       detected = nrow(out) > 0L,
       note = paste(if (!nrow(out)) "표 구조를 인식하지 못했습니다. 발췌문을 확인한 뒤 수동 규칙이나 AI 제안을 이용해 주세요." else "", note))
}

read_codebooks <- function(file_info) {
  if (is.null(file_info) || !nrow(file_info)) {
    return(list(records = empty_codebook_records(), excerpts = character(),
                summary = data.frame()))
  }
  records <- list(); excerpts <- character(); summary <- list(); source_lines <- list()
  if (anyDuplicated(basename(file_info$name))) {
    stop("코드북 파일명이 중복됩니다. 근거를 구별할 수 있도록 파일명을 바꿔 주세요.", call. = FALSE)
  }
  for (i in seq_len(nrow(file_info))) {
    file_name <- basename(file_info$name[[i]])
    path <- file_info$datapath[[i]]
    ext <- tolower(tools::file_ext(file_name))
    if (ext %in% c("xlsx", "xls")) {
      all_sheets <- readxl::excel_sheets(path)
      sheets <- head(all_sheets, 20L)
      for (sheet in sheets) {
        table <- readxl::read_excel(path, sheet = sheet, col_names = FALSE,
                                   col_types = "text", n_max = 5001L,
                                   .name_repair = "minimal")
        parsed <- parse_tabular_codebook(table, file_name, sheet = sheet)
        records[[length(records) + 1L]] <- normalize_codebook_records(parsed$records)
        if (!is.null(parsed$source_lines)) source_lines[[length(source_lines)+1L]] <- parsed$source_lines
        key <- sprintf("%s / %s", file_name, sheet)
        excerpts[[key]] <- parsed$excerpt
        summary[[length(summary) + 1L]] <- data.frame(
          file = file_name, section = sheet, recognized = parsed$detected,
          variables = length(unique(parsed$records$variable)),
          note = paste(parsed$note, if (length(all_sheets) > 20L) "처음 20개 시트만 검사했습니다." else ""),
          stringsAsFactors = FALSE
        )
      }
    } else if (ext == "pdf") {
      parsed <- parse_pdf_codebook(path, file_name)
      records[[length(records) + 1L]] <- normalize_codebook_records(parsed$records)
        if (!is.null(parsed$source_lines)) source_lines[[length(source_lines)+1L]] <- parsed$source_lines
      excerpts[[file_name]] <- parsed$excerpt
      summary[[length(summary) + 1L]] <- data.frame(
        file = file_name, section = "PDF", recognized = parsed$detected,
        variables = length(unique(parsed$records$variable)), note = parsed$note,
        stringsAsFactors = FALSE
      )
    } else if (ext == "csv") {
      table <- readr::read_csv(path, col_names = FALSE, show_col_types = FALSE,
                               name_repair = "unique", progress = FALSE,
                               col_types = readr::cols(.default = readr::col_character()),
                               n_max = 5001L)
      parsed <- parse_tabular_codebook(table, file_name)
      records[[length(records) + 1L]] <- normalize_codebook_records(parsed$records)
        if (!is.null(parsed$source_lines)) source_lines[[length(source_lines)+1L]] <- parsed$source_lines
      excerpts[[file_name]] <- parsed$excerpt
      summary[[length(summary) + 1L]] <- data.frame(
        file = file_name, section = "CSV", recognized = parsed$detected,
        variables = length(unique(parsed$records$variable)),
        note = if (is.null(parsed$note)) "" else parsed$note, stringsAsFactors = FALSE
      )
    } else {
      stop(sprintf("지원하지 않는 코드북 형식: %s", file_name), call. = FALSE)
    }
  }
  nonempty <- Filter(function(x) nrow(x) > 0L, records)
  list(
    records = if (length(nonempty)) do.call(rbind, nonempty) else normalize_codebook_records(empty_codebook_records()),
    source_lines = if (length(source_lines)) do.call(rbind, source_lines) else data.frame(),
    excerpts = excerpts,
    summary = if (length(summary)) do.call(rbind, summary) else data.frame()
  )
}
