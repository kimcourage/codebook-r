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
  if (grepl("^(\ubcc0\uc218\uba85|\ubcc0\uc218\ucf54\ub4dc|\ubcc0\uc218id|variable|varname|\ubcc0\uc218\ubc88\ud638|\ubb38\ud56d\ucf54\ub4dc|\uc9c8\ubb38\ucf54\ub4dc)$", x)) return("variable")
  if (grepl("^(\ubcc0\uc218\uc124\uba85|\ubcc0\uc218\ub77c\ubca8|\ubcc0\uc218\ub808\uc774\ube14|\ubb38\ud56d\uba85|\ubb38\ud56d\ub0b4\uc6a9|\uc9c8\ubb38\ub0b4\uc6a9|\uc9c8\ubb38|\uc124\ubb38\ubb38\ud56d|variablelabel|question|description|questiondescription|label)$", x)) return("label")
  if (grepl("^(\uac12|\ucf54\ub4dc\uac12|\uc751\ub2f5\ucf54\ub4dc|\uc751\ub2f5\uac12|\uac12\ucf54\ub4dc|value|valuecode|code)$", x)) return("value_code")
  if (grepl("^(\uac12\uc124\uba85|\uac12\ub77c\ubca8|\uac12\ub808\uc774\ube14|\uc751\ub2f5\ub0b4\uc6a9|\uc751\ub2f5\ubc94\uc8fc|\uc751\ub2f5\ub77c\ubca8|\ubcf4\uae30|\ubc94\uc8fc\uba85|\ubc94\uc8fc\uc124\uba85|valuelabel|codelabel|category)$", x)) return("value_label")
  if (grepl("^(\ube44\uace0|\uc8fc\uc11d|\ucc38\uace0|note|notes|\uacb0\uce21\ucc98\ub9ac|\uacb0\uce21\ucf54\ub4dc)$", x)) return("note")
  if (grepl("^(\ucd5c\uc18c|\ucd5c\uc18c\uac12|\ud558\ud55c|minimum|min|lower)$", x)) return("minimum")
  if (grepl("^(\ucd5c\ub300|\ucd5c\ub300\uac12|\uc0c1\ud55c|maximum|max|upper)$", x)) return("maximum")
  if (grepl("^(\uce21\uc815\uc218\uc900|\uce21\uc815\ucc99\ub3c4|\uc790\ub8cc\ud615|\ubcc0\uc218\uc720\ud615|\ubcc0\uc218\ud0c0\uc785|measurement|measurementlevel|measure|levelofmeasurement|type|variabletype)$", x)) return("measurement")
  ""
}

parse_tabular_codebook <- function(table, source_file, sheet = "", max_rows = 5000L) {
  out <- empty_codebook_records()
  if (is.null(table) || !nrow(table) || !ncol(table)) {
    return(list(records = out, excerpt = "", detected = FALSE, note = "\ube48 \uc2dc\ud2b8\uc785\ub2c8\ub2e4."))
  }
  raw <- as.data.frame(table, stringsAsFactors = FALSE)
  limits <- character()
  if (nrow(raw) > max_rows) limits <- c(limits, sprintf("\ucc98\uc74c %d\ud589\ub9cc \uac80\uc0ac\ud588\uc2b5\ub2c8\ub2e4.", max_rows))
  if (ncol(raw) > 35L) limits <- c(limits, "\ucc98\uc74c 35\uc5f4\ub9cc \uac80\uc0ac\ud588\uc2b5\ub2c8\ub2e4.")
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
      note = paste("\uccab 25\ud589\uc5d0\uc11c \ubcc0\uc218\uba85 \ud5e4\ub354\ub97c \ucc3e\uc9c0 \ubabb\ud588\uc2b5\ub2c8\ub2e4. \ubc1c\ucdcc\ubb38\uc744 \ud655\uc778\ud558\uac70\ub098 \uc218\ub3d9 \uaddc\uce59\uc744 \ucd94\uac00\ud574 \uc8fc\uc138\uc694.", note)))
  }
  header_row <- candidates[which.max(scores)]
  h <- headers[[which.max(scores)]]
  positions <- setNames(rep(NA_integer_, 8L),
                        c("variable", "label", "value_code", "value_label", "note", "minimum", "maximum", "measurement"))
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
  previous_measurement <- ""
  if (header_row >= nrow(raw)) {
    return(list(records = out, excerpt = excerpt, source_lines = source_lines, detected = TRUE, note = paste("\ud5e4\ub354 \ub4a4\uc5d0 \ubcc0\uc218 \uae30\ub85d\uc774 \uc5c6\uc2b5\ub2c8\ub2e4.", note)))
  }
  rows <- lapply(seq.int(header_row + 1L, nrow(raw)), function(i) {
    v <- val(i, "variable")
    lab <- val(i, "label")
    code <- val(i, "value_code")
    code_lab <- val(i, "value_label")
    note <- val(i, "note")
    measurement <- val(i, "measurement")
    # Some worksheets use merged cells: carry the previous variable only
    # when this row explicitly contains a response code.
    if (!nzchar(v) && nzchar(code)) {
      v <- previous_var
      if (!nzchar(lab)) lab <- previous_label
      if (!nzchar(measurement)) measurement <- previous_measurement
    }
    if (nzchar(val(i, "variable"))) {
      previous_var <<- v
      previous_label <<- lab
      previous_measurement <<- measurement
    }
    if (!nzchar(v)) return(NULL)
    data.frame(
      variable = v, label = lab, value_code = code,
      value_label = code_lab, note = paste(c(measurement, note)[nzchar(c(measurement, note))], collapse = "; "), source_file = source_file,
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
  if (!length(pages)) stop("PDF\uc5d0\uc11c \ud14d\uc2a4\ud2b8\ub97c \ucc3e\uc9c0 \ubabb\ud588\uc2b5\ub2c8\ub2e4.", call. = FALSE)
  note <- if (length(pages) > max_pages) sprintf("\ucc98\uc74c %d\ud398\uc774\uc9c0\ub9cc \uac80\uc0ac\ud588\uc2b5\ub2c8\ub2e4.", max_pages) else ""
  pages <- head(pages, max_pages)
  if (sum(nchar(pages)) < 30L) {
    stop("PDF\uc758 \ud14d\uc2a4\ud2b8\ub97c \ucd94\ucd9c\ud560 \uc218 \uc5c6\uc2b5\ub2c8\ub2e4. \uc2a4\uce94 PDF\ub294 \ud604\uc7ac OCR\uc744 \uc9c0\uc6d0\ud558\uc9c0 \uc54a\uc2b5\ub2c8\ub2e4.", call. = FALSE)
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
        value_label = parts[[4]], note = if (length(parts) >= 5L) parts[[5]] else "", source_file = source_file,
        source_loc = sprintf("page %d", p), stringsAsFactors = FALSE
      ))
    }
  }
  list(records = out, source_lines = if (length(source_lines)) do.call(rbind, source_lines) else data.frame(),
       excerpt = paste(head(all_lines, 180L), collapse = "\n"),
       detected = nrow(out) > 0L,
       note = paste(if (!nrow(out)) "\ud45c \uad6c\uc870\ub97c \uc778\uc2dd\ud558\uc9c0 \ubabb\ud588\uc2b5\ub2c8\ub2e4. \ubc1c\ucdcc\ubb38\uc744 \ud655\uc778\ud55c \ub4a4 \uc218\ub3d9 \uaddc\uce59\uc774\ub098 AI \uc81c\uc548\uc744 \uc774\uc6a9\ud574 \uc8fc\uc138\uc694." else "", note))
}

read_codebooks <- function(file_info) {
  if (is.null(file_info) || !nrow(file_info)) {
    return(list(records = empty_codebook_records(), excerpts = character(),
                summary = data.frame()))
  }
  records <- list(); excerpts <- character(); summary <- list(); source_lines <- list()
  if (anyDuplicated(basename(file_info$name))) {
    stop("\ucf54\ub4dc\ubd81 \ud30c\uc77c\uba85\uc774 \uc911\ubcf5\ub429\ub2c8\ub2e4. \uadfc\uac70\ub97c \uad6c\ubcc4\ud560 \uc218 \uc788\ub3c4\ub85d \ud30c\uc77c\uba85\uc744 \ubc14\uafd4 \uc8fc\uc138\uc694.", call. = FALSE)
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
          note = paste(parsed$note, if (length(all_sheets) > 20L) "\ucc98\uc74c 20\uac1c \uc2dc\ud2b8\ub9cc \uac80\uc0ac\ud588\uc2b5\ub2c8\ub2e4." else ""),
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
      stop(sprintf("\uc9c0\uc6d0\ud558\uc9c0 \uc54a\ub294 \ucf54\ub4dc\ubd81 \ud615\uc2dd: %s", file_name), call. = FALSE)
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
