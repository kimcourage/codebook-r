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
  if (file.info(path)$size > 20 * 1024^2) stop("\uc791\uc5c5 \ud30c\uc77c\uc740 20MB\uae4c\uc9c0 \uc77d\uc744 \uc218 \uc788\uc2b5\ub2c8\ub2e4.", call. = FALSE)
  x <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  if (!identical(x$format, "CodebookR-plan") || !identical(x$version, 1L)) stop("\uc9c0\uc6d0\ud558\uc9c0 \uc54a\ub294 \uc791\uc5c5 \ud30c\uc77c\uc785\ub2c8\ub2e4.", call. = FALSE)
  signature <- function(m) {
    if (!is.data.frame(m) || !all(c("kind", "name", "checksum") %in% names(m)) || anyNA(m)) return("")
    paste(sort(paste(m$kind, m$name, m$checksum, sep = "|")), collapse = "\n")
  }
  if (!nzchar(signature(manifest)) || !identical(signature(x$sources), signature(manifest))) stop("\uc800\uc7a5\ud560 \ub54c \uc0ac\uc6a9\ud55c \uc6d0\uc790\ub8cc\uc640 \ucf54\ub4dc\ubd81\uc744 \uba3c\uc800 \uc5f0\uacb0\ud574 \uc8fc\uc138\uc694. \ud30c\uc77c \uc774\ub984\uacfc \ub0b4\uc6a9\uc774 \ubaa8\ub450 \uac19\uc544\uc57c \ud569\ub2c8\ub2e4.", call. = FALSE)
  restored_options <- do.call(normalize_import_options, x$import_options)
  if (!identical(restored_options, do.call(normalize_import_options, options))) stop("CSV \uc77d\uae30 \uc635\uc158\uc774 \ub2e4\ub985\ub2c8\ub2e4. \uc800\uc7a5\ud560 \ub54c\uc758 \uc778\ucf54\ub529\uacfc \ubb38\uc790 \uc5f4\uc744 \uba3c\uc800 \uc9c0\uc815\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
  if (is.list(x$rules) && !length(x$rules)) x$rules <- normalize_rules(empty_rules())
  if (!is.data.frame(x$rules) || !all(names(empty_rules()) %in% names(x$rules)) || anyDuplicated(x$rules$id) || anyNA(x$rules$id) || any(x$rules$id < 1)) stop("\uc791\uc5c5 \ubaa9\ub85d \ud615\uc2dd\uc774 \uc62c\ubc14\ub974\uc9c0 \uc54a\uc2b5\ub2c8\ub2e4.", call. = FALSE)
  for (nm in c("values", "reason", "evidence", "confidence", "source")) x$rules[[nm]][is.na(x$rules[[nm]])] <- ""
  x$rules$minimum <- as.numeric(x$rules$minimum)
  x$rules$maximum <- as.numeric(x$rules$maximum)
  x$rules <- normalize_rules(x$rules)
  if (any(!x$rules$operation %in% c("set_na", "reverse", "factor", research_operations))) stop("\uc9c0\uc6d0\ud558\uc9c0 \uc54a\ub294 \uc791\uc5c5\uc774 \uc788\uc2b5\ub2c8\ub2e4.", call. = FALSE)
  invisible(lapply(seq_len(nrow(x$rules)), function(i) rule_parameters(x$rules[i, , drop = FALSE])))
  if (is.null(x$additional_ids)) x$additional_ids <- integer()
  if (anyNA(x$additional_ids) || any(!x$additional_ids %in% x$rules$id)) stop("\ucd94\uac00 \uc791\uc5c5 \uad6c\ubd84\uc744 \uc77d\uc744 \uc218 \uc5c6\uc2b5\ub2c8\ub2e4.", call. = FALSE)
  for (nm in c("mapping", "decisions", "ai_records")) {
    if (is.list(x[[nm]]) && !length(x[[nm]])) x[[nm]] <- NULL
    if (!is.null(x[[nm]]) && !is.data.frame(x[[nm]])) stop("\uc791\uc5c5 \ud30c\uc77c\uc758 \uac80\ud1a0 \uc815\ubcf4\uac00 \uc62c\ubc14\ub974\uc9c0 \uc54a\uc2b5\ub2c8\ub2e4.", call. = FALSE)
  }
  x
}
