# Codebook definitions, explicit variable mapping, local comparison and review context.
normalize_codebook_records <- function(records) {
  defaults <- c(minimum = "", maximum = "", quote = "", origin = "codebook")
  for (nm in names(defaults)) if (!nm %in% names(records)) records[[nm]] <- rep(defaults[[nm]], nrow(records))
  records[] <- lapply(records, compact_text)
  records
}

initial_mapping <- function(data, records, previous = NULL) {
  vars <- unique(records$variable)
  out <- data.frame(codebook_variable = vars, data_variable = rep("", length(vars)), suggestion = rep("", length(vars)), stringsAsFactors = FALSE)
  for (i in seq_along(vars)) {
    v <- vars[[i]]; old <- if (!is.null(previous)) match(v, previous$codebook_variable) else NA_integer_
    if (!is.na(old)) {
      saved <- previous$data_variable[[old]]
      if (!nzchar(saved) || saved %in% names(data)) { out$data_variable[[i]] <- saved; next }
    }
    if (v %in% names(data)) out$data_variable[[i]] <- v else {
      same <- names(data)[tolower(names(data)) == tolower(v)]
      if (length(same) == 1L) out$suggestion[[i]] <- same
      if (!nzchar(out$suggestion[[i]])) {
        key <- function(x) tolower(gsub("[^[:alnum:]]", "", x))
        same <- names(data)[key(names(data)) == key(v)]
        if (length(same) == 1L) out$suggestion[[i]] <- same
      }
    }
  }
  out
}

mapped_codebook <- function(codebooks, mapping) {
  cb <- codebooks; r <- normalize_codebook_records(cb$records)
  r$codebook_variable <- r$variable
  idx <- match(r$variable, mapping$codebook_variable)
  r$variable <- mapping$data_variable[idx]
  cb$records <- r[!is.na(r$variable) & nzchar(r$variable), , drop = FALSE]
  cb
}

raw_codes <- function(x) {
  if (inherits(x, "haven_labelled")) x <- unclass(x)
  as.character(x)
}

codebook_conflicts <- function(rows) {
  coded <- rows[nzchar(rows$value_code) & nzchar(rows$value_label), , drop = FALSE]
  if (!nrow(coded)) return(character())
  names(Filter(function(x) length(unique(x)) > 1L, split(coded$value_label, coded$value_code)))
}

canonical_codebook_codes <- function(records, data) {
  for (v in intersect(unique(records$variable), names(data))) {
    if (!is.numeric(data[[v]]) || inherits(data[[v]], c("Date", "POSIXt", "difftime"))) next
    at <- which(records$variable == v & nzchar(records$value_code))
    numeric_codes <- suppressWarnings(as.numeric(records$value_code[at]))
    valid <- is.finite(numeric_codes)
    records$value_code[at[valid]] <- as.character(numeric_codes[valid])
  }
  records
}

codebook_candidates <- function(data, codebooks, mapping) {
  cb <- mapped_codebook(codebooks, mapping); r <- canonical_codebook_codes(cb$records, data)
  good <- unique(r$variable)[vapply(unique(r$variable), function(v) !length(codebook_conflicts(r[r$variable == v, , drop = FALSE])) && length(unique(r$label[r$variable == v & nzchar(r$label)])) <= 1L, logical(1))]
  eligible <- r[r$variable %in% good, , drop = FALSE]
  out <- normalize_rules(heuristic_missing_rules(eligible, data))
  for (v in good) {
    rows <- eligible[eligible$variable == v, , drop = FALSE]
    label <- unique(rows$label[nzchar(rows$label)])
    # Conflicting variable descriptions also need explicit human resolution.
    if (length(label) > 1L) next
    if (!length(label)) label <- ""
    codes <- rows[nzchar(rows$value_code) & nzchar(rows$value_label), c("value_code", "value_label"), drop = FALSE]
    codes <- unique(codes)
    missing_only <- !nzchar(label) && nrow(codes) && all(codes$value_code %in% unlist(lapply(out$values[out$variable == v], split_codes)))
    if ((!nzchar(label) && !nrow(codes)) || missing_only) next
    p <- list(input = v, output = v, codes = codes$value_code, labels = codes$value_label, label = label)
    candidate <- new_research_rule("labels", p, "\ucf54\ub4dc\ubd81\uc758 \ubcc0\uc218 \uc124\uba85\uacfc \uac12 \ub77c\ubca8 \uc801\uc6a9")
    candidate$source <- "codebook"
    candidate$evidence <- paste(unique(paste(rows$source_file, rows$source_loc, sep = " / ")), collapse = "; ")
    out <- append_rule_candidates(out, candidate, data)
  }
  out
}

codebook_comparison <- function(data, codebooks, mapping, decisions = NULL) {
  r <- canonical_codebook_codes(mapped_codebook(codebooks, mapping)$records, data)
  rows <- lapply(names(data), function(v) {
    defs <- r[r$variable == v, , drop = FALSE]; issues <- character()
    if (!nrow(defs)) issues <- "\uc5f0\uacb0\ub41c \ucf54\ub4dc\ubd81 \uc815\uc758\uac00 \uc5c6\uc2b5\ub2c8\ub2e4."
    conflicts <- codebook_conflicts(defs)
    if (length(conflicts)) issues <- c(issues, paste("\ucf54\ub4dc \uc815\uc758\uac00 \uc11c\ub85c \ub2e4\ub985\ub2c8\ub2e4:", paste(conflicts, collapse = ", ")))
    descriptions <- unique(defs$label[nzchar(defs$label)])
    if (length(descriptions) > 1L) issues <- c(issues, "\ubcc0\uc218 \uc124\uba85\uc774 \uc11c\ub85c \ub2e4\ub985\ub2c8\ub2e4.")
    codes <- unique(defs$value_code[nzchar(defs$value_code)])
    missing_codes <- unlist(lapply(heuristic_missing_rules(defs, data)$values, split_codes))
    observed <- raw_codes(data[[v]]); valid <- !is.na(observed)
    # A list of missing codes alone is not an exhaustive enumeration of valid codes.
    has_categories <- any(!codes %in% missing_codes)
    unknown <- if (has_categories) unique(observed[valid & !observed %in% codes]) else character()
    if (length(unknown)) issues <- c(issues, paste0("\ucf54\ub4dc\ubd81 \ubaa9\ub85d\uc5d0 \uc5c6\ub294 \uac12 ", sum(valid & !observed %in% codes), "\uac1c: ", paste(head(unknown, 8L), collapse = ", "), ". \ubc94\uc8fc \ubaa9\ub85d\uc774 \uc644\uc804\ud55c\uc9c0 \ud655\uc778\ud558\uc138\uc694."))
    lo <- unique(defs$minimum[nzchar(defs$minimum)]); hi <- unique(defs$maximum[nzchar(defs$maximum)])
    outside <- 0L
    if (length(lo) > 1L || length(hi) > 1L) issues <- c(issues, "\ud5c8\uc6a9 \ubc94\uc704 \uc815\uc758\uac00 \uc11c\ub85c \ub2e4\ub985\ub2c8\ub2e4.")
    if (length(lo) == 1L || length(hi) == 1L) {
      lower <- if (length(lo) == 1L) suppressWarnings(as.numeric(lo)) else -Inf
      upper <- if (length(hi) == 1L) suppressWarnings(as.numeric(hi)) else Inf
      if (is.na(lower) || is.na(upper) || lower > upper) issues <- c(issues, "\ud5c8\uc6a9 \ubc94\uc704\ub97c \uc22b\uc790\ub85c \ud574\uc11d\ud558\uc9c0 \ubabb\ud588\uc2b5\ub2c8\ub2e4.") else if (!is.numeric(data[[v]])) issues <- c(issues, "\uc22b\uc790 \ubc94\uc704\uac00 \uc815\uc758\ub418\uc5b4 \uc788\uc73c\ub098 \uc790\ub8cc\ub294 \uc22b\uc790\ud615\uc774 \uc544\ub2d9\ub2c8\ub2e4.") else {
        x <- as.numeric(data[[v]]); eligible <- !is.na(data[[v]]) & !observed %in% missing_codes
        outside <- sum(eligible & (x < lower | x > upper), na.rm = TRUE)
        if (outside) issues <- c(issues, paste0("\ud5c8\uc6a9 \ubc94\uc704 \ubc16 \uac12 ", outside, "\uac1c. \uc790\ub3d9\uc73c\ub85c \ubc14\uafb8\uc9c0 \uc54a\uc2b5\ub2c8\ub2e4."))
      }
    }
    if (any(grepl("\ubd84\uae30|\uc751\ub2f5.?\ub300\uc0c1|\uac74\ub108|skip|universe|\uc5ed\ubb38\ud56d|reverse|\ucc99\ub3c4|scor", defs$note, ignore.case = TRUE))) issues <- c(issues, "\ubd84\uae30, \uc5ed\ubb38\ud56d \ub610\ub294 \uc810\uc218 \uc9c0\uce68\uc774 \uc788\uc2b5\ub2c8\ub2e4. \uc790\ub3d9 \uc810\uac80 \ub300\uc0c1\uc5d0 \ud3ec\ud568\ub418\uc9c0 \uc54a\uc740 \uc870\uac74\uc744 \uc6d0\ubb38\uc5d0\uc11c \ud655\uc778\ud558\uc138\uc694.")
    state <- if (length(issues)) "\ud655\uc778 \ud544\uc694" else "\uc815\uc758 \uc5f0\uacb0\ub428"
    decision <- "\ubbf8\uac80\ud1a0"; reason <- ""
    if (!is.null(decisions) && v %in% decisions$variable) {
      d <- decisions[match(v, decisions$variable), , drop = FALSE]; decision <- d$decision; reason <- d$reason
    }
    data.frame(variable = v, codebook_variable = paste(unique(defs$codebook_variable), collapse = ", "),
      label = paste(descriptions, collapse = " / "), status = state,
      n_missing = sum(is.na(data[[v]])), undefined_codes = if (has_categories) sum(valid & !observed %in% codes) else NA_integer_,
      out_of_range = outside, findings = paste(unique(issues), collapse = " "),
      decision = decision, reason = reason,
      evidence = paste(unique(paste(defs$source_file, defs$source_loc, sep = " / ")), collapse = "; "), stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

value_frequency <- function(data, variable) {
  if (!variable %in% names(data)) return(data.frame())
  x <- raw_codes(data[[variable]]); x[is.na(x)] <- "(NA)"
  t <- sort(table(x), decreasing = TRUE)
  data.frame("\uac12" = head(names(t), 25L), "\ube48\ub3c4" = as.integer(head(t, 25L)), check.names = FALSE)
}

sample_values <- function(data, columns = names(data), n = 12L) {
  columns <- intersect(columns, names(data)); out <- head(data[columns], n)
  for (v in names(out)) out[[v]] <- raw_codes(out[[v]])
  out
}

review_context <- function(data, codebooks, mapping, decisions, rules, after_data = data) {
  list(mapping = mapping, definitions = normalize_codebook_records(codebooks$records),
    comparison = codebook_comparison(data, codebooks, mapping, decisions),
    after_comparison = codebook_comparison(after_data, codebooks, mapping, decisions),
    documents = codebooks$summary,
    unlinked_definitions = mapping[!nzchar(mapping$data_variable), , drop = FALSE],
    basic_rule_ids = rules$id)
}

# Quotes must be present in the supplied source, within the stated location.
validate_ai_definitions <- function(definitions, codebooks) {
  if (is.null(definitions) || !nrow(definitions)) return(empty_codebook_records())
  required <- c(names(empty_codebook_records()), "quote")
  if (!all(required %in% names(definitions))) stop("AI \uc815\uc758\uc758 \ud544\uc218 \uc815\ubcf4\uac00 \uc5c6\uc2b5\ub2c8\ub2e4.", call. = FALSE)
  out <- normalize_codebook_records(definitions)
  keep <- vapply(seq_len(nrow(out)), function(i) {
    row <- out[i, ]; if (nchar(row$quote) < 8L || nchar(row$quote) > 1000L || !nzchar(row$variable)) return(FALSE)
    source <- codebooks$source_lines
    if (is.null(source) || !nrow(source)) return(FALSE)
    at <- source[source$source_file == row$source_file & source$source_loc == row$source_loc, , drop = FALSE]
    any(vapply(at$text, function(s) grepl(row$quote, s, fixed = TRUE), logical(1)))
  }, logical(1))
  out <- out[keep, , drop = FALSE]; out$origin <- rep("AI", nrow(out))
  out
}
