# Category names come from codebook definitions or existing value labels.
# Keep the original codes alongside levels for later codebook comparison.
factor_source_codes <- function(x) {
  values <- as.character(x)
  mapping <- attr(x, "codebookr_codes", exact = TRUE)
  if (is.factor(x) && length(mapping)) {
    at <- match(values, names(mapping))
    found <- !is.na(at)
    values[found] <- unname(mapping[at[found]])
  }
  values[is.na(x)] <- NA_character_
  values
}

factor_value_labels <- function(x) {
  mapping <- attr(x, "codebookr_codes", exact = TRUE)
  if (is.factor(x) && length(mapping)) return(mapping)
  attr(x, "labels", exact = TRUE)
}

make_factor_result <- function(x, parameters = list()) {
  values <- factor_source_codes(x)
  numeric_codes <- is.numeric(x) || isTRUE(attr(x, "codebookr_numeric", exact = TRUE))
  existing <- factor_value_labels(x)
  explicit <- !is.null(parameters$codes)
  codes <- if (explicit) parameters$codes else unname(existing)
  labels <- if (explicit) parameters$labels else names(existing)
  if (!explicit && length(existing)) {
    keep <- !is.na(existing)
    codes <- codes[keep]; labels <- labels[keep]
  }
  if (length(codes) != length(labels) || anyNA(codes) || anyNA(labels) ||
      any(!nzchar(trimws(labels))) || length(codes) > 5000L) {
    stop("\ubc94\uc8fc \ucf54\ub4dc\uc640 \uc774\ub984\uc758 \uc815\uc758\ub97c \ud655\uc778\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
  }
  codes <- as.character(codes); labels <- as.character(labels)
  if (numeric_codes && length(codes)) {
    numbers <- suppressWarnings(as.numeric(codes))
    if (any(!is.finite(numbers))) stop("\ubc94\uc8fc \ucf54\ub4dc\uac00 \uc22b\uc790 \uc790\ub8cc\uc640 \ub9de\uc9c0 \uc54a\uc2b5\ub2c8\ub2e4.", call. = FALSE)
    codes <- as.character(numbers)
  }
  if (anyDuplicated(codes)) stop("\uac19\uc740 \ucf54\ub4dc\uc5d0 \ubc94\uc8fc\uba85\uc774 \uc5ec\ub7ec \uac1c \uc815\uc758\ub418\uc5b4 \uc788\uc2b5\ub2c8\ub2e4.", call. = FALSE)
  if (explicit && length(existing)) {
    keep <- !is.na(existing)
    extra_codes <- as.character(unname(existing[keep])); extra_labels <- names(existing)[keep]
    if (numeric_codes) extra_codes <- as.character(suppressWarnings(as.numeric(extra_codes)))
    add <- !is.na(extra_codes) & !extra_codes %in% codes
    codes <- c(codes, extra_codes[add]); labels <- c(labels, extra_labels[add])
  }
  observed <- unique(values[!is.na(values)])
  observed <- if (numeric_codes) as.character(sort(as.numeric(observed))) else sort(observed)
  level_codes <- c(codes[codes %in% observed], setdiff(observed, codes))
  at <- match(level_codes, codes)
  level_names <- level_codes
  level_names[!is.na(at)] <- labels[at[!is.na(at)]]
  if (anyDuplicated(level_names)) {
    stop("\uc11c\ub85c \ub2e4\ub978 \ucf54\ub4dc\uc758 \ubc94\uc8fc\uba85\uc774 \uac19\uc2b5\ub2c8\ub2e4. \ubc94\uc8fc\uac00 \ud569\uccd0\uc9c0\uc9c0 \uc54a\ub3c4\ub85d \ucf54\ub4dc\ubd81\uc758 \uc774\ub984\uc744 \uad6c\ubd84\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
  }
  out <- factor(values, levels = level_codes, labels = level_names, exclude = NA)
  description <- parameters$label
  if (length(description) != 1L || is.na(description) || !nzchar(description)) description <- attr(x, "label", exact = TRUE)
  if (!is.null(description)) attr(out, "label") <- description
  attr(out, "codebookr_codes") <- stats::setNames(level_codes, level_names)
  attr(out, "codebookr_numeric") <- numeric_codes
  unknown <- setdiff(observed, codes)
  note <- if (length(codes)) "\uac12 \ub77c\ubca8\uc744 \ubc94\uc8fc\uba85\uc73c\ub85c \uc0ac\uc6a9\ud574 factor\ub85c \ubcc0\ud658\ud569\ub2c8\ub2e4." else
    "\uac12 \ub77c\ubca8\uc774 \uc5c6\uc5b4 \uad00\uce21 \ucf54\ub4dc\ub97c \ubc94\uc8fc\uba85\uc73c\ub85c \uc0ac\uc6a9\ud569\ub2c8\ub2e4."
  if (length(codes) && length(unknown)) note <- paste(note,
    sprintf("\ub77c\ubca8 \uc815\uc758\uc5d0 \uc5c6\ub294 \ucf54\ub4dc %d\uac1c\ub294 \ucf54\ub4dc \uc790\uccb4\ub97c \ubc94\uc8fc\uba85\uc73c\ub85c \uc720\uc9c0\ud569\ub2c8\ub2e4: %s", length(unknown), paste(head(unknown, 8L), collapse = ", ")))
  list(value = out, details = list(codes = level_codes, levels = level_names,
    unlabelled_codes = unknown, ordered = FALSE), note = note)
}

with_codebook_factor_labels <- function(rules, data, codebooks, mapping) {
  rules <- normalize_rules(rules)
  if (!nrow(rules) || is.null(codebooks)) return(rules)
  records <- canonical_codebook_codes(mapped_codebook(codebooks, mapping)$records, data)
  for (i in which(rules$operation == "factor")) {
    rows <- records[records$variable == rules$variable[[i]], , drop = FALSE]
    if (!nrow(rows)) next
    if (length(codebook_conflicts(rows))) stop("\ubc94\uc8fc\uba85\uc774 \ucda9\ub3cc\ud558\ub294 \ucf54\ub4dc\ubd81 \uc815\uc758\ub97c \uba3c\uc800 \ud655\uc778\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
    description <- unique(rows$label[nzchar(rows$label)])
    if (length(description) > 1L) stop("\ucf54\ub4dc\ubd81\uc758 \ubcc0\uc218 \uc124\uba85\uc774 \uc11c\ub85c \ub2e4\ub985\ub2c8\ub2e4.", call. = FALSE)
    labels <- unique(rows[nzchar(rows$value_code) & nzchar(rows$value_label), c("value_code", "value_label"), drop = FALSE])
    rules$schema_version[[i]] <- 2L
    rules$parameters[[i]] <- encode_parameters(list(codes = labels$value_code,
      labels = labels$value_label, label = if (length(description)) description else ""))
    rules$evidence[[i]] <- paste(unique(c(rules$evidence[[i]],
      paste(rows$source_file, rows$source_loc, sep = " / "))), collapse = "; ")
  }
  rules
}

codebook_requests_factor <- function(rows) {
  notes <- paste(rows$note, collapse = " ")
  positive <- grepl("\uba85\ubaa9|\ubc94\uc8fc\ud615|nominal|categorical|\\bfactor\\b", notes, ignore.case = TRUE, perl = TRUE)
  negative <- grepl("\uc218\uce58\ud615|\uc5f0\uc18d\ud615|numeric|continuous|\ubc94\uc8fc\ud615.{0,8}\uc544\ub2d8|factor.{0,8}\uc544\ub2d8|not.{0,8}(categorical|nominal|factor)", notes, ignore.case = TRUE, perl = TRUE)
  positive && !negative
}
