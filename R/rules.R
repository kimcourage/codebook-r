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
  if (!is.data.frame(data)) stop("data.frame\uc774 \uc544\ub2d9\ub2c8\ub2e4.", call. = FALSE)
  rule <- normalize_rules(rule)
  rule_parameters(rule)
  if (rule$operation[[1]] %in% research_operations) return(check_research_rule(rule, data))
  nm <- as.character(rule$variable[[1]])
  op <- as.character(rule$operation[[1]])
  if (is.na(nm) || !(nm %in% names(data))) {
    stop("\ub370\uc774\ud130\uc5d0 \uc874\uc7ac\ud558\uc9c0 \uc54a\ub294 \ubcc0\uc218\uc785\ub2c8\ub2e4.", call. = FALSE)
  }
  if (!op %in% c("set_na", "reverse", "factor")) {
    stop("\ud5c8\uc6a9\ub418\uc9c0 \uc54a\uc740 \uc804\ucc98\ub9ac \uc791\uc5c5\uc785\ub2c8\ub2e4.", call. = FALSE)
  }
  if (op == "set_na") {
    codes <- split_codes(rule$values[[1]])
    if (!length(codes) || length(codes) > 30L || any(nchar(codes) > 80L) ||
        any(grepl("[\r\n]", codes))) {
      stop("\uacb0\uce21\uac12 \ucf54\ub4dc\ub294 \uc27c\ud45c\ub85c \uad6c\ubd84\ud55c 1~30\uac1c \uac12\uc774\uc5b4\uc57c \ud569\ub2c8\ub2e4.", call. = FALSE)
    }
  }
  if (op == "reverse") {
    x <- data[[nm]]
    min_value <- suppressWarnings(as.numeric(rule$minimum[[1]]))
    max_value <- suppressWarnings(as.numeric(rule$maximum[[1]]))
    if (!(is.numeric(x) || is.integer(x)) || inherits(x, "Date") ||
        inherits(x, "POSIXt") || !is.finite(min_value) ||
        !is.finite(max_value) || min_value >= max_value) {
      stop("\uc5ed\ucf54\ub529\uc5d0\ub294 \uc22b\uc790 \ubcc0\uc218\uc640 \uc720\ud6a8\ud55c \ucd5c\uc18c/\ucd5c\ub300\uac12\uc774 \ud544\uc694\ud569\ub2c8\ub2e4.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

apply_approved_rules <- function(data, rules) {
  if (!is.data.frame(data)) stop("data.frame\uc774 \uc544\ub2d9\ub2c8\ub2e4.", call. = FALSE)
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
        stop(sprintf("%s\uc758 \uac12\uc774 \uc5ed\ucf54\ub529 \ubc94\uc704 [%g, %g]\ub97c \ubc97\uc5b4\ub0a9\ub2c8\ub2e4. \uacb0\uce21 \ucc98\ub9ac \uaddc\uce59\uc744 \uba3c\uc800 \uc801\uc6a9\ud558\uac70\ub098 \ubc94\uc704\ub97c \uc218\uc815\ud558\uc138\uc694.",
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
  keyword <- "\ubb34\uc751\ub2f5|\uc751\ub2f5.?\uac70\uc808|\uc751\ub2f5.?\uac70\ubd80|\uc798.?\ubaa8\ub984|\ubaa8\ub974\uaca0|\ube44\ud574\ub2f9|\uacb0\uce21|\ubbf8\uc751\ub2f5|\ud574\ub2f9.?\uc5c6\uc74c|missing|refus|don't know|do not know|not applicable|no response|n/a"
  hit <- grepl(keyword, records$value_label, ignore.case = TRUE, perl = TRUE) |
    grepl(keyword, records$note, ignore.case = TRUE, perl = TRUE)
  negation <- grepl("\uacb0\uce21.?\uc544\ub2d8|\uacb0\uce21.?\uac12.?\uc544\ub2d8|not missing|valid code|missing.?\uc544\ub2d8",
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
      maximum = NA_real_, reason = "\ucf54\ub4dc\ubd81\uc758 \uba85\uc2dc\uc801 \uacb0\uce21/\ubb34\uc751\ub2f5 \ubc94\uc8fc (\ud655\uc778 \ud544\uc694)",
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
  if (!all(names(empty_rules())[-1] %in% names(candidates))) stop("\uaddc\uce59 \uc81c\uc548\uc758 \ud544\uc218 \ud544\ub4dc\uac00 \ub204\ub77d\ub418\uc5c8\uc2b5\ub2c8\ub2e4.", call. = FALSE)
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
  labels <- c(set_na = "\uacb0\uce21\uac12 \ucc98\ub9ac", reverse = "\uc5ed\ucf54\ub529", factor = "\ubc94\uc8fc\ud615 \ubcc0\ud658",
    composite = "\ud569\uc131\ubcc0\uc218", center = "\uc911\uc2ec\ud654", standardize = "\ud45c\uc900\ud654", long = "wide \u2192 long", wide = "long \u2192 wide", labels = "\ubcc0\uc218 \uc124\uba85\uacfc \uac12 \ub77c\ubca8")
  unname(labels[op])
}

rule_description <- function(rule) {
  op <- rule$operation[[1]]
  if (op %in% research_operations) {
    p <- rule_parameters(rule)
    if (op == "labels") return(if (length(p$codes)) paste0("\ubcc0\uc218 \uc124\uba85\uacfc \uac12 \ub77c\ubca8 ", length(p$codes), "\uac1c \uc801\uc6a9") else "\ubcc0\uc218 \uc124\uba85 \uc801\uc6a9")
    if (op == "composite") return(sprintf("%s\uc758 %s \u2192 %s (\ucd5c\uc18c %d\ubb38\ud56d)", paste(p$inputs, collapse = ", "), ifelse(p$method == "mean", "\ud3c9\uade0", "\ud569\uacc4"), p$output, p$min_valid))
    if (op %in% c("center", "standardize")) return(paste0(p$input, " \u2192 ", p$output, " (", if (p$scope == "all") "\uc804\uccb4" else paste(p$groups, collapse = ", "), " \uae30\uc900)"))
    return(if (op == "long") paste0(paste(p$columns, collapse = ", "), "\ub97c \ud589\uc73c\ub85c, \uc2dc\uc810: ", p$time) else paste0(paste(p$values, collapse = ", "), "\ub97c ", p$time, "\ubcc4 \uc5f4\ub85c"))
  }
  if (op == "set_na") return(paste0(rule$values[[1]], " \u2192 \uacb0\uce21\uac12"))
  if (op == "reverse") return(sprintf("%g\u2013%g \ubc94\uc704\uc5d0\uc11c \ubc29\ud5a5 \ubc18\uc804", rule$minimum[[1]], rule$maximum[[1]]))
  "\uad00\uce21 \ucf54\ub4dc\ub97c \ubc94\uc8fc\ub85c \ubcc0\ud658"
}

rule_table_display <- function(rules, selected_keys = character(), keys = character()) {
  if (!nrow(rules)) return(NULL)
  data.frame(
    "\uc21c\uc11c" = seq_len(nrow(rules)), "\ubc88\ud638" = rules$id, "\ubcc0\uc218" = rules$variable,
    "\uc791\uc5c5" = operation_label(rules$operation),
    "\ubcc0\uacbd" = vapply(seq_len(nrow(rules)), function(i) rule_description(rules[i, , drop = FALSE]), character(1)),
    "\uc120\ud0dd" = ifelse(keys %in% selected_keys, "\uc2e4\ud589 \ub300\uc0c1", "\ubbf8\uc120\ud0dd"),
    "\uc774\uc720" = rules$reason, "\uadfc\uac70" = rules$evidence,
    "\uc81c\uc548\uc790" = ifelse(rules$source == "AI", "AI", ifelse(rules$source == "codebook", "\ucf54\ub4dc\ubd81", "\uc5f0\uad6c\uc790")),
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
      type_before = if (rule$operation %in% c("long", "wide")) "\ud45c" else if (is.null(before)) "\uc0c8 \ubcc0\uc218" else paste(class(before), collapse = "/"),
      type_after = "", status = "", note = "", stringsAsFactors = FALSE)
    if (nzchar(failed)) {
      row$status <- "\ub300\uae30"
      row$note <- "\uc55e\uc120 \uaddc\uce59\uc758 \uc624\ub958\ub97c \uba3c\uc800 \uc218\uc815\ud574 \uc8fc\uc138\uc694."
    } else {
      step <- tryCatch(apply_approved_rules(current, rule), error = function(e) e)
      if (inherits(step, "error")) {
        failed <- sprintf("#%s %s: %s", rule$id[[1]], nm, conditionMessage(step))
        row$status <- "\uc218\uc815 \ud544\uc694"
        row$note <- conditionMessage(step)
      } else {
        row$n_affected <- step$audit$n_affected
        row$missing_after <- step$audit$missing_after
        row$type_after <- if (rule$operation %in% c("long", "wide")) "\ud45c" else paste(class(step$data[[nm]]), collapse = "/")
        row$rows_after <- nrow(step$data); row$columns_after <- ncol(step$data)
        row$missing_before <- step$audit$missing_before
        row$details <- step$audit$details
        row$status <- "\uc2e4\ud589 \uac00\ub2a5"
        notices <- step$audit$note[nzchar(step$audit$note)]
        if (row$n_affected == 0L) notices <- c(notices, "\uc801\uc6a9 \ub300\uc0c1\uc774 \uc5c6\uc2b5\ub2c8\ub2e4. \ubcc0\uc218\uc640 \ucf54\ub4dc \uac12\uc744 \ud655\uc778\ud574 \uc8fc\uc138\uc694.")
        if (rule$operation[[1]] == "factor") notices <- c(notices, "\uc21c\uc11c \uc5c6\ub294 \ubc94\uc8fc\ud615\uc785\ub2c8\ub2e4. \ucf54\ub4dc\ubd81 \uac12 \ub77c\ubca8\uc740 \uc790\ub3d9 \uc801\uc6a9\ud558\uc9c0 \uc54a\uc2b5\ub2c8\ub2e4.")
        if (rule$operation[[1]] %in% c("reverse", "factor") && length(attr(before, "labels", exact = TRUE))) {
          notices <- c(notices, "\uae30\uc874 \uac12 \ub77c\ubca8\uc740 \uc81c\uac70\ub429\ub2c8\ub2e4. \ubcc0\uc218 \uc124\uba85\uc740 \uc720\uc9c0\ud569\ub2c8\ub2e4.")
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
  data.frame("\ubc88\ud638" = x$id, "\ubcc0\uc218" = x$variable, "\uc791\uc5c5" = operation_label(x$operation),
    "\uc801\uc6a9 \ub300\uc0c1 \uc218" = x$n_affected,
    "\uacb0\uce21 \uc218 (\uc804 \u2192 \ud6c4)" = paste(x$missing_before, "\u2192", x$missing_after),
    "\uc790\ub8cc\ud615 (\uc804 \u2192 \ud6c4)" = paste(x$type_before, "\u2192", x$type_after),
    "\ud589 \uc218" = paste(x$rows_before, "\u2192", x$rows_after),
    "\uc5f4 \uc218" = paste(x$columns_before, "\u2192", x$columns_after),
    "\uc0c1\ud0dc" = x$status, "\ud655\uc778\uc0ac\ud56d" = x$note, check.names = FALSE)
}

edit_rule_candidate <- function(existing, index, candidate, data) {
  if (length(index) != 1L || is.na(index) || !index %in% seq_len(nrow(existing))) {
    stop("\uc218\uc815\ud560 \uaddc\uce59\uc774 \ubc14\ub00c\uc5c8\uc2b5\ub2c8\ub2e4. \ubaa9\ub85d\uc5d0\uc11c \ub2e4\uc2dc \uc120\ud0dd\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
  }
  existing <- normalize_rules(existing); candidate <- normalize_rules(candidate)
  check_rule(candidate, data)
  others <- existing[-index, , drop = FALSE]
  if (nrow(append_rule_candidates(others, candidate, data)) == nrow(others)) {
    stop("\ub3d9\uc77c\ud55c \uc791\uc5c5\uc774 \uc774\ubbf8 \uc788\uc2b5\ub2c8\ub2e4. \uae30\uc874 \uaddc\uce59\uc744 \uc120\ud0dd\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
  }
  original <- existing[index, , drop = FALSE]
  candidate$id <- original$id
  candidate$source <- "manual"
  candidate$confidence <- "manual"
  candidate$evidence <- substr(paste0("\uc218\uc815 \uc804: ", rule_description(original),
    " / \uc6d0\ub798 \uadfc\uac70: ", original$evidence[[1]]), 1L, 2000L)
  existing[index, ] <- candidate[, names(existing), drop = FALSE]
  existing
}
