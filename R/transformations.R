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
      !rule$schema_version %in% c(1L, 2L)) stop("\uc9c0\uc6d0\ud558\uc9c0 \uc54a\ub294 \uc791\uc5c5 \ud615\uc2dd\uc785\ub2c8\ub2e4.", call. = FALSE)
  if (!rule$operation[[1]] %in% research_operations) return(list())
  if (rule$schema_version != 2L) stop("\uc0c8 \uc791\uc5c5\uc758 \uc124\uc815 \ud615\uc2dd\uc774 \ub9de\uc9c0 \uc54a\uc2b5\ub2c8\ub2e4.", call. = FALSE)
  p <- tryCatch(jsonlite::fromJSON(rule$parameters[[1]], simplifyVector = TRUE), error = function(e) NULL)
  if (!is.list(p)) stop("\uc791\uc5c5 \uc124\uc815\uc744 \uc77d\uc744 \uc218 \uc5c6\uc2b5\ub2c8\ub2e4.", call. = FALSE)
  p
}

new_research_rule <- function(operation, parameters, reason, id = 1L) {
  target <- if (operation %in% c("long", "wide")) "\uc790\ub8cc \uc804\uccb4" else parameters$output
  data.frame(id = as.integer(id), variable = target, operation = operation, values = "",
    minimum = NA_real_, maximum = NA_real_, reason = reason, evidence = "\uc5f0\uad6c\uc790 \uc9c0\uc815",
    confidence = "manual", source = "manual", schema_version = 2L,
    parameters = encode_parameters(parameters), stringsAsFactors = FALSE)
}

require_text <- function(x, label) {
  if (length(x) != 1L || !is.character(x) || is.na(x) || !nzchar(trimws(x)) || grepl("[\r\n\t]", x)) {
    stop(paste0(label, "\uc744(\ub97c) \uc785\ub825\ud574 \uc8fc\uc138\uc694."), call. = FALSE)
  }
  x
}
require_columns <- function(data, columns, label, empty = FALSE) {
  if (is.null(columns)) columns <- character()
  if (!is.character(columns) || anyNA(columns) || anyDuplicated(columns) ||
      (!empty && !length(columns)) || any(!columns %in% names(data))) {
    stop(paste0(label, ": \uc55e\uc120 \uc791\uc5c5 \uacb0\uacfc\uc5d0 \uc788\ub294 \ubcc0\uc218\ub97c \uc120\ud0dd\ud574 \uc8fc\uc138\uc694."), call. = FALSE)
  }
  columns
}
new_column <- function(data, name) {
  require_text(name, "\uc0c8 \ubcc0\uc218 \uc774\ub984")
  if (name %in% names(data)) stop(paste0("\uc774\ubbf8 \uc788\ub294 \ubcc0\uc218 \uc774\ub984\uc785\ub2c8\ub2e4: ", name), call. = FALSE)
}
plain_numeric <- function(x, name) {
  if (!is.numeric(x) || is.complex(x) || inherits(x, c("Date", "POSIXt", "difftime"))) {
    stop(paste0(name, ": \uc22b\uc790 \ubcc0\uc218\uac00 \ud544\uc694\ud569\ub2c8\ub2e4."), call. = FALSE)
  }
  values <- as.numeric(x)
  values[is.na(x)] <- NA_real_ # Includes labelled SPSS user missings.
  if (any(!is.na(values) & !is.finite(values))) stop(paste0(name, ": \ubb34\ud55c\uac12\uc774 \uc788\uc2b5\ub2c8\ub2e4."), call. = FALSE)
  values
}
check_keys <- function(data, columns, unique = FALSE) {
  require_columns(data, columns, "\uc2dd\ubcc4 \ubcc0\uc218")
  if (any(vapply(data[columns], function(x) any(is.na(x)) || any(trimws(as.character(x)) == "", na.rm = TRUE), logical(1)))) {
    stop("\uc2dd\ubcc4 \ubcc0\uc218\ub098 \uc2dc\uc810\uc5d0 \uacb0\uce21\uac12 \ub610\ub294 \ube48 \uac12\uc774 \uc788\uc2b5\ub2c8\ub2e4.", call. = FALSE)
  }
  if (unique && anyDuplicated(data[columns])) stop("\uc2dd\ubcc4 \ubcc0\uc218\uc640 \uc2dc\uc810\uc758 \uc870\ud569\uc774 \uc911\ubcf5\ub429\ub2c8\ub2e4. \uc911\ubcf5\uc744 \uba3c\uc800 \ud655\uc778\ud574 \uc8fc\uc138\uc694. \uc790\ub3d9\uc73c\ub85c \ud569\uce58\uc9c0 \uc54a\uc2b5\ub2c8\ub2e4.", call. = FALSE)
}

check_reshape_size <- function(rows, columns) {
  if (as.double(rows) * as.double(columns) > 5e6) {
    stop("\ubcc0\ud658 \uacb0\uacfc\uac00 500\ub9cc \uc140\uc744 \ub118\uc2b5\ub2c8\ub2e4. \uc120\ud0dd\ud55c \uc2dc\uc810\uacfc \ubcc0\uc218\ub97c \uc904\uc5ec \uc8fc\uc138\uc694.", call. = FALSE)
  }
}

long_mapping <- function(data, p) {
  columns <- require_columns(data, p$columns, "\ubc18\ubcf5 \uce21\uc815 \ubcc0\uc218")
  require_text(p$time, "\uc2dc\uc810 \ubcc0\uc218 \uc774\ub984")
  if (identical(p$layout, "single")) {
    require_text(p$value, "\uce21\uc815\uac12 \ubcc0\uc218 \uc774\ub984")
    times <- columns
    if (!is.null(p$prefix) && nzchar(p$prefix)) {
      if (any(!startsWith(columns, p$prefix))) stop("\uc120\ud0dd\ud55c \ubcc0\uc218 \uc774\ub984\uc774 \ubaa8\ub450 \uc9c0\uc815\ud55c \uc811\ub450\uc5b4\ub85c \uc2dc\uc791\ud574\uc57c \ud569\ub2c8\ub2e4.", call. = FALSE)
      times <- substring(columns, nchar(p$prefix) + 1L)
    }
    measures <- rep(p$value, length(columns))
  } else if (identical(p$layout, "multiple")) {
    require_text(p$separator, "\uc774\ub984 \uad6c\ubd84\uc790")
    pieces <- strsplit(columns, p$separator, fixed = TRUE)
    if (any(lengths(pieces) < 2L)) stop("\ubcc0\uc218 \uc774\ub984\uc744 \uce21\uc815\uac12\uacfc \uc2dc\uc810\uc73c\ub85c \ub098\ub20c \uc218 \uc5c6\uc2b5\ub2c8\ub2e4. \uad6c\ubd84\uc790\ub97c \ud655\uc778\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
    times <- vapply(pieces, tail, character(1), n = 1L)
    measures <- vapply(pieces, function(x) paste(head(x, -1L), collapse = p$separator), character(1))
  } else stop("long \ubcc0\ud658\uc758 \uc774\ub984 \uc77d\uae30 \ubc29\uc2dd\uc744 \uc120\ud0dd\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
  if (any(!nzchar(times)) || any(!nzchar(measures))) stop("\uce21\uc815\uac12 \uc774\ub984\uc774\ub098 \uc2dc\uc810 \uc774\ub984\uc774 \ube44\uc5b4 \uc788\uc2b5\ub2c8\ub2e4.", call. = FALSE)
  mapping <- data.frame(.name = columns, .value = measures, stringsAsFactors = FALSE)
  mapping[[p$time]] <- times
  if (anyDuplicated(mapping[c(".value", p$time)])) stop("\uac19\uc740 \uce21\uc815\uac12\uacfc \uc2dc\uc810\uc73c\ub85c \uc5f0\uacb0\ub418\ub294 \ubcc0\uc218\uac00 \ub458 \uc774\uc0c1\uc785\ub2c8\ub2e4.", call. = FALSE)
  reserved <- setdiff(names(data), columns)
  if (p$time %in% c(".name", ".value") || p$time %in% c(reserved, measures) || any(measures %in% reserved)) {
    stop("\ubcc0\ud658 \ud6c4 \ubcc0\uc218 \uc774\ub984\uc774 \uae30\uc874 \ubcc0\uc218 \ub610\ub294 \uc2dc\uc810 \ubcc0\uc218\uc640 \uacb9\uce69\ub2c8\ub2e4.", call. = FALSE)
  }
  # Combining differently labelled waves would silently change the interpretation.
  for (measure in unique(measures)) {
    attrs <- lapply(data[columns[measures == measure]], function(x) attributes(x)[c("labels", "na_values", "na_range", "label")])
    if (length(attrs) > 1L && !all(vapply(attrs, identical, logical(1), attrs[[1]]))) {
      stop(paste0(measure, ": \uc2dc\uc810\ubcc4 \uac12 \ub77c\ubca8\uc774\ub098 \uacb0\uce21 \uc815\uc758\uac00 \ub2e4\ub985\ub2c8\ub2e4. \uba3c\uc800 \uc77c\uce58\uc2dc\ucf1c \uc8fc\uc138\uc694."), call. = FALSE)
    }
  }
  mapping
}

check_research_rule <- function(rule, data) {
  p <- rule_parameters(rule); op <- rule$operation[[1]]
  if (op %in% c("composite", "center", "standardize")) {
    new_column(data, p$output)
    if (!identical(rule$variable[[1]], p$output)) stop("\uc791\uc5c5\uc758 \ucd9c\ub825 \ubcc0\uc218\uc640 \uc124\uc815\uc774 \uc77c\uce58\ud558\uc9c0 \uc54a\uc2b5\ub2c8\ub2e4.", call. = FALSE)
  }
  if (op == "labels") {
    require_text(p$input, "\ubcc0\uc218"); require_columns(data, p$input, "\ubcc0\uc218")
    if (!identical(rule$variable[[1]], p$input) || !identical(p$output, p$input)) stop("\ub77c\ubca8\uc758 \ub300\uc0c1 \ubcc0\uc218\uac00 \ub2e4\ub985\ub2c8\ub2e4.", call. = FALSE)
    x <- data[[p$input]]
    if (!(is.numeric(x) || is.character(x)) || is.complex(x) || inherits(x, c("Date", "POSIXt", "difftime"))) stop("\uc22b\uc790 \ub610\ub294 \ubb38\uc790 \ubcc0\uc218\uc5d0 \ub77c\ubca8\uc744 \uc801\uc6a9\ud560 \uc218 \uc788\uc2b5\ub2c8\ub2e4.", call. = FALSE)
    if (length(p$codes) != length(p$labels) || anyNA(p$codes) || anyNA(p$labels) || any(!nzchar(p$labels)) || length(p$codes) > 5000L) stop("\uac12 \ub77c\ubca8 \uc815\uc758\ub97c \ud655\uc778\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
    codes <- if (is.numeric(x)) suppressWarnings(as.numeric(p$codes)) else as.character(p$codes)
    if (anyNA(codes) || anyDuplicated(codes) || (is.numeric(codes) && any(!is.finite(codes)))) stop("\ub77c\ubca8 \ucf54\ub4dc\uac00 \uc911\ubcf5\ub418\uac70\ub098 \ubcc0\uc218\uc758 \uc790\ub8cc\ud615\uacfc \ub9de\uc9c0 \uc54a\uc2b5\ub2c8\ub2e4.", call. = FALSE)
    if (length(p$label) != 1L || is.na(p$label)) stop("\ubcc0\uc218 \uc124\uba85\uc744 \ud655\uc778\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
  } else if (op == "composite") {
    cols <- require_columns(data, p$inputs, "\ud569\uce60 \ubb38\ud56d")
    if (!p$method %in% c("mean", "sum")) stop("\ud569\uacc4 \ub610\ub294 \ud3c9\uade0\uc744 \uc120\ud0dd\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
    if (length(p$min_valid) != 1L || !is.numeric(p$min_valid) || !is.finite(p$min_valid) ||
        p$min_valid != floor(p$min_valid) || p$min_valid < 1L || p$min_valid > length(cols)) {
      stop("\ucd5c\uc18c \uc751\ub2f5 \uc218\ub294 1 \uc774\uc0c1, \uc120\ud0dd\ud55c \ubb38\ud56d \uc218 \uc774\ud558\uc5ec\uc57c \ud569\ub2c8\ub2e4.", call. = FALSE)
    }
    invisible(lapply(cols, function(nm) plain_numeric(data[[nm]], nm)))
  } else if (op %in% c("center", "standardize")) {
    require_columns(data, p$input, "\uacc4\uc0b0\ud560 \ubcc0\uc218"); require_text(p$input, "\uacc4\uc0b0\ud560 \ubcc0\uc218")
    plain_numeric(data[[p$input]], p$input)
    if (!p$scope %in% c("all", "group", "person")) stop("\uacc4\uc0b0 \uae30\uc900\uc744 \uc120\ud0dd\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
    if (p$scope != "all") check_keys(data, p$groups)
    if (p$scope == "all" && length(p$groups)) stop("\uc804\uccb4 \uc790\ub8cc \uae30\uc900\uc5d0\ub294 \ubb36\uc74c \ubcc0\uc218\ub97c \uc9c0\uc815\ud560 \uc218 \uc5c6\uc2b5\ub2c8\ub2e4.", call. = FALSE)
    if (!p$reference %in% c("all", "subset", "fixed")) stop("\uae30\uc900\uac12 \uacc4\uc0b0 \ubc29\uc2dd\uc744 \uc120\ud0dd\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
    if (p$reference == "subset") {
      require_text(p$reference_column, "\uae30\uc900 \uc790\ub8cc\ub97c \uace0\ub97c \ubcc0\uc218")
      require_columns(data, p$reference_column, "\uae30\uc900 \uc790\ub8cc\ub97c \uace0\ub97c \ubcc0\uc218")
      if (!length(p$reference_values) || anyNA(p$reference_values)) stop("\uae30\uc900 \uc790\ub8cc\uc5d0 \ud3ec\ud568\ud560 \uac12\uc744 \uc120\ud0dd\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
    }
    if (p$reference == "fixed") {
      if (p$scope != "all") stop("\uae30\uc900\uac12 \uc9c1\uc811 \uc785\ub825\uc740 \uc804\uccb4 \uc790\ub8cc\uc5d0 \uac19\uc740 \uac12\uc744 \uc801\uc6a9\ud560 \ub54c \uc0ac\uc6a9\ud560 \uc218 \uc788\uc2b5\ub2c8\ub2e4.", call. = FALSE)
      if (length(p$center) != 1L || !is.numeric(p$center) || !is.finite(p$center)) stop("\uc720\ud6a8\ud55c \uc911\uc2ec\uac12\uc744 \uc785\ub825\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
      if (op == "standardize" && (length(p$scale) != 1L || !is.numeric(p$scale) || !is.finite(p$scale) || p$scale <= 0)) stop("\ud45c\uc900\ud3b8\ucc28\ub294 0\ubcf4\ub2e4 \ucee4\uc57c \ud569\ub2c8\ub2e4.", call. = FALSE)
    }
  } else if (op == "long") {
    check_keys(data, p$ids, unique = TRUE)
    if (length(intersect(p$ids, p$columns))) stop("\uc2dd\ubcc4 \ubcc0\uc218\ub294 \ubc18\ubcf5 \uce21\uc815 \ubcc0\uc218\ub85c \uc120\ud0dd\ud560 \uc218 \uc5c6\uc2b5\ub2c8\ub2e4.", call. = FALSE)
    if (!is.logical(p$drop_na) || length(p$drop_na) != 1L || is.na(p$drop_na)) stop("\uacb0\uce21 \uc2dc\uc810 \ucc98\ub9ac \ubc29\uc2dd\uc744 \uc120\ud0dd\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
    long_mapping(data, p)
  } else if (op == "wide") {
    require_text(p$time, "\uc2dc\uc810 \ubcc0\uc218"); require_text(p$separator, "\uc774\ub984 \uad6c\ubd84\uc790")
    require_columns(data, p$values, "\ud3bc\uce60 \uce21\uc815\uac12")
    if (anyDuplicated(c(p$ids, p$time, p$values))) stop("\uc2dd\ubcc4 \ubcc0\uc218, \uc2dc\uc810, \uce21\uc815\uac12\uc740 \uc11c\ub85c \ub2e4\ub978 \ubcc0\uc218\ub97c \uace8\ub77c \uc8fc\uc138\uc694.", call. = FALSE)
    check_keys(data, c(p$ids, p$time), unique = TRUE)
    require_columns(data, p$ids, "\uc2dd\ubcc4 \ubcc0\uc218")
  }
  if (!nrow(data)) stop("\uacc4\uc0b0\ud560 \ud589\uc774 \uc5c6\uc2b5\ub2c8\ub2e4.", call. = FALSE)
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
    note <- sprintf("\uac12\uc740 \uc720\uc9c0\ud558\uace0 \ubcc0\uc218 \uc124\uba85\uacfc \uac12 \ub77c\ubca8 %d\uac1c\ub97c \uc801\uc6a9\ud569\ub2c8\ub2e4.", length(labels))
  } else if (op == "composite") {
    m <- do.call(cbind, lapply(p$inputs, function(nm) plain_numeric(data[[nm]], nm)))
    valid <- rowSums(!is.na(m)); y <- rowSums(m, na.rm = TRUE)
    if (p$method == "mean") y <- y / pmax(valid, 1L)
    y[valid < p$min_valid] <- NA_real_
    if (any(!is.na(y) & !is.finite(y))) stop("\uacc4\uc0b0 \uacb0\uacfc\uac00 \ub108\ubb34 \ud07d\ub2c8\ub2e4. \uc785\ub825\uac12\uc744 \ud655\uc778\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
    out[[p$output]] <- y
    details <- list(valid_item_counts = as.data.frame(table(valid)), min_valid = p$min_valid,
      scored = sum(!is.na(y)), unscored = sum(is.na(y)))
    note <- sprintf("%d\ubb38\ud56d \uc911 %d\uac1c \uc774\uc0c1 \uc751\ub2f5: %d\ud589 \uacc4\uc0b0, %d\ud589 \uacb0\uce21", length(p$inputs), p$min_valid, sum(!is.na(y)), sum(is.na(y)))
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
        if (!length(ref)) stop(sprintf("\ubb36\uc74c %d\uc5d0 \uae30\uc900\uac12\uc744 \uacc4\uc0b0\ud560 \uc720\ud6a8 \uc751\ub2f5\uc774 \uc5c6\uc2b5\ub2c8\ub2e4.", g), call. = FALSE)
        center <- mean(x[ref]); scale <- if (op == "standardize") stats::sd(x[ref]) else 1
        if (!is.finite(center) || !is.finite(scale) || scale <= 0) stop(sprintf("\ubb36\uc74c %d\uc758 \ud45c\uc900\ud3b8\ucc28\ub97c \uacc4\uc0b0\ud560 \uc218 \uc5c6\uc2b5\ub2c8\ub2e4. \uc720\ud6a8 \uc751\ub2f5 2\uac1c \uc774\uc0c1\uacfc \uc11c\ub85c \ub2e4\ub978 \uac12\uc774 \ud544\uc694\ud569\ub2c8\ub2e4.", g), call. = FALSE)
      }
      y[rows] <- (x[rows] - center) / scale
      group <- if (p$scope == "all") data.frame(group = "\uc804\uccb4") else data[rows[[1]], p$groups, drop = FALSE]
      names(group) <- paste0("\uae30\uc900_", names(group))
      group$reference_n <- if (p$reference == "fixed") NA_integer_ else length(ref)
      group$center <- center; group$scale <- scale; stats[[length(stats) + 1L]] <- group
    }
    if (any(!is.na(y) & !is.finite(y))) stop("\uacc4\uc0b0 \uacb0\uacfc\uac00 \ub108\ubb34 \ud07d\ub2c8\ub2e4. \uae30\uc900\uac12\uc744 \ud655\uc778\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
    out[[p$output]] <- y; table <- do.call(rbind, stats); rownames(table) <- NULL
    details <- list(reference = p$reference, scope = p$scope, constants = table,
      sd_definition = if (op == "standardize" && p$reference != "fixed") "sample n-1" else "specified or unused")
    note <- paste(vapply(seq_len(min(nrow(table), 6L)), function(i) sprintf("%s: n=%s, \uc911\uc2ec=%g%s",
      paste(as.character(table[i, seq_len(ncol(table) - 3L), drop = TRUE]), collapse = "/"),
      ifelse(is.na(table$reference_n[[i]]), "\uc9c1\uc811 \uc785\ub825", table$reference_n[[i]]), table$center[[i]],
      if (op == "standardize") sprintf(", \ud45c\uc900\ud3b8\ucc28=%g", table$scale[[i]]) else ""), character(1)), collapse = "; ")
    if (nrow(table) > 6L) note <- paste0(note, "; \ub098\uba38\uc9c0 \uae30\uc900\uac12\uc740 \ub2e4\uc6b4\ub85c\ub4dc\ud55c \uc801\uc6a9 \uae30\ub85d\uc5d0\uc11c \ud655\uc778\ud560 \uc218 \uc788\uc2b5\ub2c8\ub2e4.")
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
    note <- paste0(nrow(data), "\ud589 \u2192 ", nrow(out), "\ud589. ", paste(p$ids, collapse = ", "), "\uc640 ", p$time, " \uc870\ud569 \ud655\uc778. ",
      if (p$drop_na) paste0("\ubaa8\ub4e0 \uce21\uc815\uac12\uc774 \uacb0\uce21\uc778 \uc2dc\uc810 ", details$dropped_rows, "\ud589 \uc81c\uc678.") else "\uacb0\uce21 \uc2dc\uc810 \uc720\uc9c0.",
      " \uc5f0\uacb0: ", paste(head(paste0(mapping$.name, " \u2192 ", mapping$.value, " [", mapping[[p$time]], "]"), 8L), collapse = "; "))
    affected <- nrow(out)
  } else {
    ids <- p$ids; time <- p$time; values <- p$values
    groups <- as.integer(vctrs::vec_group_id(data[ids]))
    extra <- setdiff(names(data), c(ids, time, values))
    for (nm in extra) {
      if (nrow(unique(data[c(ids, nm)])) > length(unique(groups))) stop(paste0(nm, ": \uac19\uc740 \ub300\uc0c1 \uc548\uc5d0\uc11c \uac12\uc774 \ub2e4\ub985\ub2c8\ub2e4. \ud3bc\uce60 \uce21\uc815\uac12\uc5d0 \ud3ec\ud568\ud558\uac70\ub098 \uc2dd\ubcc4 \ubcc0\uc218\ub97c \ud655\uc778\ud574 \uc8fc\uc138\uc694."), call. = FALSE)
    }
    times <- unique(data[[time]])
    check_reshape_size(length(unique(groups)), length(ids) + length(extra) + as.double(length(values)) * length(times))
    mapping <- data.frame(.name = unlist(lapply(values, function(v) paste0(v, p$separator, as.character(times)))),
      .value = rep(values, each = length(times)), stringsAsFactors = FALSE)
    mapping[[time]] <- rep(times, times = length(values))
    if (anyDuplicated(mapping$.name) || any(mapping$.name %in% c(ids, extra))) stop("\ud3bc\uce5c \ubcc0\uc218 \uc774\ub984\uc774 \uc911\ubcf5\ub418\uac70\ub098 \uae30\uc874 \ubcc0\uc218\uc640 \uacb9\uce69\ub2c8\ub2e4. \uc774\ub984 \uad6c\ubd84\uc790\ub098 \uc2dc\uc810 \uac12\uc744 \ubc14\uafd4 \uc8fc\uc138\uc694.", call. = FALSE)
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
    note <- sprintf("%d\ud589 \u2192 %d\ud589. \uc911\ubcf5 \uc870\ud569 \uc5c6\uc74c. \uc5c6\ub294 \uc2dc\uc810 \uc870\ud569 %d\uac1c\ub294 \uacb0\uce21\uc73c\ub85c \ucc44\uc6e0\uc2b5\ub2c8\ub2e4.", nrow(data), nrow(out), details$absent_combinations)
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
