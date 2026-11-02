# Data I/O and metadata. No API requests are performed by this module.

normalize_import_options <- function(csv_encoding = "UTF-8", character_columns = character()) {
  if (!csv_encoding %in% c("UTF-8", "CP949", "EUC-KR")) {
    stop("CSV \ubb38\uc790 \uc778\ucf54\ub529\uc740 UTF-8, CP949, EUC-KR \uc911\uc5d0\uc11c \uc120\ud0dd\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
  }
  character_columns <- unique(trimws(as.character(character_columns)))
  list(csv_encoding = csv_encoding, character_columns = character_columns[nzchar(character_columns)])
}

read_dataset <- function(path, original_name = basename(path), csv_encoding = "UTF-8",
                         character_columns = character()) {
  if (!file.exists(path)) stop("\ub370\uc774\ud130 \ud30c\uc77c\uc744 \ucc3e\uc744 \uc218 \uc5c6\uc2b5\ub2c8\ub2e4.", call. = FALSE)
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
      "CSV \uad6c\uc870 \uc624\ub958 %d\uac74\uc774 \uc788\uc2b5\ub2c8\ub2e4. \ub530\uc634\ud45c, \uc5f4 \uac1c\uc218\uc640 \uc778\ucf54\ub529\uc744 \ud655\uc778\ud55c \ub4a4 \ub2e4\uc2dc \uc77d\uc5b4 \uc8fc\uc138\uc694.",
      nrow(problems)), call. = FALSE)
    if (anyNA(names(raw)) || any(!nzchar(names(raw))) || anyDuplicated(names(raw))) {
      stop("\ubcc0\uc218\uba85\uc774 \ube44\uc5b4 \uc788\uac70\ub098 \uc911\ubcf5\ub418\uc5b4 \uc788\uc2b5\ub2c8\ub2e4. \uc6d0\uc790\ub8cc\uc5d0\uc11c \uc218\uc815\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
    }
    strings <- c(names(raw), unlist(raw, use.names = FALSE))
    if (any(!is.na(strings) & is.na(iconv(strings, from = "UTF-8", to = "UTF-8", sub = NA)))) {
      stop("CSV \ud55c\uae00 \uc778\ucf54\ub529\uc744 \uc77d\uc9c0 \ubabb\ud588\uc2b5\ub2c8\ub2e4. \uc77d\uae30 \uc635\uc158\uc5d0\uc11c CP949 \ub610\ub294 EUC-KR\uc744 \uc120\ud0dd\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
    }
    absent <- setdiff(options$character_columns, names(raw))
    if (length(absent)) stop(paste("\ubb38\uc790\ub85c \uc77d\uc744 \ubcc0\uc218\uba85\uc774 \uc5c6\uc2b5\ub2c8\ub2e4:", paste(absent, collapse = ", ")),
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
    stop(sprintf("\uc9c0\uc6d0\ud558\uc9c0 \uc54a\ub294 \ub370\uc774\ud130 \ud615\uc2dd: .%s", ext), call. = FALSE)
  )
  if (!is.data.frame(data)) stop("RDS \ud30c\uc77c\uc5d0\ub294 data.frame\uc774 \ub4e4\uc5b4 \uc788\uc5b4\uc57c \ud569\ub2c8\ub2e4.", call. = FALSE)
  data <- as.data.frame(data)  # Avoid by-reference semantics from e.g. data.table RDS.
  if (!ncol(data)) stop("\uc5f4\uc774 \uc5c6\ub294 \ub370\uc774\ud130\uc14b\uc785\ub2c8\ub2e4.", call. = FALSE)
  nms <- names(data)
  if (anyNA(nms) || any(!nzchar(nms)) || anyDuplicated(nms)) {
    stop("\ubcc0\uc218\uba85\uc774 \ube44\uc5b4 \uc788\uac70\ub098 \uc911\ubcf5\ub418\uc5b4 \uc788\uc2b5\ub2c8\ub2e4. \uc6d0\uc790\ub8cc\uc5d0\uc11c \uc218\uc815\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
  }
  unsupported <- nms[vapply(data, function(x) !is.atomic(x) || !is.null(dim(x)), logical(1))]
  if (length(unsupported)) stop(paste("\ubaa9\ub85d\u00b7\ud589\ub82c \ud615\ud0dc\uc758 \uc5f4\uc740 \uc9c0\uc6d0\ud558\uc9c0 \uc54a\uc2b5\ub2c8\ub2e4:",
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
    if (length(labels) > 20L) value_labels <- paste0(value_labels, "; \u2026 (\ucd1d ", length(labels), "\uac1c)")
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
  names(x) <- c("\ubcc0\uc218", "\uc790\ub8cc\ud615", "\ubcc0\uc218 \uc124\uba85", "\uac12 \ub77c\ubca8 (\ucd5c\ub300 20\uac1c)", "\uc9c0\uc815 \uacb0\uce21\uac12", "\uacb0\uce21 \uc218", "\uacb0\uce21\ub960 (%)")
  x
}

# Safe metadata for the LLM: no row-level records and no respondent values.
metadata_for_ai <- function(data, max_variables = 200L) {
  meta <- variable_metadata(data)
  meta <- head(meta[c("variable", "class", "label")], max_variables)
  meta[] <- lapply(meta, function(x) substr(as.character(x), 1L, 200L))
  meta
}
