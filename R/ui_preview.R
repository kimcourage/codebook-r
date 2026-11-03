# Compare actual preview attributes, not proposed codebook text or sampled values.
preview_label_changes <- function(before, after, variable, limit = 100L) {
  row <- function(item, old, new, status) {
    data.frame("\ud56d\ubaa9" = item, "\uc801\uc6a9 \uc804" = old, "\uc801\uc6a9 \ud6c4" = new,
               "\ubcc0\ud654" = status, check.names = FALSE, stringsAsFactors = FALSE)
  }
  if (!variable %in% names(after)) {
    return(list(table = row("\ubcc0\uc218", variable, "\uc774 \uc774\ub984\uc758 \ubcc0\uc218 \uc5c6\uc74c", "\uad6c\uc870 \ubcc0\uacbd"),
      note = "\uc790\ub8cc \ud615\ud0dc\uac00 \ubc14\ub00c\uc5b4 \uc120\ud0dd\ud55c \ubcc0\uc218 \uc774\ub984\uc774 \uacb0\uacfc\uc5d0 \uc5c6\uc2b5\ub2c8\ub2e4. \uc544\ub798 \uc790\ub8cc \uac12\uc5d0\uc11c \uacb0\uacfc\uc758 \ubcc0\uc218\uba85\uc744 \ud655\uc778\ud558\uc138\uc694."))
  }
  description <- function(x) {
    label <- attr(x, "label", exact = TRUE)
    if (length(label) != 1L || is.na(label) || !nzchar(label)) "" else as.character(label)
  }
  labels_by_code <- function(x) {
    labels <- factor_value_labels(x)
    if (!length(labels)) return(stats::setNames(character(), character()))
    codes <- as.character(unname(labels))
    codes[is.na(codes)] <- "NA"
    if (is.double(labels)) {
      tagged <- haven::is_tagged_na(labels)
      codes[tagged] <- paste0("NA(", haven::na_tag(labels[tagged]), ")")
    }
    text <- names(labels)
    if (is.null(text)) text <- rep("\uc774\ub984 \uc5c6\uc74c", length(labels))
    keys <- unique(codes)
    stats::setNames(vapply(keys, function(code) paste(text[codes == code], collapse = ", "), character(1)), keys)
  }
  old <- before[[variable]]; new <- after[[variable]]
  old_description <- description(old); new_description <- description(new)
  rows <- row("\ubcc0\uc218 \uc124\uba85", if (nzchar(old_description)) old_description else "\uc5c6\uc74c",
    if (nzchar(new_description)) new_description else "\uc5c6\uc74c",
    if (identical(old_description, new_description)) "\ubcc0\uacbd \uc5c6\uc74c" else "\ubcc0\uacbd")
  if (is.factor(old) || is.factor(new)) {
    old_type <- paste(class(old), collapse = "/"); new_type <- paste(class(new), collapse = "/")
    rows <- rbind(row("\uc790\ub8cc\ud615", old_type, new_type,
      if (identical(old_type, new_type)) "\ubcc0\uacbd \uc5c6\uc74c" else "\ubcc0\uacbd"), rows)
  }
  old_labels <- labels_by_code(old); new_labels <- labels_by_code(new)
  codes <- union(names(old_labels), names(new_labels))
  note <- ""
  if (!length(codes)) {
    rows <- rbind(rows, row("\uac12 \ub77c\ubca8", "\uc5c6\uc74c", "\uc5c6\uc74c", "\ubcc0\uacbd \uc5c6\uc74c"))
  } else {
    old_text <- unname(old_labels[match(codes, names(old_labels))])
    new_text <- unname(new_labels[match(codes, names(new_labels))])
    added <- is.na(old_text); removed <- is.na(new_text)
    changed <- added | removed | (!added & !removed & old_text != new_text)
    status <- ifelse(added, "\ucd94\uac00", ifelse(removed, "\uc81c\uac70", ifelse(changed, "\ubcc0\uacbd", "\ubcc0\uacbd \uc5c6\uc74c")))
    old_text[added] <- "\uc5c6\uc74c"; new_text[removed] <- "\uc5c6\uc74c"
    # Show changed labels first so a large unchanged dictionary cannot hide a change.
    index <- head(order(!changed, seq_along(codes)), limit)
    rows <- rbind(rows, row(paste0("\uac12 ", codes[index], "\uc758 \ub77c\ubca8"),
      old_text[index], new_text[index], status[index]))
    if (length(codes) > limit) note <- sprintf(
      "\uac12 \ub77c\ubca8 %d\uac1c \uc911 %d\uac1c\ub97c \ud45c\uc2dc\ud569\ub2c8\ub2e4. \ubc14\ub00c\ub294 \ub77c\ubca8 %d\uac1c\ub97c \uba3c\uc800 \uc815\ub82c\ud588\uc2b5\ub2c8\ub2e4.",
      length(codes), length(index), sum(changed))
  }
  list(table = rows, note = note)
}
