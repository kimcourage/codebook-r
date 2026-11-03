# Explicit numeric intervals. Blank bounds are unbounded; no expressions are run.
binning_definition <- function(p) {
  bins <- p$bins
  fields <- c("lower", "upper", "lower_closed", "upper_closed", "label")
  if (!is.data.frame(bins) || !all(fields %in% names(bins)) ||
      nrow(bins) < 1L || nrow(bins) > 30L) stop("\uad6c\uac04\uc740 1\uac1c\uc5d0\uc11c 30\uac1c\uae4c\uc9c0 \uc9c0\uc815\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
  if (!is.character(bins$label) || anyNA(bins$label) ||
      any(!nzchar(trimws(bins$label))) || any(grepl("[\r\n\t]", bins$label))) {
    stop("\uac01 \uad6c\uac04\uc758 \ubc94\uc8fc\uba85\uc744 \uc785\ub825\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
  }
  bins$label <- trimws(bins$label)
  if (anyDuplicated(bins$label)) stop("\ubc94\uc8fc\uba85\uc774 \uc911\ubcf5\ub429\ub2c8\ub2e4. \uad6c\uac04\ub9c8\ub2e4 \ub2e4\ub978 \uc774\ub984\uc744 \uc9c0\uc815\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
  for (nm in c("lower_closed", "upper_closed")) {
    if (!is.logical(bins[[nm]]) || anyNA(bins[[nm]])) stop("\uacbd\uacc4\uac12\uc758 \ud3ec\ud568 \uc5ec\ubd80\ub97c \uc9c0\uc815\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
  }
  bound <- function(x, unlimited) {
    if (!is.character(x) || anyNA(x)) stop("\uacbd\uacc4\uac12\uc740 \uc22b\uc790\ub85c \uc785\ub825\ud558\uace0, \uc81c\ud55c\uc774 \uc5c6\uc73c\uba74 \ube44\uc6cc \ub450\uc138\uc694.", call. = FALSE)
    x <- trimws(x); absent <- !nzchar(x)
    y <- suppressWarnings(as.numeric(x)); y[absent] <- unlimited
    if (any(!absent & !is.finite(y))) stop("\uacbd\uacc4\uac12\uc740 \uc22b\uc790\ub85c \uc785\ub825\ud558\uace0, \uc81c\ud55c\uc774 \uc5c6\uc73c\uba74 \ube44\uc6cc \ub450\uc138\uc694.", call. = FALSE)
    y
  }
  lower <- bound(bins$lower, -Inf); upper <- bound(bins$upper, Inf)
  if (any(lower > upper | (lower == upper & !(bins$lower_closed & bins$upper_closed)))) {
    stop("\uc2dc\uc791\uac12\uc774 \ub05d\uac12\ubcf4\ub2e4 \ud06c\uac70\ub098 \ud3ec\ud568\ub418\ub294 \uac12\uc774 \uc5c6\ub294 \uad6c\uac04\uc774 \uc788\uc2b5\ub2c8\ub2e4.", call. = FALSE)
  }
  contains <- function(i, x) (x > lower[i] || (x == lower[i] && bins$lower_closed[i])) &&
    (x < upper[i] || (x == upper[i] && bins$upper_closed[i]))
  if (nrow(bins) > 1L) for (i in seq_len(nrow(bins) - 1L)) for (j in seq.int(i + 1L, nrow(bins))) {
    lo <- max(lower[i], lower[j]); hi <- min(upper[i], upper[j])
    if (lo < hi || (lo == hi && contains(i, lo) && contains(j, lo))) {
      stop(sprintf("%d\ubc88 \uad6c\uac04\uacfc %d\ubc88 \uad6c\uac04\uc774 \uacb9\uce69\ub2c8\ub2e4. \uacbd\uacc4\uac12\uacfc \ud3ec\ud568 \uc5ec\ubd80\ub97c \ud655\uc778\ud574 \uc8fc\uc138\uc694.", i, j), call. = FALSE)
    }
  }
  if (!identical(p$outside, "error") && !identical(p$outside, "na")) stop("\uad6c\uac04 \ubc16 \uac12\uc758 \ucc98\ub9ac \ubc29\ubc95\uc744 \uc120\ud0dd\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
  if (!is.logical(p$ordered) || length(p$ordered) != 1L || is.na(p$ordered)) stop("\ubc94\uc8fc \uc21c\uc11c \uc124\uc815\uc744 \ud655\uc778\ud574 \uc8fc\uc138\uc694.", call. = FALSE)
  ranges <- vapply(seq_len(nrow(bins)), function(i) {
    a <- if (is.infinite(lower[i])) "" else paste0(format(lower[i], trim = TRUE, digits = 15), if (bins$lower_closed[i]) " \uc774\uc0c1" else " \ucd08\uacfc")
    b <- if (is.infinite(upper[i])) "" else paste0(format(upper[i], trim = TRUE, digits = 15), if (bins$upper_closed[i]) " \uc774\ud558" else " \ubbf8\ub9cc")
    if (!nzchar(a) && !nzchar(b)) "\ubaa8\ub4e0 \uc720\ud6a8 \uac12" else paste(c(a, b)[nzchar(c(a, b))], collapse = " ~ ")
  }, character(1))
  list(bins = bins, lower = lower, upper = upper, ranges = ranges)
}

binning_result <- function(x, p, enforce = TRUE) {
  definition <- binning_definition(p)
  bins <- definition$bins
  values <- plain_numeric(x, p$input)
  membership <- rep(NA_integer_, length(values))
  for (i in seq_len(nrow(bins))) {
    hit <- !is.na(values) &
      (values > definition$lower[i] | (bins$lower_closed[i] & values == definition$lower[i])) &
      (values < definition$upper[i] | (bins$upper_closed[i] & values == definition$upper[i]))
    membership[hit] <- i
  }
  outside <- !is.na(values) & is.na(membership)
  if (enforce && any(outside) && p$outside == "error") {
    stop(sprintf("\uad6c\uac04 \ubc16 \uac12\uc774 %d\uac1c \uc788\uc2b5\ub2c8\ub2e4 (\uc608: %s). \uad6c\uac04\uc744 \uc218\uc815\ud558\uac70\ub098 \uad6c\uac04 \ubc16 \uac12\uc744 \uacb0\uce21\uc73c\ub85c \ucc98\ub9ac\ud558\ub3c4\ub85d \uc120\ud0dd\ud574 \uc8fc\uc138\uc694.",
      sum(outside), paste(head(unique(values[outside]), 5L), collapse = ", ")), call. = FALSE)
  }
  result <- factor(membership, levels = seq_len(nrow(bins)), labels = bins$label, ordered = p$ordered)
  counts <- bins
  counts$range <- definition$ranges
  counts$n <- tabulate(membership, nbins = nrow(bins))
  missing <- sum(is.na(values)); excluded <- sum(outside)
  note <- sprintf("%s \u2192 %s: %d\uac1c \uad6c\uac04, %d\ud589 \ubd84\ub958. \uc6d0\ub798 \uacb0\uce21 %d\uac1c, \uad6c\uac04 \ubc16 %d\uac1c%s.",
    p$input, p$output, nrow(bins), sum(!is.na(result)), missing, excluded,
    if (p$outside == "na") "\ub294 \uc0c8 \ubcc0\uc218\uc5d0\uc11c \uacb0\uce21 \ucc98\ub9ac" else "")
  list(value = result, missing_before = missing, missing_after = sum(is.na(result)),
    n_affected = sum(!is.na(result)), note = note,
    details = list(input = p$input, output = p$output, bins = counts,
      outside_policy = p$outside, outside_n = excluded, original_missing_n = missing, ordered = p$ordered))
}
