preview_test_rule <- function(id, variable, op, values = "", minimum = NA_real_, maximum = NA_real_) {
  data.frame(id = id, variable = variable, operation = op, values = values,
    minimum = minimum, maximum = maximum, reason = "Synthetic decision",
    evidence = "Synthetic codebook", confidence = "manual", source = "manual")
}

testthat::test_that("preview follows sequential dependencies and leaves the source unchanged", {
  d <- data.frame(q = c(1, 3, 9))
  r <- rbind(preview_test_rule(1L, "q", "set_na", "9"),
             preview_test_rule(2L, "q", "reverse", minimum = 1, maximum = 5))
  p <- preview_rules(d, r)
  testthat::expect_true(p$ok)
  testthat::expect_equal(p$report$n_affected, c(1L, 2L))
  testthat::expect_equal(p$report$missing_after, c(1L, 1L))
  testthat::expect_equal(d$q, c(1, 3, 9))
  applied <- apply_approved_rules(d, r)
  testthat::expect_equal(applied$audit$n_affected, p$report$n_affected)
  testthat::expect_equal(applied$data$q, c(5, 3, NA_real_))
})

testthat::test_that("a failed preview blocks later estimates and identifies the rule", {
  d <- data.frame(q = c(1, 9))
  r <- rbind(preview_test_rule(2L, "q", "reverse", minimum = 1, maximum = 5),
             preview_test_rule(1L, "q", "set_na", "9"))
  p <- preview_rules(d, r)
  testthat::expect_false(p$ok)
  testthat::expect_match(p$error, "#2 q", fixed = TRUE)
  testthat::expect_identical(p$report$status, c("수정 필요", "대기"))
  testthat::expect_true(all(is.na(p$report$n_affected)))
})

testthat::test_that("label changes and declared missing normalization are explicit", {
  d <- data.frame(q = haven::labelled_spss(c(1, 9), labels = c(yes = 1, missing = 9), na_values = 9, label = "Question"))
  r <- rbind(preview_test_rule(1L, "q", "set_na", "9"), preview_test_rule(2L, "q", "factor"))
  p <- preview_rules(d, r)
  x <- apply_approved_rules(d, r)$data$q
  testthat::expect_equal(p$report$n_affected, c(1L, 1L))
  testthat::expect_match(p$report$note[[2]], "값 라벨은 제거", fixed = TRUE)
  testthat::expect_identical(attr(x, "label"), "Question")
  testthat::expect_true(is.factor(x))
  testthat::expect_identical(levels(x), "1")
  testthat::expect_true(is.na(x[[2]]))
})

testthat::test_that("rule edits preserve identity and earlier evidence but reject duplicates", {
  d <- data.frame(q = c(1, 9), g = c(1, 9))
  r <- rbind(preview_test_rule(1L, "q", "set_na", "9"), preview_test_rule(2L, "g", "set_na", "9"))
  edited <- edit_rule_candidate(r, 1L, preview_test_rule(99L, "q", "set_na", "1"), d)
  testthat::expect_equal(edited$id, c(1L, 2L))
  testthat::expect_match(edited$evidence[[1]], "9 → 결측값", fixed = TRUE)
  testthat::expect_error(edit_rule_candidate(r, 1L, r[2, ], d), "동일한 작업")
  testthat::expect_error(edit_rule_candidate(r, NA_integer_, r[2, ], d), "다시 선택")
})
