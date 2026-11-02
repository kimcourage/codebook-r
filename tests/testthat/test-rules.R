testthat::test_that("missing codes come only from explicit codebook text", {
  d <- data.frame(gender = c(1, 2, 9), income = c(100, 9999, 200), score = 99)
  cb <- empty_codebook_records()
  cb <- rbind(cb, data.frame(variable = c("gender", "income", "score"),
          label = c("성별", "소득", "점수"), value_code = c("9", "9999", "99"),
          value_label = c("무응답", "응답 거부", "정상 코드"), note = "",
          source_file = "c.xlsx", source_loc = "S!row 3", stringsAsFactors = FALSE))
  rules <- heuristic_missing_rules(cb, d)
  testthat::expect_setequal(rules$variable, c("gender", "income"))
  testthat::expect_false("score" %in% rules$variable)
})

testthat::test_that("approved operations work in order and leave original untouched", {
  original <- data.frame(q = c(1, 2, 9, 5), g = c(1, 2, 9, 1))
  rules <- empty_rules()
  rules <- rbind(rules, data.frame(id = 1L, variable = "q", operation = "set_na",
          values = "9", minimum = NA_real_, maximum = NA_real_, reason = "test",
          evidence = "test", confidence = "manual", source = "test"))
  rules <- rbind(rules, data.frame(id = 2L, variable = "q", operation = "reverse",
          values = "", minimum = 1, maximum = 5, reason = "test",
          evidence = "test", confidence = "manual", source = "test"))
  result <- apply_approved_rules(original, rules)
  testthat::expect_equal(result$data$q, c(5, 4, NA_real_, 1))
  testthat::expect_equal(original$q, c(1, 2, 9, 5))
  testthat::expect_equal(result$audit$n_affected, c(1L, 3L))
})

testthat::test_that("reverse out-of-range values fail rather than silently change", {
  d <- data.frame(q = c(1, 2, 9))
  rules <- data.frame(id = 1L, variable = "q", operation = "reverse",
    values = "", minimum = 1, maximum = 5, reason = "test",
    evidence = "test", confidence = "manual", source = "test")
  testthat::expect_error(apply_approved_rules(d, rules), "벗어납니다")
})

testthat::test_that("unapproved or unknown AI operations cannot be executed", {
  d <- data.frame(q = 1:3)
  r <- data.frame(id = 1L, variable = "q", operation = "execute_r",
    values = "system('bad')", minimum = NA_real_, maximum = NA_real_,
    reason = "", evidence = "", confidence = "", source = "AI")
  testthat::expect_error(apply_approved_rules(d, r), "허용되지 않은")
  testthat::expect_equal(nrow(append_rule_candidates(empty_rules(), r, d)), 0L)
})
