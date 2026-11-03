testthat::test_that("variable descriptions appear even when every data value stays the same", {
  d <- data.frame(q = c(1, 2))
  rule <- new_research_rule("labels", list(input = "q", output = "q", label = "생활 만족도",
    codes = character(), labels = character()), "코드북 설명 적용", 1L)
  p <- preview_rules(d, rule)
  shown <- preview_label_changes(d, p$data, "q")$table
  testthat::expect_equal(raw_codes(p$data$q), raw_codes(d$q))
  testthat::expect_equal(shown[1, ], data.frame("항목" = "변수 설명", "적용 전" = "없음",
    "적용 후" = "생활 만족도", "변화" = "변경", check.names = FALSE))
  testthat::expect_null(attr(d$q, "label"))
  testthat::expect_equal(attr(apply_approved_rules(d, rule)$data$q, "label"), shown[[3]][[1]])
})

testthat::test_that("label additions, replacements, removals and unchanged labels are distinguished", {
  d <- data.frame(q = haven::labelled(1:4, c(옛이름 = 1, 유지 = 2, 삭제 = 3), label = "이전 설명"))
  after <- data.frame(q = haven::labelled(1:4, c(새이름 = 1, 유지 = 2, 추가 = 4), label = "새 설명"))
  shown <- preview_label_changes(d, after, "q")$table
  testthat::expect_equal(shown[[4]], c("변경", "변경", "제거", "추가", "변경 없음"))
  testthat::expect_equal(shown[[2]], c("이전 설명", "옛이름", "삭제", "없음", "유지"))
  testthat::expect_equal(shown[[3]], c("새 설명", "새이름", "없음", "추가", "유지"))
  testthat::expect_true(all(preview_label_changes(after, after, "q")$table[[4]] == "변경 없음"))
  removed <- preview_label_changes(d, data.frame(q = factor(1:4)), "q")$table
  testthat::expect_equal(removed[[4]][grepl("^값 ", removed[[1]])], rep("제거", 3))
})

testthat::test_that("large label lists prioritize actual changes and disclose the display limit", {
  labels <- stats::setNames(1:120, paste0("설명", 1:120))
  d <- data.frame(q = haven::labelled(1:2, labels))
  after <- d; names(labels)[120] <- "변경한 설명"; attr(after$q, "labels") <- labels
  shown <- preview_label_changes(d, after, "q")
  testthat::expect_equal(nrow(shown$table), 101)
  testthat::expect_equal(shown$table[[1]][2], "값 120의 라벨")
  testthat::expect_equal(shown$table[[3]][2], "변경한 설명")
  testthat::expect_match(shown$note, "120개 중 100개")
  testthat::expect_match(shown$note, "바뀌는 라벨 1개")
})

testthat::test_that("reshaped variables and tagged missing labels are not misrepresented", {
  d <- data.frame(q = haven::labelled(c(1, haven::tagged_na("a")),
    c(응답 = 1, 무응답 = haven::tagged_na("a"))))
  shown <- preview_label_changes(d, d, "q")$table
  testthat::expect_true("값 NA(a)의 라벨" %in% shown[[1]])
  moved <- preview_label_changes(d, data.frame(score = c(1, NA)), "q")
  testthat::expect_equal(moved$table[[4]], "구조 변경")
  testthat::expect_match(moved$note, "변수 이름이 결과에 없습니다", fixed = TRUE)
})

testthat::test_that("label preview follows selection and withholds partial results on errors", {
  f <- server_fixture()
  shiny::testServer(f$server, {
    session$flushReact()
    session$setInputs(load_example = 1L, review_variable = "case_id")
    label_key <- rule_keys()[rules()$variable == "case_id" & rules()$operation == "labels"]
    session$setInputs(approved_rules = label_key)
    testthat::expect_match(output$review_metadata, "Anonymous respondent ID", fixed = TRUE)
    testthat::expect_match(output$review_metadata, "변경", fixed = TRUE)
    testthat::expect_null(current_result())
    session$setInputs(approved_rules = character())
    testthat::expect_match(output$review_metadata, "변경 없음", fixed = TRUE)
    testthat::expect_false(grepl("Anonymous respondent ID", output$review_metadata, fixed = TRUE))
    session$setInputs(edit_rule = "new", manual_op = "reverse", manual_var = "life_sat",
      manual_min = 1, manual_max = 5, manual_reason = "범위 오류 확인", add_manual = 1L)
    reverse_key <- rule_keys()[rules()$operation == "reverse"]
    session$setInputs(approved_rules = c(label_key, reverse_key))
    testthat::expect_false(plan_preview()$ok)
    testthat::expect_null(review_label_preview()$table)
    testthat::expect_match(output$review_metadata_note, "작업 오류", fixed = TRUE)
  })
})
