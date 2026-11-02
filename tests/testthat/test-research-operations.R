testthat::test_that("composites enforce minimum answers and never turn all missing into zero", {
  d <- data.frame(q1 = c(1, 3, NA, NA), q2 = c(3, NA, 5, NA))
  mean_rule <- research_rule("composite", composite_parameters())
  x <- apply_approved_rules(d, mean_rule)
  testthat::expect_equal(x$data$score, c(2, NA, NA, NA))
  sum_rule <- research_rule("composite", composite_parameters(method = "sum", min_valid = 1L))
  testthat::expect_equal(apply_approved_rules(d, sum_rule)$data$score, c(4, 3, 5, NA))
  testthat::expect_identical(x$data[names(d)], d)
  testthat::expect_error(apply_approved_rules(d, research_rule("composite", composite_parameters(min_valid = 3))), "최소 응답")
  testthat::expect_error(apply_approved_rules(d, research_rule("composite", composite_parameters(min_valid = 0))), "최소 응답")
  testthat::expect_error(apply_approved_rules(d, research_rule("composite", composite_parameters(output = "q1"))), "이미 있는")
  testthat::expect_error(apply_approved_rules(transform(d, q1 = factor(q1)), mean_rule), "숫자")
  testthat::expect_error(apply_approved_rules(transform(d, q1 = Inf), mean_rule), "무한값")
  testthat::expect_error(apply_approved_rules(transform(d, q1 = 1 + 2i), mean_rule), "숫자")
})

testthat::test_that("SPSS missing codes are excluded from arithmetic without modifying originals", {
  d <- data.frame(q1 = haven::labelled_spss(c(1, 9), na_values = 9), q2 = c(3, 5))
  x <- apply_approved_rules(d, research_rule("composite", composite_parameters()))$data
  testthat::expect_equal(x$score, c(2, NA))
  testthat::expect_equal(as.numeric(d$q1), c(1, 9))
})

testthat::test_that("centering and z scores use explicit reference rows and sample standard deviation", {
  d <- data.frame(score = c(1, 2, 3, NA))
  r <- research_rule("standardize", scale_parameters())
  x <- apply_approved_rules(d, r)
  testthat::expect_equal(x$data$score_z, c(-1, 0, 1, NA))
  details <- jsonlite::fromJSON(x$audit$details[[1]])
  testthat::expect_equal(details$constants$center, 2)
  testthat::expect_equal(details$constants$scale, 1)
  testthat::expect_equal(details$constants$reference_n, 3)
  fixed <- research_rule("standardize", scale_parameters(reference = "fixed", center = 10, scale = 2))
  testthat::expect_equal(apply_approved_rules(d, fixed)$data$score_z, c(-4.5, -4, -3.5, NA))
  testthat::expect_error(apply_approved_rules(data.frame(score = c(2, 2)), r), "표준편차")
  testthat::expect_error(apply_approved_rules(data.frame(score = c(NA_real_, NA)), r), "유효 응답")
  testthat::expect_error(apply_approved_rules(d, research_rule("standardize", scale_parameters(reference = "fixed", scale = 0))), "0보다")
})

testthat::test_that("group and person reference choices retain the unit of analysis", {
  d <- data.frame(score = c(1, 3, 5, 7), id = c("A", "A", "B", "B"), wave = c("pre", "post", "pre", "post"))
  p <- scale_parameters(output = "centered", scope = "person", groups = "id", reference = "subset", reference_column = "wave", reference_values = "pre")
  x <- apply_approved_rules(d, research_rule("center", p))
  testthat::expect_equal(x$data$centered, c(0, 2, 0, 2))
  testthat::expect_equal(jsonlite::fromJSON(x$audit$details[[1]])$constants$center, c(1, 5))
  testthat::expect_error(apply_approved_rules(d, research_rule("standardize", p)), "표준편차")
  p$reference_values <- "absent"
  testthat::expect_error(apply_approved_rules(d, research_rule("center", p)), "유효 응답")
  p$reference <- "all"
  d$id[[1]] <- NA_character_
  testthat::expect_error(apply_approved_rules(d, research_rule("center", p)), "결측값")
})

testthat::test_that("multi-measure long and wide transformations round-trip values and identifiers", {
  d <- research_fixture(); r <- research_rule("long", long_parameters())
  long <- apply_approved_rules(d, r)
  testthat::expect_equal(dim(long$data), c(8L, 5L))
  testthat::expect_identical(long$data$id, rep(d$id, each = 2L))
  testthat::expect_equal(long$data$q1, c(1, 2, 2, 3, 3, 4, 4, 5))
  testthat::expect_equal(long$audit$rows_before, 4L)
  p <- list(ids = "id", time = "occasion", values = c("q1", "q2"), separator = "_")
  wide <- apply_approved_rules(long$data, research_rule("wide", p))$data
  testthat::expect_identical(wide[names(d)], d)
  testthat::expect_identical(d, research_fixture())
  testthat::expect_equal(jsonlite::fromJSON(long$audit$details[[1]])$mapping$.value, c("q1", "q2", "q1", "q2"))
})

testthat::test_that("single-measure long keeps missing visits unless explicitly excluded", {
  d <- data.frame(id = c("001", "002"), score_pre = c(1, NA), score_post = c(2, NA))
  p <- list(ids = "id", columns = c("score_pre", "score_post"), layout = "single", time = "occasion", value = "score", prefix = "score_", separator = "_", drop_na = FALSE)
  x <- apply_approved_rules(d, research_rule("long", p))$data
  testthat::expect_equal(nrow(x), 4L)
  testthat::expect_identical(x$occasion, rep(c("pre", "post"), 2L))
  p$drop_na <- TRUE
  testthat::expect_equal(nrow(apply_approved_rules(d, research_rule("long", p))$data), 2L)
  p$prefix <- "wrong"
  testthat::expect_error(apply_approved_rules(d, research_rule("long", p)), "접두어")
})

testthat::test_that("reshape refuses ambiguous keys, name collisions and loss of varying columns", {
  d <- research_fixture(); d$id[[2]] <- d$id[[1]]
  testthat::expect_error(apply_approved_rules(d, research_rule("long", long_parameters())), "중복")
  d <- research_fixture(); p <- long_parameters(); p$time <- "cohort"
  testthat::expect_error(apply_approved_rules(d, research_rule("long", p)), "겹칩니다")
  p <- long_parameters(); d$q1_post <- letters[1:4]
  testthat::expect_error(apply_approved_rules(d, research_rule("long", p)))
  d <- apply_approved_rules(research_fixture(), research_rule("long", p))$data
  p <- list(ids = "id", time = "occasion", values = "q1", separator = "_")
  testthat::expect_error(apply_approved_rules(d, research_rule("wide", p)), "q2")
  p$values <- c("q1", "q2")
  testthat::expect_error(apply_approved_rules(rbind(d, d[1, ]), research_rule("wide", p)), "중복")
  d$occasion[[1]] <- NA_character_
  testthat::expect_error(apply_approved_rules(d, research_rule("wide", p)), "결측값")
})

testthat::test_that("uneven panels create missing cells and preserve constant metadata", {
  d <- data.frame(id = c("a", "a", "b"), wave = c("pre", "post", "pre"), score = c(1, 2, 3), group = c("A", "A", "B"))
  x <- apply_approved_rules(d, research_rule("wide", list(ids = "id", time = "wave", values = "score", separator = "_")))
  testthat::expect_equal(x$data$score_post, c(2, NA))
  testthat::expect_identical(x$data$group, c("A", "B"))
  testthat::expect_equal(jsonlite::fromJSON(x$audit$details[[1]])$absent_combinations, 1)
})

testthat::test_that("version checks and AI boundaries prevent unsupported research operations", {
  d <- data.frame(q1 = 1, q2 = 2); r <- research_rule("composite", composite_parameters())
  r$schema_version <- 99L
  testthat::expect_error(apply_approved_rules(d, r), "지원하지 않는")
  r$schema_version <- 2L; r$parameters <- "not JSON"
  testthat::expect_error(apply_approved_rules(d, r), "설정")
  r <- research_rule("composite", composite_parameters()); r$source <- "AI"
  testthat::expect_equal(nrow(append_rule_candidates(empty_rules(), r, d)), 0L)
})

testthat::test_that("a complete research plan replays from a standalone script", {
  d <- research_fixture()
  r <- rbind(research_rule("long", long_parameters()),
    research_rule("composite", composite_parameters(), 2L),
    research_rule("center", scale_parameters(output = "centered", scope = "group", groups = "cohort"), 3L),
    research_rule("standardize", scale_parameters(), 4L),
    research_rule("wide", list(ids = "id", time = "occasion", values = c("q1", "q2", "score", "centered", "score_z"), separator = "_"), 5L))
  preview <- preview_rules(d, r)
  testthat::expect_true(preview$ok, info = preview$error)
  applied <- apply_approved_rules(d, r)
  testthat::expect_equal(dim(applied$data), c(4L, 12L))
  testthat::expect_equal(preview$report$rows_after, c(8, 8, 8, 8, 4))
  testthat::expect_identical(preview$report$type_before, c("표", "새 변수", "새 변수", "새 변수", "표"))
  root <- normalizePath(testthat::test_path("..", ".."))
  dir <- tempfile(); dir.create(dir)
  raw <- file.path(dir, "raw.rds"); saveRDS(d, raw)
  script <- file.path(dir, "preprocessing.R")
  writeLines(make_standalone_script(r, root = root), script)
  output <- file.path(dir, "reproduced.rds")
  status <- system2(file.path(R.home("bin"), "Rscript"), shQuote(c("--vanilla", script, raw, output)), stdout = TRUE, stderr = TRUE)
  testthat::expect_null(attr(status, "status"), info = paste(status, collapse = "\n"))
  testthat::expect_identical(readRDS(output), applied$data)
  audit <- read.csv(file.path(dir, "reproduced_audit.csv"), stringsAsFactors = FALSE)
  testthat::expect_identical(audit$details, applied$audit$details)
})

testthat::test_that("reshape stops excessive expansion and inconsistent wave labels", {
  d <- data.frame(id = seq_len(3000), wave = seq_len(3000), score = 1)
  p <- list(ids = "id", time = "wave", values = "score", separator = "_")
  testthat::expect_error(apply_approved_rules(d, research_rule("wide", p)), "500만 셀")
  d <- research_fixture()
  attr(d$q1_pre, "label") <- "Before"
  attr(d$q1_post, "label") <- "After"
  testthat::expect_error(apply_approved_rules(d, research_rule("long", long_parameters())), "라벨")
})
