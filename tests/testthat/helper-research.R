research_fixture <- function() data.frame(id = sprintf("%03d", 1:4), cohort = c("A", "A", "B", "B"),
  q1_pre = c(1, 2, 3, 4), q2_pre = c(2, 3, 4, 5), q1_post = c(2, 3, 4, 5), q2_post = c(3, NA, 5, NA))
research_rule <- function(op, p, id = 1L) new_research_rule(op, p, "Synthetic research decision", id)
composite_parameters <- function(inputs = c("q1", "q2"), output = "score", method = "mean", min_valid = 2L) {
  list(inputs = inputs, output = output, method = method, min_valid = min_valid)
}
scale_parameters <- function(input = "score", output = "score_z", scope = "all", groups = character(),
  reference = "all", reference_column = "", reference_values = character(), center = 0, scale = 1) {
  list(input = input, output = output, scope = scope, groups = groups, reference = reference,
    reference_column = reference_column, reference_values = reference_values, center = center, scale = scale)
}
long_parameters <- function() list(ids = "id", columns = c("q1_pre", "q2_pre", "q1_post", "q2_post"),
  layout = "multiple", time = "occasion", value = "score", prefix = "", separator = "_", drop_na = FALSE)
