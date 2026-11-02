# Use synthetic local files and a fake AI boundary; these tests never call an API.
server_fixture <- function() {
  env <- new.env(parent = asNamespace("codebookr"))
  for (name in c("codebookr_server", "propose_ai_rules")) {
    if (exists(name, envir = asNamespace("codebookr"), inherits = FALSE)) {
      fn <- get(name, envir = asNamespace("codebookr"))
      environment(fn) <- env
      env[[name]] <- fn
    }
  }
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  env$propose_ai_rules <- function(data, codebooks, provider, api_key, model) {
    calls$n <- calls$n + 1L
    data.frame(id = 1L, variable = "gender", operation = "factor", values = "",
      minimum = NA_real_, maximum = NA_real_, reason = "Synthetic proposal",
      evidence = "Synthetic codebook", confidence = "low", source = "AI")
  }
  upload <- function(object, name, csv = FALSE) {
    path <- tempfile(fileext = if (csv) ".csv" else ".rds")
    if (csv) write.csv(object, path, row.names = FALSE) else saveRDS(object, path)
    data.frame(name = name, datapath = path, stringsAsFactors = FALSE)
  }
  a <- data.frame(gender = c(1L, 9L), income = c(100L, 9999L))
  book <- function(variable, value) data.frame(Variable = variable, Value = value, Category = "missing")
  list(server = env$codebookr_server, calls = calls,
    data_a = upload(a, "a.rds"), data_same = upload(a, "same.rds"),
    data_b = upload(rbind(a, a[2, ]), "b.rds"),
    book_a = upload(book("gender", "9"), "a.csv", csv = TRUE),
    book_same = upload(book("gender", "9"), "a.csv", csv = TRUE),
    book_b = upload(book("income", "9999"), "b.csv", csv = TRUE))
}

capture_inputs <- function(session) {
  messages <- new.env(parent = emptyenv())
  session$sendInputMessage <- function(inputId, message) messages[[inputId]] <- message
  list(
    last = function(id) messages[[id]],
    choices = function(id) {
      html <- as.character(messages[[id]]$options)
      if (!length(html)) return(character())
      values <- regmatches(html, gregexpr('value="[^"]*"', html, perl = TRUE))[[1]]
      sub('^value="([^"]*)"$', '\\1', values)
    }
  )
}

load_fixture <- function(session, fixture) {
  session$setInputs(provider = "openai", model = "", api_key = "synthetic-test-key",
    data_file = fixture$data_a, book_files = fixture$book_a)
}
