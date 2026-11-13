# Load the portfolio bank schema and adapt it to the v1.2 engine.
exam_bank_load <- function(path = file.path(.workbench_root, "demo-bank")) {
  paths <- file.path(path, c("questions.rds", "solutions.rds"))
  read_table <- readRDS
  if (!all(file.exists(paths))) {
    paths <- file.path(path, c("questions.csv", "solutions.csv"))
    read_table <- function(file) {
      read.csv(file, colClasses = "character", encoding = "UTF-8")
    }
  }
  if (!all(file.exists(paths))) {
    stop("Supply a folder with a complete questions/solutions CSV or RDS pair.",
         call. = FALSE)
  }
  bank <- read_table(paths[1])
  solutions <- read_table(paths[2])
  required <- c("id", "source", "chapter", "question_no", "full_text",
                "tags", "source_pdf", "source_pages", "scope",
                "recommended_command", "parameters", "text_quality")
  if (!is.data.frame(bank) || !all(required %in% names(bank)) ||
      !nrow(bank) || anyDuplicated(bank$id) || anyNA(bank[required]) ||
      any(!nzchar(bank$id))) {
    stop("Invalid question database schema", call. = FALSE)
  }
  if (!is.data.frame(solutions) ||
      !all(c("id", "text", "source_pdf", "source_pages", "provenance") %in% names(solutions)) ||
      anyDuplicated(solutions$id)) {
    stop("Invalid solution database schema", call. = FALSE)
  }
  solution_rows <- match(bank$id, solutions$id)
  answer <- solutions$text[solution_rows]
  answer[is.na(answer)] <- ""
  scope <- bank$scope
  scope[!scope %in% c("in_syllabus", "to_confirm", "outside_syllabus")] <- "in_syllabus"
  optional <- function(name) {
    values <- if (name %in% names(bank)) as.character(bank[[name]]) else rep("", nrow(bank))
    values[is.na(values)] <- ""
    values
  }
  indexed <- data.frame(id = bank$id, source = bank$source, chapter = bank$chapter,
    qno = bank$question_no, text = bank$full_text, text_norm = optional("text_norm"),
    tags = bank$tags, pdf_file = bank$source_pdf, pdf_page_start = bank$source_pages,
    pdf_page_end = bank$source_pages, question_pdf = optional("question_pdf"),
    answer_pdf = optional("answer_pdf"), answer_file = optional("answer_file"),
    answer_page = optional("answer_page"), answer_text = answer, answer_source = "",
    scope = scope, text_quality = bank$text_quality, source_params = bank$parameters,
    aliases = optional("aliases"), stringsAsFactors = FALSE)
  .exam_v12$.exam_index(indexed)
  .exam_v12$BANK_ROOT <- normalizePath(path, mustWork = TRUE)
  .exam_bank$questions <- bank
  .exam_bank$solutions <- solutions
  invisible(nrow(bank))
}

exam_search <- function(question, top_k = 5L, threshold = 0.58,
                        parameters = NULL, context = NULL) {
  if (!is.character(question) || length(question) != 1L ||
      is.na(question) || !nzchar(trimws(question)))
    stop("question must be one nonempty text string", call. = FALSE)
  if (!is.numeric(top_k) || length(top_k) != 1L || !top_k %in% 3:5)
    stop("top_k must be 3, 4, or 5", call. = FALSE)
  if (!is.numeric(threshold) || length(threshold) != 1L ||
      is.na(threshold) || threshold < 0.5 || threshold > 0.95)
    stop("threshold must be between 0.50 and 0.95", call. = FALSE)
  if (!is.null(parameters) && (!is.list(parameters) || is.null(names(parameters))))
    stop("parameters must be a named list", call. = FALSE)
  if (!is.null(context) && (!is.list(context) || is.null(names(context))))
    stop("context must be a named list", call. = FALSE)
  invisible(capture.output(rows <- exam_find(question, top_n = top_k)))
  bank <- .exam_bank$questions
  i <- match(rows$id, bank$id)
  result <- bank[i, c("id", "source", "chapter", "question_no", "source_pdf",
                     "source_pages", "scope"), drop = FALSE]
  result$similarity <- round(as.numeric(rows$score), 4)
  result$confidence <- as.character(rows$confidence)
  result$reliable <- as.numeric(rows$score) >= threshold
  result$parameter_difference <- vapply(seq_along(i), function(j) {
    observed <- paste0("Source-only numbers: ",
      .exam_v12$.exam_num_text(rows$source_only_numbers[[j]]),
      ". Query-only numbers: ", .exam_v12$.exam_num_text(rows$query_only_numbers[[j]]),
      ". Source parameters: ", bank$parameters[i[j]])
    if (length(parameters))
      observed <- paste(observed, "| Supplied:",
        paste(names(parameters), vapply(parameters, toString, character(1)),
              sep = "=", collapse = ", "))
    observed
  }, character(1))
  result$recommended_command <- rep("", length(i))
  result$text_quality <- bank$text_quality[i]
  result$route_status <- rep("REFERENCE_ONLY", length(i))
  result$command_status <- rep("Inspect the question and choose the model", length(i))
  wanted_test <- context$test
  if (!is.null(wanted_test)) wanted_test <- match.arg(wanted_test, c("positive", "negative"))
  for (j in which(result$reliable & result$confidence == "original")) {
    cmd <- bank$recommended_command[i[j]]
    if (!nzchar(cmd)) next
    test_conflict <- !is.null(wanted_test) && grepl("exam_test", cmd, fixed = TRUE) &&
      !grepl(paste0('test = "', wanted_test, '"'), cmd, fixed = TRUE)
    continuous_conflict <- identical(context$variable_type, "continuous") &&
      grepl("JOINT_MATRIX", cmd, fixed = TRUE)
    if (test_conflict || continuous_conflict) {
      result$route_status[j] <- "BLOCKED_CONDITION_CONFLICT"
    } else {
      result$recommended_command[j] <- cmd
      result$route_status[j] <- "ORIGINAL_REFERENCE_CHECK_ASSUMPTIONS"
    }
  }
  structure(list(matches = result, threshold = threshold, reliable = any(result$reliable),
                 query = question, language = "en"), class = "exam_matches")
}

print.exam_matches <- function(x, ...) {
  if (!x$reliable) cat("No reliable match found.\n")
  cat("Similarity ranks references. Check the model and inputs.\n")
  for (i in seq_len(nrow(x$matches))) {
    r <- x$matches[i, ]
    cat(sprintf("\n%d. %s | %.2f%% | %s\n", i, r$id, 100 * r$similarity, r$confidence))
    cat("  ", r$source_pdf, " | location: ", r$source_pages, "\n", sep = "")
    cat("  ", r$parameter_difference, "\n", sep = "")
  }
  invisible(x)
}

exam_question <- function(id) {
  if (is.null(.exam_bank$questions)) exam_bank_load()
  bank <- .exam_bank$questions
  i <- match(id, bank$id)
  if (length(i) != 1 || is.na(i)) stop("Unknown question id")
  r <- bank[i, ]
  cat(r$id, " | ", r$source, " | ", r$chapter, "\n", sep = "")
  cat(r$source_pdf, " | location: ", r$source_pages, "\n")
  cat(r$full_text, "\n")
  cat("TEXT QUALITY: ", r$text_quality, "\n", sep = "")
  invisible(r)
}

exam_solution <- function(id) {
  if (is.null(.exam_bank$questions)) exam_bank_load()
  s <- .exam_bank$solutions
  i <- match(id, s$id)
  if (length(i) != 1 || is.na(i)) {
    cat("No source-linked solution available.\n")
    return(invisible(NULL))
  }
  r <- s[i, ]
  cat("SOURCE: ", r$provenance, "\n", r$source_pdf,
      " | location: ", r$source_pages, "\n", sep = "")
  cat(r$text, "\n")
  cat("Consult the source for full context.\n")
  invisible(r)
}

