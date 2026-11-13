# Console interface for the search engine.
exam_home <- function() {
  .exam_print("R Statistics Workbench v1.2")
  .exam_print(strrep("=", 60))
  .exam_print("Loaded ", nrow(.exam_bank), " questions. Solutions available: ",
    sum(nzchar(.exam_bank$answer_pdf) | nzchar(.exam_bank$answer_text)), ".")
  counts <- table(.exam_bank$source)
  .exam_print(paste(paste(names(counts), as.integer(counts), sep = ": "),
                    collapse = "  |  "))
  .exam_print('Search: exam_find("paste question here"). Interactive: exam_go().')
  .exam_print("Self-check: exam_check()")
  .exam_print("Paste a question into exam_go() and enter :end on its own line.")
  .exam_print("View a question with exam_open(1) and its solution with exam_answer(1).")
  .exam_print("Calculator: exam_calc(). Materials: exam_pdf(). Menu: exam_menu().")
  .exam_print("Topic hints: exam_topic(). Guide: exam_guide(). Details: exam_detail(1).")
  .exam_print("Similarity ranks references. Check the conditions, numbers, and units.")
  invisible(NULL)
}

# Preview long number lists. Full details are available in exam_detail().
.exam_num_preview <- function(values) {
  if (length(values) <= 8L) return(.exam_num_text(values))
  paste0(paste(head(values, 8L), collapse = ", "), " ... total: ", length(values),
         " numbers. See details for the full list.")
}
# Show search results. Solutions have a separate command.
exam_results <- function() {
  rows <- EXAM_LAST_RESULTS
  if (!nrow(rows)) { .exam_print("No search results yet."); return(invisible(rows)) }
  .exam_print(strrep("-", 60))
  for (i in seq_len(nrow(rows))) {
    x <- rows[i, ]
    .exam_print(i, ". ", x$id, " | ", sprintf("%.2f%%", 100 * x$score),
                " | ", x$confidence)
    .exam_print("   ", x$source, " / Chapter ", x$chapter, " / Question ", x$qno,
                " / Location ", x$pdf_page_start, "–", x$pdf_page_end)
    .exam_print("   Scoring: ", x$method, "; scope: ", x$scope,
                "; text quality: ", x$text_quality)
    .exam_print("   Numbers only in source: ", .exam_num_preview(x$source_only_numbers[[1]]))
    .exam_print("   Numbers only in query: ", .exam_num_preview(x$query_only_numbers[[1]]))
    .exam_identity_note(x, show_diff = i <= 3L)
    if (x$confidence %in% c("low confidence", "below threshold"))
      .exam_print("   Use as a reference for further lookup.")
  }
  .exam_print("Use exam_detail(result_number) to review source parameters.")
  .exam_print("Open the question with exam_open(1). Use exam_answer(1) to see the solution.")
  .exam_scope_note()
  invisible(rows)
}
exam_search <- function(q = "", top_n = 5, all = FALSE) {
  # Reuse cached search results for the compact display.
  output <- capture.output(rows <- exam_find(q, top_n = top_n, all = all))
  if (!nrow(rows)) {
    for (line in output) .exam_print(line)
  } else {
    .exam_query_header(q)
    if (any(grepl("Query too long", output, fixed = TRUE)))
      .exam_print("Query too long. Compared the first ", MAX_QUERY_CHARS, " characters.")
    exam_results()
  }
  invisible(rows)
}
exam_detail <- function(id) {
  x <- .exam_bank[.exam_resolve(id), ]
  .exam_print("id: ", x$id)
  .exam_print("Source: ", x$pdf_file, " | location: ", x$pdf_page_start)
  recent <- match(x$id, EXAM_LAST_RESULTS$id)
  if (!is.na(recent)) {
    .exam_print("source-only numbers: ",
      .exam_num_text(EXAM_LAST_RESULTS$source_only_numbers[[recent]]))
    .exam_print("query-only numbers: ",
      .exam_num_text(EXAM_LAST_RESULTS$query_only_numbers[[recent]]))
    .exam_print("labels not inferred")
    .exam_identity_note(EXAM_LAST_RESULTS[recent, ], show_diff = TRUE)
  }
  if (nzchar(x$aliases)) .exam_print("Aliases: ", x$aliases)
  .exam_print("SCOPE: ", x$scope, "; TEXT QUALITY: ", x$text_quality)
  .exam_print("source parameters: ", .exam_public_params(x$source_params))
  .exam_print("Question text. Consult the source for formulas and tables.")
  .exam_print(x$text)
  invisible(NULL)
}

# End the query with :end, or use :cancel to clear the results.
exam_paste <- function(reader = readline, top_n = 5) {
  .exam_clear()
  if (!interactive() && identical(reader, readline)) {
    .exam_print("Use multiline paste in a local R console.")
    return(invisible(NULL))
  }
  .exam_print("Paste the question. Enter :end or :cancel on its own line.")
  lines <- character()
  repeat {
    line <- reader("")
    if (!length(line) || is.na(line)) {
      .exam_print("Input ended before the search."); return(invisible(NULL))
    }
    if (trimws(line) == ":cancel") {
      .exam_print("Search cancelled and previous results cleared."); return(invisible(NULL))
    }
    if (trimws(line) == ":end") break
    lines <- c(lines, line)
  }
  exam_search(paste(lines, collapse = "\n"), top_n = top_n)
}

# Same paste protocol, explicitly three candidates; never launch PDF or answers.
exam_go <- function(reader = readline) {
  rows <- exam_paste(reader = reader, top_n = 3)
  if (!is.data.frame(rows) || !nzchar(trimws(EXAM_LAST_QUERY)))
    return(invisible(NULL))
  if (!nrow(rows)) {
    .exam_go_topics()
    .exam_print("Use exam_topic() for hints or run exam_go() with the complete question.")
  } else {
    .exam_go_topics(has_candidates = TRUE)
    .exam_print("Use exam_detail(1) to check conditions or exam_open(1) to view the source.")
  }
  invisible(rows)
}
exam_guide <- function() {
  path <- file.path(WORKBENCH_ROOT, "guide/workbench.md")
  if (!file.exists(path)) .exam_error("Could not find the user guide.")
  for (line in readLines(path, encoding = "UTF-8", warn = FALSE))
    .exam_print(line)
  invisible(NULL)
}

exam_pdf <- function(n) {
  paths <- list.files(file.path(WORKBENCH_ROOT, "private-bank/materials"), pattern = "[.]pdf$",
    ignore.case = TRUE)
  if (missing(n)) {
    if (!length(paths)) .exam_print("Add your own PDFs to private-bank/materials.")
    for (i in seq_along(paths)) .exam_print(i, "  ", paths[i])
    return(invisible(paths))
  }
  if (length(n) != 1L || !is.numeric(n) || is.complex(n) ||
      !n %in% seq_along(paths)) {
    .exam_print("Choose a material number from the list.")
    return(invisible(NULL))
  }
  path <- file.path("private-bank/materials", paths[n])
  .exam_open_file(path, path, root = WORKBENCH_ROOT)
}

exam_materials <- function(reader = readline) {
  if (!interactive() && identical(reader, readline)) {
    .exam_print("Run exam_materials() in a local R console.")
    return(invisible(NULL))
  }
  repeat {
    paths <- exam_pdf()
    if (!length(paths)) break
    .exam_print("Enter 0 or b to return.")
    choice <- trimws(reader("Material number > "))
    if (!length(choice) || is.na(choice) || choice %in% c("0", "b")) break
    tryCatch(exam_pdf(match(choice, as.character(seq_along(paths)))),
      error = function(e) .exam_print(trimws(conditionMessage(e))))
  }
  invisible(NULL)
}

.exam_menu_question <- function(reader) {
  .exam_print("1 Question  2 Solution  3 Details  back Return")
  action <- trimws(reader("View option > "))
  if (!length(action) || is.na(action) || action == "back")
    return(invisible(NULL))
  if (!action %in% c("1", "2", "3")) {
    .exam_print("Select 3 in the main menu, then enter 1, 2, or 3.")
    return(invisible(NULL))
  }
  id <- trimws(reader("Result number or exact ID (back to return) > "))
  if (!length(id) || is.na(id) || id == "back") return(invisible(NULL))
  if (grepl("^[0-9]+$", id)) id <- as.numeric(id)
  switch(action, `1` = exam_open(id), `2` = exam_answer(id),
         `3` = exam_detail(id))
}

exam_menu <- function(reader = readline) {
  # Require an explicit reader outside an interactive session.
  if (!interactive() && identical(reader, readline)) {
    .exam_print("Run exam_menu() in a local R console.")
    return(invisible(NULL))
  }
  repeat {
    .exam_print("1 Paste a question         2 Show last results")
    .exam_print("3 Question/solution/details 4 Calculator")
    .exam_print("5 Local materials          6 Topic hints")
    .exam_print("7 Calculator rules/guide   8 Self-check")
    .exam_print("0 Exit")
    tryCatch({
      choice <- trimws(reader("Option > "))
      if (!length(choice) || is.na(choice) || choice == "0") {
        .exam_print("Menu closed.")
        return(invisible(NULL))
      }
      if (choice == "1") exam_go(reader)
      else if (choice == "2") exam_results()
      else if (choice == "3") .exam_menu_question(reader)
      else if (choice == "4") exam_calc(reader = reader)
      else if (choice == "5") exam_materials(reader)
      else if (choice == "6") exam_topic()
      else if (choice == "7") {
        exam_rules()
        answer <- trimws(reader("Read the user guide? Enter y, or press Enter to return > "))
        if (length(answer) && !is.na(answer) && tolower(answer) == "y")
          exam_guide()
      } else if (choice == "8") exam_check()
      else .exam_print("Enter a number from 0 to 8.")
    }, error = function(e) {
      .exam_print(trimws(conditionMessage(e)))
      .exam_print("Enter 0 to exit or choose another menu option.")
    })
  }
}

# Optional offline checks are loaded by the public workbench entry point.
sys.source(file.path(WORKBENCH_ROOT, "R/v12/calc.R"), envir = environment())
