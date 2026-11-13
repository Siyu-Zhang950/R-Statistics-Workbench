source("workbench.R")

close_to <- function(actual, expected) {
    stopifnot(isTRUE(all.equal(unname(actual), expected, tolerance = 1e-10)))
}

late_orders <- exam_prob("binomial", "ge", q = 3, n = 12, p = 0.10)
close_to(late_orders$value, 0.110869977745)
stopifnot(length(capture.output(print(late_orders))) > 0)

between <- exam_prob("binomial", "between", lower = 1, upper = 3,
                     lower_closed = FALSE, upper_closed = FALSE, n = 5, p = 0.25)
close_to(between$value, 0.263671875)

times <- exam_describe(c(8, 10, 10, 12, 15))
close_to(times$value["mean"], 11)
close_to(times$value["variance_n"], 5.6)
close_to(times$value["variance_n_minus_1"], 7)

posterior <- exam_bayes(c(0.75, 0.25), c(0.1, 0.6))
close_to(posterior$value, c(1 / 3, 2 / 3))

tail <- exam_prob("normal", "gt", q = 40, sd = 1)
stopifnot(identical(tail$numeric_status, "underflow"))
close_to(tail$log_probability, -804.608442013754)

stopifnot(inherits(try(exam_prob("binomial", "ge", q = 3, n = 12, p = 1.1),
                       silent = TRUE), "try-error"))
cat("6 smoke checks passed.\n")

stopifnot(exam_bank_load() == 5)
invisible(capture.output(original <- exam_question("DEMO-01")))
exact <- exam_search(original$full_text, top_k = 3)$matches
stopifnot(exact$id[1] == "DEMO-01", exact$similarity[1] == 1)

changed <- exam_search(sub("12", "20", original$full_text), top_k = 3)$matches
stopifnot(changed$id[1] == "DEMO-01", changed$similarity[1] == 1,
          changed$confidence[1] == "changed numbers",
          changed$recommended_command[1] == "",
          changed$source_pdf[1] == "demo-bank/questions.csv",
          changed$source_pages[1] == "DEMO-01")

unrelated <- exam_search("asparagus and pineapples", top_k = 3)
stopifnot(!unrelated$reliable, all(unrelated$matches$recommended_command == ""))

invisible(capture.output(solution <- exam_solution("DEMO-01")))
stopifnot(solution$id == original$id, grepl("0.110870", solution$text, fixed = TRUE))
cat("5 question bank checks passed.\n")


# Exercise the public calculator and menu without writing history to the checkout.
read_from <- function(lines) {
  i <- 0L
  function(prompt) {
    i <<- i + 1L
    lines[i]
  }
}
log_root <- .exam_v12$WORKBENCH_ROOT
history_root <- tempfile("workbench-check-")
dir.create(history_root)
dir.create(file.path(history_root, "guide"))
invisible(file.copy("guide/calculator-rules.md", file.path(history_root, "guide")))
invisible(file.copy("guide/workbench.md", file.path(history_root, "guide")))
.exam_v12$WORKBENCH_ROOT <- history_root
before <- .exam_v12$EXAM_LAST_QUERY
invisible(capture.output(checks <- exam_check()))
stopifnot(all(checks$pass), identical(before, .exam_v12$EXAM_LAST_QUERY),
          !dir.exists(file.path(history_root, "history")))
interaction <- capture.output(exam_calc(reader = read_from(c(
  "help", "k=2", "sum(C(5,k)*0.3^k*0.7^(5-k),k,0,2)", "1,000+2",
  "integral(integral(2,y,0,x),x,0,1)", "history", "q"))))
stopifnot(any(grepl("Summation: Σ_{k=0}^{2}", interaction, fixed = TRUE)),
          any(grepl("0.83692", interaction, fixed = TRUE)),
          any(grepl("thousands separator", interaction, fixed = TRUE)),
          any(grepl("Rounded to 4 places: 1.0000", interaction, fixed = TRUE)),
          tail(interaction, 1) == "Calculator closed.",
          length(readLines(.exam_v12$.exam_calc_log_path())) == 4L)
menu <- capture.output(exam_menu(read_from(c("7", "y", "0"))))
stopifnot(which(menu == "# Calculator rules") < which(menu == "# Workbench guide"))
unlink(history_root, recursive = TRUE)
.exam_v12$WORKBENCH_ROOT <- log_root

fragment <- sub("\\. Under.*", ".", original$full_text)
paste_output <- capture.output(result <- exam_go(read_from(c(fragment, ":end"))))
stopifnot(result$id[1] == "DEMO-01", result$is_fragment[1],
          !result$confidence[1] %in% c("original", "changed numbers"),
          any(grepl("Topic", paste_output, fixed = TRUE)))
invisible(capture.output(swapped <- exam_find(
  sub("12(.*)at least 3", "3\\1at least 12", original$full_text), top_n = 3)))
stopifnot(swapped$id[1] == "DEMO-01", !swapped$same_number_sequence[1],
          swapped$confidence[1] == "same type")
invisible(capture.output(question <- exam_open("DEMO-01")))
stopifnot(identical(question, original$full_text))
cat("Public v1.2 calculator, menu, and search checks passed.\n")
