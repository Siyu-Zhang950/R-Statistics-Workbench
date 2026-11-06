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
stopifnot(changed$id[1] == "DEMO-01", changed$similarity[1] == 0.97,
          changed$confidence[1] == "changed numbers",
          changed$recommended_command[1] == "",
          changed$source_pdf[1] == "demo-bank/questions.csv",
          changed$source_pages[1] == "DEMO-01")

unrelated <- exam_search("asparagus and pineapples", top_k = 3)
stopifnot(!unrelated$reliable, all(unrelated$matches$recommended_command == ""))

invisible(capture.output(solution <- exam_solution("DEMO-01")))
stopifnot(solution$id == original$id, grepl("0.110870", solution$text, fixed = TRUE))
cat("5 question bank checks passed.\n")

