exam_check <- function() {
  checks <- c(
    calculator = function() {
      result <- exam_calc("C(10,3)*0.4^3*0.6^7")
      isTRUE(all.equal(result, 0.214990848))
    },
    nested_integral = function() {
      result <- exam_calc("integral(integral(2,y,0,x),x,0,1)")
      isTRUE(all.equal(result, 1))
    },
    unsafe_call_rejected = function() is.null(exam_calc("system('echo unsafe')")),
    original = function() {
      found <- exam_find(.exam_bank$text[[1]], top_n = 3)
      nrow(found) && identical(found$id[[1]], .exam_bank$id[[1]]) &&
        identical(found$confidence[[1]], "original")
    }
  )
  # Exercise normal code in an isolated environment without changing search
  # state or writing calculator history.
  check_env <- new.env(parent = environment(exam_check))
  check_env$.exam_calc_log <- function(...) invisible(NULL)
  for (name in c("exam_calc", ".exam_calc_line", ".exam_calc_one",
                 "exam_find", ".exam_clear")) {
    fn <- get(name, envir = check_env)
    environment(fn) <- check_env
    check_env[[name]] <- fn
  }
  for (name in c("EXAM_LAST_QUERY", "EXAM_LAST_TOPICS", "EXAM_LAST_RESULTS",
                 "EXAM_LAST_OUTSIDE_COUNT", "EXAM_LAST_ALL"))
    check_env[[name]] <- get(name, envir = check_env)
  pass <- vapply(checks, function(check) {
    environment(check) <- check_env
    tryCatch({
      invisible(capture.output(result <- check()))
      isTRUE(result)
    }, error = function(e) FALSE)
  }, logical(1))
  for (name in names(pass)) .exam_print(if (pass[[name]]) "PASS " else "FAIL ", name)
  invisible(data.frame(check = names(pass), pass = unname(pass)))
}
