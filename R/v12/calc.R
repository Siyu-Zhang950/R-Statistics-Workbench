# Calculator evaluation, formatting, and local history. See calc_syntax.R for allowed syntax.
sys.source(file.path(BANK_ROOT, "R/v12/calc_syntax.R"), envir = environment())

.exam_calc_new_state <- function() {
  state <- new.env(parent = emptyenv())
  state$values <- new.env(parent = emptyenv())
  state$record <- TRUE
  state$record_warned <- FALSE
  state
}
.exam_calc_state <- .exam_calc_new_state()

.exam_calc_run <- function(compiled, state) {
  # Generated code sees only the bindings below. Session variables are stored separately.
  bindings <- new.env(parent = emptyenv())
  base_functions <- c("+", "-", "*", "/", "^", "(", "==", "!=", "<", "<=",
    ">", ">=", "sqrt", "exp", "log", "log10", "factorial", "abs", "choose",
    "sum", "function", "$", "sapply", "Vectorize", "quote", "eval", "list",
    "c", "sin", "cos", "tan", "sinh", "cosh", "sign", "digamma", "if", "prod")
  for (name in base_functions) bindings[[name]] <- get(name, envir = baseenv())
  bindings$pi <- base::pi
  for (name in names(.exam_calc_distributions))
    bindings[[name]] <- getExportedValue("stats", name)
  bindings$D <- function(expr, variable) {
    tryCatch(stats::D(expr, variable), error = function(e)
      .exam_calc_fail("Cannot differentiate this expression symbolically. Try a polynomial, exp, or one-argument log."))
  }
  remaining_terms <- 100000L
  bindings[[":"]] <- function(from, to) {
    valid <- function(x) is.numeric(x) && length(x) == 1L &&
      is.finite(x) && x == trunc(x)
    if (!valid(from) || !valid(to) || from > to)
      .exam_calc_fail("Summation bounds must be ordered integers. P(n,k) requires 0 <= k <= n.")
    terms <- to - from + 1
    if (terms > remaining_terms)
      .exam_calc_fail("A single expression can sum at most 100,000 terms. Reduce the range or split it.")
    remaining_terms <<- remaining_terms - terms
    from:to
  }
  error_estimate <- 0
  remaining_integrals <- 10000L
  bindings$integrate <- function(f, lower, upper, rel.tol) {
    if (remaining_integrals <= 0L)
      .exam_calc_fail("Expression exceeds 10,000 integration calls. Split the calculation.")
    remaining_integrals <<- remaining_integrals - 1L
    message <- "Integral may diverge or be undefined on this interval. Check the bounds."
    bound <- function(x) is.numeric(x) && length(x) == 1L && !is.na(x)
    if (!bound(lower) || !bound(upper)) .exam_calc_fail(message)
    fit <- tryCatch(stats::integrate(f, lower, upper, rel.tol = rel.tol),
      exam_calc_error = function(e) stop(e),
      warning = function(w) .exam_calc_fail(message),
      error = function(e) .exam_calc_fail(message))
    error_estimate <<- max(error_estimate, fit$abs.error)
    fit
  }
  for (name in .exam_calc_legacy) {
    calculator <- get(name, mode = "function", inherits = TRUE)
    bindings[[name]] <- local({
      fn <- calculator
      function(...) {
        # Run the helper and let exam_calc format the result.
        tryCatch({
          invisible(capture.output(value <- fn(...)))
          value
        }, exam_calc_error = function(e) stop(e), error = function(e) {
          message <- conditionMessage(e)
          if (startsWith(message, "\n")) .exam_calc_fail(trimws(message))
          .exam_calc_fail("Invalid arguments or result from the named calculator. See help.")
        })
      }
    })
  }
  lockEnvironment(bindings, bindings = TRUE)
  values <- list2env(as.list(state$values, all.names = TRUE), parent = bindings)
  value <- withCallingHandlers(eval(compiled$code, values), warning = function(w)
    .exam_calc_fail("Calculation exceeded the function domain or numeric range. Check the inputs."))
  if (is.numeric(value) || is.logical(value)) {
    if (!length(value) || any(!is.finite(value)))
      .exam_calc_fail("Result is not finite. Check denominators, domains, and parameters.")
    if (!is.null(compiled$target)) state$values[[compiled$target]] <- value
    state$values$ans <- value
  } else if (!is.null(compiled$target)) {
    .exam_calc_fail("Variables can hold only numbers or comparison results. Supply a numeric value.")
  }
  list(value = value, error_estimate = error_estimate)
}

.exam_calc_show_value <- function(value, digits = NULL) {
  if (is.language(value)) return(.exam_calc_code(value))
  rendered <- if (is.logical(value)) as.character(value) else if (is.null(digits))
    sprintf("%.10g", value) else formatC(value, format = "f", digits = digits)
  if (!is.null(names(value))) rendered <- paste0(names(value), "=", rendered)
  paste(rendered, collapse = "; ")
}

.exam_calc_fraction <- function(value) {
  if (!is.numeric(value) || length(value) != 1L || value == trunc(value))
    return(NULL)
  denominator <- seq_len(10000L)
  numerator <- round(value * denominator)
  hits <- which(abs(numerator / denominator - value) <= 1e-9 &
    abs(numerator) <= 2^53 & numerator != 0)
  if (!length(hits)) return(NULL)
  first <- hits[1L]
  sprintf("%.0f/%d", numerator[first], denominator[first])
}

.exam_calc_log_path <- function(now = Sys.time()) {
  file.path(WORKBENCH_ROOT, "history", paste0("calculations_", format(now, "%Y-%m-%d"), ".txt"))
}
.exam_calc_log <- function(input, code, result, rounded, state) {
  if (!state$record) return(invisible(NULL))
  saved <- tryCatch({
    now <- Sys.time()
    path <- .exam_calc_log_path(now)
    if (!dir.exists(dirname(path)))
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    fields <- c(format(now, "%H:%M:%S"), input, code, result, rounded)
    fields <- gsub("[\r\n]", " ", fields)
    fields <- gsub("|", "\\|", fields, fixed = TRUE)
    line <- paste0(paste(fields, collapse = " | "), "\n")
    connection <- file(path, "ab")
    tryCatch(writeBin(charToRaw(enc2utf8(line)), connection),
             finally = close(connection))
    TRUE
  }, warning = function(w) FALSE, error = function(e) FALSE)
  if (!saved && !state$record_warned) {
    state$record_warned <- TRUE
    .exam_print("Calculation history could not be saved.")
  }
  invisible(saved)
}

exam_history <- function(n = 20) {
  if (!is.numeric(n) || is.complex(n) || length(n) != 1L || !is.finite(n) ||
      n < 1 || n != trunc(n)) {
    .exam_print("Enter a positive integer, for example exam_history(20).")
    return(invisible(character()))
  }
  path <- .exam_calc_log_path()
  lines <- tryCatch({
    if (file.exists(path)) readLines(path, encoding = "UTF-8", warn = FALSE)
    else character()
  }, warning = function(w) NULL, error = function(e) NULL)
  if (is.null(lines)) {
    .exam_print("Could not read calculation history. Check folder permissions and try exam_history() again.")
    return(invisible(character()))
  }
  if (!length(lines)) .exam_print("No calculations recorded today.")
  lines <- tail(lines, min(n, length(lines)))
  for (line in lines) .exam_print(line)
  invisible(lines)
}

exam_rules <- function() {
  path <- file.path(WORKBENCH_ROOT, "guide/calculator-rules.md")
  lines <- tryCatch(readLines(path, encoding = "UTF-8", warn = FALSE),
    warning = function(w) NULL, error = function(e) NULL)
  if (is.null(lines)) {
    .exam_print("Could not read guide/calculator-rules.md. Restore it and try exam_rules() again.")
    return(invisible(NULL))
  }
  for (line in lines) .exam_print(line)
  invisible(NULL)
}

.exam_calc_help <- function() {
  path <- file.path(WORKBENCH_ROOT, "guide/calculator-rules.md")
  lines <- tryCatch(readLines(path, encoding = "UTF-8", warn = FALSE),
    warning = function(w) character(), error = function(e) character())
  if (!length(lines)) return(exam_rules())
  .exam_print("# Calculator rules")
  rows <- grep("^\\| ", lines, value = TRUE)
  rows <- rows[!startsWith(rows, "| Input ")]
  extra_examples <- c(
    "," = "C(10,3) → 120", "pi" = "π → 3.141592654",
    "log(x)" = "log(e) → 1", "abs(x)" = "abs(-4) → 4",
    "ans" = "After 6, enter ans/2 → 3",
    "Nested integral" = "integral(integral(2, y, 0, x), x, 0, 1) → 1",
    "exam_binom(...)" = 'exam_binom(12, 0.1, 2, type = "le") → 0.8891'
  )
  for (row in rows) {
    cells <- trimws(strsplit(row, "|", fixed = TRUE)[[1L]][2:5])
    syntax <- gsub("`", "", cells[1L], fixed = TRUE)
    prefix <- paste(syntax,
                    gsub("**", "", cells[2L], fixed = TRUE), sep = "  ")
    if (syntax == "log(x)") prefix <- "log(x)  Natural logarithm"
    keys <- names(extra_examples)
    override <- which(startsWith(syntax, keys))
    example <- if (length(override)) extra_examples[[override[1L]]] else cells[4L]
    # Remove Markdown markup and keep multiplication operators.
    example <- gsub("`", "", example, fixed = TRUE)
    joined <- paste(prefix, example, sep = "  ")
    if (nchar(joined, type = "width") <= CONSOLE_WIDTH) .exam_print(joined)
    else {
      .exam_print(prefix)
      if (nchar(example, type = "width") <= CONSOLE_WIDTH) .exam_print(example)
      else local({
          CONSOLE_WIDTH <- nchar(example, type = "width")
          printer <- .exam_print
          environment(printer) <- environment()
          printer(example)
        })
    }
  }
  .exam_print("history  Show history   :digits 3  Set decimal places   q  Quit")
  invisible(NULL)
}

.exam_calc_split <- function(input) {
  chars <- strsplit(input, "", fixed = TRUE)[[1L]]
  quoted <- ""
  start <- 1L
  pieces <- character()
  for (i in seq_along(chars)) {
    char <- chars[i]
    if (nzchar(quoted)) {
      if (char == quoted) quoted <- ""
    } else if (char %in% c("'", '"')) {
      quoted <- char
    } else if (char %in% c(";", "；")) {
      pieces <- c(pieces, trimws(substr(input, start, i - 1L)))
      start <- i + 1L
    }
  }
  c(pieces, trimws(substring(input, start)))
}

.exam_calc_one <- function(input, digits, state) {
  display <- ""
  tryCatch({
    compiled <- .exam_calc_compile(.exam_calc_parse(input), ls(state$values))
    display <- .exam_calc_code(compiled$display)
    .exam_print("Input:   ", input)
    code_lines <- deparse(compiled$display, width.cutoff = 45L)
    .exam_print("R code: ", code_lines[1L])
    for (line in code_lines[-1L]) .exam_print("        ", trimws(line))
    for (note in compiled$notes) .exam_print(note)
    result <- .exam_calc_run(compiled, state)
    value <- result$value
    formatted <- .exam_calc_show_value(value)
    rounded <- if (is.numeric(value)) .exam_calc_show_value(value, digits) else ""
    .exam_print("Result:   ", formatted)
    if (nzchar(rounded)) .exam_print("Rounded to ", digits, " places: ", rounded)
    fraction <- .exam_calc_fraction(value)
    if (!is.null(fraction)) .exam_print("Fraction:   ≈ ", fraction)
    if (compiled$integral && !is.language(value))
      .exam_print("Estimated numerical error: < ",
        sprintf("%.6g", max(.Machine$double.eps, result$error_estimate * 1.01)))
    log_rounded <- if (is.numeric(value)) .exam_calc_show_value(value, 4L) else ""
    .exam_calc_log(input, display, formatted, log_rounded, state)
    list(ok = TRUE, code = display, value = value, result = formatted)
  }, error = function(e) {
    reason <- if (inherits(e, "exam_calc_error")) conditionMessage(e) else
      "Calculation failed. Check the expression and arguments or enter help."
    .exam_print("Could not calculate: ", input)
    .exam_print("Reason: ", reason)
    .exam_print("Example: C(10, 3) or integral(2*x, x, 0, 1)")
    .exam_calc_log(input, display, paste0("ERROR: ", reason), "", state)
    list(ok = FALSE, code = display, value = NULL, result = paste0("ERROR: ", reason))
  })
}

.exam_calc_valid_digits <- function(digits) {
  is.numeric(digits) && !is.complex(digits) && length(digits) == 1L &&
    is.finite(digits) && digits == trunc(digits) && digits >= 0 && digits <= 15
}

.exam_calc_line <- function(line, digits, state) {
  pieces <- if (nchar(line) > 4000L) line else .exam_calc_split(line)
  lapply(pieces, .exam_calc_one, digits = digits, state = state)
}

exam_calc <- function(input, digits = 4L, reader = readline) {
  if (!.exam_calc_valid_digits(digits)) {
    .exam_print("Decimal places must be an integer from 0 to 15, for example exam_calc(\"3/7\", digits = 3).")
    return(invisible(NULL))
  }
  calculate <- function(line) {
    results <- .exam_calc_line(line, digits, .exam_calc_state)
    invisible(tail(results, 1L)[[1L]]$value)
  }
  if (!missing(input)) {
    if (!is.character(input) || length(input) != 1L || is.na(input)) {
      .exam_print('Supply a quoted expression, for example exam_calc("3/7").')
      return(invisible(NULL))
    }
    return(invisible(calculate(input)))
  }
  if (!interactive() && identical(reader, readline)) {
    .exam_print("Run exam_calc() in the R console, or call exam_calc(\"3/7\").")
    return(invisible(NULL))
  }
  .exam_print("Enter an expression. Use help for rules, history for recent calculations, or q to quit.")
  repeat {
    line <- trimws(reader("Calculate > "))
    if (!length(line) || is.na(line) || line %in% c("q", ":q", "0")) break
    if (!nzchar(line)) next
    if (line == "help") {
      .exam_calc_help()
    } else if (line == "history") {
      exam_history()
    } else if (grepl("^:digits(?:[[:space:]]|$)", line, perl = TRUE)) {
      value <- suppressWarnings(as.numeric(trimws(sub("^:digits", "", line))))
      if (.exam_calc_valid_digits(value)) {
        digits <- value
        .exam_print("Decimal places set to ", digits, ".")
      } else .exam_print("Enter :digits 3. Choose from 0 to 15 decimal places.")
    } else calculate(line)
  }
  .exam_print("Calculator closed.")
  invisible(NULL)
}
