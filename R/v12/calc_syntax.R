# Compile calculator input into a validated R syntax tree without evaluating the raw input.
.exam_calc_fail <- function(message) {
  stop(errorCondition(message, class = "exam_calc_error", call = NULL))
}

.exam_calc_legacy <- c(
  "exam_binom", "exam_pois", "exam_norm", "exam_exp", "exam_exp_cond",
  "exam_bayes", "exam_comb", "exam_mean_var"
)
.exam_calc_arity <- list(
  `+` = 1:2, `-` = 1:2, `*` = 2L, `/` = 2L, `^` = 2L, `(` = 1L,
  `==` = 2L, `!=` = 2L, `<` = 2L, `<=` = 2L, `>` = 2L, `>=` = 2L,
  sqrt = 1L, exp = 1L, ln = 1L, log = 1:2, log10 = 1L, factorial = 1L,
  C = 2L, P = 2L, abs = 1L, Phi = 1L, invPhi = 1L, integral = 4L, d = 2:3
)
.exam_calc_distributions <- list(
  pnorm = c("q", "mean", "sd"), qnorm = c("p", "mean", "sd"),
  dbinom = c("x", "size", "prob"), pbinom = c("q", "size", "prob"),
  dpois = c("x", "lambda"), ppois = c("q", "lambda"),
  pexp = c("q", "rate"), qexp = c("p", "rate")
)
.exam_calc_distribution_names <- c("mean", "sd", "size", "prob", "lambda", "rate")
.exam_calc_reserved <- c(
  names(.exam_calc_arity), names(.exam_calc_distributions), .exam_calc_legacy,
  "sum", "c", "e", "pi", "inf", "prod",
  "Inf", "ans", "TRUE", "FALSE", "T", "F", "NA", "NaN", "NULL",
  "q", "help", "history", "choose", "integrate", "Vectorize",
  "D", "quote", "eval", "list", "sapply", "sin", "cos", "tan", "sinh",
  "cosh", "sign", "digamma"
)
.exam_calc_is_variable <- function(name) {
  grepl("^[A-Za-z][A-Za-z0-9_.]*$", name) && !name %in% .exam_calc_reserved
}
.exam_calc_code <- function(node) deparse1(node, width.cutoff = 500L)

.exam_calc_parse <- function(input) {
  if (nchar(input) > 4000L)
    .exam_calc_fail("Expression exceeds 4,000 characters. Split it into shorter expressions.")
  input <- chartr("０１２３４５６７８９（），．＋－−＊／＾％；＝×÷·",
                  "0123456789(),.+--*/^%;=*/*", input)
  input <- gsub("√", "sqrt", input, fixed = TRUE)
  input <- gsub("π", "pi", input, fixed = TRUE)
  input <- gsub("∞", "Inf", input, fixed = TRUE)
  number <- "(?:[0-9]+(?:[.][0-9]*)?|[.][0-9]+)(?:[eE][+-]?[0-9]+)?"
  pattern <- paste0(
    "\"[^\"\\\\\r\n]*\"|'[^'\\\\\r\n]*'|", number,
    "|[A-Za-z][A-Za-z0-9_.]*|\\*\\*|<-|<=|>=|==|!=|[+*/^(),%!=<>-]|[[:space:]]+"
  )
  tokens <- regmatches(input, gregexpr(pattern, input, perl = TRUE))[[1L]]
  if (!identical(paste0(tokens, collapse = ""), input))
    .exam_calc_fail("Unsupported syntax. Check parentheses, quotes, and operators.")
  tokens <- tokens[!grepl("^[[:space:]]+$", tokens)]
  output <- character()
  groups <- integer()
  last <- ""
  primary <- NA_integer_
  for (token in tokens) {
    if (token == "(") {
      if (last %in% c("number", "close")) output <- c(output, "*")
      start <- if (last == "name") primary else length(output) + 1L
      groups <- c(groups, start)
      if (length(groups) > 64L)
        .exam_calc_fail("Parentheses are nested more than 64 levels deep. Split the expression.")
      primary <- NA_integer_
      last <- ""
    } else if (token == ")") {
      if (!length(groups)) .exam_calc_fail("Unmatched closing parenthesis.")
      primary <- tail(groups, 1L)
      groups <- head(groups, -1L)
      last <- "close"
    } else if (token %in% c("!", "%")) {
      if (is.na(primary) || !last %in% c("number", "name", "close"))
        .exam_calc_fail("Put a value before factorial or percent, such as 5! or 2%.")
      atom <- output[seq.int(primary, length(output))]
      output <- c(head(output, primary - 1L),
        if (token == "!") c("factorial", "(", atom, ")")
        else c("(", atom, "/", "100", ")"))
      last <- "postfix"
      next
    } else if (grepl("^[A-Za-z]", token)) {
      if (last == "number")
        .exam_calc_fail(paste0("Use explicit multiplication between a number and a variable, such as ", tail(output, 1L),
                             "*", token, "."))
      primary <- length(output) + 1L
      last <- "name"
    } else if (grepl(paste0("^", number, "$"), token, perl = TRUE)) {
      primary <- length(output) + 1L
      last <- "number"
    } else {
      primary <- NA_integer_
      last <- ""
    }
    output <- c(output, if (token == "**") "^" else token)
  }
  if (length(groups)) .exam_calc_fail("Missing closing parenthesis.")
  parsed <- tryCatch(parse(text = paste(output, collapse = " "), keep.source = FALSE),
    error = function(e) {
      message <- "Incomplete or invalid expression. Check operators and argument separators."
      if (grepl("(?<![0-9])[0-9]{1,3}(?:,[0-9]{3})+(?![0-9])", input, perl = TRUE))
        message <- paste0(message, " If the comma is a thousands separator, write 1000 instead.")
      .exam_calc_fail(message)
    })
  if (length(parsed) != 1L)
    .exam_calc_fail("Enter one expression at a time or separate expressions with semicolons.")
  parsed[[1L]]
}

.exam_calc_compile <- function(node, variables) {
  notes <- character()
  integral <- FALSE
  closure <- function(name, body) {
    args <- alist(dummy = )
    names(args) <- name
    as.call(list(as.name("function"), as.pairlist(args), body))
  }
  compile <- function(node, scope = character(), integral_depth = 0L) {
    if (is.numeric(node) && !is.complex(node) && !is.na(node)) return(node)
    if (is.logical(node) && !is.na(node)) return(node)
    if (is.symbol(node)) {
      name <- as.character(node)
      if (name == "e") return(quote(exp(1)))
      if (name == "pi") return(quote(pi))
      if (name == "inf") return(Inf)
      if (name %in% c(scope, variables)) return(node)
      .exam_calc_fail(paste0("Undefined or disallowed variable: ", name,
                           ". Assign a value first, for example a = 3."))
    }
    if (!is.call(node))
      .exam_calc_fail("Enter a number or mathematical expression.")
    if (!is.symbol(node[[1L]]))
      .exam_calc_fail("Call allowed functions directly by name.")
    name <- as.character(node[[1L]])
    old_calculator <- name %in% .exam_calc_legacy
    distribution <- .exam_calc_distributions[[name]]
    if (!name %in% c(names(.exam_calc_arity), "sum") &&
        !old_calculator && is.null(distribution))
      .exam_calc_fail(paste0("Function or operator is not allowed: ", name, ". Enter help for the rules."))
    args <- as.list(node)[-1L]
    n <- length(args)
    if (any(vapply(seq_len(n), function(i) identical(args[[i]], quote(expr = )),
                   logical(1))))
      .exam_calc_fail(paste0(name, " has an empty argument. Supply a value between the commas."))
    if (old_calculator) {
      parameters <- formals(get(name, mode = "function", inherits = TRUE))
      allowed <- names(parameters)
      labels <- names(args)
      if (is.null(labels)) labels <- rep("", n)
      tagged <- labels[nzchar(labels)]
      if (anyDuplicated(tagged) || !all(tagged %in% allowed) || n > length(allowed))
        .exam_calc_fail(paste0(name, " has an invalid argument name or count. Use full argument names."))
      labels[!nzchar(labels)] <- head(setdiff(allowed, tagged), sum(!nzchar(labels)))
      for (i in seq_len(n)) {
        arg <- args[[i]]
        if (labels[i] %in% c("type", "kind") && is.character(arg)) next
        if (is.null(arg) && is.null(parameters[[labels[i]]])) next
        if (labels[i] %in% c("x", "p", "groups") && is.call(arg) &&
            identical(arg[[1L]], as.name("c"))) {
          elements <- as.list(arg)[-1L]
          if (!length(elements) || !is.null(names(elements)) ||
              any(vapply(seq_along(elements), function(j)
                identical(elements[[j]], quote(expr = )), logical(1))))
            .exam_calc_fail("Use c(0, 1, 2) for numeric vectors and supply every value.")
          args[[i]] <- as.call(c(list(as.name("c")),
            lapply(elements, compile, scope, integral_depth)))
        } else args[[i]] <- compile(arg, scope, integral_depth)
      }
      return(as.call(c(list(as.name(name)), args)))
    }
    if (!is.null(distribution)) {
      labels <- names(args)
      tagged <- labels[nzchar(labels)]
      allowed <- intersect(distribution, .exam_calc_distribution_names)
      if (!n || n > length(distribution) || anyDuplicated(tagged) ||
          !all(tagged %in% allowed))
        .exam_calc_fail(paste0(name, " has an invalid argument count or name. Use positional or documented named arguments."))
      args <- lapply(args, compile, scope, integral_depth)
      return(as.call(c(list(as.name(name)), args)))
    }
    if (!is.null(names(args)) && any(nzchar(names(args))))
      .exam_calc_fail(paste0(name, " expects arguments in the order shown in help."))
    if (name != "sum" && !n %in% .exam_calc_arity[[name]])
      .exam_calc_fail(paste0(name, " has the wrong number of arguments. Enter help for examples."))
    variable <- if (n >= 2L && is.symbol(args[[2L]])) as.character(args[[2L]]) else ""
    sigma <- name == "sum" && n == 4L && .exam_calc_is_variable(variable) &&
      variable %in% all.vars(args[[1L]])
    if (sigma || name %in% c("integral", "d")) {
      if (!.exam_calc_is_variable(variable))
        .exam_calc_fail(paste0(name, " expects a variable name as its second argument, such as x or k."))
      depth <- integral_depth + as.integer(name == "integral")
      if (depth > 2L) .exam_calc_fail("At most two integrals can be nested. Split the calculation.")
      body <- compile(args[[1L]], c(variable, scope), depth)
      if (name == "d") {
        derivative <- call("D", call("quote", body), variable)
        if (n == 2L) return(derivative)
        value <- compile(args[[3L]], scope, integral_depth)
        return(call("eval", derivative, as.call(c(list(as.name("list")),
                                                setNames(list(value), variable)))))
      }
      lo <- compile(args[[3L]], scope, depth)
      hi <- compile(args[[4L]], scope, depth)
      fn <- closure(variable, body)
      if (sigma) {
        notes <<- c(notes, paste0("Summation: Σ_{", variable, "=",
          .exam_calc_code(lo), "}^{", .exam_calc_code(hi), "}"))
        return(call("sum", call("sapply", call(":", lo, hi), fn)))
      }
      integral <<- TRUE
      return(call("$", call("integrate", call("Vectorize", fn, variable),
                            lo, hi, rel.tol = 1e-10), as.name("value")))
    }
    if (name == "sum") {
      if (!n) .exam_calc_fail("sum needs at least one value, for example sum(1, 2, 3).")
      notes <<- c(notes, "Ordinary addition.")
    }
    args <- lapply(args, compile, scope, integral_depth)
    if (name == "P") {
      fn <- quote(function(n, k) if (k == 0) 1 else prod((n - k + 1):n))
      return(call("(", as.call(c(list(call("(", fn)), args))))
    }
    translated <- c(C = "choose", Phi = "pnorm", invPhi = "qnorm", ln = "log")
    if (name %in% names(translated)) name <- translated[[name]]
    as.call(c(list(as.name(name)), args))
  }
  target <- NULL
  if (is.call(node) && is.symbol(node[[1L]]) &&
      as.character(node[[1L]]) %in% c("=", "<-")) {
    if (!is.symbol(node[[2L]]) || !.exam_calc_is_variable(as.character(node[[2L]])))
      .exam_calc_fail("Assign to a non-reserved name starting with a letter, for example a = 3.")
    target <- as.character(node[[2L]])
    node <- node[[3L]]
  }
  code <- compile(node)
  display <- if (is.null(target)) code else call("<-", as.name(target), code)
  list(code = code, target = target, notes = notes, integral = integral,
       display = display)
}
