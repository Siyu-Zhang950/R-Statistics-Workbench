# Input safety and independent numerical verification. No external services.

exam_check_inputs <- function(p = NULL, variance = NULL, sd = NULL,
                              lambda = NULL, lambda_unit = NULL,
                              question = NULL) {
  notes <- character()
  if (!is.null(p)) {
    .prob(p, "p")
    if (!is.null(question)) {
      question <- gsub("([0-9.]+)\\s*(percent|per cent)", "\\1%",
                        question, perl = TRUE, ignore.case = TRUE)
      pct <- regmatches(question, gregexpr(
        "[0-9]+(?:\\.[0-9]+)?\\s*%", question, perl = TRUE))[[1]]
      expected <- as.numeric(gsub("[% ]", "", pct)) / 100
      if (length(p) == 1 && length(expected) == 1 &&
          !grepl('at least|at most|more than|less than', question, ignore.case=TRUE) &&
          abs(p - expected) > 1e-10) {
        stop("Percentage mismatch: question says ", pct,
             " => probability ", expected, "; supplied p=", p,
             call. = FALSE)
      }
    }
    if (any(abs(p - 0.5) < 1e-12)) notes <- c(notes,
      "p=0.5 means 50%, not 0.5%. For 0.5% use 0.005.")
  }
  if (!is.null(variance) && !is.null(sd)) stop(
    "Supply variance OR sd, not both. N(mu,v) uses variance v.",
    call. = FALSE)
  if (!is.null(variance)) .positive(variance, "variance")
  if (!is.null(sd)) .positive(sd, "sd")
  if (!is.null(lambda)) {
    .num(lambda, "lambda", TRUE)
    if (lambda < 0) stop("lambda must be nonnegative")
    if (is.null(lambda_unit) || !is.character(lambda_unit) ||
        length(lambda_unit) != 1 || !nzchar(trimws(lambda_unit))) {
      notes <- c(notes,
        "Lambda exposure/time unit is unspecified. State and align units.")
    }
  }
  if (length(notes)) warning(paste(notes, collapse = " "),
                             call. = FALSE)
  invisible(notes)
}

.prob_event <- function(relation, q, lower, upper,
                        lower_closed, upper_closed, discrete) {
  if (relation == "between") {
    lo <- lower; hi <- upper
  } else {
    lo <- if (relation %in% c("ge", "gt", "eq")) q else -Inf
    hi <- if (relation %in% c("le", "lt", "eq")) q else Inf
    lower_closed <- relation %in% c("ge", "eq")
    upper_closed <- relation %in% c("le", "eq")
  }
  if (discrete) {
    lo <- if (lower_closed) ceiling(lo) else floor(lo) + 1
    hi <- if (upper_closed) floor(hi) else ceiling(hi) - 1
  }
  c(lo, hi)
}

.independent_probability <- function(dist, relation, q, lower, upper,
                                     lower_closed, upper_closed,
                                     n, p, lambda, mu, variance, a, b,
                                     successes, population, draws,
                                     df, df1, df2) {
  discrete <- dist %in% c("binomial", "bernoulli", "poisson",
                          "geometric", "hypergeometric",
                          "discrete_uniform")
  bounds <- .prob_event(relation, q, lower, upper, lower_closed,
                        upper_closed, discrete)
  lo <- bounds[1]; hi <- bounds[2]
  if (relation == "eq" && !discrete) return(list(
    value = 0, error = 0,
    method = "Analytic zero-width density integral; continuous CDF has no jump"))
  if (relation == "eq" && discrete && q != floor(q)) return(list(
    value = 0, error = 0, method = "Noninteger outside lattice support"))
  if (lo > hi) return(list(value = 0, error = 0,
                           method = "Empty event after support intersection"))
  if (discrete) {
    if (dist == "bernoulli") n <- 1
    low_support <- if (dist == "discrete_uniform") a else 0
    high_support <- switch(dist, binomial = n, bernoulli = n,
      hypergeometric = min(draws, successes), discrete_uniform = b, Inf)
    lo <- max(lo, low_support)
    hi <- min(hi, high_support)
    if (lo > hi) return(list(value = 0, error = 0,
                             method = "Empty event"))
    if (dist == "geometric") {
      val <- if (p == 1) as.numeric(lo <= 0 && hi >= 0) else {
        logbase <- log1p(-p)
        if (is.infinite(hi)) exp(lo*logbase) else exp(lo*logbase)*(-expm1((hi+1-lo)*logbase))
      }
      return(list(value = val, error = 0,
                  method = "Closed geometric-series probability"))
    }
    if (dist == "poisson" && lambda == 0) return(list(
      value = as.numeric(lo <= 0 && hi >= 0), error = 0,
      method = "Degenerate Poisson(0) mass"))
    if (dist %in% c("binomial", "bernoulli") && p %in% c(0, 1)) {
      x <- n * p
      return(list(value = as.numeric(lo <= x && x <= hi), error = 0,
                  method = "Degenerate binomial mass"))
    }
    tail_error <- 0
    if (is.infinite(hi)) {
      # Bound omitted Poisson tail by its decreasing geometric ratio.
      hi <- max(lo, ceiling(lambda + 12 * sqrt(lambda + 1) + 30))
      if (hi - lo > 100000) stop("Independent finite sum exceeds budget")
      next_term <- exp(-lambda + (hi + 1) * log(lambda) -
                         lgamma(hi + 2))
      ratio <- lambda / (hi + 2)
      tail_error <- next_term / (1 - ratio)
      if (!is.finite(tail_error) || tail_error > 1e-9) {
        stop("Cannot establish a sufficiently small omitted tail")
      }
    }
    if (hi - lo > 100000) stop("Independent finite sum exceeds budget")
    k <- seq(lo, hi)
    log_mass <- switch(dist,
      binomial = lgamma(n + 1) - lgamma(k + 1) -
        lgamma(n - k + 1) + k * log(p) + (n - k) * log1p(-p),
      bernoulli = lgamma(n + 1) - lgamma(k + 1) -
        lgamma(n - k + 1) + k * log(p) + (n - k) * log1p(-p),
      poisson = -lambda + k * log(lambda) - lgamma(k + 1),
      hypergeometric = lchoose(successes, k) +
        lchoose(population - successes, draws - k) -
        lchoose(population, draws),
      discrete_uniform = rep(-log(b - a + 1), length(k)))
    return(list(value = sum(exp(log_mass)), error = tail_error,
                method = "Direct mass formula summed over the event"))
  }
  if (dist == "exponential") {
    lo <- max(0, lo); hi <- max(0, hi)
    return(list(value = if (is.infinite(lo)) 0 else exp(-lambda*lo)*(-expm1(-lambda*(hi-lo))),
                error = 0, method = "Closed survival formula"))
  }
  if (dist == "uniform") return(list(
    value = max(0, min(b, hi) - max(a, lo)) / (b - a), error = 0,
    method = "Intersection length / support length"))
  if (dist %in% c("chisq", "f")) lo <- max(0, lo)
  if (lo >= hi) return(list(value = 0, error = 0,
                             method = "Empty support intersection"))
  f <- switch(dist,
    normal = function(x) exp(-(x - mu)^2 / (2 * variance)) /
      sqrt(2 * pi * variance),
    chisq = function(x) exp((df / 2 - 1) * log(x) - x / 2 -
                             df / 2 * log(2) - lgamma(df / 2)),
    t = function(x) exp(lgamma((df + 1) / 2) - lgamma(df / 2) -
                         0.5 * log(df * pi) -
                         (df + 1) / 2 * log1p(x^2 / df)),
    f = function(x) exp(df1 / 2 * log(df1 / df2) +
      (df1 / 2 - 1) * log(x) - lbeta(df1 / 2, df2 / 2) -
      (df1 + df2) / 2 * log1p(df1 * x / df2)))
  if (dist == 'normal') {
    lo <- (lo-mu)/sqrt(variance); hi <- (hi-mu)/sqrt(variance)
    f <- function(z) exp(-z*z/2)/sqrt(2*pi)
  }
  z <- integrate(f, lo, hi, rel.tol = 1e-10, abs.tol=1e-300, subdivisions = 1000)
  list(value = z$value, error = z$abs.error,
       method = "Numerical integration of explicit density formula")
}

.core_exam_prob <- exam_prob
exam_prob <- function(dist,
                      relation = c("le", "lt", "eq", "ge", "gt", "between"),
                      q = NULL, lower = NULL, upper = NULL,
                      lower_closed = TRUE, upper_closed = TRUE,
                      n = NULL, p = NULL, lambda = NULL, mu = 0,
                      variance = 1, a = 0, b = 1,
                      successes = NULL, population = NULL, draws = NULL,
                      df = NULL, df1 = NULL, df2 = NULL, digits = 6,
                      sd = NULL, lambda_unit = NULL, question = NULL) {
  relation <- match.arg(relation)
  specified_variance <- if (missing(variance)) NULL else variance
  notes <- exam_check_inputs(p, specified_variance, sd, lambda,
                             lambda_unit, question)
  if (!is.null(sd)) variance <- sd^2
  if (dist == "normal" && !is.null(question)) {
    nums <- regmatches(question, regexec(
      "N\\s*\\(\\s*([0-9.+-]+)\\s*,\\s*([0-9.+-]+)\\s*\\)",
      question, perl = TRUE))[[1]]
    if (length(nums) == 3 && abs(mu-as.numeric(nums[2])) > 1e-9) stop('Normal mean mismatch: check mu against N(mu,v) in question', call.=FALSE)
    if (length(nums) == 3 && abs(variance - as.numeric(nums[3])) > 1e-9) {
      stop("N(mu,v) in question uses variance v=", nums[3],
           "; supplied inputs imply variance=", variance,
           ". Check sd versus variance.", call. = FALSE)
    }
  }
  answer <- .core_exam_prob(dist, relation, q, lower, upper,
    lower_closed, upper_closed, n, p, lambda, mu, variance, a, b,
    successes, population, draws, df, df1, df2, digits)
  check <- tryCatch(.independent_probability(dist, relation, q, lower,
    upper, lower_closed, upper_closed, n, p, lambda, mu, variance,
    a, b, successes, population, draws, df, df1, df2),
    error = function(e) list(value = NA_real_, error = NA_real_,
                             method = conditionMessage(e)))
  difference <- abs(answer$value - check$value)
  tolerance <- max(1e-11 * max(abs(answer$value), abs(check$value)),
                   10 * check$error, 1e-300, na.rm = TRUE)
  passed <- is.finite(difference) && difference <= tolerance
  answer$verification <- list(primary = answer$value,
    independent = check$value, difference = difference,
    tolerance = tolerance, error_estimate = check$error,
    independent_method = check$method, passed = passed)
  answer$check <- paste0(if (passed) "DUAL CHECK PASS: " else
    "DUAL CHECK INCOMPLETE/FAILED: ", check$method,
    "; independent=", .fmt(check$value), "; abs diff=", .fmt(difference),
    "; tolerance=", .fmt(tolerance))
  if (!passed) warning(answer$check, call. = FALSE)
  if (length(notes)) answer$note <- c(answer$note,
                                     paste("INPUT WARNING:", notes))
  if (dist == "normal") answer$note <- c(answer$note,
    paste0("Normal variance=", variance, "; sd=", sqrt(variance)))
  answer
}

.core_exam_test <- exam_test
exam_test <- function(prevalence, sensitivity, false_positive,
                      test = c("positive", "negative"), digits = 6,
                      question = NULL) {
  notes <- exam_check_inputs(p = prevalence, question = question)
  answer <- .core_exam_test(prevalence, sensitivity, false_positive,
                            test, digits)
  test <- match.arg(test)
  # Count-table construction independently verifies the two-way partition.
  lk <- c(sensitivity, false_positive)
  if (test == "negative") lk <- 1 - lk
  counts <- c(prevalence, 1 - prevalence) * 1e8 * lk
  other <- counts / sum(counts)
  answer$verification <- list(passed = max(abs(other - answer$value)) <
                                1e-12, independent = other,
                              method = "Hypothetical two-way count table")
  answer$check <- paste0("Count-table cross-check: ",
                         .fmt(other), "; sums to ", sum(other))
  if (length(notes)) answer$note <- c(answer$note, notes)
  answer
}

.core_bayes <- exam_bayes
exam_bayes <- function(prior, likelihood, labels = NULL, digits = 6) {
  answer <- .core_bayes(prior, likelihood, labels, digits)
  logw <- log(prior) + log(likelihood)
  scaled <- exp(logw - max(logw))
  other <- scaled / sum(scaled)
  pass <- max(abs(other - answer$value)) < 1e-12
  answer$verification <- list(passed = pass, independent = other,
                              method = "Log-weight normalization")
  answer$check <- paste0("Log-weight normalization: ", .fmt(other),
                         "; dual check ", if (pass) "PASS" else "FAIL")
  if (!pass) warning(answer$check, call. = FALSE)
  answer
}

.core_multinomial <- exam_multinomial
exam_multinomial <- function(counts, p, digits = 6) {
  answer <- .core_multinomial(counts, p, digits)
  nz <- counts > 0
  lp <- lgamma(sum(counts) + 1) - sum(lgamma(counts + 1)) +
    sum(counts[nz] * log(p[nz]))
  other <- exp(lp)
  pass <- abs(other - answer$value$probability) <=
    1e-11 * max(other, answer$value$probability, 1e-300)
  answer$verification <- list(passed = pass, independent = other,
                              method = "Explicit multinomial mass formula")
  answer$check <- paste0("Explicit factorial/product formula: ",
                         .fmt(other), "; ", if (pass) "PASS" else "FAIL")
  if (!pass) warning(answer$check, call. = FALSE)
  answer
}

.core_clt <- exam_clt
exam_clt <- function(n, mu, variance, lower = -Inf, upper = Inf,
                     quantity = c("sum", "mean"),
                     normal_population = FALSE, digits = 6) {
  answer <- .core_clt(n, mu, variance, lower, upper, quantity,
                      normal_population, digits)
  z <- answer$value[c("z_lower", "z_upper")]
  other <- integrate(function(x) exp(-x^2 / 2) / sqrt(2 * pi),
                     z[1], z[2], rel.tol = 1e-10)
  pass <- abs(answer$value["probability"] - other$value) <=
    max(1e-11 * abs(other$value), 10 * other$abs.error, 1e-300)
  answer$verification <- list(passed = pass, independent = other$value,
                              method = "Standard normal density integral")
  answer$check <- paste0("Density integral: ", .fmt(other$value),
    "; ", if (pass) "PASS" else "FAIL",
    "; verifies arithmetic only, not finite-n CLT accuracy")
  if (!pass) warning(answer$check, call. = FALSE)
  answer
}

.core_continuous <- exam_continuous
exam_continuous <- function(f, lower, upper, interval = NULL, digits = 6,
                            cdf = NULL) {
  answer <- .core_continuous(f, lower, upper, interval, digits)
  if (is.null(interval)) return(answer)
  lo <- max(lower, interval[1]); hi <- min(upper, interval[2])
  check <- tryCatch({
    if (!is.null(cdf)) {
      if (!is.function(cdf)) stop("cdf must be a function")
      other <- if (lo >= hi) 0 else cdf(hi) - cdf(lo)
      .prob(other, "CDF difference")
      list(value = other, error = 0,
           method = "User-supplied analytic CDF difference; verify its derivation")
    } else {
      if (!all(is.finite(c(lo, hi)))) stop(
        "Infinite event bounds: supply an independently derived cdf= for a second check")
      simpson <- function(n) {
        if (lo >= hi) return(0)
        y <- f(seq(lo, hi, length.out = n + 1L))
        if (length(y) != n + 1L || any(!is.finite(y))) stop(
          "Simpson check needs finite vectorized density values at endpoints")
        (hi - lo) / (3 * n) * (y[1] + y[n + 1L] +
          4 * sum(y[seq(2L, n, by = 2L)]) +
          2 * sum(y[seq(3L, n - 1L, by = 2L)]))
      }
      coarse <- simpson(2048L); fine <- simpson(4096L)
      list(value = fine, error = abs(fine - coarse) / 15,
           method = "Composite Simpson rule at two resolutions; estimated error assumes smooth density")
    }
  }, error = function(e) list(value = NA_real_, error = NA_real_,
                              method = conditionMessage(e)))
  diff <- abs(unname(answer$value["probability"]) - check$value)
  tol <- max(1e-10 * abs(check$value), 10 * check$error, 1e-12,
             na.rm = TRUE)
  pass <- length(diff) == 1L && is.finite(diff) && diff <= tol
  answer$verification <- list(passed = pass, independent = check$value,
    difference = diff, tolerance = tol, method = check$method)
  answer$check <- c(answer$check, paste0("Probability dual check ",
    if (pass) "PASS: " else "INCOMPLETE/FAILED: ", check$method,
    "; independent=", .fmt(check$value)))
  answer$note <- c(answer$note,
    "Agreement of two numerical methods is not an analytic proof or a rigorous error bound.")
  if (!pass) warning(tail(answer$check, 1), call. = FALSE)
  answer
}

.core_normal_interval <- exam_normal_interval
exam_normal_interval <- function(mu, variance, level = .95, digits = 6) {
  answer <- .core_normal_interval(mu, variance, level, digits)
  other <- exam_prob("normal", "between", lower = answer$value[1],
    upper = answer$value[2], mu = mu, variance = variance)
  answer$verification <- other$verification
  answer$verification$passed <- isTRUE(other$verification$passed) &&
    abs(other$value - level) < 1e-10
  answer$check <- c(answer$check, other$check)
  answer
}

