# Calculation core: base R; no question text database.
.num <- function(x, name, scalar = FALSE, finite = TRUE) {
    if (!is.numeric(x) || !length(x) || anyNA(x) || (finite && any(!is.finite(x))) || 
        (scalar && length(x) != 1L)) 
        stop(name, " must be a non-missing numeric ", if (scalar) 
            "scalar"
        else "vector", call. = FALSE)
    invisible(x)
}

.int <- function(x, name, min = 0) {
    .num(x, name, TRUE)
    if (x < min || x != floor(x)) 
        stop(name, " must be an integer >= ", min, call. = FALSE)
}

.positive <- function(x, name) {
    .num(x, name, TRUE)
    if (x <= 0) 
        stop(name, " must be > 0", call. = FALSE)
}

.prob <- function(x, name) {
    .num(x, name)
    if (any(x < 0 | x > 1)) 
        stop(name, " must be in [0,1]", call. = FALSE)
}

.clean <- function(x, na_rm = FALSE) {
    if (!is.logical(na_rm) || length(na_rm) != 1 || is.na(na_rm)) 
        stop("na_rm must be TRUE/FALSE")
    if (anyNA(x) && !na_rm) 
        stop("Missing values: explicitly set na_rm=TRUE to remove them.", call. = FALSE)
    if (na_rm) 
        x <- x[!is.na(x)]
    .num(x, "x")
    x
}

.fmt <- function(x) paste(format(x, digits = 10, trim = TRUE, scientific = FALSE), 
    collapse = ", ")

.res <- function(title, type, conditions, formula, steps, value, check = "Not independently checked in this call.", 
    sources = "", note = "", digits = 6) {
    .int(digits, "digits")
    if (digits > 15) 
        stop("digits must be <= 15")
    structure(list(title = title, type = type, conditions = conditions, formula = formula, 
        steps = steps, value = value, check = check, sources = sources, note = note, 
        digits = digits), class = "exam_result")
}

print.exam_result <- function(x, ...) {
    cat("\n", x$title, " [", x$type, "]\n", sep = "")
    for (nm in c("conditions", "formula", "steps")) {
        cat(toupper(nm), ":\n", paste0("  ", x[[nm]], collapse = "\n"), "\n", 
            sep = "")
    }
    cat("RESULT (calculation retains full precision):\n")
    if (identical(x$numeric_status,"underflow"))
        cat("Positive probability below floating-point range; log_probability =",format(x$log_probability,digits=15),"\n")
    else if (is.numeric(x$value) && is.null(dim(x$value))) 
        print(.exam_display(x$value, x$digits), quote=FALSE)
    else print(x$value, digits = max(3, x$digits))
    cat("CHECK: ", paste(x$check, collapse = "; "), "\n", sep = "")
    if (any(nzchar(x$sources))) cat("SOURCE: ", paste(x$sources, 
        collapse = "; "), "\n", sep = "")
    if (length(x$note) && any(nzchar(x$note))) 
        cat("NOTE: ", paste(x$note, collapse = "; "), "\n", sep = "")
    invisible(x)
}

exam_paper <- function(answer) {
    if (!inherits(answer, "exam_result")) 
        stop("Supply a result from an exam_* solver")
    cat("\n", answer$title, "\nMethod: ", answer$type, "\n", sep = "")
    for (nm in c("conditions", "formula", "steps")) cat(toupper(nm), ":\n", paste0("  ", 
        answer[[nm]], collapse = "\n"), "\n", sep = "")
    cat("CONCLUSION:\n")
    if (identical(answer$numeric_status,"underflow"))
        cat("Positive probability below floating-point range; log_probability =",format(answer$log_probability,digits=15),"\n")
    else if (is.numeric(answer$value) && is.null(dim(answer$value))) 
        print(.exam_display(answer$value, answer$digits), quote=FALSE)
    else print(answer$value, digits = max(3, answer$digits))
    if (length(answer$note) && any(nzchar(answer$note))) 
        cat("LIMITS: ", paste(answer$note, collapse = "; "), "\n", sep = "")
    invisible(answer)
}

.dist <- function(dist, n, p, lambda, mu, variance, a, b, successes, population, 
    draws, df, df1, df2) {
    dist <- match.arg(dist, c("normal", "binomial", "poisson", "exponential", 
        "uniform", "discrete_uniform", "bernoulli", "hypergeometric", "geometric", 
        "chisq", "t", "f"))
    discrete <- dist %in% c("binomial", "poisson", "discrete_uniform", "bernoulli", 
        "hypergeometric", "geometric")
    support <- c(-Inf, Inf)
    refs <- ""
    if (dist == "normal") {
        .num(mu, "mu", TRUE)
        .positive(variance, "variance")
        den <- function(x) dnorm(x, mu, sqrt(variance))
        cdf <- function(x, tail = TRUE) pnorm(x, mu, sqrt(variance), lower.tail = tail)
        desc <- paste0("X ~ N(", .fmt(mu), ", ", .fmt(variance), "); second parameter is variance")
        formula <- "f(x)=exp(-(x-mu)^2/(2*variance))/sqrt(2*pi*variance); Z=(X-mu)/sqrt(variance)"
        cond <- "Normal model; variance > 0. R's sd argument equals sqrt(variance)."
    }
    else if (dist %in% c("binomial", "bernoulli")) {
        if (dist == "bernoulli") 
            n <- 1
        .int(n, "n")
        .prob(p, "p")
        .num(p, "p", TRUE)
        support <- c(0, n)
        den <- function(x) dbinom(x, n, p)
        cdf <- function(x, tail = TRUE) pbinom(x, n, p, lower.tail = tail)
        desc <- paste0("X ~ Binomial(n=", n, ", p=", .fmt(p), ")")
        formula <- "P(X=k)=choose(n,k)*p^k*(1-p)^(n-k), k=0,...,n"
        cond <- "Fixed n; independent Bernoulli trials; same success probability p."
    }
    else if (dist == "poisson") {
        .num(lambda, "lambda", TRUE)
        if (lambda < 0) 
            stop("lambda must be >= 0")
        support <- c(0, Inf)
        den <- function(x) dpois(x, lambda)
        cdf <- function(x, tail = TRUE) ppois(x, lambda, lower.tail = tail)
        desc <- paste0("X ~ Poisson(lambda=", .fmt(lambda), ")")
        formula <- "P(X=k)=exp(-lambda)*lambda^k/k!; E(X)=Var(X)=lambda"
        cond <- "Event count in a fixed exposure; Poisson model. Match units of lambda and exposure."
    }
    else if (dist == "exponential") {
        .positive(lambda, "lambda")
        support <- c(0, Inf)
        den <- function(x) dexp(x, rate = lambda)
        cdf <- function(x, tail = TRUE) pexp(x, rate = lambda, lower.tail = tail)
        desc <- paste0("X ~ Exponential(rate=", .fmt(lambda), ")")
        formula <- "f(x)=lambda*exp(-lambda*x), x>=0; F(x)=1-exp(-lambda*x); E(X)=1/lambda"
        cond <- "Waiting time under an exponential model; lambda is rate, not mean."
    }
    else if (dist == "uniform") {
        .num(a, "a", TRUE)
        .num(b, "b", TRUE)
        if (a >= b) 
            stop("Require a < b")
        support <- c(a, b)
        den <- function(x) dunif(x, a, b)
        cdf <- function(x, tail = TRUE) punif(x, a, b, lower.tail = tail)
        desc <- paste0("X ~ Uniform(", a, ", ", b, ")")
        formula <- "f(x)=1/(b-a) on [a,b]; F(x)=(x-a)/(b-a) inside support"
        cond <- "All equal-width intervals inside [a,b] have equal probability."
    }
    else if (dist == "discrete_uniform") {
        .num(a, "a", TRUE)
        .num(b, "b", TRUE)
        if (a != floor(a) || b != floor(b) || a > b) 
            stop("Integer a<=b required")
        support <- c(a, b)
        den <- function(x) ifelse(x == floor(x) & x >= a & x <= b, 1/(b - a + 
            1), 0)
        cdf <- function(x, tail = TRUE) {
            v <- pmin(1, pmax(0, (floor(x) - a + 1)/(b - a + 1)))
            if (tail) 
                v
            else 1 - v
        }
        desc <- paste0("X uniform on integers ", a, " to ", b)
        formula <- "P(X=k)=1/(b-a+1), k=a,...,b"
        cond <- "Consecutive integer outcomes are equally likely."
    }
    else if (dist == "hypergeometric") {
        .int(population, "population")
        .int(successes, "successes")
        .int(draws, "draws")
        if (successes > population || draws > population) 
            stop("successes and draws cannot exceed population")
        support <- c(max(0, draws - (population - successes)), min(draws, successes))
        den <- function(x) dhyper(x, successes, population - successes, draws)
        cdf <- function(x, tail = TRUE) phyper(x, successes, population - successes, 
            draws, lower.tail = tail)
        desc <- paste0("X ~ Hypergeometric(N=", population, ", M=", successes, 
            ", draws=", draws, ")")
        formula <- "P(X=k)=choose(M,k)*choose(N-M,draws-k)/choose(N,draws)"
        cond <- "Uniform sampling WITHOUT replacement from a finite population with fixed success count."
    }
    else if (dist == "geometric") {
        .prob(p, "p")
        .positive(p, "p")
        support <- c(0, Inf)
        den <- function(x) dgeom(x, p)
        cdf <- function(x, tail = TRUE) pgeom(x, p, lower.tail = tail)
        desc <- paste0("X = failures before first success, p=", .fmt(p))
        formula <- "P(X=k)=(1-p)^k*p, k=0,1,...; trials until success T=X+1"
        cond <- "Independent Bernoulli trials with constant p. If counting trial number, subtract 1 before calling."
    }
    else if (dist == "chisq") {
        .positive(df, "df")
        support <- c(0, Inf)
        den <- function(x) dchisq(x, df)
        cdf <- function(x, tail = TRUE) pchisq(x, df, lower.tail = tail)
        desc <- paste0("X ~ chi-square(df=", df, ")")
        formula <- "f(x)=x^(df/2-1)*exp(-x/2)/(2^(df/2)*Gamma(df/2)), x>0; zero otherwise"
        cond <- "Specified chi-square distribution; df>0. This call does not select a hypothesis test."
    }
    else if (dist == "t") {
        .positive(df, "df")
        den <- function(x) dt(x, df)
        cdf <- function(x, tail = TRUE) pt(x, df, lower.tail = tail)
        desc <- paste0("X ~ Student t(df=", df, ")")
        formula <- "f(x)=Gamma((df+1)/2)/(sqrt(df*pi)*Gamma(df/2))*(1+x^2/df)^(-(df+1)/2); T=Z/sqrt(U/df) with independent Z,U"
        cond <- "Specified Student t distribution; df>0. Mean requires df>1; variance requires df>2."
    }
    else {
        .positive(df1, "df1")
        .positive(df2, "df2")
        support <- c(0, Inf)
        den <- function(x) df(x, df1, df2)
        cdf <- function(x, tail = TRUE) pf(x, df1, df2, lower.tail = tail)
        desc <- paste0("X ~ F(df1=", df1, ", df2=", df2, ")")
        formula <- "f(x)=(df1/df2)^(df1/2)*x^(df1/2-1)/(B(df1/2,df2/2)*(1+df1*x/df2)^((df1+df2)/2)), x>0"
        cond <- "Specified F distribution; numerator and denominator degrees of freedom must keep their order."
    }
    list(name = desc, den = den, cdf = cdf, discrete = discrete, support = support, 
        formula = formula, conditions = cond, refs = refs)
}

exam_prob <- function(dist, relation = c("le", "lt", "eq", "ge", "gt", "between"), 
    q = NULL, lower = NULL, upper = NULL, lower_closed = TRUE, upper_closed = TRUE, 
    n = NULL, p = NULL, lambda = NULL, mu = 0, variance = 1, a = 0, b = 1, successes = NULL, 
    population = NULL, draws = NULL, df = NULL, df1 = NULL, df2 = NULL, digits = 6) {
    relation <- match.arg(relation)
    d <- .dist(dist, n, p, lambda, mu, variance, a, b, successes, population, 
        draws, df, df1, df2)
    for (v in list(lower_closed, upper_closed)) if (!is.logical(v) || length(v) != 
        1 || is.na(v)) 
        stop("Endpoint flags must be TRUE/FALSE")
    if (relation != "between") {
        .num(q, "q", TRUE, FALSE)
        if (is.nan(q)) 
            stop("Invalid q")
    }
    if (relation == "eq") {
        value <- if (d$discrete && is.finite(q) && q == floor(q)) 
            d$den(q)
        else 0
        event <- paste0("P(X = ", q, ")")
        steps <- if (d$discrete) 
            paste0("Evaluate the PMF at ", q, " (zero outside integer support).")
        else "For an absolutely continuous distribution, P(X=q)=integral_q^q f(x) dx=0; density height is not probability."
        check <- if (d$discrete) 
            "PMF evaluation; no additional independent check."
        else "Zero-width interval and the continuity argument agree."
    }
    else {
        if (relation == "between") {
            .num(lower, "lower", TRUE, FALSE)
            .num(upper, "upper", TRUE, FALSE)
            if (lower > upper) 
                stop("Require lower <= upper")
            lo <- lower
            hi <- upper
        }
        else {
            lo <- if (relation %in% c("ge", "gt")) 
                q
            else -Inf
            hi <- if (relation %in% c("le", "lt")) 
                q
            else Inf
            lower_closed <- relation == "ge"
            upper_closed <- relation == "le"
        }
        event <- paste0("P(", lo, if (lower_closed) 
            " <= X"
        else " < X", if (upper_closed) 
            " <= "
        else " < ", hi, ")")
        if (d$discrete) {
            L <- if (lower_closed) 
                ceiling(lo)
            else floor(lo) + 1
            U <- if (upper_closed) 
                floor(hi)
            else ceiling(hi) - 1
            L <- max(L, d$support[1])
            U <- min(U, d$support[2])
            if (L > U) 
                value <- 0
            else if (is.infinite(L) && L < 0) 
                value <- d$cdf(U)
            else if (is.infinite(U) && U > 0) 
                value <- d$cdf(L - 1, FALSE)
            else {
                value <- if (d$cdf(L - 1) > 0.5) 
                  d$cdf(L - 1, FALSE) - d$cdf(U, FALSE)
                else d$cdf(U) - d$cdf(L - 1)
            }
            steps <- c(paste0("Integer support intersected with event: L=", L, 
                ", U=", U, "."), "P(L<=X<=U)=F(U)-F(L-1). For upper tails use lower.tail=FALSE to avoid cancellation.")
            if (is.finite(L) && is.finite(U) && L <= U && U - L <= 5000) {
                direct <- sum(d$den(seq(L, U)))
                check <- paste0("Direct finite PMF sum = ", .fmt(direct), "; difference = ", 
                  .fmt(value - direct))
                if (U - L <= 20) 
                  steps <- c(steps, paste0("k = ", .fmt(seq(L, U)), "; PMF terms = ", 
                    .fmt(d$den(seq(L, U)))))
            }
            else if (is.finite(L) && is.infinite(U) && L >= 0 && L <= 5000) {
                direct <- if (L == 0) 
                  1
                else 1 - sum(d$den(seq(0, L - 1)))
                check <- paste0("Complement of finite PMF sum = ", .fmt(direct), 
                  "; difference = ", .fmt(value - direct), ". Complement check may lose accuracy in extreme tails.")
                if (L > 0 && L <= 21) 
                  steps <- c(steps, paste0("Subtract lower-event terms at k=", 
                    .fmt(seq(0, L - 1)), ": ", .fmt(d$den(seq(0, L - 1)))))
            }
            else check <- "No bounded finite-sum check attempted (empty, infinite, or large support)."
        }
        else {
            if (lo == hi) 
                value <- 0
            else if (is.infinite(lo) && lo < 0) 
                value <- d$cdf(hi)
            else if (is.infinite(hi) && hi > 0) 
                value <- d$cdf(lo, FALSE)
            else value <- if (d$cdf(lo) > 0.5) 
                d$cdf(lo, FALSE) - d$cdf(hi, FALSE)
            else d$cdf(hi) - d$cdf(lo)
            steps <- c("Point masses at finite endpoints are zero, so open/closed endpoints give the same value.", 
                paste0("F(upper)=", .fmt(d$cdf(hi)), "; F(lower)=", .fmt(d$cdf(lo)), 
                  "; interval probability obtained from CDF/tails."))
            if (dist == "normal") 
                steps <- c(paste0("sigma=sqrt(", .fmt(variance), ")=", .fmt(sqrt(variance)), 
                  "; standardized limits: (", lo, "-", mu, ")/sigma=", .fmt((lo - 
                    mu)/sqrt(variance)), " and (", hi, "-", mu, ")/sigma=", .fmt((hi - 
                    mu)/sqrt(variance))), steps)
            check <- "Probability uses the specified distribution's CDF; no independent numerical integration attempted."
            il <- max(lo, d$support[1])
            iu <- min(hi, d$support[2])
            if (il < iu) {
                z <- try(integrate(d$den, il, iu, rel.tol = 1e-09), silent = TRUE)
                if (!inherits(z, "try-error")) 
                  check <- paste0("Independent numerical density integral = ", 
                    .fmt(z$value), "; estimated absolute error = ", .fmt(z$abs.error), 
                    "; difference = ", .fmt(value - z$value), " (not a proof).")
            }
        }
    }
    if (!is.finite(value) || value < -1e-10 || value > 1 + 1e-10) 
        stop("Invalid computed probability")
    .res(paste(d$name, event, sep = " | "), "MODEL-BASED NUMERIC", d$conditions, 
        d$formula, steps, value, check, d$refs, "Choose the model and translate the event before calculating. R does not infer missing assumptions.", 
        digits)
}

exam_bayes <- function(prior, likelihood, labels = NULL, digits = 6) {
    .prob(prior, "prior")
    .prob(likelihood, "likelihood")
    if (length(prior) != length(likelihood) || abs(sum(prior) - 1) > 1e-10) 
        stop("Matched vectors and sum(prior)=1 required")
    if (is.null(labels)) 
        labels <- paste0("A", seq_along(prior))
    if (length(labels) != length(prior) || anyNA(labels) || anyDuplicated(labels)) 
        stop("Unique labels of matching length required")
    numer <- prior * likelihood
    denom <- sum(numer)
    if (denom <= 0) 
        stop("Conditioning event has probability zero")
    posterior <- setNames(numer/denom, labels)
    .res("Total probability and Bayes", "EXACT FORMULA / NUMERIC EVALUATION", 
        "A_i form a mutually exclusive exhaustive partition; likelihood_i=P(B|A_i); P(B)>0.", 
        "P(B)=sum_i P(A_i)P(B|A_i); P(A_i|B)=P(A_i)P(B|A_i)/P(B)", c(paste0(labels, 
            ": prior=", format(prior, digits = 10), "; likelihood=", format(likelihood, 
                digits = 10), "; product=", format(numer, digits = 10)), paste0("Numerators = ", 
            .fmt(numer)), paste0("Denominator = ", .fmt(denom))), posterior, 
        paste0("Posterior sum = ", .fmt(sum(posterior))), "", 
        digits = digits)
}

exam_test <- function(prevalence, sensitivity, false_positive, test = c("positive", 
    "negative"), digits = 6) {
    test <- match.arg(test)
    for (z in list(prevalence, sensitivity, false_positive)) {
        .prob(z, "test parameter")
        .num(z, "test parameter", TRUE)
    }
    lik <- c(sensitivity, false_positive)
    if (test == "negative") 
        lik <- 1 - lik
    exam_bayes(c(prevalence, 1 - prevalence), lik, c("infected", "uninfected"), 
        digits)
}

exam_multinomial <- function(counts, p, digits = 6) {
    .num(counts, "counts")
    .prob(p, "p")
    if (length(counts) != length(p) || any(counts < 0 | counts != floor(counts)) || 
        abs(sum(p) - 1) > 1e-10) 
        stop("Integer counts and matching probabilities summing to 1 required")
    n <- sum(counts)
    lp <- dmultinom(counts, prob = p, log = TRUE)
    .res("Multinomial chapter supplement", "EXACT FORMULA / NUMERIC EVALUATION", 
        "n independent trials; mutually exclusive exhaustive categories with fixed p_i.", 
        "P(X_i=x_i all i)=n!/prod_i(x_i!)*prod_i(p_i^x_i)", c(paste0("n=", n, 
            "; counts=", .fmt(counts)), paste0("p=", .fmt(p), "; log probability=", 
            .fmt(lp))), list(probability = exp(lp), expected = n * p, covariance = n * 
            (diag(p, length(p)) - outer(p, p))), "Each marginal is binomial; covariance off-diagonal is -n*p_i*p_j.", 
        "", digits = digits)
}

exam_count <- function(n, k = NULL, ordered = FALSE, replacement = FALSE, groups = NULL, 
    digits = 6) {
    .int(n, "n")
    for (z in list(ordered, replacement)) if (!is.logical(z) || length(z) != 
        1 || is.na(z)) 
        stop("ordered/replacement must be TRUE/FALSE")
    if (!is.null(groups)) {
        .num(groups, "groups")
        if (any(groups <= 0 | groups != floor(groups)) || sum(groups) != n) 
            stop("Positive integer groups must sum to n")
        if (!is.null(k)) 
            stop("Use groups only for full permutations (omit k)")
        logv <- lfactorial(n) - sum(lfactorial(groups))
        formula <- "n! / product(group_size!)"
        label <- "Permutation of a multiset"
    }
    else if (is.null(k)) {
        logv <- lfactorial(n)
        formula <- "n!"
        label <- "Permutation of n distinct objects"
    }
    else {
        .int(k, "k")
        if (!replacement && k > n) 
            stop("Without replacement require k<=n")
        if (n == 0) {
            logv <- if (k == 0) 
                0
            else -Inf
        }
        else if (ordered) 
            logv <- if (replacement) 
                k * log(n)
            else lfactorial(n) - lfactorial(n - k)
        else logv <- if (replacement) 
            lchoose(n + k - 1, k)
        else lchoose(n, k)
        formula <- if (ordered) {
            if (replacement) 
                "n^k"
            else "n!/(n-k)!"
        }
        else {
            if (replacement) 
                "choose(n+k-1,k)"
            else "choose(n,k)"
        }
        label <- paste(if (ordered) 
            "Variation (ordered)"
        else "Combination (unordered)", if (replacement) 
            "with repetition"
        else "without repetition")
    }
    count <- exp(logv)
    if (is.finite(count) && count < 2^53) 
        count <- round(count)
    .res(label, "COUNTING", c("Identify whether order matters and whether repetition is allowed.", 
        "For groups, supply every group of identical objects, including singleton groups."), 
        formula, c(paste0("n=", n, if (!is.null(k)) paste0("; k=", k)), paste0("log(count)=", 
            .fmt(logv))), c(count = count, log_count = logv), "Uses log-factorials/log-binomial coefficients; no large outcome enumeration.", 
        "", "Counts above 2^53 may lose integer precision; Inf means numeric overflow. This is not arbitrary-precision arithmetic.", 
        digits)
}

exam_quantile <- function(x, p = c(0.25, 0.5, 0.75), na_rm = FALSE, digits = 6) {
    x <- sort(.clean(x, na_rm))
    .prob(p, "p")
    n <- length(x)
    v <- as.numeric(quantile(x, p, type = 2, names = FALSE))
    explain <- vapply(p, function(z) {
        nz <- n * z
        if (z == 0) 
            "p=0: minimum"
        else if (z == 1) 
            "p=1: maximum"
        else if (nz == floor(nz)) 
            paste0("n*p=", .fmt(nz), ": average order statistics ", round(nz), 
                " and ", round(nz) + 1)
        else paste0("n*p=", .fmt(nz), ": order statistic ", ceiling(nz))
    }, character(1))
    .res("Empirical quantiles (R type 2)", "DESCRIPTIVE", "Numeric ungrouped data; remove NA only explicitly.", 
        "If n*p is not integer: x_(ceiling(n*p)); if integer k in 1,...,n-1: (x_(k)+x_(k+1))/2.", 
        explain, setNames(v, paste0("p=", p)), paste0("R type=7 would give: ", 
            .fmt(quantile(x, p, type = 7, names = FALSE))), "", 
        "R uses type 7 by default. The two definitions can give different results.", 
        digits)
}

exam_describe <- function(x, na_rm = FALSE, c = NULL, digits = 6) {
    missing <- sum(is.na(x))
    x <- .clean(x, na_rm)
    n <- length(x)
    m <- mean(x)
    ss <- sum((x - m)^2)
    v <- ss/n
    val <- c(n = n, missing_removed = missing, mean = m, median = median(x), 
        variance_n = v, sd_n = sqrt(v), variance_n_minus_1 = if (n > 1) ss/(n - 
            1) else NA_real_, minimum = min(x), maximum = max(x), range = diff(range(x)), 
        Q1_type2 = quantile(x, 0.25, type = 2, names = FALSE), Q3_type2 = quantile(x, 
            0.75, type = 2, names = FALSE), IQR_type2 = diff(quantile(x, c(0.25, 
            0.75), type = 2, names = FALSE)), mean_absolute_deviation_from_median = mean(abs(x - 
            median(x))), median_absolute_deviation_unscaled = median(abs(x - 
            median(x))), CV_n = if (m > 0) sqrt(v)/m else NA_real_, geometric_mean = if (all(x > 
            0)) exp(mean(log(x))) else NA_real_, harmonic_mean = if (all(x > 
            0)) n/sum(1/x) else NA_real_)
    steps <- c(paste0("n=", n, "; sum(x)=", .fmt(sum(x)), "; mean=sum(x)/n=", 
        .fmt(m)), paste0("sum squared deviations=", .fmt(ss), "; divide by n=", 
        n, " to get descriptive variance."))
    if (!is.null(c)) {
        .num(c, "c", TRUE)
        val <- base::c(val, MSE_at_c = mean((x - c)^2))
        steps <- base::c(steps, paste0("MSE(c)=variance_n+(mean-c)^2=", .fmt(v + 
            (m - c)^2)))
    }
    .res("Descriptive measures", "DESCRIPTIVE", "Finite numeric observations; geometric/harmonic mean require positive data.", 
        "mean=sum(x)/n; variance_n=sum((x-mean)^2)/n; sample variance uses n-1", 
        steps, val, paste0("sum(x-mean)=", .fmt(sum(x - m)), "; E_empirical(X^2)-mean^2=", 
            .fmt(mean(x^2) - m^2)), "", 
        "variance_n uses denominator n. var(x) uses n-1. CV requires a meaningful ratio scale and positive mean. Absolute deviations here are centered on the median and named by aggregation.", 
        digits)
}

exam_frequency <- function(x, breaks = NULL, right = TRUE, include_lowest = TRUE, 
    na_rm = FALSE, digits = 6) {
    missing <- sum(is.na(x))
    x <- .clean(x, na_rm)
    if (is.null(breaks)) {
        t <- table(x)
        tab <- data.frame(value = as.numeric(names(t)), count = as.numeric(t))
        tab$relative <- tab$count/length(x)
    }
    else {
        .num(breaks, "breaks")
        if (length(breaks) < 2 || any(diff(breaks) <= 0)) 
            stop("breaks must be strictly increasing")
        groups <- cut(x, breaks, right = right, include.lowest = include_lowest)
        if (anyNA(groups)) 
            stop("Some observations are outside the specified bins")
        t <- table(groups)
        tab <- data.frame(interval = levels(groups), count = as.numeric(t), relative = as.numeric(t)/length(x), 
            density = as.numeric(t)/length(x)/diff(breaks))
    }
    tab$cumulative <- cumsum(tab$relative)
    .res("Frequency table", "DESCRIPTIVE", "Denominator is the non-missing observation count; interval endpoints must match the question.", 
        "relative_j=count_j/n; density_j=relative_j/width_j; cumulative=cumsum(relative)", 
        paste0("n=", length(x), "; NA removed=", missing), tab, paste0("sum(relative)=", 
            .fmt(sum(tab$relative))), "", 
        digits = digits)
}

exam_ecdf <- function(x, at = NULL, na_rm = FALSE, digits = 6) {
    x <- .clean(x, na_rm)
    z <- sort(unique(x))
    p <- tabulate(match(x, z), length(z))/length(x)
    if (!is.null(at)) 
        .num(at, "at", finite = FALSE)
    .res("Empirical CDF", "DESCRIPTIVE", "Finite numeric data; each observed point has mass equal to its relative frequency.", 
        "F_n(t)=sum_i I(x_i<=t)/n", c("Combine ties before writing the step function.", 
            "F_n(t)=0 below minimum; equals cumulative frequency on [z_j,z_(j+1)); equals 1 at/above maximum."), 
        list(steps = data.frame(x = z, mass = p, cdf = cumsum(p)), evaluated = if (!is.null(at)) data.frame(at = at, 
            F = ecdf(x)(at)) else NULL), "Direct indicator counts agree with ecdf().", 
        "", digits = digits)
}

exam_grouped <- function(breaks, counts, p = NULL, at = NULL, digits = 6) {
    .num(breaks, "breaks")
    .num(counts, "counts")
    if (length(breaks) != length(counts) + 1 || any(diff(breaks) <= 0) || any(counts < 
        0) || sum(counts) <= 0) 
        stop("Increasing breaks, nonnegative counts, positive total required")
    rel <- counts/sum(counts)
    cum <- c(0, cumsum(rel))
    widths <- diff(breaks)
    mids <- (head(breaks, -1) + tail(breaks, -1))/2
    qv <- NULL
    if (!is.null(p)) {
        .prob(p, "p")
        qv <- vapply(p, function(z) {
            if (z == 0) 
                return(breaks[which(rel > 0)[1]])
            if (z == 1) 
                return(breaks[max(which(rel > 0)) + 1])
            j <- which(cum[-1] >= z & rel > 0)[1]
            breaks[j] + (z - cum[j])/rel[j] * widths[j]
        }, numeric(1))
        names(qv) <- paste0("p=", p)
    }
    fv <- NULL
    if (!is.null(at)) {
        .num(at, "at", finite = FALSE)
        fv <- vapply(at, function(z) {
            if (z <= breaks[1]) 
                return(0)
            if (z >= tail(breaks, 1)) 
                return(1)
            j <- findInterval(z, breaks)
            cum[j] + (z - breaks[j])/widths[j] * rel[j]
        }, numeric(1))
    }
    m <- sum(rel * mids)
    vm <- sum(rel * (mids - m)^2)
    .res("Grouped data interpolation", "GROUPED MODEL", "For interpolation assume uniform values within each class; raw values are not recovered.", 
        c("F(x)=F(l_j)+(x-l_j)*f_j/(u_j-l_j)", "x_p=l_j+(p-F(l_j))*(u_j-l_j)/f_j"), 
        c(paste0("n=", .fmt(sum(counts)), "; relative frequencies=", .fmt(rel)), 
            "Locate the class before substituting into the interpolation formula."), 
        list(table = data.frame(lower = head(breaks, -1), upper = tail(breaks, 
            -1), count = counts, relative = rel, cumulative = cum[-1], density = rel/widths), 
            quantiles = qv, cdf = if (!is.null(at)) data.frame(at = at, F = fv) else NULL, 
            midpoint_mean = m, midpoint_variance = vm, variance_under_uniform_within_classes = vm + 
                sum(rel * widths^2/12)), "Check cumulative frequencies start at 0 and end at 1.", 
        "", "Midpoint variance ignores within-class spread. The added width^2/12 term requires the uniform-within-class model.", 
        digits)
}

exam_kde <- function(x, at, h, kernel = c("normal", "epanechnikov", "rectangular"), 
    na_rm = FALSE, digits = 6) {
    x <- .clean(x, na_rm)
    .num(at, "at")
    .positive(h, "h")
    kernel <- match.arg(kernel)
    K <- switch(kernel, normal = function(u) dnorm(u), epanechnikov = function(u) 0.75 * 
        pmax(0, 1 - u^2) * (abs(u) <= 1), rectangular = function(u) 0.5 * (abs(u) <= 
        1))
    v <- vapply(at, function(t) mean(K((x - t)/h))/h, numeric(1))
    .res("Kernel density estimate", "DESCRIPTIVE / NUMERIC EVALUATION", "h>0; symmetric normalized kernel. h is the formula's raw bandwidth, not density()'s kernel rescaling.", 
        "f_hat(t)=sum_i K((x_i-t)/h)/(n*h)", paste0("n=", length(x), "; h=", 
            h, "; kernel=", kernel), data.frame(at = at, density = v), paste0("Analytic mean under symmetric kernel = sample mean = ", 
            .fmt(mean(x))), "", "Bandwidth and kernel affect the estimated density.", 
        digits)
}

exam_lorenz <- function(x, digits = 6) {
    .num(x, "x")
    if (any(x < 0) || sum(x) <= 0) 
        stop("Nonnegative values with positive total required")
    x <- sort(x)
    n <- length(x)
    g <- sum((2 * seq_len(n) - n - 1) * x)/(n * sum(x))
    .res("Lorenz curve and Gini (chapter supplement)", "DESCRIPTIVE", "Nonnegative observations; positive total. Equal observational weights.", 
        "G=sum_i(2*i-n-1)*x_(i)/(n*sum(x)); standardized G=n*G/(n-1)", "Sort values, accumulate population shares and wealth shares, then calculate area inequality.", 
        list(curve = data.frame(population_share = (0:n)/n, wealth_share = c(0, 
            cumsum(x)/sum(x))), Gini = g, standardized_Gini = if (n > 1) n * 
            g/(n - 1) else NA_real_), "Lorenz endpoints (0,0),(1,1); G is in [0,(n-1)/n].", 
        "", digits = digits)
}

exam_pool <- function(sizes, means, variances = NULL, digits = 6) {
    .num(sizes, "sizes")
    .num(means, "means")
    if (length(sizes) != length(means) || any(sizes <= 0)) 
        stop("Positive sizes matching means required")
    m <- sum(sizes * means)/sum(sizes)
    value <- c(mean = m)
    if (!is.null(variances)) {
        .num(variances, "variances")
        if (length(variances) != length(sizes) || any(variances < 0)) 
            stop("Matching nonnegative within-group variances (denominator n_j) required")
        value <- c(value, variance_n = sum(sizes * (variances + (means - m)^2))/sum(sizes))
    }
    .res("Pooled descriptive measures", "DESCRIPTIVE", "Disjoint groups; within-group variances use denominator n_j.", 
        c("pooled_mean=sum(n_j*mean_j)/sum(n_j)", "pooled_variance_n=sum(n_j*(variance_j+(mean_j-pooled_mean)^2))/sum(n_j)"), 
        c(paste0("Total n=", .fmt(sum(sizes))), paste0("Weighted mean numerator=", 
            .fmt(sum(sizes * means)))), value, "Within-group spread plus between-group spread.", 
        "", digits = digits)
}

exam_discrete <- function(x, p, transform = function(z) z, digits = 6) {
    .num(x, "x")
    .prob(p, "p")
    if (length(x) != length(p) || abs(sum(p) - 1) > 1e-10) 
        stop("p must match x and sum to 1. Invalid PMFs are never silently normalized.")
    y <- transform(x)
    .num(y, "transform(x)")
    if (length(y) != length(x)) 
        stop("Transform must return one value per outcome")
    m <- sum(y * p)
    e2 <- sum(y^2 * p)
    v <- sum((y - m)^2 * p)
    .res("Discrete expectation and variance", "FINITE-SUM CALCULATION", "Outcomes with probabilities summing to 1; transformed values must be finite.", 
        "E[g(X)]=sum g(x_i)*p_i; Var[g(X)]=sum (g(x_i)-E[g(X)])^2*p_i=E[g(X)^2]-E[g(X)]^2", 
        c(paste0("E=sum(y*p)=", .fmt(m)), paste0("E(y^2)=", .fmt(e2))), list(table = data.frame(x = x, 
            p = p, y = y, y_p = y * p, y2_p = y^2 * p), mean = m, second_moment = e2, 
            variance = v, sd = sqrt(v)), paste0("Second-moment variance=", .fmt(e2 - 
            m^2), "; centered-sum variance=", .fmt(v)), "", 
        "Probabilities must sum to one.", 
        digits)
}

exam_affine <- function(mu, variance, a = 0, b = 1, digits = 6) {
    for (nm in c("mu", "variance", "a", "b")) .num(get(nm), nm, TRUE)
    if (variance < 0) 
        stop("variance must be nonnegative")
    .res("Affine transformation Y=a+bX", "EXACT FORMULA / NUMERIC EVALUATION", 
        "E(X) and finite Var(X) exist; a,b are constants.", c("E(a+bX)=a+b*mu", 
            "Var(a+bX)=b^2*Var(X); sd(Y)=abs(b)*sd(X)"), c(paste0("E(Y)=", a, 
            " + (", b, ")*", mu), paste0("Var(Y)=(", b, ")^2*", variance)), c(mean = a + 
            b * mu, variance = b^2 * variance, sd = abs(b) * sqrt(variance)), 
        "Cross-check: adding a constant leaves variance unchanged.", "", 
        digits = digits)
}

exam_integral <- function(f, lower, upper, digits = 6) {
    if (!is.function(f)) 
        stop("f must be a vectorized R function")
    .num(lower, "lower", TRUE, FALSE)
    .num(upper, "upper", TRUE, FALSE)
    z <- integrate(f, lower, upper, rel.tol = 1e-09, subdivisions = 1000)
    .res("Definite integral", "NUMERICAL INTEGRATION", "Integrable vectorized function; split discontinuities/singularities into separate calls where needed.", 
        "I=integral_lower^upper f(x) dx", paste0("Bounds: ", lower, " to ", upper), 
        c(estimate = z$value, estimated_abs_error = z$abs.error), z$message, 
        "", "No antiderivative or proof is produced. Error is an algorithmic estimate, not an exact symbolic guarantee.", 
        digits)
}

exam_continuous <- function(f, lower, upper, interval = NULL, digits = 6) {
    if (!is.function(f)) 
        stop("f must be a vectorized R function")
    .num(lower, "lower", TRUE, FALSE)
    .num(upper, "upper", TRUE, FALSE)
    if (lower >= upper) 
        stop("Require lower<upper")
    if (all(is.finite(c(lower,upper)))) {
        probe <- seq(lower,upper,length.out=1027)[-c(1,1027)]
        samples <- f(probe)
        if (length(samples)!=length(probe) || anyNA(samples) || any(samples < -1e-12))
            stop('Invalid density: negative values or invalid vectorization on support grid')
    }
    int <- function(g) integrate(g, lower, upper, rel.tol = 1e-09, subdivisions = 1000)
    mass <- int(f)
    if (abs(mass$value - 1) > 1e-07) 
        stop("Density integral is not 1 within tolerance; provide a normalized density")
    m <- int(function(x) x * f(x))
    e2 <- int(function(x) x^2 * f(x))
    v <- int(function(x) (x - m$value)^2 * f(x))
    if (!is.finite(v$value) || v$value < -1e-10) stop('Invalid density: negative or nonfinite variance')
    if (v$value < 0) v$value <- 0
    val <- c(mass = mass$value, mean = m$value, second_moment = e2$value, variance = v$value, 
        sd = sqrt(v$value))
    errors <- c(mass$abs.error, m$abs.error, e2$abs.error, v$abs.error)
    if (!is.null(interval)) {
        .num(interval, "interval", finite = FALSE)
        if (length(interval) != 2 || interval[1] > interval[2]) 
            stop("interval must have ordered lower,upper")
        lo <- max(lower, interval[1])
        hi <- min(upper, interval[2])
        val <- c(val, probability = if (lo >= hi) 0 else integrate(f, lo, hi, rel.tol = 1e-09)$value)
        if (!is.finite(val['probability']) || val['probability'] < -1e-10 || val['probability'] > 1+1e-10) stop('Invalid density: probability outside [0,1]')
    }
    .res("Continuous density moments", "NUMERICAL INTEGRATION", "User must establish f(x)>=0 on the full support and existence of moments; normalization is checked numerically only.", 
        "E(X)=integral x*f(x); Var(X)=integral (x-E(X))^2*f(x)=E(X^2)-E(X)^2", 
        c(paste0("Support: [", lower, ", ", upper, "]"), paste0("Numerical mass=", 
            .fmt(mass$value))), val, c(paste0("Centered variance minus second-moment formula = ", 
            .fmt(v$value - e2$value + m$value^2)), paste0("Estimated integral errors = ", 
            .fmt(errors))), "", 
        "Numerical normalization does not prove nonnegativity, integrability, or an analytic moment identity.", 
        digits)
}

exam_double <- function(f, xlower, xupper, ylower, yupper, digits = 6) {
    if (!is.function(f)) 
        stop("f must be function(x,y), vectorized in y")
    .num(xlower, "xlower", TRUE, FALSE)
    .num(xupper, "xupper", TRUE, FALSE)
    if (xlower > xupper) 
        stop("Require xlower<=xupper")
    for (z in list(ylower, yupper)) if (!is.function(z)) 
        .num(z, "y bound", TRUE, FALSE)
    low <- if (is.function(ylower)) 
        ylower
    else function(x) ylower
    high <- if (is.function(yupper)) 
        yupper
    else function(x) yupper
    errs <- numeric(0)
    outer_f <- function(xs) vapply(xs, function(x) {
        l <- low(x)
        u <- high(x)
        .num(l, "ylower(x)", TRUE, FALSE)
        .num(u, "yupper(x)", TRUE, FALSE)
        if (l > u) 
            stop("At an evaluated x, ylower(x)>yupper(x)")
        z <- integrate(function(y) f(x, y), l, u, rel.tol = 1e-09, subdivisions = 1000)
        errs <<- c(errs, z$abs.error)
        z$value
    }, numeric(1))
    z <- integrate(outer_f, xlower, xupper, rel.tol = 1e-08, subdivisions = 1000)
    .res("Double integral over an explicit region", "NUMERICAL INTEGRATION", 
        "Set up and justify the region first. Variable inner bounds must not be reversed.", 
        "integral_xlower^xupper [integral_ylower(x)^yupper(x) f(x,y) dy] dx", 
        paste0("Outer x bounds: ", xlower, " to ", xupper), c(estimate = z$value, 
            outer_estimated_abs_error = z$abs.error, max_inner_estimated_abs_error = max(c(0, 
                errs))), "Inner and outer error estimates are reported separately; neither is a rigorous total double-integral error bound.", 
        "", "Numeric integration is not an analytic derivation.", 
        digits)
}

exam_joint <- function(x, y, p, digits = 6) {
    .num(x, "x")
    .num(y, "y")
    if (!is.matrix(p) || !is.numeric(p) || anyNA(p) || any(!is.finite(p)) || 
        any(dim(p) != c(length(x), length(y))) || any(p < 0) || abs(sum(p) - 
        1) > 1e-10) 
        stop("p must be a nonnegative matrix with x rows, y columns, total 1")
    if (anyDuplicated(x) || anyDuplicated(y)) 
        stop("x and y support values must be distinct")
    px <- rowSums(p)
    py <- colSums(p)
    mx <- sum(x * px)
    my <- sum(y * py)
    vx <- sum((x - mx)^2 * px)
    vy <- sum((y - my)^2 * py)
    cv <- sum(outer(x - mx, y - my) * p)
    rho <- if (vx > 0 && vy > 0) 
        cv/sqrt(vx * vy)
    else NA_real_
    y_given_x <- sweep(p, 1, ifelse(px > 0, px, NA_real_), "/")
    x_given_y <- sweep(p, 2, ifelse(py > 0, py, NA_real_), "/")
    .res("Joint discrete distribution", "FINITE-SUM CALCULATION", "Rows correspond to x, columns to y; matrix sums to one.", 
        c("p_X=rowSums(p); p_Y=colSums(p); p(Y=y|X=x)=p(x,y)/p_X(x)", "Cov(X,Y)=sum (x-mu_X)*(y-mu_Y)*p(x,y); rho=Cov/sqrt(VarX*VarY)"), 
        c(paste0("Marginal X=", .fmt(px)), paste0("Marginal Y=", .fmt(py))), 
        list(p_X = px, p_Y = py, Y_given_X = y_given_x, X_given_Y = x_given_y, 
            mean_X = mx, mean_Y = my, variance_X = vx, variance_Y = vy, covariance = cv, 
            correlation = rho, independent_to_tolerance = max(abs(p - outer(px, 
                py))) < 1e-10), paste0("E(XY)-E(X)E(Y)=", .fmt(sum(outer(x, y) * 
            p) - mx * my)), "", 
        "Zero marginal: that conditional is undefined (NA). Zero variance: correlation is undefined. Zero covariance does not imply independence.", 
        digits)
}

.covcheck <- function(V, n, positive_definite = FALSE) {
    if (!is.matrix(V) || !is.numeric(V) || any(dim(V) != c(n, n)) || anyNA(V) || 
        any(!is.finite(V)) || max(abs(V - t(V))) > 1e-10) 
        stop("Finite symmetric covariance matrix with matching dimension required")
    e <- eigen(V, symmetric = TRUE, only.values = TRUE)$values
    tol <- 1e-10 * max(1, max(abs(e)))
    if (any(e < -tol) || (positive_definite && any(e <= tol))) 
        stop("Covariance matrix must be ", if (positive_definite) 
            "positive definite"
        else "positive semidefinite")
}

exam_portfolio <- function(mu, V, weights, digits = 6) {
    .num(mu, "mu")
    .num(weights, "weights")
    if (length(mu) != length(weights) || abs(sum(weights) - 1) > 1e-10) 
        stop("Matching weights summing to 1 required")
    .covcheck(V, length(mu))
    m <- sum(weights * mu)
    v <- as.numeric(crossprod(weights, V %*% weights))
    if (v < 0 && v > -1e-10) 
        v <- 0
    .res("Portfolio linear combination", "EXACT FORMULA / NUMERIC EVALUATION", 
        "Finite means and covariance matrix; returns use consistent units; weights sum to one.", 
        "E(w'X)=w'mu; Var(w'X)=w'Vw; for two assets = a^2*VarX+b^2*VarY+2*a*b*CovXY", 
        c(paste0("weights=", .fmt(weights)), paste0("means=", .fmt(mu))), c(mean = m, 
            variance = v, sd = sqrt(v)), paste0("Elementwise covariance sum = ", 
            .fmt(sum(outer(weights, weights) * V))), "", digits = digits)
}

exam_chebyshev <- function(mu, variance, k = NULL, coverage = NULL, digits = 6) {
    .num(mu, "mu", TRUE)
    .positive(variance, "variance")
    if (is.null(k) == is.null(coverage)) 
        stop("Supply exactly one of k or coverage")
    if (!is.null(coverage)) {
        .num(coverage, "coverage", TRUE)
        if (coverage <= 0 || coverage >= 1) 
            stop("Require 0<coverage<1")
        k <- 1/sqrt(1 - coverage)
    }
    .positive(k, "k")
    r <- k * sqrt(variance)
    .res("Chebyshev bound", "PROBABILITY BOUND", "Finite variance>0; no normality assumption; k>0.", 
        "P(|X-mu|>=k*sigma)<=1/k^2; P(|X-mu|<k*sigma)>=1-1/k^2", c(paste0("sigma=", 
            .fmt(sqrt(variance)), "; k=", .fmt(k), "; radius=", .fmt(r)), "Clip bounds to [0,1] when k<1."), 
        c(lower = mu - r, upper = mu + r, inside_probability_at_least = max(0, 
            1 - 1/k^2), outside_probability_at_most = min(1, 1/k^2)), "The bound gives an upper limit for the event probability.", 
        "", digits = digits)
}

exam_mahalanobis <- function(mu, V, t, x = NULL, digits = 6) {
    .num(mu, "mu")
    .positive(t, "t")
    .covcheck(V, length(mu), TRUE)
    n <- length(mu)
    value <- c(dimension = n, threshold_squared = t^2, tail_probability_at_most = min(1, 
        n/t^2), inside_probability_at_least = max(0, 1 - n/t^2))
    if (!is.null(x)) {
        .num(x, "x")
        if (length(x) != n) 
            stop("x length must match mu")
        value <- c(value, distance_squared = as.numeric(crossprod(x - mu, solve(V, 
            x - mu))))
    }
    .res("Multidimensional Chebyshev", "PROBABILITY BOUND", "Finite covariance V is positive definite; dimension n; t>0.", 
        "P(sqrt((X-mu)' V^(-1) (X-mu))>t)<=n/t^2", paste0("n=", n, "; t=", t), 
        value, "E[(X-mu)'V^(-1)(X-mu)]=trace(V^(-1)V)=n; see the proof template.", 
        "", digits = digits)
}

exam_clt <- function(n, mu, variance, lower = -Inf, upper = Inf, quantity = c("sum", 
    "mean"), normal_population = FALSE, digits = 6) {
    .int(n, "n", 1)
    .num(mu, "mu", TRUE)
    .positive(variance, "variance")
    .num(lower, "lower", TRUE, FALSE)
    .num(upper, "upper", TRUE, FALSE)
    if (lower > upper) 
        stop("Require lower<=upper")
    if (!is.logical(normal_population) || length(normal_population) != 1 || is.na(normal_population)) 
        stop("normal_population must be TRUE/FALSE")
    quantity <- match.arg(quantity)
    m <- if (quantity == "sum") 
        n * mu
    else mu
    v <- if (quantity == "sum") 
        n * variance
    else variance/n
    zl <- (lower - m)/sqrt(v)
    zu <- (upper - m)/sqrt(v)
    value <- if (zl == zu) 
        0
    else if (zl > 0) 
        pnorm(zl, lower.tail = FALSE) - pnorm(zu, lower.tail = FALSE)
    else pnorm(zu) - pnorm(zl)
    .res(paste("Distribution of", quantity), if (normal_population) 
        "EXACT NORMAL MODEL"
    else "CLT APPROXIMATION", "Independent identically distributed X_i with finite mean and positive finite variance. For exact normal statement, each X_i must be normal.", 
        c("S_n: mean=n*mu, variance=n*variance; X_bar: mean=mu, variance=variance/n", 
            "Z=(quantity-its_mean)/its_sd"), c(paste0("mean=", .fmt(m), "; variance=", 
            .fmt(v), "; sd=", .fmt(sqrt(v))), paste0("z_lower=", .fmt(zl), "; z_upper=", 
            .fmt(zu))), c(probability = value, model_mean = m, model_variance = v, 
            z_lower = zl, z_upper = zu), "Cross-check by standardizing the sum and the sample mean; same event gives the same z values.", 
        "", if (normal_population) 
            "Normal sums are normal for every n."
        else "No universal n cutoff guarantees accuracy. Finite-sample approximation error is not determined from mean and variance alone.", 
        digits)
}

exam_normal_interval <- function(mu, variance, level = 0.95, digits = 6) {
    .num(mu, "mu", TRUE)
    .positive(variance, "variance")
    .num(level, "level", TRUE)
    if (level <= 0 || level >= 1) 
        stop("Require 0<level<1")
    z <- qnorm((1 + level)/2)
    ends <- mu + c(-1, 1) * z * sqrt(variance)
    .res("Central probability interval for a normal RANDOM VARIABLE", "EXACT NORMAL MODEL", 
        "X~N(mu,variance), both parameters given.", "P(mu-z_(1-alpha/2)*sigma <= X <= mu+z_(1-alpha/2)*sigma)=1-alpha", 
        c(paste0("alpha=", .fmt(1 - level), "; z=", .fmt(z)), "Multiply by sigma, not the variance."), 
        setNames(ends, c("lower", "upper")), paste0("CDF interval probability=", 
            .fmt(pnorm(ends[2], mu, sqrt(variance)) - pnorm(ends[1], mu, sqrt(variance)))), 
        "", "This interval covers an observation from the specified normal distribution.", 
        digits)
}

