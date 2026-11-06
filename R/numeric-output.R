.exam_display <- function(x, digits=6) {
  out <- vapply(x, function(v) {
    if (!is.finite(v)) return(as.character(v))
    if (v != 0 && (abs(v) < 10^-digits || abs(v) >= 1e10))
      format(v, scientific=TRUE, digits=max(4,digits), trim=TRUE)
    else format(round(v,digits), scientific=FALSE, trim=TRUE)
  }, character(1))
  names(out) <- names(x); out
}
.exam_logdiff <- function(a,b) {
  if (is.infinite(b) && b < 0) return(a)
  if (a == b) return(-Inf)
  a + log(-expm1(b-a))
}
.exam_log_probability <- function(a, value) {
  if (value > 0) return(log(value))
  if (a$dist != 'normal') return(NA_real_)
  if (a$relation == 'eq') return(-Inf)
  bounds <- .prob_event(a$relation,a$q,a$lower,a$upper,
                       a$lower_closed,a$upper_closed,FALSE)
  if (bounds[1] >= bounds[2]) return(-Inf)
  sigma <- if (is.null(a$sd)) sqrt(a$variance) else a$sd
  z <- (bounds-a$mu)/sigma
  if (z[1] > 0) .exam_logdiff(pnorm(z[1],lower.tail=FALSE,log.p=TRUE),
                            pnorm(z[2],lower.tail=FALSE,log.p=TRUE))
  else .exam_logdiff(pnorm(z[2],log.p=TRUE),pnorm(z[1],log.p=TRUE))
}
.v3_prob <- exam_prob
exam_prob <- function() {
  a <- as.list(environment())
  a$relation <- match.arg(a$relation,c('le','lt','eq','ge','gt','between'))
  if(missing(variance) && !is.null(a$sd)) a$variance <- NULL
  answer <- do.call(.v3_prob,a)
  answer$log_probability <- .exam_log_probability(a,answer$value)
  answer$numeric_status <- if (answer$value > 0) 'positive' else
    if (is.finite(answer$log_probability)) 'underflow' else
    if (is.infinite(answer$log_probability)) 'exact_zero' else 'zero_or_underflow'
  answer$validation <- list(result_valid=TRUE, parameters_confirmed=FALSE)
  answer$note <- c(answer$note,'Arithmetic agreement does not confirm question interpretation or model assumptions.')
  if (answer$numeric_status == 'underflow') {
    answer$verification$passed <- FALSE
    answer$verification$status <- 'UNDERFLOW'
    answer$check <- paste('UNDERFLOW: nonzero probability is below numeric range; log_probability=',
                         format(answer$log_probability,digits=15))
    answer$note <- c(answer$note,answer$check)
  }
  answer
}
formals(exam_prob) <- formals(.v3_prob)
