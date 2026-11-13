# Calculator rules

| Input | Meaning | R equivalent | Example |
|---|---|---|---|
| `+ - * /` | Arithmetic | Same | `3/7` → 0.4285714286 |
| `× ÷ ·` | Multiplication or division | `* / *` | `3×4÷2` → 6 |
| `^` or `**` | Power | `^` | `6^2` → 36 |
| `( )` | Grouping | `( )` | `2(3+4)` → 14 |
| `,` | Argument separator | `,` | `C(10,3)` → 120 |
| `2%` | Percentage | `2/100` | `0.95*2%` → 0.019 |
| `sqrt(x)` or `√(x)` | Square root | `sqrt(x)` | `sqrt(16)` → 4 |
| `e` | Euler's number | `exp(1)` | `e^2` → 7.389056099 |
| `exp(x)` | Exponential | `exp(x)` | `exp(-2)` → 0.1353352832 |
| `pi` or `π` | Circle constant | `pi` | `pi` → 3.141592654 |
| `ln(x)` | Natural logarithm | `log(x)` | `ln(e)` → 1 |
| `log(x)` | Natural logarithm | `log(x)` | `log(e)` → 1 |
| `log10(x), log(x,b)` | Base 10 or base b logarithm | Same | `log(8,2)` → 3 |
| `n!` | Factorial | `factorial(n)` | `5!` → 120 |
| `C(n,k)` | Combinations | `choose(n,k)` | `C(10,3)` → 120 |
| `P(n,k)` | Permutations | Product of k descending factors | `P(365,2)` → 132860 |
| `abs(x)` | Absolute value | `abs(x)` | `abs(-4)` → 4 |
| `Phi(z)` | Standard normal cumulative probability | `pnorm(z)` | `Phi(1.96)` → 0.9750021049 |
| `invPhi(p)` | Standard normal quantile | `qnorm(p)` | `invPhi(0.95)` → 1.644853627 |
| `sum(expr,k,a,b)` | Sum over integer k from a to b | Sum over local k | `sum(k^2,k,1,3)` → 14 |
| `sum(x,y,...)` | Ordinary addition | `sum(...)` | `sum(0.1,0.2,0.7)` → 1 |
| `integral(expr,x,a,b)` | Definite integral | `integrate(...)` | `integral(2*x,x,0,1)` → 1 |
| `inf, -inf, ∞` | Infinite bounds | `Inf, -Inf` | `integral(2*exp(-2*x),x,0,inf)` → 1 |
| Nested integral | Inner bounds may use the outer variable | Nested numerical integration | `integral(integral(2,y,0,x),x,0,1)` → 1 |
| `d(expr,x)` | Symbolic derivative | `D(...)` | `d(x^3,x)` → `3 * x^2` |
| `d(expr,x,value)` | Derivative evaluated at a value | Differentiate, then substitute | `d(x^3,x,2)` → 12 |
| `a = 3` or `a <- 3` | Store a session variable | Calculator-local assignment | `a=3; a*2` → 6 |
| `ans` | Previous numeric result | Calculator-local value | After `6`, enter `ans/2` → 3 |
| `;` | Separate expressions | Evaluate in sequence | `a=2; a^3` → 8 |
| `pnorm(x, mean, sd)` | Normal cumulative probability | Same | `pnorm(10,mean=10,sd=2)` → 0.5 |
| `qnorm(p, mean, sd)` | Normal quantile | Same | `qnorm(0.5,10,2)` → 10 |
| `dbinom(k,n,p)` | Binomial probability at k | Same | `dbinom(1,2,0.5)` → 0.5 |
| `pbinom(k,n,p)` | Binomial cumulative probability | Same | `pbinom(0,size=2,prob=0.5)` → 0.25 |
| `dpois(k,lambda)` | Poisson probability at k | Same | `dpois(0,1)` → 0.3678794412 |
| `ppois(k,lambda)` | Poisson cumulative probability | Same | `ppois(0,lambda=1)` → 0.3678794412 |
| `pexp(x,rate)` | Exponential cumulative probability | Same | `pexp(0,2)` → 0 |
| `qexp(p,rate)` | Exponential quantile | Same | `qexp(0.5,rate=2)` → 0.3465735903 |
| `exam_binom(...)` and related helpers | Named calculation helpers | Calls the selected helper | `exam_binom(12,0.1,2,type="le")` → 0.8891 |

Run `exam_calc()`. Enter `help`, `history`, `:digits 3`, or `q` to view the rules,
show today's history, set decimal places, or exit. Single calculations also
accept a quoted expression, for example `exam_calc("3/7", digits = 4)`.
Results retain their full working precision.

- Write multiplication by a variable as `2*x`. Write `1000` without a separator.
- `sum` uses summation only when it has four arguments and its second argument
  is a variable that occurs in the first. The local variable shadows any stored
  variable with the same name.
- Named distribution arguments are limited to the applicable `mean`, `sd`,
  `size`, `prob`, `lambda`, and `rate`. To calculate an upper tail, use an
  expression such as `1-pnorm(z)`.
- The other named helpers accepted by the calculator are `exam_pois`,
  `exam_norm`, `exam_exp`, `exam_exp_cond`, `exam_bayes`, `exam_comb`, and
  `exam_mean_var`. In the calculator, `exam_bayes(prior,sens,fpr)` models a
  diagnostic test. In the R console, use `exam_test(prior,sens,fpr)` for that
  model or `exam_bayes(prior,likelihood)` for general Bayes calculations.
