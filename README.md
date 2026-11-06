# R Statistics Workbench

An offline, base R toolkit for working through probability and descriptive statistics. Results show the model conditions, formula, calculation steps, and, where implemented, a numerical cross-check.

## Example run

| Task | Example result |
| --- | --- |
| Calculate the chance of at least 3 late orders out of 12 at a 10% late rate | **0.110870**, with the model assumptions and calculation shown |
| Search the same question with 20 orders | **DEMO-01**, similarity **0.97**, labeled `changed numbers` |
| Look up `DEMO-01` | The question text, its location in the demo bank, and a worked solution |

The workflow is to find a reference, inspect the model and inputs, and calculate using the current question's parameters.

## Quick start

Install R and open a terminal in this folder. Run:

```sh
Rscript demo.R
Rscript check.R
```

Or run the following in an R console with this folder as the working directory:

```r
source("workbench.R")

# At least 3 late orders out of 12, assuming independent 10% late probabilities.
result <- exam_prob("binomial", "ge", q = 3, n = 12, p = 0.10)
print(result)
result$value
# Approximately 0.110870
```

The example uses fictional order data. The project runs offline with the packages that ship with R. Verified on R 4.6.1 on Windows.

## What you can calculate

| Task | Examples of functions |
| --- | --- |
| Probabilities and event endpoints | `exam_prob()`, `exam_multinomial()` |
| Conditional probabilities | `exam_bayes()`, `exam_test()` |
| Descriptive statistics | `exam_describe()`, `exam_quantile()`, `exam_frequency()` |
| Empirical distributions and density estimates | `exam_ecdf()`, `exam_kde()` |
| Moments and normal approximations | `exam_discrete()`, `exam_continuous()`, `exam_clt()` |

`R/core.R` implements the calculations. `R/verification.R` adds input checks and alternative calculations for selected results. `R/numeric-output.R` handles display precision and normal-tail underflow.

## Search the question bank

```r
source("workbench.R")

query <- paste(
  "A warehouse receives 20 independent orders.",
  "Each order has a 10% chance of arriving late.",
  "Under a binomial model, what is the probability that at least 3 orders arrive late?"
)
exam_search(query, top_k = 3)
exam_question("DEMO-01")
exam_solution("DEMO-01")
```

The top result is `DEMO-01` with a score of 0.97 and the label `changed numbers`. The result points to the matching record in `demo-bank/questions.csv` and shows that the order count changed from 12 to 20. Search combines text overlap, character trigrams, and topic keywords. Its scores rank references and do not measure the probability of a correct answer. Calculation still requires the model and parameters for the current question.

### Load a personal question bank locally

Call `exam_bank_load("path/to/your/data")` with the folder that contains both `questions.rds` and `solutions.rds`, or both `questions.csv` and `solutions.csv`. Existing R Statistics Exam Solver v0.3.0 tables use the supported schema. For a new bank, copy the column headers from the demo tables and use the same ID in both files to link a question to its solution. The location fields hold the source filename and page or record ID. To restore the demo bank, call `exam_bank_load()`.

On Windows, if R cannot read a folder with non-ASCII characters, start R with a UTF-8 locale or use a local folder whose path contains only ASCII characters.

Keep personal question banks outside the repository. The repository ignores `private-bank/` and local RDS files.


## Assumptions and limits

Choose the statistical model and event before running a calculation. Cross-checks test the arithmetic under those inputs. Quantiles use R's type 2 convention. The descriptive summary reports both variance denominators, `n` and `n - 1`. Numerical integration and normal approximations can introduce error.

This portfolio edition extracts the reusable calculation code from a personal offline study tool. It contains self-contained examples and excludes the original course question bank, course documents, and study records.

---

中文速用说明：把 R 的工作目录设为此文件夹，运行 `source("demo.R")` 查看虚构的订单延迟案例，运行 `source("check.R")` 检查示例结果。日常调用可先运行 `source("workbench.R")`。
