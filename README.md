# R Statistics Workbench v1.2

An offline, base R workbench for probability calculations, descriptive statistics,
and finding references in a local question bank. The public edition comes with
five fictional questions and worked solutions.

| Task | Example |
| --- | --- |
| Calculate an exact binomial probability | `exam_calc("C(10,3)*0.4^3*0.6^7")` shows **0.2150** to four decimal places |
| Find a related worked question | Changing the demo's 12 orders to 20 finds `DEMO-01` with the label `changed numbers` |
| Review the model and inputs | `exam_detail(1)` shows the question and highlights numerical and textual differences |

## Quick start

Install R, open this folder in R or RStudio, and run:

```r
source("workbench.R")
exam_menu()
```

From a terminal in this folder, run `Rscript demo.R` for a walkthrough or
`Rscript check.R` for the regression checks. The workbench uses the packages
that ship with R.

## Calculate

```r
exam_calc("C(10,3)*0.4^3*0.6^7")
exam_calc("integral(integral(2,y,0,x),x,0,1)")
exam_calc("invPhi(0.95)")

result <- exam_prob("binomial", "ge", q = 3, n = 12, p = 0.10)
print(result)
```

The calculator shows the translated R expression and the result. Run
`exam_calc()` for interactive variables, `ans`, and local history.
The [calculator rules](guide/calculator-rules.md) list supported syntax and
arguments. The structured solvers include `exam_prob()`, `exam_bayes()`,
`exam_test()`, `exam_describe()`, `exam_quantile()`, and `exam_clt()`.

## Search and inspect

```r
query <- paste(
  "A warehouse receives 20 independent orders.",
  "Each order has a 10% chance of arriving late.",
  "Under a binomial model, what is the probability that at least 3 orders arrive late?"
)
exam_find(query, top_n = 3)
exam_detail(1)
exam_topic()
exam_answer(1)
```

`exam_go()` accepts pasted text. Enter `:end` on its own line to search.
Search combines TF-IDF cosine and weighted term containment. The workbench
also compares question structure, number order, and repeated numbers before
assigning a label. Scores rank references. Inspect the question and choose the
model before calculating.

Existing scripts can still call `exam_search(query, top_k = 3)`,
`exam_question("DEMO-01")`, and `exam_solution("DEMO-01")`. Search scores
and labels reflect the v1.2 engine.

## Use a personal bank

Keep your files outside the repository or in the ignored `private-bank/` folder.
Call `exam_bank_load("path/to/your/bank")` with a folder containing
`questions.csv` and `solutions.csv`, or the equivalent RDS pair. Use the
[demo headers](demo-bank/questions.csv) and share IDs between the two tables.
Call `exam_bank_load()` to restore the fictional bank.

Optional `question_pdf` and `answer_pdf` columns hold paths relative to the
loaded bank folder. Add standalone local PDFs to `private-bank/materials/`
to open them through menu option 5. See the [workbench guide](guide/workbench.md)
for commands.

The public edition contains the workbench, fictional examples, and English
documentation. Course PDFs, private question banks, and local history are
kept in the personal edition.
