# Run source("workbench.R") from the repository root.
source("R/core.R")
source("R/verification.R")
source("R/numeric-output.R")
.exam_bank <- new.env(parent = emptyenv())
source("R/question-bank.R", encoding = "UTF-8")
