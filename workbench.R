# Run source("workbench.R") from the repository root.
.workbench_root <- normalizePath(getwd(), mustWork = TRUE)
source("R/core.R")
source("R/verification.R")
source("R/numeric-output.R")

# The v1.2 calculator uses its own helpers, including a separate Bayes signature.
.exam_v12 <- new.env(parent = globalenv())
.exam_v12$WORKBENCH_ROOT <- .workbench_root
.exam_v12$BANK_ROOT <- .workbench_root
sys.source("R/v12/search_bank.R", envir = .exam_v12)
sys.source("R/v12/workbench.R", envir = .exam_v12)
sys.source("R/v12/check.R", envir = .exam_v12)
for (.exam_api in c("exam_calc", "exam_history", "exam_rules", "exam_find",
                   "exam_home", "exam_results", "exam_detail", "exam_paste",
                   "exam_go", "exam_topic", "exam_open", "exam_answer",
                   "exam_pdf", "exam_materials", "exam_guide", "exam_menu", "exam_check"))
  assign(.exam_api, .exam_v12[[.exam_api]])
rm(.exam_api)

.exam_bank <- new.env(parent = emptyenv())
source("R/question-bank.R", encoding = "UTF-8")
exam_bank_load()
