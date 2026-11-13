# Workbench guide

Run source("workbench.R") from the repository root, then exam_menu().
Use option 1 to paste question text. Enter :end on its own line to search.
Use exam_find(query, top_n = 3) to run the same search directly.

Review exam_detail(1) before using a result. The original and changed-numbers
labels require matching question structure. A fragment, extra text, or numbers
in different roles can receive a lower label. Use exam_topic() for hints on
the last query. A similarity score ranks references, so check the model and
inputs before calculating.

Use option 4 or exam_calc() for mathematical expressions. Use option 7 for the
calculator rules. Calculation history is saved locally in history/.
Use option 8 or exam_check() to check search and calculator behavior offline.

Option 5 opens locally supplied PDFs. Place PDF files you can use in
private-bank/materials/ and use the number shown in the materials menu.
The fictional demo bank provides question and solution text directly in R.
