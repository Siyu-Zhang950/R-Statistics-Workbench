source("workbench.R")

# Fictional example: 12 independent orders each have a 10% chance of arriving late.
late_orders <- exam_prob("binomial", "ge", q = 3, n = 12, p = 0.10)
print(late_orders)

# Five fictional picking times, in minutes.
picking_times <- exam_describe(c(8, 10, 10, 12, 15))
print(picking_times)

query <- paste(
    "A warehouse receives 20 independent orders.",
    "Each order has a 10% chance of arriving late.",
    "Under a binomial model, what is the probability that at least 3 orders arrive late?"
)
print(exam_search(query, top_k = 3))
exam_question("DEMO-01")
exam_solution("DEMO-01")


# Natural notation for an exact binomial probability.
exam_calc("C(10,3)*0.4^3*0.6^7", digits = 4)
