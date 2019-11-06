library(arules)
library(arulesViz)
library(datasets)

data("Groceries")
inspect(Groceries[1 : 20])

itemFrequencyPlot(Groceries, topN = 10, type = "absolute")

rules = apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))
rules = sort(rules, by = "confidence")
options(digits = 2)

inspect(rules[is.redundant(rules)])
rules = rules[!is.redundant(rules)]
inspect(rules[1 : 5])

rules = apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8, maxlen = 4), 
                appearance = list(default = "lhs", rhs = "yogurt"))
inspect(rules)

plot(rules, method = "graph", engine = "interactive")
plot(rules, method = "paracoord")
plot(rules, method = "matrix", control = list(reorder = "none"))
arulesViz :: plotly_arules(rules)
