# https://towardsdatascience.com/association-rule-mining-in-r-ddf2d044ae50

data(Groceries)

class(Groceries)

inspect(head(Groceries, 2))

# Support is an indication of how frequently the itemset appears in the dataset. 
# Consider only the two transactions from the above output. The support of the 
# item citrus fruit is 1/2 as it appears in only 1 out of the two transactions.
# 
# Confidence is an indication of how often the rule has been found to be true. 
# We will discuss more about confidence after generating the rules.
grocery_rules <- apriori(Groceries, 
                         parameter = list(support = 0.01, confidence = 0.5))
summary(grocery_rules)

inspect(head(sort(grocery_rules, by = "confidence"), 3))


# Limiting the number of rules generated
# 
wholemilk_rules <- apriori(data=Groceries, 
                           parameter=list (supp=0.001,conf = 0.08), 
                           appearance = list (rhs="whole milk"))
# The above code shows what products are bought before buying "whole milk" and 
# will generate rules that lead to buying "whole milk".

grocery_rules_increased_support <- apriori(Groceries, 
                                           parameter = list(support = 0.02, 
                                                            confidence = 0.5))
# This generates only one rule in the output.

subsets <- which(colSums(is.subset(grocery_rules, groery_rules)) > 1)
grocery_rules <- grocery_rules[-subsets]

# Converting Data Frame into Transactional data

data(AdultUCI)
class(AdultUCI)

str(AdultUCI)

# AdultUCI <- lapply(AdultUCI, function(x){as.factor(x)})
AdultUCI <- AdultUCI %>% mutate_if(~!is.factor(.x), factor)
transactional_data <- as(AdultUCI, "transactions")

rules <- apriori(AdultUCI)
summary(rules)

inspect(rules[1:10])
