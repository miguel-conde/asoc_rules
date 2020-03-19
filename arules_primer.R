# https://www.datacamp.com/community/tutorials/market-basket-analysis-r

#install and load package arules
#install.packages("arules")
# Provides the infrastructure for representing, manipulating and analyzing 
# transaction data and patterns (frequent itemsets and association rules).
library(arules)
#install and load arulesViz
#install.packages("arulesViz")
# Extends package 'arules' with various visualization techniques for association 
# rules and item-sets. The package also includes several interactive 
# visualizations for rule exploration.
library(arulesViz)
#install and load tidyverse
#install.packages("tidyverse")
library(tidyverse)
#install and load readxml
#install.packages("readxml")
library(readxl)
#install and load knitr
#install.packages("knitr")
library(knitr)
#load ggplot2 as it comes in tidyverse
library(ggplot2)
#install and load lubridate
#install.packages("lubridate")
library(lubridate)
#install and load plyr
#install.packages("plyr")
library(plyr)
library(dplyr)

# Data - http://archive.ics.uci.edu/ml/datasets/online+retail
#read excel into R dataframe
retail <- read_excel(here::here("data", 'Online Retail.xlsx'))
#complete.cases(data) will return a logical vector indicating which rows have 
#no missing values. Then use the vector to get only rows that are complete using 
#retail[,].
retail <- retail[complete.cases(retail), ]
#mutate function is from dplyr package. It is used to edit or add new columns to 
#dataframe. Here Description column is being converted to factor column. 
#as.factor converts column to factor column. %>% is an operator with which you 
#may pipe values to another function or expression
retail %>% mutate(Description = as.factor(Description))

retail %>% mutate(Country = as.factor(Country))

#Converts character data to date. Store InvoiceDate as date in new variable
retail$Date <- as.Date(retail$InvoiceDate)
#Extract time from InvoiceDate and store in another variable
TransTime<- format(retail$InvoiceDate,"%H:%M:%S")
#Convert and edit InvoiceNo into numeric
InvoiceNo <- as.numeric(as.character(retail$InvoiceNo))

#Bind new columns TransTime and InvoiceNo into dataframe retail
cbind(retail,TransTime)

glimpse(retail)

transactionData <- ddply(retail,c("InvoiceNo","Date"),
                         function(df1)paste(df1$Description,
                                            collapse = ","))

#set column InvoiceNo of dataframe transactionData  
transactionData$InvoiceNo <- NULL
#set column Date of dataframe transactionData
transactionData$Date <- NULL
#Rename column to items
colnames(transactionData) <- c("items")
#Show Dataframe transactionData
glimpse(transactionData)

# Save data in BASKET FORMAT
write.csv(transactionData,
          here::here("data", "market_basket_transactions.csv"),
                     quote = FALSE, 
                     row.names = FALSE)
          
tr <- read.transactions(here::here("data", "market_basket_transactions.csv"), 
                        format = 'basket', 
                        sep=',')         

# Create an item frequency plot for the top 20 items
if (!require("RColorBrewer")) {
  # install color package of R
  install.packages("RColorBrewer")
  #include library RColorBrewer
  library(RColorBrewer)
}
itemFrequencyPlot(tr,topN=20,type="absolute",
                  col=brewer.pal(8,'Pastel2'), 
                  main="Absolute Item Frequency Plot")
itemFrequencyPlot(tr,topN=20,type="relative",
                  col=brewer.pal(8,'Pastel2'),
                  main="Relative Item Frequency Plot")

# Min Support as 0.001, confidence as 0.8.
association.rules <- apriori(tr, 
                             parameter = list(supp=0.001, 
                                              conf=0.8,
                                              maxlen=10))                  
association.rules %>% summary()

inspect(association.rules[1:10])


shorter.association.rules <- 
  apriori(tr,
          parameter = list(supp=0.001, 
                           conf=0.8,
                           maxlen=3))

summary(shorter.association.rules)

# Removing redundant rules
#
# get subset rules in vector
subset.rules <- which(colSums(is.subset(association.rules, association.rules)) > 1) 
length(subset.rules)

# remove subset rules.
subset.association.rules. <- association.rules[-subset.rules] 
summary(subset.association.rules.)

# Finding Rules related to given items
# what customers buy before buying 'METAL'
metal.association.rules <- apriori(tr, 
                                   parameter = list(supp=0.001, 
                                                    conf=0.8),
                                   appearance = list(default="lhs",
                                                     rhs="METAL"))
summary(metal.association.rules)
inspect(head(metal.association.rules))

# Customers who bought METAL also bought....
metal.association.rules <- apriori(tr, 
                                   parameter = list(supp=0.001, 
                                                    conf=0.8),
                                   appearance = list(lhs="METAL",
                                                     default="rhs"))
summary(metal.association.rules)
inspect(head(metal.association.rules))                                   

# Visualizing Association Rules

# Scatter-Plot
# Filter rules with confidence greater than 0.4 or 40%
subRules<-association.rules[quality(association.rules)$confidence>0.4]
#Plot SubRules
plot(subRules)

plot(subRules,method="two-key plot")

plotly_arules(subRules)

# Graph-Based Visualizations
top10subRules <- head(subRules, n = 10, by = "confidence")
plot(top10subRules, method = "graph",  engine = "htmlwidget")

# saveAsGraph(head(subRules, n = 1000, by = "lift"), file = "rules.graphml")

# Individual Rule Representation
# Filter top 20 rules with highest lift
subRules2<-head(subRules, n=20, by="lift")
plot(subRules2, method="paracoord")
