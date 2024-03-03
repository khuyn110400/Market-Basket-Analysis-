install.packages("igraph")
library(igraph)
library(arules)
library(arulesViz)
library(tidyverse)
library(lubridate)
groceries = read.transactions('C:/Users/Khanh Huyen/OneDrive - The Danang University of Economics/Desktop/groceries.csv'
                              ,format= 'basket', sep=',')
data = read.csv("C:/Users/Khanh Huyen/OneDrive - The Danang University of Economics/Desktop/groceries.csv",stringsAsFactors = TRUE)
str(data)
itemFrequencyPlot(groceries,topN=20,xlab='Item')

# Initial values
suplvl <- c(0.1,0.05,0.01,0.005,0.001)
conflvl <- c(0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1)

#blank integers
rule_s10 <- integer(length=9)
rule_s5 <- integer(length=9)
rule_s1 <- integer(length=9)
rule_s0.5 <- integer(length=9)
rule_s0.1 <- integer(length=9)

# Apriori Algorithm: support level of 10%
for(i in 1:length(conflvl)) {
  rule_s10[i] <- length(apriori(groceries, parameter = list(sup=suplvl[1],
                                                             conf=conflvl[i], target= "rules")))
}

# Apriori Algorithm: support level of 5%
for(i in 1:length(conflvl)) {
  rule_s5[i] <- length(apriori(groceries, parameter = list(sup=suplvl[2],
                                                             conf=conflvl[i], target= "rules")))
}

# Apriori Algorithm: support level of 1%
for(i in 1:length(conflvl)) {
  rule_s1[i] <- length(apriori(groceries, parameter = list(sup=suplvl[3],
                                                           conf=conflvl[i], target= "rules")))
}

# Apriori Algorithm: support level of 0.5%
for(i in 1:length(conflvl)) {
  rule_s0.5[i] <- length(apriori(groceries, parameter = list(sup=suplvl[4],
                                                           conf=conflvl[i], target= "rules")))
}

# Apriori Algorithm: support level of 0.1%
for(i in 1:length(conflvl)) {
  rule_s0.1[i] <- length(apriori(groceries, parameter = list(sup=suplvl[5],
                                                             conf=conflvl[i], target= "rules")))
}

num_rules <- data.frame(rule_s10,rule_s5,rule_s1,rule_s0.5, rule_s0.1, conflvl)

ggplot(data=num_rules,aes(x=conflvl)) +
  geom_line(aes(y=rule_s10,colour="Support level of 10%")) +
  geom_point(aes(y=rule_s10,colour="Support level of 10%")) +
  geom_line(aes(y=rule_s5,colour="Support level of 5%")) +
  geom_point(aes(y=rule_s5,colour="Support level of 5%")) +
  geom_line(aes(y=rule_s1,colour="Support level of 1%")) +
  geom_point(aes(y=rule_s1,colour="Support level of 1%")) +
  geom_line(aes(y=rule_s0.5,colour="Support level of 0.5%")) +
  geom_point(aes(y=rule_s0.5,colour="Support level of 0.5%")) +
  geom_line(aes(y=rule_s0.1,colour="Support level of 0.1%")) +
  geom_point(aes(y=rule_s0.1,colour="Support level of 0.1%")) +
  labs(x="Confidence levels", y="Count of rules found",
       title="Apriori Algorithm with different support levels")
  
final_rule <- apriori(groceries,parameter=list(sup=suplvl[3],
                                               conf=conflvl[5], target="rules"))

inspect(final_rule)

#CHECK YOGURT
rule.yogurt <- apriori(data=groceries,parameter=list(supp=0.001,conf=0.4),
                       appearance = list(default="lhs",rhs="yogurt"), 
                       control=list(verbose=F))
rule.yogurt.byconf <- sort(rule.yogurt,by="confidence", descreasing=TRUE)
inspect(head(rule.yogurt.byconf))

rule.yogurt <- apriori(data=groceries,parameter=list(supp=0.001,conf=0.1),
                       appearance = list(default="rhs",lhs="yogurt"), 
                       control=list(verbose=F))
rule.yogurt.byconf <- sort(rule.yogurt,by="support", descreasing=FALSE)
inspect(head(rule.yogurt.byconf))

#CHECK WHOLE MILK
rule.wholemilk <- apriori(data=groceries,parameter=list(supp=0.1,conf=0.1),
                       appearance = list(default="lhs",rhs="whole milk"), 
                       control=list(verbose=T))
rule.yogurt.byconf <- sort(rule.yogurt,by="confidence", descreasing=TRUE)
inspect(head(rule.yogurt.byconf))

rule.yogurt <- apriori(data=groceries,parameter=list(supp=0.001,conf=0.1),
                       appearance = list(default="rhs",lhs="yogurt"), 
                       control=list(verbose=F))
rule.yogurt.byconf <- sort(rule.yogurt,by="support", descreasing=FALSE)
inspect(head(rule.yogurt.byconf))


plot(final_rule,measure=c("support","lift"),shading="confidence")

plot(final_rule,method="graph",control=list(layout=circle())

plot(final_rule, method = "graph", control = list(layout = "circle"))