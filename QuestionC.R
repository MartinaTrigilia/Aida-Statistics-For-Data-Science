
# Authors : Giovanni Scognamiglio and Martina Trigilia
# Course  : Statistics For Data Science
# Teacher : Salvatore Ruggieri 

##########

#lib and functions
library(DescTools)
library(ks)
library(BSDA)
library(nortest)
library(tseries)
library(plotrix)  
library(ggplot2)

load('aida.clean.RData')

table(aida$Last.accounting.closing.date[aida$Failed==1])

year_selected = aida$Last.accounting.closing.date==2016

# fix an year 
aida = aida[year_selected,]

                            ######## AGE ########

# What is the probability of failures conditional to age of firms 
# at a specific year (2016)?

failed = aida$Failed==1

# table of frequency
table(aida[,c('Age','Failed')])

# Probability P(Failure=1|Age=x) is P(Failure=1,Age=x)/P(Age=x)
list_values = c()
list_keys = c()

for (age in unique(aida$Age)){
  
  p <- nrow(aida[aida$Age==age & failed,])/nrow(aida[aida$Age==age,])
  list_values <- append(list_values,p)
  list_keys <- append(list_keys,age)
  
}

ageCond <- data.frame (
  age = list_keys,  
  probability = list_values
)

# ageCond <-ageCond[order(ageCond$age),]

# Barplot Conditional Probability of Failure given age

ggplot(ageCond, aes(x=age, y=probability)) + 
  geom_bar(stat = "identity", fill="steelblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("P(Failed=Yes | Age=x) in 2016") + 
  xlab("Age") + ylab("P(Failed=Yes | Age=x)")


# plot(ageCond, xlab = 'Age', type = "h", ylab = 'P(Failed=Yes | Age=x)')


