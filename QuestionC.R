
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
library(car)
library(dplyr)

load('aida.clean.RData')

table(aida$Last.accounting.closing.date[aida$Failed==1])

year_selected = aida$Last.accounting.closing.date==2016

# fix an year 
aida = aida[year_selected,]

######## AGE ########

# 1) What is the probability of failures conditional to age of firms 
# at a specific year (2016)?

failed = aida$Failed==1

# table of frequency
table(aida[,c('Age','Failed')])
table(aida[,c('Age','Failed', 'Location')])

summary(aida$Age)

aida$Age <- as.character(aida$Age)

aida$Age[aida$Age %in% as.character(c(50:114))] <- ">=50"

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

rm(list_keys)
rm(list_values)

# ageCond <-ageCond[order(ageCond$age),]

# ageCond[match(as.character(c(0:49), ageCond$age),]

v <- as.character(c(0:49))

order_key <- append(v,">=50")

ageCond <- ageCond[match(order_key, ageCond$age),]

plot(density(ageCond$probability))

# Barplot Conditional Probability of Failure given age
ggplot(ageCond, aes(x=factor(age, level = order_key), y=probability)) + 
  geom_bar(stat = "identity", fill="steelblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("P(Failed=Yes | Age=x) in 2016") + 
  xlab("Age") + ylab("P(Failed=Yes | Age=x)")

# Probability P(Failure=1|Age=x & Location =l) is P(Failure=1,Age=x,Location =l)/P(Age=x,Location =l)

rel_prod = function(age_value,loc) {
  data = aida$Failed[aida$Age== age_value & aida$Location == loc]
  return((table(data)/length(data))[[2]])
}

list_res = c()

for (loc in levels(aida$Location)){
  
  res = list(sapply(unique(aida$Age), rel_prod, loc))
  list_res <- append(list_res, res)
  
}

ageCondLocation <- data.frame (
  Age = unique(aida$Age),  
  PFailedAgeCentro = list_res[1],
  PFailedAgesole = list_res[2],
  PFailedAgeNordest = list_res[3],
  PFailedAgeNordovest = list_res[4],
  PFailedAgeSud = list_res[5]
)

colnames(ageCondLocation) <- c("Age", "PFailedAgeCentro", "PFailedAgesole", "PFailedAgeNordest",
                              "PFailedAgeNordovest","PFailedAgeSud")

plot(density(ageCondLocation$PFailedAgeNordovest))
lines(density(ageCondLocation$PFailedAgeCentro), col=2)
lines(density(ageCondLocation$PFailedAgesole), col=3)
lines(density(ageCondLocation$PFailedAgeNordest), col=4)
lines(density(ageCondLocation$PFailedAgeNordovest), col=5)
lines(density(ageCondLocation$PFailedAgeSud), col=6)

t.test(ageCondLocation$PFailedAgeSud,ageCondLocation$PFailedAgeNordovest)


