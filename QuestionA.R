#12/06
##########
#lib and functions
library(DescTools)
library(ks)
library(BSDA)
library(nortest)
library(tseries)
library(plotrix)    


#get samples function
tryf = function(data,n) {
  fhat = kde(data)
  samp = rkde(n, fhat) 
  ks_res = ks.test(samp,data)
  return(list('ks_res'=ks_res,'samp'=samp))
}
get_best_sample = function(data, n) {
  smallest_d = 1
  for (x in 1:10) {
    ks_output = tryf(data, n)
    dstat = unname(unlist(ks_output[['ks_res']]['statistic']))
    #print(dstat)
    if (dstat < smallest_d) {
      smallest_d = dstat
      best_ns0 = ks_output[['samp']]
    }
  }
  return(best_ns0)
}

load('aida.clean.RData')



############ QUESTION 1.0 ############

#### SIZE ###

fulls0 = aida$Size[aida$Failed==0 & year_selected]
fulls1 = aida$Size[aida$Failed==1 & year_selected]

plot(density(fulls0), col=3); lines(density(fulls1), col=2)
mean(fulls1); mean(fulls0)

#do we reject that both sample come the same dist?
ks.test(fulls1, fulls0) #D = 0.12354, p-value < 2.2e-16

#checking for normality
# KS TEST, reject H0, data does not follow a normal distribution
ks.test(fulls1, pnorm, mean(fulls1), sd(fulls1)) #D = 0.0087027, p-value = 0.002588
ks.test(fulls0, pnorm, mean(fulls0), sd(fulls0)) #D = 0.030286, p-value < 2.2e-16

# Anderson-test, (reject H0) data does not follow a normal distribution
ad.test(fulls1) # A = 7.7405, p-value < 2.2e-16
ad.test(fulls0) # A = 67.434, p-value < 2.2e-16

# Jarque Bera (reject H0)
jarque.bera.test(fulls1)
jarque.bera.test(fulls0) # X-squared = 615.09,  p-value < 2.2e-16

# QQ-plot, reject normal distribution 

qqnorm(fulls0, pch = 1, frame = FALSE)
qqline(fulls0, col = "steelblue", lwd = 2)

qqnorm(fulls1, pch = 1, frame = FALSE)
qqline(fulls1, col = "steelblue", lwd = 2)

#extracting smaller samples
s1 = get_best_sample(fulls1,1000)
s0 = get_best_sample(fulls0,1000)

shapiro.test(s1) #p-value = 0.04886
shapiro.test(s0) #p-value = 8.672e-05

###if we assume normality----- SOLO PER CONFRONTARE:
#F-test
var.test(s0, s1) #pvalue: p-value = 5.87e-09
t.test(s0,s1) #1.291e-07 (95%CI: 0.3668698 0.5670563)
t.test(s0,s1, alternative ='less') #pvalue= 1
t.test(s0,s1,var.equal = T) #1.291e-07 (95%CI: 0.366870 0.567056)
### we see that there is statistical significance that muFailed < muActive

###NOT NORMAL:
z.test(fulls0, fulls1, sigma.x=sd(fulls0), sigma.y=sd(fulls1)) #p-value < 2.2e-16 (95%CI: 0.4401488 0.4941443)

### --> we see that the more precise CI is provided by the large number z.test. 
#We are 95% confident that the true difference of the mean size of Active and Failed is between 0.44 and 0.49

industry_form <- c("Consortium","S.R.L.", "S.P.A.","S.C.A.R.L.P.A.", "S.R.L. one-person", "S.C.A.R.L.",
                   "S.R.L. simplified", "Social cooperative company", "S.A.S.", "S.N.C.", "Other")

m_form <- length(industry_form)

# Bonferroni Correction For Multi Test on Legal Form
bonf_alfa.form <- 1-(0.05/m_form)

unique.ateco.name <- levels(aida$ATECO.NAME)

m <- length(unique.ateco.name)

# Bonferroni Correction For Multi Test on ATECO NAME
bonf_alfa.ateco <- 1-(0.05/m)

get_ztest.ateco = function(ateco.name, aida_attr){
  
  year_selected = aida$Last.accounting.closing.date==2016
  aida_ateco = aida$ATECO.NAME==ateco.name 
  
  fulls0 = aida_attr[aida$Failed==0 & year_selected & aida_ateco]
  fulls1 = aida_attr[aida$Failed==1 & year_selected & aida_ateco]
  
  print(ateco.name)
  
  # large sample, general data assum. 
  ztest <- z.test(fulls0, fulls1, sigma.x=sd(fulls0), sigma.y=sd(fulls1), conf.level = bonf_alfa.ateco)
  
  return(ztest)
  
}

get_ztest.form = function(form, aida_attr){
  
  aida_form = aida$Legal.form==form 
  year_selected = aida$Last.accounting.closing.date==2016
  fulls0 = aida_attr[aida$Failed==0 & year_selected & aida_form]
  fulls1 = aida_attr[aida$Failed==1 & year_selected & aida_form]
  
  print(form)
  # large sample, general data assum. 
  
  ztest <- z.test(fulls0, fulls1, sigma.x=sd(fulls0), sigma.y=sd(fulls1), conf.level = bonf_alfa.form)
  
  #ztest <- z.test(fulls0, fulls1, sigma.x=sd(fulls0), sigma.y=sd(fulls1))
  
  return(ztest)
  
}

############ QUESTION 1. A) ############

# fix the year

table(aida$Legal.form[year_selected])
year_selected = aida$Last.accounting.closing.date==2016
table(aida$Legal.form[year_selected & aida$Failed==0])
table(aida$Legal.form[year_selected & aida$Failed==1])

z_list.form_size = sapply(industry_form, get_ztest.form, aida$Size) 

z_list.form_size['conf.int',]

# QUESTION A.1.2 SIZE (ATECO NAME)

z_list.ateco_size <- sapply(unique.ateco.name, get_ztest.ateco, aida$Size) 

z_list.ateco_size['conf.int',]

############ QUESTION 1.0 ############

###### AGE ##########
year_selected = aida$Last.accounting.closing.date==2016

fullAge0 = aida$Age[aida$Failed==0 & year_selected]
fullAge1 = aida$Age[aida$Failed==1 & year_selected]

plot(density(fullAge0), col=3); lines(density(fullAge1), col=2)
mean(fullAge1) # 10.91962
mean(fullAge0) # 8.185734

# we reject that both sample come the same dist
ks.test(fullAge1, fullAge0) # D = 0.15674, p-value < 2.2e-16

###General Data, Large Sample
### --> we see that the more precise CI is provided by the large number z.test. 
z.test(fullAge1, fullAge0, sigma.x=sd(fullAge1), sigma.y=sd(fullAge0)) #p-value < 2.2e-16 (95%CI:2.587521 2.880247)

###Question A.1.1 Age

z_list.form_age = sapply(industry_form, get_ztest.form.age, aida$Age) 

z_list['conf.int',]

### QUESTION A.1.2 AGE

z_list.ateco_age <- sapply(unique.ateco.name, get_ztest.ateco, aida$Age) 

z_list.ateco_age['conf.int',]