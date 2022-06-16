#15/06

# authors: Giovanni Scognamiglio; Martina Trigilia
##########
#lib and functions
library(DescTools)
library(ks)
library(BSDA)
library(nortest)
library(tseries)
library(plotrix)  
library(ggplot2)


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

# let's fix the Failed at 1

aida <- aida[aida$Failed==1,]

ggplot(aida, aes(x = Last.accounting.closing.date)) +
  geom_bar()

year_X = aida$Last.accounting.closing.date==2014

year_Y = aida$Last.accounting.closing.date==2016

############ QUESTION 1.0 ############ #### SIZE ###

# let's take the two distribution of Size to compare

fullSizeX = aida$Size[year_X]
fullSizeY = aida$Size[year_Y]

# sX = get_best_sample(fullSizeX,1000)
# sY = get_best_sample(fullSizeY,1000)

plot(density(fullSizeX))
lines(density(fullSizeY), col="blue")

length(fullSizeX);length(fullSizeY);

# ks.test(sX, sY) # D = 0.025, p-value = 0.9135

# we reject H0, that both sample come the same distribution
ks.test(fullSizeX, fullSizeY) # D = 0.0188, p-value = 2.877e-07

### GENERAL DATA

# we reject H0, that the samples have the same mean
z.test(fullSizeX, fullSizeY, sigma.x=sd(fullSizeX), sigma.y=sd(fullSizeY)) # p-value = 0.0004778 (95%CI: 0.1103032 0.1650406)

wilcox.test(fullSizeX, fullSizeY, conf.int = T) # p-value = 2.004e-05,  95%CI(0.03241141 0.08756097), DELTA: 0.05999382


# Exclude Other from the analysis, in year 2014 there are not sufficient records to apply for the assumption of large sample
table(aida$Legal.form[year_X])
table(aida$Legal.form[year_Y])

table(aida$Location[year_X])
table(aida$Location[year_Y])

# CALCULATE BONFERRONI CORRECTION ON CI FOR LEGAL FORM AND LOCATION

industry_form <- c("Consortium","S.R.L.", "S.P.A.","S.C.A.R.L.P.A.", "S.R.L. one-person", "S.C.A.R.L.",
                                    "S.R.L. simplified", "Social cooperative company", "S.A.S.", "S.N.C.")

# Bonferroni Correction For Multi Test on Legal Form

m_form <- length(industry_form)
bonf_alfa.form <- 1-(0.05/m_form)

locations <- levels(aida$Location)

# Bonferroni Correction For Multi Test  on Location
m_loc <- nlevels(aida$Location)
bonf_alfa.loc <- 1-(0.05/m_loc)

get_ztest.location = function(location, aida_attr){
  
  aida_location = aida$Location==location
  
  year_X = aida$Last.accounting.closing.date==2014
  
  year_Y = aida$Last.accounting.closing.date==2016
  
  fullX = aida_attr[year_X & aida_location]
  fullY = aida_attr[year_Y & aida_location]
  
  print(location)
 
  # large sample, general data assum. 
  ztest <- z.test(fullX, fullY, sigma.x=sd(fullX), sigma.y=sd(fullY), conf.level = bonf_alfa.loc)
  
  return(ztest)
  
}

get_ztest.form = function(form, aida_attr){
  
  aida_form = aida$Legal.form==form 
  
  year_X = aida$Last.accounting.closing.date==2014
  
  year_Y = aida$Last.accounting.closing.date==2016
  
  fullX = aida_attr[year_X & aida_form]
  fullY = aida_attr[year_Y & aida_form]
  
  print(form)
  # same shape test for wilcox assumption
  
  Z = mean(fullX) - mean(fullY)
  fullX_adjusted = fullX - Z
 
  ks.test(fullX_adjusted,fullY)
  
  res_kstest<- ks.test(fullX_adjusted,fullY)
  res_kstest.pval <- res_kstest$p.value
  same_shape <- (res_kstest.pval >= 0.05)
  
  if (same_shape) {    
    test_list <- wilcox.test(fullX, fullY, conf.int = T, conf.level = bonf_alfa.form ) 
    
    # we also tested without continuity correction for normality
    # test_list <- wilcox.test(fullX, fullY, conf.int = T, conf.level = bonf_alfa.form, correct=F )
  }
  else 
    # large sample, general data assum. 
    test_list <- z.test(fullX, fullY, sigma.x=sd(fullX), sigma.y=sd(fullY), conf.level = bonf_alfa.form)
  
  return(test_list)
  
}

########### QUESTION B. 1 ######## SIZE

z_list.form_size <- sapply(industry_form, get_ztest.form, aida$Size) 

z_list.form_size['conf.int',]

# z_list.form_size['conf.int',]$Consortium[[2]]

########### QUESTION B.2 ######## SIZE

z_list.loc_size <- sapply(locations, get_ztest.location, aida$Size) 

z_list.loc_size['conf.int',]



############ QUESTION 1.0############# AGE #######


# let's take the two distribution of Size to compare

fullAgeX = aida$Age[year_X]
fullAgeY = aida$Age[year_Y]

# AX = get_best_sample(fullSizeX,1000)
# AY = get_best_sample(fullSizeY,1000)

plot(density(fullAgeX))
lines(density(fullAgeY), col="blue")

length(fullAgeX);length(fullAgeY);

# we reject that both sample come the same distribution

# ks.test(AX, AY) # D = 0.049, p-value = 0.1811

ks.test(fullAgeX, fullAgeY) # D = 0.035205, p-value < 2.2e-16

### GENERAL DATA

# they return the same confidence interval at 95% 
z.test(fullAgeX, fullAgeY, sigma.x=sd(fullAgeX), sigma.y=sd(fullAgeY)) #  p-value < 2.2e-16 (95%CI: -0.9082662 -0.6223939)

# wilcox.test(fullAgeX, fullAgeY, conf.int = T) # p-value = 8.181e-09,  95%CI(-0.9082662 -0.6223939), DELTA: -3.275076e-05

########### QUESTION B. 1 ######## Age

z_list.form_size <- sapply(industry_form, get_ztest.form, aida$Size) 

z_list.form_size['conf.int',]

# z_list.form_size['conf.int',]$Consortium[[2]]

########### QUESTION B.1 ######## Age

z_list.form_age <- sapply(industry_form, get_ztest.form, aida$Age) 

z_list.form_age['conf.int',]

########### QUESTION B.2 ######## Age

z_list.loc_age <- sapply(locations, get_ztest.location, aida$Age) 

z_list.loc_age['conf.int',]





