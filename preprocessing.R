# Load library
library(tidyverse)
library(plyr)
library(hash)
###########
# AIDA data loading
###########

# clean all
rm(list=ls())
# working dir
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# to reload the whole dataset at once, run
load("aida.RData")
# View(aida)
# summaries

# summary(aida)
# summary(aida['Legal status'])

# sub all blank spaces with dot
names(aida) <- make.names(names(aida), unique=TRUE)

# I valori di Legal Status merger e demerger possono essere eliminati  
merger_vect <- c("Dissolved (demerger)", "Dissolved (merger)")
aida <- aida[!(aida$Legal.status %in% merger_vect),] 
aida$Legal.status <- droplevels(aida$Legal.status)

# Define binary variable Failed: 1 for failed firm, 0 otherwise
active_status_vect <- c("Active", "Active (receivership)", "Active (default of payments)")
aida$Failed <- ifelse(aida$Legal.status %in% active_status_vect, 0, 1)
#sort(table(aida$Failed)*100/nr, decreasing=T)
# Create levels for Failed
aida$Failed <- factor(aida$Failed)
levels(aida$Failed)

# remove NA in LEGAL FORM
#è importante droppare prima i NAN perchè sennò non si possono droppare i levels!
aida = aida[!is.na(aida$Legal.form),] 
## LEGAL FORM ACCORPIAMO I VALORI POCHI IN OTHER
accorpate = c('S.A.P.A.','Foundation','Foreign company','S.C.A.R.I.',
              'Public agency',  'Mutual aid society', 'Association')
aida$Legal.form[aida$Legal.form %in% accorpate] = 'Other'
aida$Legal.form <- droplevels(aida$Legal.form)

# CREAZIONE VARIABILE AGE
aida <- aida[!(is.na(aida['Incorporation.year'])),] 
#269 rows for this condition (aida[aida$Incorporation.year > aida$Last.accounting.closing.date,])
aida <- aida[!(aida$Incorporation.year > aida$Last.accounting.closing.date),]
aida$Age <- aida$Last.accounting.closing.date - aida$Incorporation.year
# rimuoviamo dal dataset le aziende che hanno un età negativa
aida <- aida[!(aida$Age<0),] 

# FARE PRE-PROCESSIG DELLA VARIABILE AGE ---


# PRE-PROCESSING VARIABILE ATECO

# droppiamo i NA di Ateco_2007Code
aida = aida[!is.na(aida$ATECO.2007code),] 

aida = aida[!(aida$ATECO.2007code=="000000"),] 

# nrow(aida[aida$ATECO.2007code=="000000",]) #193 rows with missing Ateco.Code

aida$ATECO.2007code <- substr(aida$ATECO.2007code, 1, 2)

get_ATECO.NAME = function(ateco.value) {
  
  if(ateco.value %in% c("01","02","03"))
    return ("A - Agricoltura")
  
  if(ateco.value %in% c("05", "06", "07", "08", "09"))
    return ("B - Miniere")
  
  if(ateco.value %in% as.character(c(10:33)))
    return("C - Manifattura")
  
  if(ateco.value=="35")
    return("D - Energia")
  
  if(ateco.value %in% as.character(c(36:39)))
    return("E - Fornitura Acqua")
  
  if(ateco.value %in% as.character(c(41:43)))
    return("F - Costruzioni")
  
  if(ateco.value %in% as.character(c(45:47)))
    return("G - Ingrosso")
  
  if(ateco.value %in% as.character(c(49:53)))
    return("H - Trasporti")
  
  if(ateco.value %in% as.character(c(55:56)))
    return("I - Ristorazione")
  
  if(ateco.value %in% as.character(c(58:63)))
    return("J - Comunicazioni")
  
  if(ateco.value %in% as.character(c(64:66)))
    return("K - Assicurazioni")
  
  if(ateco.value=="68")
    return("L - Immobiliari")
  
  if(ateco.value %in% as.character(c(69:75)))
    return("M - Attività Scientifiche")
  
  if(ateco.value %in% as.character(c(77:82)))
    return("N - Noleggio")
  
  if(ateco.value=="84")
    return("O - Amministrazione Pubblica")
  
  if(ateco.value=="85")
    return("P - Istruzione")
  
  if(ateco.value %in% as.character(c(86:88)))
    return("Q - Sanità")
  
  if(ateco.value %in% as.character(c(90:93)))
    return("R - Arte e Sparti")
  
  if(ateco.value %in% as.character(c(94:96)))
    return("S - Altri Servizi")
  
  if(ateco.value %in% as.character(c(97:98)))
    return("T - Attività Familiari")
  
  if(ateco.value=="99")
    return("U - Organizzazione Extraterritoriali")
  
}


aida$ATECO.NAME <- sapply(aida$ATECO.2007code, get_ATECO.NAME) 


# set ATECO.NAME as a factor
aida$ATECO.NAME <- as.factor(aida$ATECO.NAME)


# CREAZIONE E PRE-PROCESSIG DELLA VARIABILE SIZE
aida$Size <- ifelse(aida$Total.assetsth.EURLast.avail..yr==0, 0,log(aida$Total.assetsth.EURLast.avail..yr))
# rimuoviamo dal dataset le aziende che hanno Size mancante
aida <- aida[!(is.na(aida$Size)), ] 
#cleaning outliers from Size
iqr = IQR(aida$Size)
quan = quantile(aida$Size,c(0.25,0.75))
lowlim = quan[1]-2*iqr
uplim = quan[2]+2*iqr

aida = data.frame(aida[aida$Size>lowlim & aida$Size<uplim,
                       c('Size','Failed','Last.accounting.closing.date',
                         'Legal.form', 'ATECO.NAME', 'Age')])

aidaAge = data.frame(aida[aida$Size>lowlim & aida$Size<uplim,
                       c('Size','Failed','Last.accounting.closing.date',
                         'Legal.form', 'ATECO.NAME', 'Age')])


save(aida, file = "aida.clean.RData")
save(aida, file = "aidaAge.clean.RData")
save(aida, file = "aidaSize.clean.RData")

