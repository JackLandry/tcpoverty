# TCPOVERTY: STEP 2A
# MAKE TAXSIM27
# Ernie Tedeschi
# with modifications to code from Sam Portnow: https://users.nber.org/~taxsim/to-taxsim/cps/cps-portnow/TaxSimRScriptForDan.R
# Last Updated: 21 March 2019

# library(foreign)
# library(plyr)
# library(psych)
# library(maps)
# library(RCurl)
# library(dplyr)
# library(ggplot2)
library(tidyverse)
# SETTINGS

## pexemp: Personal exemption for year of analysis
pexemp <- data.frame(year=2018, pexemp = 0) #Using 2019 data, which references income form 2018, 2018
#https://taxfoundation.org/2018-tax-brackets/
# ## wd: Working Directory
# wd <- '~/Documents/data/cpsasec/make taxsim'
# 
# ## wdtaxsim: Tax-Calculator taxsim directory
# wdtaxsim <- '~/tc/Tax-Calculator/taxcalc/validation/taxsim'
# 
# ## datafile: Name of ASEC extract file
# datafile <- 'dtadata.csv'

# END SETTINGS 

##################################################
library(ipumsr)
library(here)
here()

#### = My Comment
# = original comment
####Import CPS data keeping some handly labels
ddi <- read_ipums_ddi(here("cps_data","cps_00053.xml"))
# ddi <- read_ipums_ddi(here("cps_data","cps_00041.xml"))
#ddi <- read_ipums_ddi("/Users/jacklandry/Documents/GitHub/tcpoverty/cps_data/cps_00040.xml")
ipum <-  read_ipums_micro(ddi)

#setwd(wd)

#ipum <- read.csv(datafile, stringsAsFactors = F)   
# set to lower case
names(ipum) <- tolower(names(ipum))

# /* Set missing income items to zero so that non-filers etc will get zeroes.*/
# find out what statatax is and get it
vars1 <- c('eitcred', 'fedretir')
vars2 <- c('fedtax', 'statetax', 'adjginc', 'taxinc', 'fedtaxac', 'fica',
           #'caploss', ####Variables does not exist
           'stataxac',
           'incdivid', 'incint',   'incrent',   'incother', 
           #'incalim',  ####Variable does not exist
           'incasist',
           'incss',    'incwelfr', 'incwkcom',  'incvet',   'incchild', 'incunemp',
           'inceduc',  'gotveduc', 'gotvothe',  'gotvpens', 'gotvsurv', 'incssi')
vars3 <- c('incwage', 'incbus', 'incfarm', 'incsurv', 'incdisab', 'incretir')
vars <- c(vars1, vars2, vars3)


# these are the missing codes
ipum[,vars][is.na(ipum[,vars])] <- 0
ipum[,vars][ipum[,vars] == 9999] <- 0
ipum[,vars][ipum[,vars] == 99999] <- 0
ipum[,vars][ipum[,vars] == 999999] <- 0
ipum[,vars][ipum[,vars] == 9999999] <- 0

ipum[,vars][is.na(ipum[,vars])] <- 0
ipum[,vars][ipum[,vars] == -9999] <- 0
ipum[,vars][ipum[,vars] == -99999] <- 0
ipum[,vars][ipum[,vars] == -999999] <- 0
ipum[,vars][ipum[,vars] == -9999999] <- 0


ipum[,vars][ipum[,vars] == 9997] <- 0
ipum[,vars][ipum[,vars] == 99997] <- 0
ipum[,vars][ipum[,vars] == 999997] <- 0
ipum[,vars][ipum[,vars] == 9999997] <- 0

# more missing codes

# vars <- 'capgain'  #### Does not Exist, so commented this section out
# 
# ipum[,vars][is.na(ipum[,vars])] <- -999
# ipum[,vars][ipum[,vars] == 9999] <- -999
# ipum[,vars][ipum[,vars] == 99999] <- -999
# ipum[,vars][ipum[,vars] == 999999] <- -999
# ipum[,vars][ipum[,vars] == 9999999] <- -999
# 
# ipum[,vars][ipum[,vars] == -9999] <- -999
# ipum[,vars][ipum[,vars] == -99999] <- -999
# ipum[,vars][ipum[,vars] == -999999] <- -999
# ipum[,vars][ipum[,vars] == -9999999] <- -999
# 
# ipum[,vars][ipum[,vars] == 9997] <- -999
# ipum[,vars][ipum[,vars] == 99997] <- -999
# ipum[,vars][ipum[,vars] == 999997] <- -999
# ipum[,vars][ipum[,vars] == 9999997] <- -999


# set 0's to NA for location

ipum[ipum$momloc == 0,]$momloc <- NA
ipum[ipum$poploc == 0,]$poploc <- NA
ipum[ipum$sploc == 0,]$sploc <- NA


# year before tax returns
ipum$x2 <- ipum$year - 1

# set x3 to  fips code
ipum$x3 <- ipum$statecensus

# /* Marital status will be sum of spouse's x4 values */
ipum$x4 <- 1

# Cohabitators not married

ipum$sploc[ipum$relate == 1114] <- NA

# x6 is just age for now
ipum$x6 <- ifelse((is.na(ipum$sploc) | (ipum$sploc > 0 & ipum$sploc > ipum$pernum)), ipum$age, 0)
ipum$x24 <- ifelse((!is.na(ipum$sploc) & ipum$sploc > 0 & ipum$sploc < ipum$pernum), ipum$age, 0)


# primary wage or spouse wage
ipum$x7 <- ifelse((is.na(ipum$sploc) | (ipum$sploc > 0 & ipum$sploc > ipum$pernum)), ipum$incwage + ipum$incbus + ipum$incfarm, 0)
ipum$x8 <- ifelse((!is.na(ipum$sploc) & ipum$sploc > 0 & ipum$sploc < ipum$pernum), ipum$incwage + ipum$incbus + ipum$incfarm, 0)


ipum$x9 <- ipum$incdivid
ipum$x10 <- ipum$incrent + ipum$incother #+ ipum$incalim ####Variable does not exist
ipum$x11 <- ipum$incretir
ipum$x12 <- ipum$incss
ipum$x27 <- ipum$incint
ipum$x28 <- 0

# /* Commented out got* items below because they are an error - 
# hope to fix soon. drf, 
# Nov18, 2015
# */

attach(ipum)
ipum$x13 <- incwelfr+incwkcom+incvet+incsurv+incdisab+incchild+inceduc+
  # /*gotveduc+gotvothe+gotvpens+gotvsurv+*/ 
  incssi+incasist
detach(ipum)

ipum$x14 <- ipum$incrent
ipum$x15 <- 0


# /* use Census imputation of itemized deductions where available.*/
# first have to join the exemption table
names(pexemp)[1] <- 'x2'
# ipum <- join(ipum, pexemp, by='x2')
#### re-doing this with modern command
ipum <- left_join(ipum, pexemp, by='x2')


# adjusted gross - taxes + exemptions
#proptax no longer available, should look at how AGI is calculated
#Maybe calc AGI straight from the thing
ipum$x16 <- ipum$adjginc - rowSums(ipum[,c('pexemp', 'statetax', 'taxinc')], na.rm=T)
# no values less than 0
ipum$x16 <- ifelse(ipum$x16 < 0, 0, ipum$x16)

ipum$x17 <- 0
ipum$x18 <- ipum$incunemp
ipum$x19 <- 0
ipum$x20 <- 0
ipum$x21 <- 0

# * Assume capgain and caploss are long term;
#### Reffereing to variables that don't exist so commneted out
# ipum$x22 <- ifelse(! ipum$capgain==-999, ipum$capgain - ipum$caploss, 0)
# ipum$capgain[ipum$capgain==-999] <- 0


# Here we output a record for each person, so that tax units can be formed 
# later by summing over person records. The taxunit id is the minimum of
# the pernum or sploc, so spouses will get the same id. For children
# it is the minimum of the momloc or poploc. Other relatives are made
# dependent on the household head (which may be incorrect) and non-relatives
# are separate tax units. 
# */

attach(ipum)

ipum$hnum <- 0
ipum[ipum$relate==101,]$hnum <- ipum[ipum$relate==101,]$pernum
ipum[ipum$hnum==0,]$hnum <- NA

# if claiming > personal exemption than they're their own filer
ipum$notself <- ifelse((ipum$x7 + ipum$x8 + ipum$x9 + ipum$x10 + ipum$x11 + ipum$x12 + ipum$x13) <= ipum$pexemp, 1, 0)
#ipum$notself <- ifelse((ipum$x7 + ipum$x8 + ipum$x9 + ipum$x10 + ipum$x11 + ipum$x12 + ipum$x13 + ipum$x22) <= ipum$pexemp, 1, 0)
#### x22 not in data so removed

ipum$sum <- rowSums(ipum[,c('x7', 'x8', 'x9', 'x10', 'x11', 'x12', 'x13')], na.rm=T)
#ipum$sum <- rowSums(ipum[,c('x7', 'x8', 'x9', 'x10', 'x11', 'x12', 'x13', 'x22')], na.rm=T)
#### x22 not in data so removed

ipum$notself <- ifelse(rowSums(ipum[,c('x7', 'x8', 'x9', 'x10', 'x11', 'x12', 'x13')], na.rm=T) <= ipum$pexemp, 1, 0)
# ipum$notself <- ifelse(rowSums(ipum[,c('x7', 'x8', 'x9', 'x10', 'x11', 'x12', 'x13', 'x22')], na.rm=T) <= ipum$pexemp, 1, 0)
#### x22 not in data so removed
ipum$depstat[!is.na(ipum$sploc) & ipum$depstat > 0 & ipum$depstat == ipum$sploc] <- 0


ipum$depchild <- ifelse(ipum$depstat > 0 & (!is.na(ipum$momloc) | !is.na(ipum$poploc)) & (ipum$age < 18 | (ipum$age < 24 & ipum$schlcoll > 0)), 1, 0)
ipum$deprel <- ifelse(ipum$depstat > 0 & ipum$depchild == 0, 1, 0)
ipum$dep13 <- ifelse((ipum$deprel == 1 | ipum$depchild == 1) & ipum$age < 13, 1, 0)
ipum$dep17 <- ifelse((ipum$deprel == 1 | ipum$depchild == 1) & ipum$age < 17, 1, 0)
ipum$dep18 <- ifelse((ipum$deprel == 1 | ipum$depchild == 1) & ipum$age < 18, 1, 0)


detach(ipum)

# set dependents and taxpayers
dpndnts <- ipum[ipum$depchild==1 | ipum$deprel==1,]
dpndnts$x1 <- 0
dpndnts[dpndnts$depchild == 1,]$x1 <- 100*dpndnts[dpndnts$depchild == 1,]$serial + apply(dpndnts[dpndnts$depchild == 1,][,c('momloc', 'poploc')], 1, min, na.rm=T)
dpndnts[dpndnts$deprel == 1,]$x1 <- 100*dpndnts[dpndnts$deprel == 1,]$serial + dpndnts[dpndnts$deprel==1,]$hnum

dpndnts$x4 <- NA
dpndnts$x5 <- 1
dpndnts$x6 <- 0
dpndnts$x19 <- NA
dpndnts$x23 <- NA
dpndnts$x24 <- 0

txpyrs <- ipum[ipum$depchild == 0 & ipum$deprel == 0,]
txpyrs$x1 <- 0
txpyrs$x1 <- 100*txpyrs$serial + apply(txpyrs[,c('pernum', 'sploc')], 1, min, na.rm=T)

txpyrs$x5 <- 0
txpyrs$x23 <- NA

# set whats not x1, x2, or x5 in deps to NA
vars <- c('x3', paste0('x', 4), paste0('x', 7:23), 'x27', 'x28')
dpndnts[,vars] <- NA

#### x22 not in txpyrs causing an issue in rbind... Maybe should set to 0 instead in txpyrs
# dpndnts <- dpndnts %>% select(-x22)
txpyrs <- txpyrs %>% mutate(x22=0)
# put them back together
ipum <- rbind(txpyrs, dpndnts) #This  is not working now


# sum value over tax #
# concat <- group_by(ipum, x2, x1)
# concat <- summarise(concat, x3 = mean(x3, na.rm=T), x4 = sum(x4, na.rm=T),
#                     x5 = sum(x5, na.rm=T), x6 = max(x6, na.rm=T), x7 = sum(x7, na.rm=T), x8 = sum(x8, na.rm=T), x9 = sum(x9, na.rm=T),
#                     x10 = sum(x10, na.rm=T), x11 = sum(x11, na.rm=T), x12 = sum(x12, na.rm=T), x13 = sum(x13, na.rm=T), x14 = sum(x14, na.rm=T), x15 = sum(x15, na.rm=T),
#                     x16 = sum(x16, na.rm=T), x17 = sum(x17, na.rm=T), x18 = sum(x18, na.rm=T), x19 = sum(depchild, na.rm=T), x20 = sum(x20, na.rm=T), x21 = sum(x21, na.rm=T),
#                     # x22 = sum(x22, na.rm=T), #### x22 not in data so removed
#                     x23 = sum(dep13, na.rm=T), x24 = max(x24, na.rm=T), x25 = sum(dep17, na.rm=T), x26 = sum(dep18, na.rm=T), x27 = sum(x27, na.rm=T), x28 = sum(x28, na.rm=T), x29 = min(serial), x30 = min(pernum))

#### updating this section with modern syntax
concat <- ipum %>% group_by(x2, x1) %>% 
  summarise(x3 = mean(x3, na.rm=T), x4 = sum(x4, na.rm=T),
            x5 = sum(x5, na.rm=T), x6 = max(x6, na.rm=T), x7 = sum(x7, na.rm=T), x8 = sum(x8, na.rm=T), x9 = sum(x9, na.rm=T),
            x10 = sum(x10, na.rm=T), x11 = sum(x11, na.rm=T), x12 = sum(x12, na.rm=T), x13 = sum(x13, na.rm=T), x14 = sum(x14, na.rm=T), x15 = sum(x15, na.rm=T),
            x16 = sum(x16, na.rm=T), x17 = sum(x17, na.rm=T), x18 = sum(x18, na.rm=T), x19 = sum(depchild, na.rm=T), x20 = sum(x20, na.rm=T), x21 = sum(x21, na.rm=T),
            x22 = sum(x22, na.rm=T), #### x22 not in data but set to zero
            x23 = sum(dep13, na.rm=T), x24 = max(x24, na.rm=T), x25 = sum(dep17, na.rm=T), x26 = sum(dep18, na.rm=T), x27 = sum(x27, na.rm=T), x28 = sum(x28, na.rm=T), x29 = min(serial), x30 = min(pernum))



concat <- data.frame(concat)

concat <- concat[concat$x19 >= 0,]
concat <- concat[concat$x4 > 0, ]
vars <- paste0('x', 1:30) #No x22 causing issue

concat <- concat[,vars] #Now no x1 or x2

names(concat) <- c('taxsimid', 'year', 'state', 'mstat', 'depx', 'page', 'pwages', 'swages', 'dividends', 'otherprop', 'pensions', 'gssi', 'transfers', 'rentpaid', 'proptax', 
                   'otheritem', 'childcare', 'ui', 'depchild', 'mortgage', 'stcg', 'ltcg', 'dep13', 'sage', 'dep17', 'dep18', 'intrec', 'nonprop', 'serial', 'pernum')

ids <- data.frame(col1=concat$taxsimid, col2=concat$serial, col3=concat$pernum)
colnames(ids) <- c('RECID', 'serial', 'pernum')

concat <- concat[,c(1:4,6,24,5,23,25,26,7:9,27,21,22,10,28,11:12,18,13:17,20)]

# setwd(wdtaxsim)

#NBER file throwing an error for negative values here
concat %>% filter(taxsimid==4401) #taxsimid: otherprop= -8000 and rentpaid = -8000
#Trying to restrict to positive values
concat <- concat %>% 
  mutate(across(.cols = everything(),
                .fns = ~ ifelse(.x < 0 , 0, .x)))

#Need to make sure I'm crosswalking to the right state variable here
concat <- concat %>% left_join(state_crosswalk_soi,  by = c("state" = "census_id")) %>% 
  mutate(state=tax_id) %>% select(-tax_id)

#concat <- concat %>% mutate(state=0)
####I think this needs further editing to 
concat %>% count(mstat)
#Updated to c19
write.table(concat, sep = " ", file = here("outputs","c19_taxsim"), row.names = F, col.names = F)
concat
head(concat)
write_csv(concat, here("outputs","c19_taxsim.csv"))

# The one after this is throwing an error...
concat %>% filter(taxsimid==206401)
concat %>% mutate(row_number()) %>% filter(taxsimid==206401)
concat %>% mutate(row_number=row_number()) %>% filter(row_number==1736) #NA for state causing issue



write.table(ids, sep = ",", file = here("outputs","ids.csv"), row.names = F, col.names = T)
