
#The general idea here is to transform the CPS data into tax return data for tax units, which is the data which the tax calculator takes
#Tax units are not individuals, e.g. couple filing jointly with children
#You have to construct households than sum some variables like income for the tax calculator
#The first step is to run them though the NBER tax simulation model, THEN the other thing
library(tidyverse)
library(ipumsr)
library(tidylog)
library(here)
here()


#Import CPS data keeping some handly labels
ddi <- read_ipums_ddi(here("cps_data","cps_00040.xml"))
ipum <-  read_ipums_micro(ddi)

# set to lower case
names(ipum) <- tolower(names(ipum))

#Set missing data to 0 for many variables
ipum <- ipum %>% mutate(eitcred=ifelse(eitcred==9999,0,eitcred),
                fica=ifelse(fica==99999,0,fica)) %>% 
  mutate(across(c(fedretir,incss,incwelfr,incchild,incunemp,inceduc,incssi),
                ~ifelse(.==999999,0,.))) %>% 
  mutate(across(c(incsurv,incdisab,incint,incvet,incdivid,incrent,incasist,incother,taxinc,statetax,stataxac), #These next two steps don't change anything, did I get the numbers right
                ~ifelse(.==9999999,0,.))) %>% 
  mutate(across(c(incretir,incbus,incfarm,incwage,incdivid,incint,adjginc,incvet,incbus,fedtax),
                ~ifelse(.==99999999,0,.))) 

#gotveduc gotvothe gotvpens gotvsurv are yes no variables, eventually commented out in Ernie code

# set 0's to NA for these variables
ipum <- ipum %>% mutate(across(c(momloc,poploc,sploc),
                       ~ifelse(.==0,NA,.)))

#SPLOC is a constructed variable that indicates whether the person's spouse lived in the same household and, 
#if so, gives the person number (PERNUM) of the spouse.

ipum %>% count(sploc)
ipum %>% count(pernum)

#creating many new variabes
ipum <- ipum %>% mutate(year_n1=year-1,
                state=statefip,
                maritial_status=1, #Subsequently summed
                #age if person number of spouse is greater than person number?
                #I think that means age of the tax filer, lower person number = head of HH
                primary_taxpayer_age=ifelse((is.na(sploc)) | (sploc > 0 & sploc > pernum),age,0), 
                #Age if person number of spouse is less than person number
                #I think this means age 
                spouse_age=ifelse((!is.na(sploc) & sploc > 0 & sploc < pernum), age,0),
                #total income from wages, buisness, and farm if person number of spouse is greater than person number
                primary_taxpayer_income=ifelse((is.na(sploc)) | (sploc > 0 & sploc > pernum),incwage + incbus + incfarm,0),
                #total income from wages, buisness, and farm if person number of spose is less than person number
                spouse_income=ifelse((!is.na(sploc) & sploc > 0 & sploc < pernum),incwage + incbus + incfarm,0),
                dividends=incdivid,
                #Not including income from alimony, which is probably included in "other"
                #Not sure if this is accurately programmed in original
                interest=incrent+incother, 
                x11=incretir,
                x12=incss,
                x27=incint,
                x28=0,
                x13=incwelfr+incwkcom+incvet+incsurv+incdisab+incchild+inceduc+incssi+incasist, #Most assistance income
                x14=incrent,
                x15=0,#incalim
                pexemp = 4050,
                # Adjusted gross income - sum of personal exemption, property, state tax, and taxable income
                x16=adjginc-sum(pexemp,proptax,statetax,taxinc),#Personal exemption, depends on year 
                x16=ifelse(x16<0,0,x16), #no negatives for x16
                x17=0,
                x18=incunemp,
                x19=0,
                x20=0,
                x21=0)

ipum <- ipum %>% mutate(sploc=ifelse(relate == 1114,NA,sploc)) #If relationship is unmarried partner, set sploc to 0

ipum <- ipum %>% mutate(hnum=ifelse(relate==101,pernum,NA)) #If head of household, hnum is set to pernum, otherwise missing

#Checking if income is less than personal exsmpltion amount
ipum <- ipum %>% mutate(notself=ifelse((primary_taxpayer_income+spouse_income+dividends+x10+x11+x12+x13)<=pexemp,1,0), #Doesn't include capital gains
                        #Summing income measures
                sum=primary_taxpayer_income+spouse_income+dividends+x10+x11+x12+x13, #Doesn't include capital gains
                #depstat=person line number of person who claimed the respondent as his/her dependent
                #Depstat=0 if depstat>0 and spouse is not missing or dependent line number = spouse line number
                depstat=ifelse(!is.na(sploc) & depstat>0 & depstat==sploc,0,depstat),
                #Dependent child indicator if 
                depchild=ifelse(depstat>0 & (!is.na(momloc) | !is.na(poploc)) & (age<18 | age<24 & schlcoll > 0),1,0),
                deprel = ifelse(depstat>0 & depchild==1,1,0),
                num_children_under_13 = ifelse((deprel==1 | depchild==1) & age<13,1,0),
                num_children_under_17 = ifelse((deprel==1 | depchild==1) & age<17,1,0),
                num_children_under_18 = ifelse((deprel==1 | depchild==1) & age<18,1,0))



# Here we output a record for each person, so that tax units can be formed 
# later by summing over person records. The taxunit id is the minimum of
# the pernum or sploc, so spouses will get the same id. For children
# it is the minimum of the momloc or poploc. Other relatives are made
# dependent on the household head (which may be incorrect) and non-relatives
# are separate tax units. 

#Creating seperate dataframe for dependents
dpndnts <- ipum %>% filter(depchild==1 | deprel==1)
dpndnts <- dpndnts %>% mutate(x1=0)

#What does this do
dpndnts[dpndnts$depchild == 1,]$x1 <- 100*dpndnts[dpndnts$depchild == 1,]$serial + apply(dpndnts[dpndnts$depchild == 1,][,c('momloc', 'poploc')], 1, min, na.rm=T)
##Not sure about this either
dpndnts[dpndnts$deprel == 1,]$x1 <- 100*dpndnts[dpndnts$deprel == 1,]$serial + dpndnts[dpndnts$deprel==1,]$hnum

dpndnts <- dpndnts %>% mutate(maritial_status=NA,number_of_dependents=1,age_spouse=0,x19=NA,x23=NA,x24=0)

#Creating a dataframe for nondependns
txpyrs <- ipum %>% filter(depchild==0 & deprel==0) %>% 
  mutate(x1=0, #Placeholder variables
         number_of_dependents=0,
         x23=NA)

#Unclear what this is doing
#x1 = 100 * 
txpyrs$x1 <- 100*txpyrs$serial + apply(txpyrs[,c('pernum', 'sploc')], 1, min, na.rm=T)

# set whats not x1, x2, or x5 in deps to NA
#Can do this tidyer
vars <- c('x3', paste0('x', 4), paste0('x', 7:23), 'x27', 'x28')
vars
dpndnts[,vars] <- NA
dpndnts <- dpndnts %>% select(-x22) #capital gains, not in txpyrs, causes issue in rbind


ipum <- rbind(txpyrs, dpndnts)
s
names(txpyrs)
names(dpndnts)

# sum value over tax #
#Group by year (x2), should rename that, and x1, which I think is an indicator of a tax unit
concat <- ipum %>% group_by(year_n1,x1) %>% 
  summarise(state = mean(state, na.rm=T), maritial_status = sum(maritial_status, na.rm=T),
            number_of_dependents = sum(number_of_dependents, na.rm=T), age_spouse = max(age_spouse, na.rm=T), primary_taxpayer_income = sum(primary_taxpayer_income, na.rm=T), spouse_income = sum(spouse_income, na.rm=T), dividends = sum(dividends, na.rm=T),
            x10 = sum(x10, na.rm=T), x11 = sum(x11, na.rm=T), x12 = sum(x12, na.rm=T), x13 = sum(x13, na.rm=T), x14 = sum(x14, na.rm=T), x15 = sum(x15, na.rm=T),
            x16 = sum(x16, na.rm=T), x17 = sum(x17, na.rm=T), x18 = sum(x18, na.rm=T), x19 = sum(depchild, na.rm=T), x20 = sum(x20, na.rm=T), x21 = sum(x21, na.rm=T),
            x23 = sum(dep13, na.rm=T), x24 = max(x24, na.rm=T), x25 = sum(dep17, na.rm=T), x26 = sum(dep18, na.rm=T), x27 = sum(x27, na.rm=T), x28 = sum(x28, na.rm=T), x29 = min(serial), x30 = min(pernum))


#Should be able to combine this without separating each line
means <- ipum %>% group_by(year_n1,x1) %>% 
  summarise(across(c(state,age_spouse), mean, na.rm=TRUE)) 

sums <- ipum %>% group_by(year_n1,x1) %>% 
  summarise(across(c(maritial_status,number_of_dependents,primary_taxpayer_income:x23,x27,x28), ~sum, na.rm=TRUE))

sums_custom <- ipum %>% group_by(year_n1,x1) %>% 
  summarise(x25 = sum(dep17, na.rm=T), x26 = sum(dep18, na.rm=T))

maxes <-  ipum %>% group_by(year_n1,x1) %>% 
  summarise(age_spouse=max(age_spouse,na.rm=TRUE))

mins <-  ipum %>% group_by(year_n1,x1) %>% 
  summarise(x29 = min(serial),
            x30 = min(pernum))

#x19 is sum of dependent children, x4 is sum of non-dependents, so removing households with no dependent children of no non-depedents either..
#Removes minimal number of HHs <1%
#Need to double check this, I think I got it wrong the first pass
concat <- concat %>% filter(!x19>=0 & !maritial_status>0)  
#Selecting variables x1-x30
vars <- paste0('x', 1:30)
concat <- concat %>% select(vars)

#Need to go back to number of dependents, I messed that up

#Maybe want to go back though to better name this stuff, but it's still weird
#Think the order got messed up a little bit here...
names(concat) <- c('taxsimid', 'year', 'state', 'mstat', 'depx', 'page', 'pwages', 
                   'swages', 'dividends', 'otherprop', 'pensions', 'gssi', 'transfers', 
                   'rentpaid', 'proptax', 'otheritem', 'childcare', 'ui', 'depchild', 
                   'mortgage', 'stcg', 'dep13', 'sage', 'dep17', 'dep18', #removing ltcg, captial gains (I guess need to set to 0)
                   'intrec', 'nonprop', 'serial', 'pernum') #Need to add scorp, pbusinc, pprofinc, sbusinc, sprofinc

#Need to rename all of these in preperation for NBER simulation, maybe I should keep these names instead



#1. taxsimid Case ID (arbitrary, but must be a non-negative numeric)
#2. yearTax year ending Dec 31(4 digits between 1960 and 2023, but state must be zero if year is before 1977 or after 2023. We don't have code for state laws before 1977.) State tax laws are effectively inflated by 2.5%/year after 2018.
#3. state (SOI codes. These run from 1 for Alabama to 51 for Wyoming and are not the Census or PSID codes. See state list,and also item two above.). Use zero for "no state tax calculation".
#4. mstat Marital Status 
    #1. single or head of household (unmarried) 2. joint (married) 
    #6. separate (married). Note that Married-separate is not usually desirable under US tax law. 
    #8. Dependent taxpayer. (Typically child with income). 
#5. page Age of primary taxpayer December 31st of the tax year (or zero).
#6. sage Age of spouse (or zero).
#7. depx Number of dependents (part of personal exemption calculation).
#8. dep13 Number of children under 13 with eligible child care expenses (Dependent Care Credit).
#9. dep17 Number of children under 17 for the entire tax year (Child Credit). This includes children under 13.
#10. dep18 Number of qualifying children for EITC. (Typically younger than 19 or younger than 24 and a full-time student). Note that a young child is counted in all three depNN variables.
#11. pwages Wage and salary income of Primary Taxpayer (include self-employment but no QBI).
#12. swages Wage and salary income of Spouse (include self-employment but no QBI). Note that this must be zero for non-joint returns. Watch out for this if you use current marital status and last year's income.
#13. dividends Dividend income (qualified dividends only for 2003 on).
#14. intrec Interest Received (+/-)
#15. stcg Short Term Capital Gains or losses. (+/-)
#16. ltcg Long Term Capital Gains or losses. (+/-)
#17. otherprop Other property income subject to NIIT (details at NBER site)
#18. nonprop Other non-property income not subject to Medicare NIIT (deatils at NBER site)
# 19. pensions Taxable Pensions and IRA distributions
# 20. gssi Gross Social Security Benefits
# 21. ui Unemployment compensation received.
# 22. transfers Other non-taxable transfer Income such as welfare workers comp veterans benefits child support that would affect eligibility for state property tax rebates but would not be taxable at the federal level. 
# 23. rentpaid Rent Paid (used only for calculating state property tax rebates)
# 24. proptax Real Estate taxes paid. This is a preference for the AMT and is is also used to calculate state property tax rebates.
# 25. otheritem Other Itemized deductions that are a preference for the Alternative Minimum Tax. #(Details on NBER website)
# 26. childcare Child care expenses.
# 27. mortgage Deductions not included in item 25 and not a preference for the AMT (Details on number site)
# 28. scorp Active S-Corp income (is SSTB).
# 29. pbusinc Primary Taxpayer's Qualified Business Income (QBI) 
# 30. pprofinc Primary Taxpayer's Specialized Service Trade or Business service (SSTB) with a preferential rate subject to claw-back. Subject to SECA and Medicare Additional Earnings Tax.
# 31. sbusinc Spouse's QBI. Must be zero for non-joint returns.
# 32. sprofinc Spouse's SSTB. Must be zero for non-joint returns. 






ids <- data.frame(col1=concat$taxsimid, col2=concat$serial, col3=concat$pernum)
colnames(ids) <- c('RECID', 'serial', 'pernum')

#What is this doing? #Removed two variables and reordered things
#Removes the 19th variable (depchild) and 29th variable (pernum) #Need to double check that after removing capital gains (order issues)
concat
concat <- concat[,c(1:4,6,24,5,23,25,26,7:9,27,21,22,10,28,11:12,18,13:17,20)]
concat


write.table(concat, sep = " ", file = "c17_taxsim", row.names = F, col.names = F)

write.table(ids, sep = ",", file = "ids.csv", row.names = F, col.names = T)