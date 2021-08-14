###UBI 3k/year to every person with phase out same as child tax credit
###Kind of like how much more does it cost to include adults after the CTC


library(tidylog)
ddi <- read_ipums_ddi(here("cps_data","cps_00053.xml"))
ipum_raw <-  read_ipums_micro(ddi)
names(ipum_raw) <- tolower(names(ipum_raw))

#The CPS ASEC data has information on every individual in a household
#Each individual in a household is identified by a person number, pernum
#Family units for supplemental poverty calculations are identified by spmfamunit
#sploc is the person number of the spouse


ipum <- ipum_raw %>% mutate(single=ifelse(sploc==0,1,0),
                            spouse=ifelse(sploc!=0,1,0))

#Need combined income of couples for eligability, can just give everyone else money I guess
#Should probably talk to Steve before going too far
ipum