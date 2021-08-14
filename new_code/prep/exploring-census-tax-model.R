library(here)
library(tidyverse)
library(ipumsr)
ddi <- read_ipums_ddi(here("cps_data","cps_00046.xml"))
ipum_raw <-  read_ipums_micro(ddi)
names(ipum_raw) <- tolower(names(ipum_raw))

ipum_clean <- ipum_raw %>% mutate(nonfiler=ifelse(filestat==6,1,
                                    ifelse(filestat==0,NA,0)),
                                  wage_income=ifelse(incwage!=99999999,incwage,NA),
                                  total_income=ifelse(inctot==999999999,NA,inctot),
                                  adj_gross_inc=ifelse(adjginc==99999999,NA,adjginc))


#Obviously children are nonfilers, need to know if there parents are nonfilers which would come from the tax ID
ipum_clean %>% mutate(poor=ifelse(spmtotres<spmthresh,1,0)) %>% filter(poor==1) %>% 
  summarise(mean(nonfiler))



#If nonfiler, adjusted gross inc = 0
#Most ninfilers have almost no earnings, which comes from an assumption in the CPS-ASEC not grounded in reality
ipum_clean %>% filter(nonfiler==1 & wage_income <100000 & age>=18) %>% 
  ggplot(aes(x = wage_income)) +
  geom_density()

ipum_clean %>% filter(nonfiler==1 & wage_income <100000 & wage_income!=0) %>% select(wage_income)

#If you use the tax model, you have to deal with nonfilers
ipum_clean %>% count(filestat)
ipum_clean %>% filter(filestat==6) %>% count(depstat) #Nonfilers are still getting dependency pointers
ipum_clean %>% filter(filestat==6)
ipum_clean %>% count(depstat) #Points to line number of primarily filer
#Data checking has revealed dramatic shifts across time in the proportion of persons who are dependent, 
#which may indicate inaccuracies in the data. 
#Researchers should exercise caution in using DEPSTAT until the cause of these problems can be determined.
ipum_clean %>% count(pernum)

