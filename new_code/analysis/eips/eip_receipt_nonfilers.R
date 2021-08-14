#Investigating EIP by AGI compared to census and maybe SOI?

#Source: https://www.irs.gov/statistics/soi-tax-stats-coronavirus-aid-relief-and-economic-security-act-cares-act-statistics#EIP3
library(tidyverse)
library(tidylog)
eip_data <- read_csv("~/Dropbox/JFI/irs-soi-data/eip-payments/EIP payments round 3 agi.csv")

returns_2018 <- readxl::read_xls("~/Dropbox/JFI/irs-soi-data/2018_returns_by_agi.xls", col_names = FALSE)
View(returns_2018)
#Want to
returns_2018_clean <- returns_2018 %>% rename(num_returns=...2, agi_cat=...1) %>% 
  filter(!is.na(num_returns) & !is.na(agi_cat)) %>% 
  filter(agi_cat!="All returns" & num_returns!="All returns") %>% 
  filter(row_number()<20) %>% select(agi_cat,num_returns)
returns_2018_clean %>% filter(row_number()<10) %>% mutate(num_returns=as.numeric(num_returns)) %>% 
  summarise(total_returns_under_75k=sum(num_returns)) #88.9M returns under 75k

eip_data %>% filter((row_number()<10 | row_number()==13) & row_number()!=1) %>% 
  summarise(total_payents_under_75k=sum(num_payments)) #138.9M payments under 75k


#17,614,076 Social Secuity
#2,888,543 SSI
#403,265 VA
#59,196 RR
#5,480,702 Get my payment

#