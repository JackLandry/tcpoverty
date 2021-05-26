#Analysis


ids <- read_csv(here("outputs","ids.csv"))
new_policy <- read_csv(here("tax-calculator-code","c17_taxsim-17-#-policy_alt-#.csv"))
status_quo_poicy <- read_csv(here("tax-calculator-code","c17_taxsim-17-#-#-#.csv"))
ids
new_policy <- new_policy %>% select(RECID,aftertax_income) %>% rename(new_pol_aftertax_income=aftertax_income)
status_quo_poicy <- status_quo_poicy %>% select(RECID,aftertax_income) %>% rename(stats_quo_pol_aftertax_income=aftertax_income)

combo_data <- ids %>% left_join(status_quo_poicy) %>% left_join(new_policy)
combo_data <- combo_data %>% mutate(diff_aftertax_income=new_pol_aftertax_income - stats_quo_pol_aftertax_income)
#Now have to bring in CPS ASEC data... Is it the raw data?
ddi <- read_ipums_ddi(here("cps_data","cps_00041.xml"))
#ddi <- read_ipums_ddi("/Users/jacklandry/Documents/GitHub/tcpoverty/cps_data/cps_00040.xml")
ipum <-  read_ipums_micro(ddi)
names(ipum) <- tolower(names(ipum))
ipum <- ipum %>% select(year,age,serial,pernum,spmwt,spmthresh,spmtotres,spmfamunit)

ipum <- ipum  %>% left_join(combo_data, by = c("serial","pernum"))

#Need to be careful about how missings are treated
ipum <- ipum %>% group_by(spmfamunit) %>% 
  mutate(diff_aftertax_income_family=sum(diff_aftertax_income)) %>% 
  ungroup()

ipum <- ipum %>% mutate(spm1 = spmtotres < spmthresh,
                spm2 = (spmtotres + diff_aftertax_income_family) < spmthresh,
                out_of_spm = ifelse((spm2 == 0 & spm1 == 1),1,0),
                out_of_spm = ifelse(is.na(out_of_spm),0,out_of_spm)) #I think this is right but not sure 

#Need to get the total HH numbers too
ipum %>% summarise(spm1_mean = weighted.mean(spm1, spmwt),
                   spm2_mean = weighted.mean(spm1, spmwt),
                   out_of_spm_mean = weighted.mean(out_of_spm, spmwt))

ggplot



