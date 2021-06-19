library(tidyverse)
library(ipumsr)
library(tidylog)
library(kableExtra)
library(stringr.tools)
library(here)
here()
#Tax units sent to calculator
ids <- read_csv(here("outputs","ids.csv")) 
#New policy results file after rnning UBI reform
new_policy <- read_csv(here("c17_taxsim-17-#-abc-act-ubi-nontaxable-#.csv"))
new_policy %>% summarize(mean(ubi)) #Not working!!!
#Status quo policy results file
status_quo_poicy <- read_csv(here("tax-calculator-code","c17_taxsim-17-#-#-#.csv"))

#Need to change this to keep all these output variabes, but add a prefix or something
names(new_policy) <- paste("reform", names(new_policy), sep = "_")

# new_policy <- new_policy %>% rename(new_pol_aftertax_income=raftertax_income)
names(status_quo_poicy) <- paste("status_quo", names(status_quo_poicy), sep = "_")
# status_quo_poicy <- status_quo_poicy %>%
#   rename(stats_quo_pol_aftertax_income=aftertax_income)

combo_data <- ids %>% left_join(status_quo_poicy, by = c("RECID" = "status_quo_RECID")) %>% 
  left_join(new_policy, by = c("RECID" = "reform_RECID"))
combo_data <- combo_data %>% mutate(diff_aftertax_income=reform_aftertax_income - status_quo_aftertax_income)
combo_data %>% summarize(mean(diff_aftertax_income))

#Now bring in CPS ASEC data raw data for analysis
ddi <- read_ipums_ddi(here("cps_data","cps_00042.xml"))
ipum <-  read_ipums_micro(ddi)
names(ipum) <- tolower(names(ipum))


#"Not every ASEC record became a tax unit, so make sure you keep master non-matches" (From Earnie)
#So ASEC records that are not tax units had no change in income
#Also, why didn't they become tax units, how is nonfiling handled?
ipum <- ipum  %>% left_join(combo_data, by = c("serial","pernum"))