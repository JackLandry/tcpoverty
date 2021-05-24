#Analysis


ids <- read_csv(here("outputs","ids.csv"))
new_policy <- read_csv(here("tax-calculator-code","c17_taxsim-17-#-policy_alt-#.csv"))
status_quo_poicy <- read_csv(here("tax-calculator-code","c17_taxsim-17-#-#-#.csv"))
ids
new_policy <- new_policy %>% select(RECID,aftertax_income) %>% rename(new_pol_aftertax_income=aftertax_income)
status_quo_poicy <- status_quo_poicy %>% select(RECID,aftertax_income) %>% rename(stats_quo_pol_aftertax_income=aftertax_income)

combo_data <- ids %>% left_join(status_quo_poicy) %>% left_join(new_policy)

#Now have to bring in CPS ASEC data... Is it the raw data?


