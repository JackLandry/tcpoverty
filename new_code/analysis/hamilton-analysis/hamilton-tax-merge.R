
hamilton_analysis_data <- read_rds(here("inter_data","ipum_hamilton_for_analysis.Rds"))

#Need to review what is going on with the tax calculator setup, check out the google doc
#Status quo ta policy
status_quo_poicy <- read_csv(here("tax-calculator-code","c17_taxsim-17-#-#-#.csv"))
#IDs
ids <- read_csv(here("outputs","ids.csv")) 
status_quo_poicy <- ids %>% left_join(status_quo_poicy) 
hamilton_analysis_data <- hamilton_analysis_data  %>% left_join(status_quo_poicy, by = c("serial","pernum"))
hamilton_analysis_data <- hamilton_analysis_data %>% mutate(inctot=ifelse(inctot==999999999,NA,inctot),
                                                            adjginc=ifelse(inctot==99999999,NA,adjginc))
#If aftertax income is missing in the tax data, fill in regular income?
#I think it's pretty important to understand what exactly the ta calculator is doing, esp. with couples vs. singles
#don't want to be do double counting income
#Analyze by adjusted gross income or not?
sum(is.na(hamilton_analysis_data$aftertax_income))
hamilton_analysis_data <- hamilton_analysis_data %>% 
  mutate(aftertax_income=ifelse(is.na(aftertax_income),adjginc,aftertax_income))
#Now we need to analyze after tax income by income

hamilton_analysis_data <- hamilton_analysis_data %>% 
  mutate(aftertax_income_hamilton=aftertax_income + hamilton_money,
         aftertax_income_no_hamilton=aftertax_income)

write_rds(hamilton_analysis_data, here("inter_data","hamilton_tax_merged_for_analysis.rds"))










