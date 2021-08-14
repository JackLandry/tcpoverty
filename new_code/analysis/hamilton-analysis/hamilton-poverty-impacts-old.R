
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
  mutate(aftertax_income=ifelse(is.na(aftertax_income),0,aftertax_income))
#Now we need to analyze after tax income by income

hamilton_analysis_data <- hamilton_analysis_data %>% 
  mutate(aftertax_income_hamilton=aftertax_income + hamilton_money,
         aftertax_income_no_hamilton=aftertax_income)

hamilton_analysis_data <- hamilton_analysis_data %>% group_by(spmfamunit) %>% 
  mutate(diff_aftertax_income_family_hamilton=sum(aftertax_income_hamilton),
         diff_aftertax_income_family_no_hamilton=sum(aftertax_income_no_hamilton)) %>% 
  ungroup()

hamilton_analysis_data <- hamilton_analysis_data %>% mutate(deep_poverty=spmthresh*.5,
                        baseline_poverty = spmtotres < spmthresh,
                        post_reform_poverty = (spmtotres + diff_aftertax_income_family_hamilton) < spmthresh,
                        out_of_poverty = ifelse((post_reform_poverty == 0 & baseline_poverty == 1),1,0),
                        out_of_poverty = ifelse(is.na(out_of_poverty),0,out_of_poverty),
                        baseline_deep_poverty=spmtotres < deep_poverty,
                        post_reform_deep_poverty=(spmtotres + diff_aftertax_income_family_hamilton) < deep_poverty,
                        out_of_deep_poverty = ifelse((post_reform_deep_poverty == 0 & baseline_deep_poverty == 1),1,0),
                        out_of_deep_poverty = ifelse(is.na(out_of_deep_poverty),0,out_of_deep_poverty)) 

poverty_by_group_filter <- function(group) {
  hamilton_analysis_data %>% filter({{ group }}==1) %>% summarise(
    baseline_poverty_mean = weighted.mean(baseline_poverty, spmwt),
    reform_poverty_mean = weighted.mean(post_reform_poverty, spmwt),
    reform_poverty_reduction = weighted.mean(out_of_poverty, spmwt),
    baseline_deep_poverty_mean = weighted.mean(baseline_deep_poverty,spmwt),
    reform_deep_poverty_mean = weighted.mean(post_reform_deep_poverty,spmwt),
    reform_deep_poverty_reduction = weighted.mean(out_of_deep_poverty,spmwt)) 
}
poverty_by_group <- function(group) {
  hamilton_analysis_data %>% group_by({{ group }}) %>% summarise(
    baseline_poverty_mean = weighted.mean(baseline_poverty, spmwt),
    reform_poverty_mean = weighted.mean(post_reform_poverty, spmwt),
    reform_poverty_reduction = weighted.mean(out_of_poverty, spmwt),
    baseline_deep_poverty_mean = weighted.mean(baseline_deep_poverty,spmwt),
    reform_deep_poverty_mean = weighted.mean(post_reform_deep_poverty,spmwt),
    reform_deep_poverty_reduction = weighted.mean(out_of_deep_poverty,spmwt)) 
}

#Making variables to calculate poverty rates over
hamilton_analysis_data <- hamilton_analysis_data %>% mutate(child=ifelse(age<18,1,0),
                        everyone=1)
#Ignoring multi-racial here
hamilton_analysis_data <- hamilton_analysis_data %>% mutate(race_bin=ifelse(race==100 & hispan==0,"White",
                                        ifelse(race==200 & hispan==0,"Black",
                                               ifelse(race==651 & hispan==0,"Asian",
                                                      ifelse(race==100 & hispan!=0,"Hispanic","Other")))))

hamilton_analysis_data <- hamilton_analysis_data %>% mutate(employment_status=ifelse(empstat<10,NA,
                                                 ifelse(empstat>=10 & empstat<=12,"Has Job",
                                                        ifelse(empstat>=21 & empstat<=22,"Unenployed",
                                                               ifelse(empstat==32, "Unable to work",
                                                                      ifelse(empstat==36, "Retired",NA))))))
#Household size
spm_fam_unit_size_df <- hamilton_analysis_data %>% group_by(spmfamunit) %>% count() %>% rename(spm_fam_unit_size=n)

hamilton_analysis_data <- hamilton_analysis_data %>% left_join(spm_fam_unit_size_df)
hamilton_analysis_data %>% group_by(spm_fam_unit_size) %>% count() #Need to cut it off at a certain size
hamilton_analysis_data <- hamilton_analysis_data %>% mutate(spm_fam_unit_size_cap=ifelse(spm_fam_unit_size>=8,8,spm_fam_unit_size),
                        spm_fam_unit_size_cap=as.character(spm_fam_unit_size_cap),
                        spm_fam_unit_size_cap=ifelse(spm_fam_unit_size_cap==8,"8+",spm_fam_unit_size_cap))

#Maybe I can do a function for grouping rather than just filtering
child_poverty <- poverty_by_group_filter(child) %>% mutate(Group = "Children")
overall_poverty <- poverty_by_group_filter(everyone) %>% mutate(Group = "Overall")
poverty_by_race <- poverty_by_group(race_bin)
poverty_by_race <- poverty_by_race %>% rename(Group=race_bin)
poverty_by_employment_status <- poverty_by_group(employment_status)
poverty_by_fam_unit_size <- poverty_by_group(spm_fam_unit_size_cap)

#Function to make nice tables
table_maker <- function(dataframe,group_var,group_name) {
  dataframe %>% rename(`Status Quo Poverty` = baseline_poverty_mean,
                       `Reform Poverty` = reform_poverty_mean,
                       `Poverty Reduction` = reform_poverty_reduction,
                       `Status Quo Deep Poverty` = baseline_deep_poverty_mean,
                       `Reform Deep Poverty` = reform_deep_poverty_mean,
                       `Deep Poverty Reduction` = reform_deep_poverty_reduction,
                       {{group_name}} := {{group_var}}) %>% 
    mutate(across(where(is.numeric), round, 3)) %>% 
    mutate(across(where(is.numeric), ~ .x*100)) %>% 
    mutate(across(where(is.numeric),as.character)) %>% 
    mutate(across(`Status Quo Poverty`:`Deep Poverty Reduction`, ~str_suffix(.x,"%"))) %>% 
    relocate({{group_name}})
  
}
#Applying function and a bit of manual cleanup
poverty_by_fam_unit_size <- table_maker(poverty_by_fam_unit_size,spm_fam_unit_size_cap,`Family Size`)

combo_poverty <- bind_rows(overall_poverty,child_poverty,poverty_by_race) 
combo_poverty <- table_maker(combo_poverty,Group,Group)
poverty_by_employment_status <- table_maker(poverty_by_employment_status,employment_status,`Employment Status`)

combo_poverty %>%
  kbl() %>%
  kable_minimal() %>% 
  kable_styling(html_font = "Unna",
                full_width = FALSE,
                position = "center")

#effect on median income since that's easy
#illustrates how big this is
hamilton_analysis_data %>% 
  summarise(aftertax_fam_income_median_hamilton=median(diff_aftertax_income_family_hamilton),
            aftertax_fam_income_median=median(diff_aftertax_income_family_no_hamilton)) %>% 
  mutate(change_family_income=aftertax_fam_income_median_hamilton-aftertax_fam_income_median)



poverty_by_employment_status <- poverty_by_employment_status %>% filter(!is.na(`Employment Status`))