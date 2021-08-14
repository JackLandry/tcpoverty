
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

#Creating poverty ranges
hamilton_analysis_data <- hamilton_analysis_data %>% mutate(deep_poverty=spmthresh*.5,
                        baseline_poverty = spmtotres < spmthresh,
                        baseline_poverty_2x = spmtotres < spmthresh*2,
                        baseline_poverty_3x = spmtotres < spmthresh*3,
                        baseline_poverty_1_2x = spmtotres > spmthresh & spmtotres < spmthresh*2,
                        baseline_poverty_2_3x = spmtotres > spmthresh*2 & spmtotres < spmthresh*3,
                        baseline_poverty_3_5x = spmtotres > spmthresh*3 & spmtotres < spmthresh*5,
                        baseline_poverty_5x_plus = spmtotres > spmthresh*5)


hamilton_analysis_data <- hamilton_analysis_data %>% mutate(
                        post_reform_poverty = (spmtotres + diff_aftertax_income_family_hamilton) < spmthresh,
                        post_reform_poverty_2x = (spmtotres + diff_aftertax_income_family_hamilton) < (spmthresh*2),
                        post_reform_poverty_1_2x = ((spmtotres + diff_aftertax_income_family_hamilton) < (spmthresh*2)) &
                          ((spmtotres + diff_aftertax_income_family_hamilton) >= (spmthresh)),
                        post_reform_poverty_3x = (spmtotres + diff_aftertax_income_family_hamilton) < (spmthresh*3),
                        post_reform_poverty_2_3x = ((spmtotres + diff_aftertax_income_family_hamilton) >= (spmthresh*2)) &
                          ((spmtotres + diff_aftertax_income_family_hamilton) < (spmthresh*3)),
                        post_reform_poverty_3_5x = ((spmtotres + diff_aftertax_income_family_hamilton) >= (spmthresh*3)) &
                          ((spmtotres + diff_aftertax_income_family_hamilton) < (spmthresh*5)),
                        post_reform_poverty_5x_plus =  ((spmtotres + diff_aftertax_income_family_hamilton) >= (spmthresh*5)))



####writing new functions for hamilton.... 
#need to do a summarise across
poverty_by_group_filter <- function(group) {
  hamilton_analysis_data %>% filter({{ group }}==1) %>% 
    summarise(across(baseline_poverty:post_reform_poverty_5x_plus, ~weighted.mean(.x,spmwt))) 
  }

poverty_by_group <- function(group) {
  hamilton_analysis_data %>% group_by({{ group }}) %>% 
    summarise(across(baseline_poverty:post_reform_poverty_5x_plus, ~weighted.mean(.x,spmwt))) 
}


#Making variables to calculate poverty rates over
hamilton_analysis_data <- hamilton_analysis_data %>% mutate(child=ifelse(age<18,1,0),
                                                            everyone=1)
#Ignoring multi-racial here
hamilton_analysis_data <- hamilton_analysis_data %>% 
  mutate(race_bin=ifelse(race==100 & hispan==0,"White",
                         ifelse(race==200 & hispan==0,"Black",
                                ifelse(race==651 & hispan==0,"Asian",
                                       ifelse(race==100 & hispan!=0,"Hispanic","Other")))))

hamilton_analysis_data <- hamilton_analysis_data %>% 
  mutate(employment_status=ifelse(empstat<10,NA,
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

#applying functions
overall_poverty <- poverty_by_group_filter(everyone) %>% mutate(Group = "Overall")
child_poverty <- poverty_by_group_filter(child) %>% mutate(Group = "Children")
poverty_by_race <- poverty_by_group(race_bin)
poverty_by_race <- poverty_by_race %>% rename(Group=race_bin)
poverty_by_employment_status <- poverty_by_group(employment_status)
poverty_by_fam_unit_size <- poverty_by_group(spm_fam_unit_size_cap)
poverty_by_fam_unit_size
#Function to make nice tables, need to edit for this stuff
names(combo_poverty)
table_maker <- function(dataframe,group_var,group_name) {
  dataframe %>% rename(`Status Quo Poverty` = baseline_poverty, #Probably could do this more effiecently
                       `Status Quo Poverty 2x` = baseline_poverty_2x,
                       `Status Quo Poverty 3x` = baseline_poverty_3x,
                       `Status Quo Poverty 1-2x` = baseline_poverty_1_2x,
                       `Status Quo Poverty 2-3x` = baseline_poverty_2_3x,
                       `Status Quo Poverty 3-5x` = baseline_poverty_3_5x,
                       `Status Quo Poverty 5x+` = baseline_poverty_5x_plus,
                       `Post Reform Poverty` = post_reform_poverty,
                       `Post Reform Poverty 2x` = post_reform_poverty_2x,
                       `Post Reform Poverty 3x` = post_reform_poverty_3x,
                       `Post Reform Poverty 1-2x` = post_reform_poverty_1_2x,
                       `Post Reform Poverty 2-3x` = post_reform_poverty_2_3x,
                       `Post Reform Poverty 3-5x` = post_reform_poverty_3_5x,
                       `Post Reform Poverty 5x+` = post_reform_poverty_5x_plus,
                       {{group_name}} := {{group_var}}) %>% 
    mutate(across(where(is.numeric), round, 3)) %>% 
    mutate(across(where(is.numeric), ~ .x*100)) %>% 
    mutate(across(where(is.numeric),as.character)) %>% 
    mutate(across(`Status Quo Poverty`:`Post Reform Poverty 5x+`, ~str_suffix(.x,"%"))) %>% 
    relocate({{group_name}})
}
#Applying function and a bit of manual cleanup
poverty_by_fam_unit_size_table <- table_maker(poverty_by_fam_unit_size,spm_fam_unit_size_cap,`Family Size`)

combo_poverty <- bind_rows(overall_poverty,child_poverty,poverty_by_race) 

combo_poverty_table <- table_maker(combo_poverty,Group,Group)

poverty_by_employment_status_table <- table_maker(poverty_by_employment_status,employment_status,`Employment Status`)

#This doesn't actually work well, but I guess I can just restrict the columns
#Also need to order status quo, then reform, can use select
combo_poverty_table %>%
  kbl() %>%
  kable_minimal() %>% 
  kable_styling(html_font = "Unna",
                full_width = FALSE,
                position = "center")


#Making stacked bar charts showing poverty impact for different groups
names(combo_poverty)
#Maybe make this part a function that I just apply for different groupings
bar_graph_prepper <- function(df) {
  df %>% select(-c(baseline_poverty_2x,baseline_poverty_3x,post_reform_poverty_2x,post_reform_poverty_3x)) %>% 
    pivot_longer(cols = baseline_poverty:post_reform_poverty_5x_plus) %>% 
    mutate(baseline=ifelse(str_detect(name, "baseline"),"Baseline","Reform")) %>% 
    mutate(name=str_remove(name,"baseline_"),
           name=str_remove(name,"post_reform_"),
           name=ifelse(name=="poverty","Baseline Poverty",
                       ifelse(name=="poverty_1_2x", "1-2X Poverty",
                              ifelse(name=="poverty_2_3x", "2-3X Poverty",
                                     ifelse(name=="poverty_3_5x", "3-5X Poverty",
                                            ifelse(name=="poverty_5x_plus","5X+ Poverty",NA))))),
           name=fct_relevel(name,"Baseline Poverty")) #Possible reverse the order?
}
combo_poverty_for_bar_chart <- bar_graph_prepper(combo_poverty)


combo_poverty_for_bar_chart %>% filter(Group!="Children") %>% 
  mutate(Group=fct_relevel(Group, "Overall", "White")) %>% 
  ggplot(aes(fill=name, y=value, x=baseline)) + #need to modify the orders of things and remove chilren, make that a seperate graph
  geom_bar(position="stack", stat="identity", alpha = .5) +
  facet_grid(~ Group) + #this works, but better to have it sie by side right?
  #Maybe change the group variable, but i want it 
  scale_fill_brewer(palette = "Set1", name = "") +
  scale_color_grey() +
  theme_jfi() +
  labs(y = "Percent\nIn\nPoverty\nGroup",
       x = "",
       title = "Poverty Impacts of Hamilton Program\n") +
  scale_y_continuous(labels=scales::percent_format(), n.breaks = 5) 

combo_poverty_for_bar_chart %>% filter(Group=="Children" | Group=="Overall") %>% 
  mutate(Group=fct_relevel(Group, "Overall", "Children")) %>% 
  ggplot(aes(fill=name, y=value, x=baseline)) + 
  geom_bar(position="stack", stat="identity", alpha = .5) +
  facet_grid(~ Group) + 
  scale_fill_brewer(palette = "Set1", name = "") +
  scale_color_grey() +
  theme_jfi() +
  labs(y = "Percent\nIn\nPoverty\nGroup",
       x = "",
       title = "Poverty Impacts of Hamilton Program\n") +
  scale_y_continuous(labels=scales::percent_format(), n.breaks = 5) 


poverty_by_employment_status_bar_chart <- bar_graph_prepper(poverty_by_employment_status)
poverty_by_employment_status_bar_chart
#How did I get the percent in value, maybe I need to not go though table maker
poverty_by_employment_status_bar_chart %>% filter(employment_status!="NA")  %>% 
  ggplot(aes(fill=name, y=value, x=baseline)) + 
  geom_bar(position="stack", stat="identity", alpha = .5) +
  facet_grid(~ employment_status) +
  scale_fill_brewer(palette = "Set1", name = "") +
  scale_color_grey() +
  theme_jfi() +
  labs(y = "Percent\nIn\nPoverty\nGroup",
       x = "",
       title = "Poverty Impacts of Hamilton Program\n") +
  scale_y_continuous(labels=scales::percent_format(), n.breaks = 5) 


poverty_by_fam_unit_size_bar_chart <- bar_graph_prepper(poverty_by_fam_unit_size)
poverty_by_fam_unit_size_bar_chart #Maybe write out family unit size

poverty_by_fam_unit_size_bar_chart %>%  
  mutate(spm_fam_unit_size_cap=str_glue("Family\nSize: {spm_fam_unit_size_cap}")) %>% 
  ggplot(aes(fill=name, y=value, x=baseline)) + 
  geom_bar(position="stack", stat="identity", alpha = .5) +
  facet_wrap(~ spm_fam_unit_size_cap, nrow = 2) + 
  scale_fill_brewer(palette = "Set1", name = "") +
  scale_color_grey() +
  theme_jfi() +
  labs(y = "Percent\nIn\nPoverty\nGroup",
       x = "",
       title = "Poverty Impacts of Hamilton Program\n") +
  scale_y_continuous(labels=scales::percent_format(), n.breaks = 5) 

#Ok, this was nice, now what other graphs to make


#Think about other viz possibilities, maybe like a line grpah type thing
# 
# combo_poverty %>% pivot_longer(cols = baseline_poverty:post_reform_poverty_5x_plus) %>% 
#   mutate(baseline=ifelse(str_detect(name, "baseline"),"Baseline","Reform")) %>% 
#   mutate(name=str_remove(name,"baseline_"),
#          name=str_remove(name,"post_reform_")) %>% #Need to learn how to make this one line
#   pivot_wider(names_from = baseline, values_from = value) %>% #I think I might actually need to make it one line


#effect on median income since that's easy
#illustrates how big this is relative to Hamilton
hamilton_analysis_data %>% 
  summarise(aftertax_fam_income_median_hamilton=median(diff_aftertax_income_family_hamilton),
            aftertax_fam_income_median=median(diff_aftertax_income_family_no_hamilton)) %>% 
  mutate(change_family_income=aftertax_fam_income_median_hamilton-aftertax_fam_income_median)



poverty_by_employment_status <- poverty_by_employment_status %>% filter(!is.na(`Employment Status`))