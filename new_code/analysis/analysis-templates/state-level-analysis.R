#I'm currently using the tax calculator data, do I need to run stuff though the tax calculator three times?


#NEed to get the three years of data from ipums
ddi <- read_ipums_ddi(here("cps_data","cps_00044.xml")) #If I changes both names does the import still work?
ipum <-  read_ipums_micro(ddi)
names(ipum) <- tolower(names(ipum))

#This doesn't make the best test of CIs since the numbers are so low
library(srvyr)
ipum_spmwt <- ipum %>%
  as_survey_design(weights = spmwt)
baseline_poverty_by_state <- ipum_spmwt %>% group_by(state_name) %>% 
  summarise(post_reform_poverty_state=survey_mean(baseline_poverty, vartype = "ci"))
baseline_poverty_by_state



#Basic poverty map
library(statebins)
baseline_poverty_by_state <- baseline_poverty_by_state %>% mutate(state=as.character(state_name))
statebins(baseline_poverty_by_state, value_col = "post_reform_poverty_state",
          name = "Poverty Rate",
          palette = "OrRd", 
          direction = 1) +
  labs(title="") +
  theme_statebins(legend_position = "right", base_family="LM Roman 10")
#scale_x_continuous(labels = scales::percent_format()) +
#scale_y_continuous(n.breaks=7)
