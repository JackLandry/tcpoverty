library(janitor)
library(readxl)


ctc_data <- read_excel("~/Dropbox/JFI/irs-soi-data/ctc-unclaimed/Estimated-Counts-of-Children-Unclaimed-for-CTC-by-ZIP-Code-2019.xlsx", sheet = "Table")

ctc_data <- ctc_data %>% row_to_names(row_number = 1) 

ctc_data <- ctc_data %>% rename(zip=`ZIP Code`,
                    adults_claiming_missing_children=`Policy Holders`,
                    ctc_children=`Children [1-5]`) %>% 
  mutate(across(adults_claiming_missing_children:ctc_children,as.numeric))

ctc_data %>% filter(zip=="Other")
ctc_data %>% filter(State=="NJ") #Missing leading zeros, need to insert
ctc_data <- ctc_data %>% mutate(zip=str_pad(zip, 5, side = "left", pad = "0"))
ctc_data <- ctc_data %>% filter(zip!="Other")
ctc_data %>% filter(zip=="07040")
ctc_data %>% filter(zip=="07052")
library(tidycensus)
v17 <- load_variables(2019, "acs5", cache = TRUE)
v17 %>% filter(label=="Estimate!!Total:") %>% View()
View(v17)

zip_code_variables <- get_acs(geography = "zcta",
        variables = c("B06011_001","B09001_001","B17001_001","B01001_001"), 
        data = "acs5")
zip_code_variables <- zip_code_variables %>% 
  pivot_wider(names_from = "variable", values_from =  estimate:moe) %>% 
  rename(median_income=estimate_B06011_001,
         child_pop=estimate_B09001_001,
         poverty=estimate_B17001_001,
         overall_pop=estimate_B01001_001)

#Need to get poverty working
zip_code_variables %>% select(poverty,overall_pop,GEOID)

ctc_data %>% left_join(zip_code_variables, by = c("zip" = "GEOID")) %>% 
  mutate(missing_children_tax=child_pop-ctc_children) %>% select(-starts_with("moe_")) %>% 
  mutate(poverty_percent=poverty/overall_pop) %>% #Obviosuly this is wrong fml
  mutate(missing_children_tax_percentage=1-(missing_children_tax/child_pop)) %>% 
  filter(missing_children_tax_percentage<.2) %>% 
  ggplot(aes(x = median_income, y = missing_children_tax_percentage, weight = child_pop)) +
  geom_point(aes(size = child_pop)) +
  geom_smooth() +
  labs(y = "Percentage of\nChildren\not getting CTC", x = "Median Income") +
  scale_y_continuous(labels=scales::percent_format()) +
  theme_jfi()


v17 <- load_variables(2019, "acs5", cache = TRUE)
v17 %>% distinct(concept) %>% View()
View(v17)