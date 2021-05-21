
#Need to convert from state codes to IRS state ID codes for NBER TAXSIM
#Crosswalk very messy, cleaning up here...
state_crosswalk_soi <- read_csv(here("random","tabula-state_crosswalk.csv"), col_names = FALSE)
                                
state_crosswalk_soi <- state_crosswalk_soi %>% mutate(census_id=str_extract(X1,"[0-9][0-9]? "),
                               tax_id=str_extract(X1, " [0-9][0-9]?")) %>% 
  mutate(across(where(is.character), str_trim)) %>% 
  select(-X1) %>% 
  mutate(across(where(is.character), as.numeric))

state_crosswalk_soi %>% filter(is.na(tax_id)) 

