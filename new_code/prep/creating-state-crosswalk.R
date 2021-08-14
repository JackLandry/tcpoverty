
#Need to convert from state codes to IRS state ID codes for NBER TAXSIM
#Crosswalk very messy, cleaning up here...
state_crosswalk_soi <- read_csv(here("random","tabula-state_crosswalk.csv"), col_names = FALSE)
state_crosswalk_soi
#Need to add state name to fips code here             
state_crosswalk_soi <- state_crosswalk_soi %>% mutate(census_id=str_extract(X1,"[0-9][0-9]? "),
                               tax_id=str_extract(X1, " [0-9][0-9]?")) %>% 
  mutate(across(where(is.character), str_trim)) %>% 
  select(-X1) %>% 
  mutate(across(where(is.character), as.numeric))

state_crosswalk_soi %>% filter(is.na(tax_id)) 

state_crosswalk_soi <- read_csv(here("random","state-code-crosswalk.csv"), col_names = FALSE)

state_crosswalk_soi <- state_crosswalk_soi %>% mutate(census_id=str_extract(X1,"[0-9][0-9]? "),
                                                      tax_id=str_extract(X1, " [0-9][0-9]?")) %>% 
  mutate(X1=str_remove_all(X1,"[:digit:]")) %>% 
  mutate(across(where(is.character), str_trim)) %>% 
  mutate(across(census_id:tax_id,as.numeric)) %>% 
  rename(state_name=X1)



state_crosswalk_soi

write_csv(state_crosswalk_soi,here("random","state-code-crosswalk-clean.csv"))



state_fips <- read_csv("~/Dropbox/us-geographies/fips-codes-master/state_fips_master.csv")
state_fips