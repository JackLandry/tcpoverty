
#Want to convert this to regular taxsim ready

#Need to add some stuff like children under 6
#Maybe I should start from scrach
nber_taxsim_ready <- read_csv(here("outputs","c19_taxsim.csv"))

nber_taxsim_ready
nber_taxsim_ready %>% select(-state)