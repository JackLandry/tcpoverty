library(tidylog)
ipum <- read_rds(here("inter_data","ipum_prepped.Rds"))

ipum %>% mutate(property_taxes=ifelse(proptax==99997,NA,proptax),
                hh_inc=iflese(adjginc==99999999,NA,adjginc))

#Would need to bin HH income and then impute property taxes
