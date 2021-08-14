

library(rio)
library(tidyverse)
library(tidylog)
#Ok this is ready to go I think
cps_reweight <- read_csv("/Users/jacklandry/Dropbox/JFI/cps_data/clean/taxdata-reweighted-cps.csv")


cps_reweight %>% count(FLPDYR) #Stops in 2014, unever in years, why doesn't it use more recent data
View(cps_reweight)

head(cps_reweight)