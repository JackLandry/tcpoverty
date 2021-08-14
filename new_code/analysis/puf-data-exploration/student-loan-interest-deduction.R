library(rio)
library(tidyverse)
library(tidylog)

soi <- import("/Users/jacklandry/Dropbox/JFI/irs-soi-data/saez-zucman/14rptaxexpendcapwealthdata/pufonline2016.dta")

soi %>% summarise(avg_student_loan_deduc=weighted.mean(studentded,dweght))

#I think the aggregation method might throw things off a bit here, 5 returns grouped together so 
#It's hard to realize who is max claiming.. 
soi %>% filter(studentded!=0 & waginc<200000) %>% 
  ggplot(aes(x = waginc)) +
  geom_histogram()

soi %>% filter(studentded!=0 & waginc<200000) %>% 
  ggplot(aes(x = waginc, y = studentded)) +
  geom_point()