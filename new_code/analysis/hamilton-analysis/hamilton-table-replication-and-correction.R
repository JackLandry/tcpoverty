library(tidyverse)
library(tidylog)
library(stringr.tools)
library(kableExtra)
library(here)

hamilton_singles <- read_rds(here("inter_data","singles_hamilton.rds"))
main_benefit <- hamilton_singles$hamilton_money[1]
hamilton_singles <- hamilton_singles %>% 
  mutate(hamilton_money=ifelse(hamilton_money!=main_benefit,hamilton_money-main_benefit,hamilton_money)) %>% 
  mutate(`Corrected Refund Amount`=hamilton_money) %>% 
  select(`Corrected Refund Amount`)

hamilton_table <- tibble(`Refund Recepient` = c("Adult", "Child 1", "Child 2"),
                         `Refund Amount Hamilton` = c(3269,1159,2318))
hamilton_table <- bind_cols(hamilton_table,hamilton_singles) %>% mutate(Difference=`Refund Amount Hamilton`-`Corrected Refund Amount`)
hamilton_table

hamilton_table <- hamilton_table %>% mutate(across(where(is.numeric), round, 0)) %>% 
  mutate(across(where(is.numeric),as.character)) %>% #What about comma insertion
  mutate(across(`Refund Amount Hamilton`:Difference, ~prettyNum(.x,big.mark=",")))  %>% 
  mutate(across(`Refund Amount Hamilton`:Difference, ~str_prefix(.x,"$"))) 


#Note that they both get to claim the kid...
hamilton_table_couples <- tibble(`Refund Recepient` = c("Couple", "Child 1", "Child 2"),
                                 `Refund Amount Hamilton` = c(14048,2475,4958))

hamilton_couples <- read_rds(here("inter_data","couples_hamilton.rds"))
main_benefit_couples <- hamilton_couples$hamilton_money[1]
main_benefit <- hamilton_singles$hamilton_money[1]
hamilton_couples <- hamilton_couples %>% 
  mutate(hamilton_money=ifelse(hamilton_money!=main_benefit_couples,hamilton_money-main_benefit_couples,hamilton_money)) %>% 
  mutate(`Corrected Refund Amount`=hamilton_money) %>% 
  select(`Corrected Refund Amount`)

hamilton_table_couples <- bind_cols(hamilton_table_couples,hamilton_couples) %>% mutate(Difference=`Refund Amount Hamilton`-`Corrected Refund Amount`)
hamilton_table_couples


hamilton_table_couples <- hamilton_table_couples %>% mutate(across(where(is.numeric), round, 0)) %>% 
  mutate(across(where(is.numeric),as.character)) %>% #What about comma insertion
  mutate(across(`Refund Amount Hamilton`:Difference, ~prettyNum(.x,big.mark=",")))  %>% 
  mutate(across(`Refund Amount Hamilton`:Difference, ~str_prefix(.x,"$"))) 

hamilton_table_couples %>%
  kbl() %>%
  kable_minimal() %>% 
  kable_styling(html_font = "Unna",
                full_width = FALSE,
                position = "center")
