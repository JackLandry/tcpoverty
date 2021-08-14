mt_data <- read_rds(here("inter_data","mt_data_hamilton_for_analysis.Rds"))
mt_data_long_mt_rates <- mt_data %>% 
  select(e00200,couple_status,num_children,num_children_clean,marginal_tax_rate_hamilton_no_eitc,marginal_tax_rate_hamilton_eitc,
         marginal_tax_rate) %>% 
  pivot_longer(cols = c(marginal_tax_rate_hamilton_no_eitc,marginal_tax_rate_hamilton_eitc,marginal_tax_rate),
               names_to = "tax_rate_type",
               values_to = "marginal_tax_rate")

mt_data_long_mt_rates

mt_data_long_mt_rates <- mt_data_long_mt_rates %>% 
  mutate(tax_rate_type=ifelse(tax_rate_type=="marginal_tax_rate", "Status Quo",
                              ifelse(tax_rate_type=="marginal_tax_rate_hamilton_no_eitc","Hamilton Reform\nEIC Repealed", 
                                     ifelse(tax_rate_type=="marginal_tax_rate_hamilton_eitc","Hamilton Reform\nKeep EIC",NA))))

linesize <- 1.25
line_trans <- .5



#Want to do the same thing here, include current EITC
mt_data_long_posttax_income <- mt_data %>% 
  select(e00200,couple_status,num_children,num_children_clean,aftertax_hamilton_no_eitc,aftertax_hamilton_eitc,aftertax_income) %>% 
  pivot_longer(cols = c(aftertax_hamilton_no_eitc,aftertax_hamilton_eitc,aftertax_income),
               names_to = "aftertax_income_type",
               values_to = "aftertax_income")


mt_data_long_posttax_income <- mt_data_long_posttax_income %>% 
  mutate(aftertax_income_type=ifelse(aftertax_income_type=="aftertax_income", "Status Quo",
                                     ifelse(aftertax_income_type=="aftertax_hamilton_no_eitc","Hamilton Reform\nEIC Repealed", 
                                            ifelse(aftertax_income_type=="aftertax_hamilton_eitc","Hamilton Reform\nKeep EIC",NA))))




#Need to change the y axis to start at 0
singles_posttax_hamilton_inc_graph <- ggplot(data= mt_data_long_posttax_income %>% filter(couple_status=="Single" & num_children!=6 & e00200!=0 & e00200<=60000), 
                                             aes(x = e00200, y = aftertax_income, group = aftertax_income_type)) +
  geom_line(aes(color = aftertax_income_type), size=linesize, alpha = line_trans) +
  facet_wrap(vars(num_children_clean)) +
  labs(y = "Aftertax\nIncome\nWith\nHamilton\nBenefit",
       x = "Labor Income",
       title = "Pre vs- Post Tax Income for Single Filers\n") +
  scale_y_continuous(labels=scales::dollar_format(), n.breaks = 6, limits = c(0,80000)) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme_jfi() +
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1),
        legend.title = element_blank(),
        # axis.text.y = element_text(angle = 20, vjust = 1, hjust=1),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))

couples_posttax_hamilton_inc_graph <- ggplot(data= mt_data_long_posttax_income %>% filter(couple_status=="Couple" & num_children!=6 & e00200!=0 & e00200<=80000), 
                                             aes(x = e00200, y = aftertax_income, group = aftertax_income_type)) +
  geom_line(aes(color = aftertax_income_type), size=linesize, alpha = line_trans) +
  facet_wrap(vars(num_children_clean)) +
  labs(y = "Aftertax\nIncome\nWith\nHamilton\nBenefit",
       x = "Labor Income",
       title = "Pre vs- Post Tax Income for Married Filers\n") +
  scale_y_continuous(labels=scales::dollar_format(), n.breaks = 6, limits = c(0,80000)) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme_jfi() +
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1),
        legend.title = element_blank(),
        # axis.text.y = element_text(angle = 20, vjust = 1, hjust=1),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))


singles_posttax_hamilton_inc_graph
couples_posttax_hamilton_inc_graph