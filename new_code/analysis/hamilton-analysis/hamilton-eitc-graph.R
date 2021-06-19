
#Need to add married filers EITC, just add another dataframe changing the phase out threshold then make it the linetype
#No limit for number of children is another thing not in the tax calculator, maybe something that's more easily ignorable

#Benefit =  max benefit - 

reform_eitc_alt <- tibble(eitc = c(12500,12500,0,
                                   17000,17000,0,
                                   21500,21500,0,
                                   26000,26000,0),
                          labor_income = c(0,10000,50000,
                                           0,10000,50000,
                                           0,10000,50000,
                                           0,10000,50000),
                          group = c("0 Children","0 Children","0 Children",
                                    "1 Child", "1 Child", "1 Child",
                                    "2 Children", "2 Children", "2 Children",
                                    "3+ Children","3+ Children","3+ Children")) #Unclear if the phase out threshold is different for children, I don't think it is
reform_eitc_alt <- reform_eitc_alt %>% mutate(`EITC Type`="Guranteed Income EITC",
                                              `Filing Status` = "Single",
                                      group=str_c("Reform EITC: ", group))

reform_eitc_alt_cohabitating <- tibble(eitc = c(25000,25000,0,
                                   29500,29500,0,
                                   34000,34000,0,
                                   38500,38500,0),
                          labor_income = c(0,15000,70000, #Does the start phase out change for cohabitating
                                           0,15000,70000,
                                           0,15000,70000,
                                           0,15000,70000),
                          group = c("0 Children","0 Children","0 Children",
                                    "1 Child", "1 Child", "1 Child",
                                    "2 Children", "2 Children", "2 Children",
                                    "3+ Children","3+ Children","3+ Children")) #Unclear if the phase out threshold is different for children, I don't think it is
reform_eitc_alt_cohabitating <- reform_eitc_alt_cohabitating %>% 
  mutate(`EITC Type`="Guranteed Income EITC",
         `Filing Status` = "Married/Cohabitating",
         group=str_c("Reform EITC Married: ", group))
#Wait it might be a different phase out for the child benefit amount and the regular benefit amount
#The child benefit ends up at the same place, so I don't think so
#For single, child benefit declines at rate of 11.25%, which is 4500
#31.9% for main benefit, which is 12500 


#Have to calculate the new stuff for the reform, maybe I should just make it for the reform I'm actually modeling
original_eitc <- tibble(eitc = c(0,538,538,0,
                                 0,3584,3584,0,
                                 0,5920,5920,0,
                                 0,6660,6660,0),
                        labor_income = c(0,7030,8790,15820,
                                         0,10540,19330,41756,
                                         0,14800,19330,47440,
                                         0,14800,19330,50954),
                        group = c("0 Children","0 Children","0 Children","0 Children",
                                  "1 Child", "1 Child", "1 Child","1 Child",
                                  "2 Children", "2 Children", "2 Children","2 Children",
                                  "3+ Children","3+ Children","3+ Children","3+ Children"))

original_eitc_married <- tibble(eitc = c(0,538,538,0,
                                 0,3584,3584,0,
                                 0,5920,5920,0,
                                 0,6660,6660,0),
                        labor_income = c(0,7030,14680,21710,
                                         0,10540,25220,47646,
                                         0,14800,25220,53330,
                                         0,14800,25220,56844),
                        group = c("0 Children","0 Children","0 Children","0 Children",
                                  "1 Child", "1 Child", "1 Child","1 Child",
                                  "2 Children", "2 Children", "2 Children","2 Children",
                                  "3+ Children","3+ Children","3+ Children","3+ Children"))

original_eitc <- original_eitc %>% mutate(`EITC Type`="Original EITC",
                                          `Filing Status` = "Single",
                                          group=str_c("Original EITC: ", group))

original_eitc_married <- original_eitc_married %>% mutate(`EITC Type`="Original EITC",
                                                          `Filing Status` = "Married/Cohabitating",
                                          group=str_c("Original EITC Married: ", group))



hamilton_only <- bind_rows(reform_eitc_alt,reform_eitc_alt_cohabitating)
# hamilton_only <- hamilton_only %>% mutate(group=str_remove(group,"Reform EITC: "),
#                          group=str_remove(group,"Reform EITC Married: "))
#Maybe do singles and married side by side, labeling the lines?
color_for_plots <- "#572dff"

singles <- ggplot(reform_eitc_alt, aes(x = labor_income, y = eitc, group = group)) +
  geom_line(color = color_for_plots) + #Need to control colors so the old is gray
  theme_bw() + 
  labs(y = "Benefit  ", x = "\nLabor Income", 
       title = "Singles") +
  scale_y_continuous(labels=scales::dollar_format(), n.breaks = 10, limits = c(0,30000)) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme(plot.title = element_text(hjust = 0.5),
        text=element_text(family="Unna"), #Want to change this to Jain font, need to ask for it
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(angle=0, vjust = 0.5),
        panel.background = element_rect(fill="#fffdfc"), #Not sure if this is actually modifying background
        plot.background = element_rect(fill="#fffdfc"),
        plot.margin=unit(c(.15,.75,.15,.15),"cm")) 

cohabitating <- ggplot(reform_eitc_alt_cohabitating, aes(x = labor_income, y = eitc, group = group)) +
  geom_line(color = color_for_plots) + #Need to control colors so the old is gray
  theme_bw() + 
  labs(y = "Benefit  ", x = "\nAdjusted Gross Income", 
       title = "Couples") +
  scale_y_continuous(labels=scales::dollar_format(), n.breaks = 9) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme(plot.title = element_text(hjust = 0.5),
        text=element_text(family="Unna"), #Want to change this to Jain font, need to ask for it
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(angle=0, vjust = 0.5),
        panel.background = element_rect(fill="#fffdfc"), #Not sure if this is actually modifying background
        plot.background = element_rect(fill="#fffdfc"),
        plot.margin=unit(c(.15,.75,.15,.15),"cm")) 

size_for_annotation <- 3
singles <- singles + 
  annotate("label", label = "0 Children", x = 5000, y = 14000, size = size_for_annotation, colour = color_for_plots,
           family= "Unna") +
  annotate("label", label = "1 Child", x = 5000, y = 18500, size = size_for_annotation, colour = color_for_plots,
           family= "Unna") +
  annotate("label", label = "2 Children", x = 5000, y = 23000, size = size_for_annotation, colour = color_for_plots,
           family= "Unna") +
  annotate("label", label = "3+ Children", x = 5000, y = 27500, size = size_for_annotation, colour = color_for_plots,
           family= "Unna") 
singles


cohabitating <- cohabitating + 
  annotate("label", label = "0 Children", x = 5000, y = 27000, size = size_for_annotation, colour = color_for_plots,
           family= "Unna") +
  annotate("label", label = "1 Child", x = 5000, y = 31500, size = size_for_annotation, colour = color_for_plots,
           family= "Unna") +
  annotate("label", label = "2 Children", x = 5000, y = 36000, size = size_for_annotation, colour = color_for_plots,
           family= "Unna") +
  annotate("label", label = "3+ Children", x = 5000, y = 40500, size = size_for_annotation, colour = color_for_plots,
           family= "Unna") 
cohabitating

library(cowplot)

library(patchwork)
(singles + cohabitating) +
  plot_annotation(title = 'Guaranteed Income Benefit Size', theme = theme(plot.title = element_text(hjust = 0.5, size = 16),
                                                text=element_text(family="Unna")))



combo_eitc <- bind_rows(original_eitc,original_eitc_married,reform_eitc_alt,reform_eitc_alt_cohabitating)
combo_eitc


#Need to get the legend going nicely
ggplot(combo_eitc, aes(x = labor_income, y = eitc, group = group, color = `EITC Type`, linetype=`Filing Status`)) +
  geom_line() + #Need to control colors so the old is gray
  scale_color_manual(values=c("#572dff","gray")) +
  theme_bw() + 
  labs(y = "EITC  \nBenefit  ", x = "\nAdjusted Gross Income", 
       title = "Guranteed Income\nEITC Benefit",
       caption = "Jain Family Institute") +
  scale_y_continuous(labels=scales::dollar_format(), n.breaks = 9) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme(plot.title = element_text(hjust = 0.5),
        text=element_text(family="Unna"), #Want to change this to Jain font, need to ask for it
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(angle=0, vjust = 0.5),
        panel.background = element_rect(fill="#fffdfc"), #Not sure if this is actually modifying background
        plot.background = element_rect(fill="#fffdfc"),
        plot.margin=unit(c(.15,.75,.15,.15),"cm")) 



combo_eitc <- combo_eitc %>% mutate(total_earnings=eitc+labor_income)
combo_eitc %>% filter(`EITC Type`=="Guranteed Income EITC")
#Unclear why the 0 labor earnings points arn't really shoing up
ggplot(combo_eitc, aes(x = labor_income, y = total_earnings, group = group, color = `EITC Type`, linetype=`Filing Status`)) +
  geom_line() + #Need to control colors so the old is gray
  scale_color_manual(values=c("#572dff","gray")) +
  theme_bw() + 
  labs(y = "Total  \nBenefit  ", x = "\nLabor Income", 
       title = "Guranteed Income\nEITC Benefit",
       caption = "Jain Family Institute") +
  scale_y_continuous(labels=scales::dollar_format(), n.breaks = 9) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme(plot.title = element_text(hjust = 0.5),
        text=element_text(family="Unna"), #Want to change this to Jain font, need to ask for it
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(angle=0, vjust = 0.5),
        panel.background = element_rect(fill="#fffdfc"), #Not sure if this is actually modifying background
        plot.background = element_rect(fill="#fffdfc"),
        plot.margin=unit(c(.15,.75,.15,.15),"cm")) 


##Calculating implicit tax rate
combo_eitc %>% mutate(tax_rate=(total_earnings-lag(total_earnings))/(labor_income-lag(labor_income)),
                      tax_rate=ifelse(labor_income==0,NA,tax_rate),
                      tax_rate=1-tax_rate)  %>% 
  filter(`EITC Type`=="Guranteed Income EITC" & !is.na(tax_rate))
                              
                              ###Old




reform_eitc <- tibble(eitc = c(5000,5000,0,
                               10000,10000,0,
                               15000,15000,0,
                               20000,20000,0), 
                      labor_income = c(0,30000,95359,
                                       0,30000,92578.2,
                                       0,30000,101025,
                                       0,30000,124967), 
                      group = c("0 Children","0 Children","0 Children",
                                "1 Child", "1 Child", "1 Child",
                                "2 Children", "2 Children", "2 Children",
                                "3+ Children","3+ Children","3+ Children"))
reform_eitc <- reform_eitc %>% mutate(`EITC Type`="Reform",
                                      group=str_c("Reform EITC: ", group))

