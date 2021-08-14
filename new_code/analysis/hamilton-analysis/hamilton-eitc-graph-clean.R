library(tidyverse)
library(RColorBrewer)
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






combo_eitc <- bind_rows(original_eitc,original_eitc_married,reform_eitc_alt,reform_eitc_alt_cohabitating)
combo_eitc
hamilton_only <- bind_rows(reform_eitc_alt,reform_eitc_alt_cohabitating)
# hamilton_only <- hamilton_only %>% mutate(group=str_remove(group,"Reform EITC: "),
#                          group=str_remove(group,"Reform EITC Married: "))
#Maybe do singles and married side by side, labeling the lines?
color_for_plots <- "#572dff"


singles <- ggplot(reform_eitc_alt, aes(x = labor_income, y = eitc, group = group)) +
  geom_line(color = color_for_plots) + #Need to control colors so the old is gray
  theme_bw() + 
  labs(y = "Benefit\nSize", x = "\nAdjusted Gross Income", 
       title = "Singles\n") +
  scale_y_continuous(labels=scales::dollar_format(), limits = c(0,45000), breaks = seq(0,45000,10000)) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme_jfi() +
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1),
        # axis.text.y = element_text(angle = 20, vjust = 1, hjust=1),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))

cohabitating <- ggplot(reform_eitc_alt_cohabitating, aes(x = labor_income, y = eitc, group = group)) +
  geom_line(color = color_for_plots) + #Need to control colors so the old is gray
  theme_bw() + 
  labs(y = "Benefit\nSize", x = "\nAdjusted Gross Income", 
       title = "Couples\n") +
  scale_y_continuous(labels=scales::dollar_format(), limits = c(0,45000), breaks = seq(0,45000,10000)) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme_jfi() +
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1),
        # axis.text.y = element_text(angle = 20, vjust = 1, hjust=1),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))

# scale_y_continuous(labels=scales::dollar_format(seq(from = 0, to = 45000, by = 5000), accuracy = 1), 
#                    breaks = seq(from = 0, to = 45000, by = 5000), limits = c(0,45000)) +
# scale_x_continuous(labels=scales::dollar_format(seq(from = 0, to = 70000, by = 15000), accuracy = 1), 
#                    breaks = seq(from = 0, to = 70000, by = 15000), limits = c(0,70000)) +

cohabitating

size_for_annotation <- 2.5
singles <- singles + 
  annotate("label", label = "0 Children", x = 5000, y = 14000, size = size_for_annotation, colour = color_for_plots,
           family= "Unna") +
  annotate("label", label = "1 Child", x = 5000, y = 18500, size = size_for_annotation, colour = color_for_plots,
           family= "Unna") +
  annotate("label", label = "2 Children", x = 5000, y = 23000, size = size_for_annotation, colour = color_for_plots,
           family= "Unna") +
  annotate("label", label = "3 Children", x = 5000, y = 27500, size = size_for_annotation, colour = color_for_plots,
           family= "Unna") 
singles


cohabitating <- cohabitating + 
  annotate("label", label = "0 Children", x = 5000, y = 27000, size = size_for_annotation, colour = color_for_plots,
           family= "Unna") +
  annotate("label", label = "1 Child", x = 5000, y = 31500, size = size_for_annotation, colour = color_for_plots,
           family= "Unna") +
  annotate("label", label = "2 Children", x = 5000, y = 36000, size = size_for_annotation, colour = color_for_plots,
           family= "Unna") +
  annotate("label", label = "3 Children", x = 5000, y = 40500, size = size_for_annotation, colour = color_for_plots,
           family= "Unna") 
cohabitating

library(cowplot)

library(patchwork)
hamilton_by_kids <- (singles + cohabitating) +
  plot_annotation(title = 'Guaranteed Income Benefit Size', theme = theme(plot.title = element_text(hjust = 0.5, size = 16), text=element_text(family="Unna")))


hamilton_by_kids #Want to adjust axes

#Need to get the legend going nicely
guranteed_income_plot <- ggplot(combo_eitc, aes(x = labor_income, y = eitc, group = group, color = `EITC Type`, linetype=`Filing Status`)) +
  geom_line() + #Need to control colors so the old is gray
  scale_color_manual(values=c("#572dff","gray")) +
  theme_bw() + 
  labs(y = "Benefit\nSize", x = "\nAdjusted Gross Income", 
       title = "Guranteed Income Benefit\n Vs. Standard EITC",
       caption = "Jain Family Institute") +
  scale_y_continuous(labels=scales::dollar_format(), n.breaks = 9) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme_jfi()

guranteed_income_plot