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

original_eitc <- original_eitc %>% mutate(`EITC Type`="Original",
                                          group=str_c("Original EITC: ", group))

combo_eitc <- bind_rows(original_eitc,reform_eitc)

#Make line type the married thing
ggplot(combo_eitc, aes(x = labor_income, y = eitc, group = group, color = `EITC Type`)) +
  geom_line() + #Need to control colors so the old is gray
  scale_color_manual(values=c("gray", "#572dff")) +
  theme_bw() + 
  labs(y = "EITC  \nBenefit  ", x = "\nLabor Income", 
       title = "New EITC Benefit For Single Filers",
       caption = "Jain Family Institute") +
  scale_y_continuous(labels=scales::dollar_format()) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme(plot.title = element_text(hjust = 0.5),
        text=element_text(family="Unna"), #Want to change this to Jain font, need to ask for it
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(angle=0, vjust = 0.5),
        panel.background = element_rect(fill="#fffdfc"), #Not sure if this is actually modifying background
        plot.background = element_rect(fill="#fffdfc"),
        plot.margin=unit(c(.15,.75,.15,.15),"cm")) 
