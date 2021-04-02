library(tidytuesdayR)
library(ggplot2)
library(dplyr)
library(waffle)
library(yarrr)
tues <- tt_load("2021-01-26")
plastics <- tues$plastics
plasticsUS <- plastics %>% 
  filter(country == "United States of America",year==2019,
         grand_total > 23, grand_total < 1000) %>% 
  select(parent_company,grand_total) %>% 
  arrange(desc(grand_total))

level <-c("PepsiCo","Anheuser-Busch InBev","The Hershey Company",
            "Philip Morris","The Coca-Cola Company","Reynolds American, Inc.",
            "Mars, Incorporated","Nestlé","Kraft Heinz Company", "Starbucks")
plasticsUS$parent_company <- factor(plasticsUS$parent_company,level=level)
pallete <- piratepal(palette = "basel")

plot <- plasticsUS %>% 
  count(parent_company,wt=grand_total) %>%
  ggplot(aes(fill = parent_company,values=n)) +
  geom_waffle(n_rows= 10,size = 0.33, color='white',flip=TRUE,
              make_proportional = TRUE)+
  coord_equal()+
  scale_fill_manual(name=NULL, 
                    values = c("#0C5BB0FF","#EE0011FF","#15983DFF","#EC579AFF",
                               "#FA6B09FF", "#149BEDFF", "#A1C720FF","#FEC10BFF",
                               "#16A08CFF", "#9A703EFF"))+
  theme_enhance_waffle() +
  theme(axis.ticks = element_line(linetype = "blank"), 
    axis.text = element_text(colour = NA), 
    panel.background = element_rect(fill = NA), 
    legend.key = element_rect(size = 0.1),
    plot.caption = element_text(hjust = 0)) +
  labs(title = "The Brands Behind Plastic Litter in the US", 
    caption = "By @rppfeiffer | Data from Break Free from Plastic| #30DayChartChallenge")
ggsave("April1-21.png",plot=plot)

