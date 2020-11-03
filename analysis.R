library(tidyverse)
library(readxl)
setwd("D:\\Dropbox\\pollster.ge\\Geo Parl 2020\\Analysis\\PVT")

pvt <- read_xlsx("PVT.xlsx", sheet=1)

mean(pvt$Modulo)

pvt %>%
  filter(Modulo > 1)%>%
  mutate(diff = CEC-ISFED)%>%
  data.table::melt(value.name = "value", id.vars=c("Elections", "Cand", "Modulo", "diff"), measure.vars=c("ISFED", "CEC"))%>%
  mutate(who = paste0(Cand, ", ", Elections))%>%
  ggplot(aes(reorder(who, Modulo), value, group=variable, label=value, fill = variable))+
  geom_col(position=position_dodge())+
  geom_text(position=position_dodge(width=1))+
  scale_fill_manual(values=c("#af1615", "#60c028"))+
  coord_flip()+
  ylim(0, 70)+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title = element_blank())+
  labs(title="Parallel Vote Tabulation vs. Official CEC results, 2004-2020",
       subtitle = "Only differences more than 1% are displayed.\nSome statistically significant differences tally to less than 1%",
       caption = "Source: ISFED website, CEC website, Civil.ge")

ggsave("pvt_cec_results.png", width = 10, height=8)
