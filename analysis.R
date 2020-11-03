library(tidyverse)
library(readxl)
setwd("D:\\Dropbox\\pollster.ge\\Geo Parl 2020\\Analysis\\PVT")

pvt <- read_xlsx("PVT.xlsx", sheet=1)

mean(pvt$Modulo)

pvt %>%
  filter(Modulo > 1)%>%
  data.table::melt(value.name = "value", id.vars=c("Elections", "Cand", "Modulo"), measure.vars=c("ISFED", "CEC"))%>%
  mutate(who = paste0(Cand, ", ", Elections))%>%
  ggplot(aes(reorder(who, Modulo), value, group=variable, label=value, fill = variable))+
  geom_col(position=position_dodge())+
  geom_text(position=position_dodge(width=1))+
  scale_fill_manual(values=c("#af1615", "#60c028"))+
  coord_flip()+
  ylim(0, 70)+
  theme_minimal()+
  annotate("text", x=4, y = 65,  label = paste0("Diff: +", pvt$Modulo[pvt$Elections=="2008 Pres" & pvt$Cand=="Saakashvili"]), fontface="bold", color = "#60c028")+
  annotate("text", x=3, y = 15,  label = paste0("Diff: -", pvt$Modulo[pvt$Elections=="2004 Parl" & pvt$Cand=="Aghordzineba"]), fontface="bold", color = "#af1615")+
  annotate("text", x=2, y = 60,  label = paste0("Diff: +", pvt$Modulo[pvt$Elections=="2020 Parliamentary" & pvt$Cand=="GD"]), fontface="bold", color = "#60c028")+
  annotate("text", x=1, y = 50,  label = paste0("Diff: +", pvt$Modulo[pvt$Elections=="2018 1" & pvt$Cand=="Zourabichvili"]), fontface="bold", color = "#60c028")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title = element_blank())+
  labs(title="Parallel Vote Tabulation vs. Official CEC results",
       subtitle = "2004-2020",
       caption = "Source: ISFED website, CEC website, Civil.ge")
