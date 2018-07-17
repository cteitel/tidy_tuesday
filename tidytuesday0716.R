library(tidyverse)
library(readxl)
library(jsonlite)

#exercise data week 16
exercise <- read_xlsx("week16_exercise.xlsx")
head(exercise)

#american community survey data from week 5
acs = read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week5_acs2015_county_data.csv",
               stringsAsFactors = F)
#get state-level weighted means of ACS data
acs_states <- 
  acs %>%
  group_by(State) %>%
  summarise_at(vars(Hispanic:Unemployment) , 
               funs(weighted.mean(.,w = TotalPop, na.rm = TRUE))) 
acs_states <- mutate(acs_states , income_scale = Income / mean(acs_states$Income))


#join all datasets
#and add state abbreviations for labels
exercise <- left_join(exercise , acs_states , by = c("state"="State")) %>%
  mutate(stateNum = match(state , state.name)) %>%
  mutate(st = state.abb[stateNum] , div = state.division[stateNum])

#group into fewer regions (remove "east" and "west" subdivisions)
exercise <- mutate(exercise , divNew = div) %>%
  mutate(divNew = str_replace(divNew , c("East ") , "")) %>%
  mutate(divNew = str_replace(divNew , c("West ") , ""))

plotdats = filter(exercise , !is.na(Income))


#plot
p <- ggplot(filter(plotdats , state != "District of Columbia") , 
            aes(x = Walk , y = adults)) +
  geom_point(aes(color = divNew) , size = 3 ) +
  theme_classic() +
  scale_color_brewer("Region" , palette = "Set2") +
  geom_text(aes(label = st , color = divNew) , fontface = "bold" ,
            position = position_nudge(x = -0.2 , y = -0.2) ,
              check_overlap = T) +
  ylab("Adults meeting exercise guidelines (%)") + 
  xlab("Adults walking to work (%)") + 
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=14),
        legend.text = element_text(size = 11),
        legend.title = element_text(face = "bold"),
        legend.position = "bottom")+
  NULL 
p

