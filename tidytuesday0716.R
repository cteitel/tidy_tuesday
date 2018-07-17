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
  left_join(coords , by = c("state"="State"))

exercise <- mutate(exercise , stateNum = match(state , state.name)) %>%
  mutate(st = state.abb[stateNum] , div = state.division[stateNum])


plotdats = filter(exercise , !is.na(Income))


#plot
p <- ggplot(filter(plotdats , state != "District of Columbia") , 
            aes(x = Walk , y = adults)) +
  geom_point(aes(color = div) , size = 3) +
  theme_classic() +
  #scale_color_continuous("Per capita \nincome", low = "red" , high = "black") +
  geom_text(aes(label = st , color = div) , hjust = -.5) +
  NULL
p

