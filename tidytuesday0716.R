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

#state centroids from: "https://raw.githubusercontent.com/benkeen/miscellaneous/master/d3collision/us-state-centroids.json"
stateCenters <- read_json("us-state-centroids.json" , simplifyVector = T)
#non-tidy data extraction
coords <- do.call(rbind , stateCenters$features$geometry$coordinates )
coords <- cbind.data.frame(coords , State = stateCenters$features$properties$name)
names(coords) <- c("Long" , "Lat" , "State")

#join all datasets
exercise <- left_join(exercise , acs_states , by = c("state"="State")) %>%
  left_join(coords , by = c("state"="State"))

plotdats = filter(exercise , !is.na(Income) & !state %in% c("Alaska","Hawaii"))

#plot
p <- ggplot(plotdats , aes(x = Long , y = Lat)) +
  geom_point(aes(fill = adults , size = Walk) , shape = 22 , color = "grey") +
  theme_classic() +
  scale_fill_continuous(low = "pink" , high = "black") +
  scale_size(range=c(1,15))
p
