library(dplyr)
library(ggplot2)
horton = read.csv("timhorton.csv" , stringsAsFactors = F)
starbucks = read.csv("starbucks.csv" , stringsAsFactors = F)
dunkin = read.csv("dunkin.csv" , stringsAsFactors = F)

acs = read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/acs2015_county_data.csv",
               stringsAsFactors = F)

names(horton)
names(starbucks)
names(dunkin)

dunkin_zip = group_by(filter(dunkin , e_country == "USA") , e_postal)
starbucks_zip = group_by(
  mutate(filter(starbucks , country == "us"), 
    postal_code = as.numeric(postal_code)) , postal_code)
horton_zip = group_by(filter(horton , e_country == "USA") , e_postal)

dunkin_state = group_by(filter(dunkin , e_country == "USA") , e_state)
starbucks_state = group_by(filter(starbucks , country == "us") , state)
horton_state = group_by(filter(horton , e_country == "USA") , e_state)

dunkin_count = summarize(dunkin_state , dunkin = n())
starbucks_count = summarize(starbucks_state , starbucks = n())
horton_count = summarize(horton_state , horton = n())

all_count = full_join(dunkin_count , horton_count , "e_state")
all_count = full_join(all_count , starbucks_count , c("e_state"="state"))

