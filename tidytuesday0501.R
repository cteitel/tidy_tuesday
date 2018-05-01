library(dplyr)
library(ggplot2)

acs = read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/acs2015_county_data.csv",
               stringsAsFactors = F)
head(acs)

acs = arrange(acs , desc(Income))
acs_states = group_by(acs , State)

is.numeric_int = function(x) { is.numeric(x) | is.integer(x) }
acs_max = summarize_if(acs_states , is.numeric_int , c("first" , "last" , "mean"))

acs_max = arrange(acs_max , Income_first)

p <- ggplot(acs_max) +
  geom_segment(aes(x = Income_first , xend = 0 , y = 1:nrow(acs_max) , yend = 1:nrow(acs_max),
                   color = Black_first) , size = 2) +
  geom_segment(aes(x = (-1) * IncomePerCap_last , xend = 0 , y = 1:nrow(acs_max) , yend = 1:nrow(acs_max),
               color =  Black_last), size = 2) +
  geom_point(aes(x = Income_first ,  y = 1:nrow(acs_max))) +
  geom_point(aes(x = (-1) * IncomePerCap_last ,  y = 1:nrow(acs_max) )) +
  scale_colour_gradient2(low = "blue",mid="grey",high = "red" ,name="Percent black",midpoint=12.3) +
  theme_classic() + 
  theme(axis.ticks.y = element_blank() , axis.text.y = element_blank() , 
        axis.title.y = element_blank() , 
        legend.position = c(1,.01) , legend.justification = c("right","bottom")) +
  ggtitle("Income of highest- and lowest- earning counties per state") +
  scale_x_continuous("Household Income (K USD)", breaks = c(-50000,0,50000,100000) ,
                     labels = c(50,0,50,100) , expand = c(0.15, 0)) +
  geom_vline(xintercept = 0)+
  geom_text(aes(x = Income_first+3000 , y = seq(1,nrow(acs_max),1) , label = State) , 
            size= 3 ,hjust = 0) +
  geom_text(aes(x = -3000 , y = 54 , label = c("Lowest earning county")) , hjust = 1) + 
  geom_text(aes(x = 3000 , y = 54 , label = c("Highest earning county")) , hjust = 0) 

p


