library(ggplot2)
library(dplyr)
library(tidyr)

dats <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week4_australian_salary.csv")
names(dats)
head(dats)

employ <- arrange(dats , desc(average_taxable_income))
employ <- mutate(employ,
                 rank_new = ifelse(gender == "Female" , gender_rank + 0.4 , gender_rank))
employH <- filter(employ , gender_rank <= 10)
employL <- filter(employ , gender_rank >= (max(gender_rank)-20))

employWide = select(employ[employ$individuals>1000,] 
                    , gender , occupation , average_taxable_income)
employWide = spread(employWide , gender , average_taxable_income)
employWide = mutate(employWide , 
                    mean_salary = (Male+Female)/2,
                    salary_diff = Male - Female,
                    salary_gain = (Male-Female)/Male)
employWide = filter(employWide , !is.na(salary_diff))
employWide = arrange(employWide , desc(salary_diff))


p <- ggplot(employWide , aes(x = (mean_salary) , y = log10(abs(salary_gain*100)) * sign(salary_gain))) +
  theme_classic()+
  theme(legend.position = c(.99,.01) , legend.justification = c("right","bottom"),
        legend.background = element_rect(linetype = 2 , colour = 1)) +
  geom_point(aes(color = abs(salary_diff))) +
  scale_colour_gradient2(low = "blue",mid = "grey",high = "red" ,name="Absolute diff. \nin income") +
  geom_line(aes(y = 0)) +
  scale_y_continuous(name = "Males make _% of female income" , breaks = c(-1.48,-1,0,1,1.48) , labels = c(70,90,100,110,130)) +
  # scale_x_continuous(name = "Average income" , breaks = c(4.18,4.477,4.7,5,5.255) , 
  #                    labels = paste(round(10^ c(4.18,4.477,4.7,5,5.255)/1000),"K")) 
  scale_x_continuous(name = "Average income" , breaks = c(50,100,150,200)*1000 , labels = paste(c(50,100,150,200),"K")) 
p



