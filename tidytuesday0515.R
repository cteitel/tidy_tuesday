library(tidyverse)
sw <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/star-wars-survey/StarWars.csv",
               header = F , stringsAsFactors = F)
#csv did not read with header b/c of question marks
#get questions and remove from dataset
questions <- as.character(sw[1,])
sw = sw[-c(1:2),]

#separate out multiple-choice questions
filmsSeen <- select(sw , c(1,4:9))
colnames(filmsSeen)[c(2:7)] <- apply(filmsSeen[,c(2:7)] , 2 , function(x) unique(x[x!=""]))
filmsSeen <- mutate_at(filmsSeen , c(2:7) , function(x) x!="")

filmsSeen = full_join(filmsSeen , select(sw , c(1,34:38)) , by = "V1")
colnames(filmsSeen)[c(8:12)] <- questions[c(34:38)]

filmsByAge = gather(select(filmsSeen , c(2:7,9)) , Movie , Seen , 1:6)
filmsByAge = group_by(filmsByAge , Age)
filmsByAge = summarize(filmsByAge , Movie)

plot <- geom_bar(aes(x = Age , y = Seen) , filmsByAge)
plot
