library(dplyr)
sw <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/star-wars-survey/StarWars.csv",
               header = F , stringsAsFactors = F)
#csv did not read with header b/c of question marks
#get questions and remove from dataset
questions <- as.character(sw[1,])
sw = sw[-1,]

#separate out multiple-choice questions
filmsSeen <- select(sw , 4:9)
colnames(filmsSeen) <- apply(filmsSeen , 2 , function(x) unique(x[x!=""]))
filmsSeen <- filmsSeen[filmsSeen!=""]

questions

head(sw)
