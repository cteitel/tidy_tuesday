library(tidyverse)
sw <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/star-wars-survey/StarWars.csv",
               header = F , stringsAsFactors = F)
#csv did not read with header b/c of question marks
#get questions and remove from dataset
questions <- as.character(sw[1,])
movies = as.character(sw[2,4:9])
sw = sw[-c(1:2),]

#separate out multiple-choice questions: movies seen
filmsSeen <- select(sw , c(1,4:9))
colnames(filmsSeen)[c(2:7)] <- apply(filmsSeen[,c(2:7)] , 2 , function(x) unique(x[x!=""]))
filmsSeen <- mutate_at(filmsSeen , c(2:7) , function(x) x!="")
#NOT used here

#separate out multiple-choice questions: favorite movies
favorites <- select(sw , c(1,11,34:38))
colnames(favorites) <- c("ID","favorite",questions[c(34:38)])
favorites <- filter(favorites , favorite != "" & Age!="")
favorites <- mutate(favorites , favorite = as.numeric(favorite))
favorites <- mutate(favorites , faveName = movies[favorites$favorite])
favorites$Age = gsub("> 60" , "60+" , favorites$Age)


plot <- ggplot(aes(x = Age) , data = favorites) + 
  geom_bar(aes(fill = faveName) , position = "fill") + 
  theme_classic() +
  scale_fill_brewer("Favorite movie",palette = "Set2") +
  ylab("Proportion") +
  ggtitle("Everyone Likes Episode V") +
  theme(axis.text=element_text(size=11) , 
        axis.title=element_text(size=14),
        title=element_text(size=16))
plot
