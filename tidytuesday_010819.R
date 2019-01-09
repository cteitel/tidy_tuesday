library(tidyverse)

ratings_in <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-08/IMDb_Economist_tv_ratings.csv")
ratings_in <- rownames_to_column(ratings_in)

allGenreNames <- ratings_in$genres %>%
  paste(collapse = ",") %>%
  str_split(",") %>%
  unlist() %>%
  unique() 

allGenres <-  map(allGenreNames , grepl , ratings_in$genres) %>%
  setNames(allGenreNames) %>%
  bind_cols() %>%
  rownames_to_column()

ratings <-  left_join(ratings_in , allGenres , by = "rowname") 

ratings_long <- gather(allGenres , genres_all , tf , -rowname) %>%
  filter(tf) %>%
  select(rowname , genres_all) %>%
  left_join(select(ratings_in , -genres) , by = "rowname")

mod <- lm(av_rating ~ . , data = select(ratings , allGenreNames , av_rating))
summary(mod)

mod2 <- lm(share ~ . , data = select(ratings , allGenreNames , seasonNumber , share))
summary(mod2)


ggplot(ratings , aes(x = date , y = av_rating)) +
  geom_point(aes(size = share)) +
  geom_line(aes(group = titleId)) +
  theme_classic()



Ns = group_by(ratings_long , genres_all) %>%
  summarize(N = n()) %>%
  arrange(desc(N))
ratings_long <- mutate(ratings_long , genres_all = factor(ratings_long$genres_all , levels = Ns$genres_all))
include = filter(Ns , N > 200 & genres_all != "Drama")$genres_all



meanRating = mean(ratings$av_rating)
meanShare = mean(ratings$share)

ratings = mutate(ratings , 
                 status = ifelse(av_rating > meanRating & share < meanShare , "underwatched",
                                 ifelse(av_rating < meanRating & share > meanShare , "overwatched",
                                        "expected")))
ratings_long = mutate(ratings_long , 
                 status = ifelse(av_rating > meanRating & share < meanShare , "underwatched",
                                 ifelse(av_rating < meanRating & share > meanShare , "overwatched",
                                        "expected")))

ggplot(filter(ratings , share > 0) , 
       aes(y = share , x = av_rating)) +
  geom_point(aes(color= status)) +
  geom_vline(aes(xintercept = meanRating)) +
  geom_hline(aes(yintercept = meanShare)) +
  scale_y_continuous(trans = "log" , breaks = c(0.05 , 1 , 20) , labels = paste(c(0.05 , 1 , 20) , "%")) +
  theme_classic() +
  scale_color_brewer(palette = "Set2" , name = "Number watching")

ggplot(filter(ratings_long, share > 0)) +
  geom_bar(aes(x = status , fill = status)) +
  scale_color_brewer(palette = "Set2" , name = "Number watching") +
  facet_wrap(~genres_all)




ggplot(filter(ratings_long , genres_all %in% include & seasonNumber <=15) , 
       aes(x = date , y = share)) +
  #geom_point() +
  geom_line(aes(group = titleId)) +
  theme_classic() +
  facet_wrap(~genres_all)



ggplot(filter(ratings) , 
       aes(y = share , x = av_rating)) +
  geom_point(aes(color= Action , fill = Action)) +
  geom_vline(aes(xintercept = mean(ratings$av_rating)))


