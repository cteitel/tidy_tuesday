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


ggplot(ratings , aes(x = seasonNumber , y = share)) +
  geom_point() +
  geom_line(aes(group = titleId)) +
  theme_classic()



Ns = group_by(ratings_long , genres_all) %>%
  summarize(N = n()) %>%
  arrange(desc(N))
ratings_long <- mutate(ratings_long , genres_all = factor(ratings_long$genres_all , levels = Ns$genres_all))
include = filter(Ns , N > 200)$genres_all

ggplot(filter(ratings_long , genres_all %in% include & seasonNumber <=15) , 
       aes(x = date , y = share)) +
  #geom_point() +
  geom_line(aes(group = titleId)) +
  theme_classic() +
  facet_wrap(~genres_all)
