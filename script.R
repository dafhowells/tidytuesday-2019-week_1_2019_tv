library(rio)
library(tidyverse)

data <- import("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-08/IMDb_Economist_tv_ratings.csv")

# How does the average rating for a program change per season and by genre?

# What programs had the lowest average rating but ran for the longest?

by_show_seasons <- data %>%
                    ncount(title)

data %>%
  group_by(title) %>%
  summarise(av_rating = mean(av_rating)) %>%
  inner_join(by_show_seasons) %>%
  filter(n >= 3) %>%
  arrange(n, av_rating) %>% 
  top_n(15)
  
  
  

# What genre get the highest ratings?

# What shows with at least 5 seasons show the most variability in the av rating?
most_variable_shows <- data %>%
                          filter(seasonNumber >= 5) %>% 
                          group_by(title) %>% 
                          summarize(sd = sd(av_rating)) %>%
                          top_n(10)

# Break out the genres column into their own rows
genres_data <- data %>% 
  mutate(genre = str_split(genres, pattern = ",")) %>%
  unnest() 
  
genres_data %>% 
  mutate(year = lubridate::year(date)) %>% 
  ggplot(aes(x = genre, y = av_rating, colour = share)) +
  geom_point(position = "jitter", alpha = 0.2) +
  coord_flip()
  


data %>%
  mutate(year = lubridate::year(date)) %>% 
  ggplot(aes(x = date, y = av_rating, size = share)) +
  geom_point(color = "red", alpha = .8) +
  geom_smooth(method = "lm", se = F, linetype = "dashed", 
              color = "skyblue4") +
  theme_minimal() 