library(tidyverse)
library(dplyr)
library(vtable)
library(estimatr)
library(ggplot2)
library(modelsummary)
library(MatchIt)
library(optmatch)

movies <- read.csv("/Users/Alex/Downloads/movies_metadata.csv")

# Mutates
movies = movies %>%
  mutate(release_year = substr(release_date,1,4),
         release_month = substr(release_date,6,7),
         release_day = substr(release_date,9,10))

movies = movies %>%
  mutate(isCollection = ifelse(belongs_to_collection == "", 0, 1))

# Filters
movies = movies %>%
  filter(budget >= 10000000,
         original_language == "en",
         release_year >=2010,
         revenue != "0",
         runtime>70,
         status == "Released")

# Selects
movies = movies %>% 
  select(-video, -tagline, -status, -spoken_languages, -production_countries, -production_companies, -poster_path, -overview, -imdb_id, -homepage, -genres, -adult, -vote_count, -vote_average, -belongs_to_collection)
write.to_csv(movies, "movies.csv")

library(tidytext)
library(plyr)

tweets = read.csv("cleaned_tweets.csv")

tweets = subset(tweets, select = -c(X))
tweets
tweets = tweets %>% mutate(isOutlier = ifelse(Revenue<1000000000 & Like.Count < 20, "Not Outlier", "Outlier"))
tweets
tweets_without_outliers = tweets[tweets$Revenue<1000000000 & tweets$Like.Count < 20,]
tweets_without_outliers

ggplot(data = tweets, aes(x = Like.Count, y = Revenue, color=isOutlier)) +
    geom_point() +
    scale_color_manual("Outlier", values = c("orange4","orange1"), labels = c("No","Yes")) +
    geom_smooth(method="lm", formula=y~x, color ="purple", lwd = 1.5, se = FALSE) +
    geom_smooth(data=tweets_without_outliers, method="lm", formula=y~x, color ="blue", lwd = 1.5, se = FALSE) +
    xlab("Likes") +
    ylab("Revenue") +
  annotate("text", label = "Without outliers", x = 14, y = 250000000, color = "blue") + #Add annotations to the plot
  annotate("text", label = "With outliers", x = 10, y = 400000000, color = "purple") +
  theme_light()

ggplot(data = tweets, aes(x= Revenue)) + 
  geom_histogram(color="black", fill="orange") +
  theme_light()

summary(tweets)

lm_tweets = lm(Revenue ~ . - Movie - Date - Revenue - X, data = tweets)
summary(lm_tweets)

library(psych)
describe(tweets)

