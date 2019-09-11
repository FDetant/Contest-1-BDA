library(tidyverse)
library(tidytext)

# Sets it to the correct working directory for me.
setwd("C:/Users/Administrator/Documents/Contest-1-BDA/youtube-personality")

# Reads in the files that are not the transcripts
##ik doe nu read table omdat ik niet snap hoe read_csv colomen maakt voor elke blank
features <- as_tibble(read.table("YouTube-Personality-audiovisual_features.csv", header = T, sep=""))
gender <- as_tibble(read.table("YouTube-Personality-gender.csv", header = T, sep=""))
impression_scores <- as_tibble(read.table("YouTube-Personality-Personality_impression_scores_train.csv", header = T, sep=""))
View(features)
View(gender)
View(impression_scores)

# Makes the vlogId that can be used in the tibble.
transcript_files <- dir(path = "C:/Users/Administrator/Documents/Contest-1-BDA/youtube-personality/transcripts", full.names = T)
vlogId = basename(transcript_files)
vlogId = str_replace(vlogId, pattern = ".txt$", replacement = "")

#Sets the working directory for me to read in the transcripts
setwd("C:/Users/Administrator/Documents/Contest-1-BDA/youtube-personality/transcripts")

# Makes a tibble with the vlogId, the transcript and the filename. 
transcripts_df = tibble(Id=vlogId, Text = map_chr(transcript_files, ~ paste(readLines(.x), collapse = "\\n")), filename = transcript_files)
View(transcripts_df)

#tokenization
data = transcripts_df %>%
  unnest_tokens(word, Text)
data.2=as_tibble(data[,-2]) #delete filename and make a tibble

#remove stopwords because they are meaningless
data(stop_words)
data.3 = data.2 %>%
  anti_join(stop_words, by = "word")

#most common words
data.3 %>%
  count(word, sort = TRUE)

#plot most common words
library(ggplot2)
data.3 %>%
  count(word, sort = TRUE) %>%
  filter(n > 400) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# sentiment analysis
library(textdata)
library(tidyr)
library(dplyr)
View(data.3)

vlog_sentiments = data.3 %>%
  inner_join(get_sentiments("nrc"), by="word") %>%
  group_by(sentiment) %>%
  count(Id)
View(vlog_sentiments)

# wide format
vlog_sentiments_wide = spread(vlog_sentiments, sentiment, n)
# replace NA by 0
vlog_sentiments_wide <-
vlog_sentiments_wide %>%
  replace_na(list(anger = 0, anticipation = 0, disgust = 0, fear = 0, joy = 0, negative = 0, positive = 0, sadness = 0, surprise = 0, trust = 0))
##dit kan waarschijnlijk mooier

data.vlogs = inner_join(vlog_sentiments_wide, impression_scores, by = c("Id" = "vlogId"))
data.vlogs.features = 
  inner_join(data.vlogs, features, by = c("Id" = "vlogId"))
data.all =
  inner_join(data.vlogs.features, gender, by = c("Id" = "vlogId"))
View(data.all)

# prediction extraversion
lm.fit.extr = lm(Extr ~ anger + anticipation + joy + mean.pitch + sd.pitch + mean.energy + time.speaking, data = data.all)
summary(lm.fit.extr)

