# libraries
library(tidytext)
library(textdata)
library(tidyverse)

# load data
survey <- read.csv('dataclean_Nov2.csv', header = TRUE)

## transform all columns
survey <- survey %>% mutate_each(funs(empty_as_na)) 

# convert columns from character to factor
str(survey)
survey$condition <- as.factor(survey$condition)
survey$BIPOC <- as.factor(survey$BIPOC)
survey$first_gen <- as.factor(survey$first_gen)
survey$first_language_english <- as.factor(survey$first_language_english)
survey$gender_identity <- as.factor(survey$gender_identity)
survey$writing_word <- as.factor(survey$writing_word)
survey$review_word <- as.factor(survey$review_word)

# recode gender identity to eliminate non-binary/third
survey$female <- ifelse(test = survey$gender_identity == "Female", 1, 0)
survey$ESL <- ifelse(test = survey$first_language_english == "Yes", 0, 1)
survey$firstgen <- ifelse(test = survey$first_gen == "Yes", 1, 0)
survey$BIPOC <- ifelse(test = survey$BIPOC == "Yes", 1, 0)
survey$condition <- ifelse(test = survey$condition == "Yes", 1, 0)

# standardize words to lower case
survey$science_writing_open <- tolower(survey$science_writing_open)
survey$review_open <- tolower(survey$review_open)

# trim white space around words
# survey$science_writing_open <- trimws(survey$science_writing_open)
# survey$review_open <- trimws(survey$review_open)


# columns of interest: science_writing_open [57] and review_open [59]

# use AFINN lexicon to get value between -5 to 5 for words ----
# Note: afinn lexicon cut down entries from 277 to 90. :/
afinn <- get_sentiments("afinn")

writing_afinn <- survey %>%      #creates new dataframe "writing_afinn"
  inner_join(get_sentiments(lexicon = "afinn"), by = c("science_writing_open" = "word")) %>% #just joins words in AFINN lexicon
  rename(writesentiment = value)

ggplot(aes(y = hrs_wk_writing, x = writesentiment), data = writing_afinn) +
  theme_bw(base_size = 14) +
  geom_jitter(width = 0.1)

summary(lm(hrs_wk_writing ~ writesentiment, data = writing_afinn))

review_afinn <- survey %>%      #creates new dataframe "both_afinn"
  inner_join(get_sentiments(lexicon = "afinn"), by = c("review_open" = "word")) %>% #just joins words in AFINN lexicon
  rename(reviewsentiment = value)

ggplot(aes(y = hrs_wk_writing, x = reviewsentiment), data = review_afinn) +
  theme_bw(base_size = 14) +
  geom_jitter(width = 0.1)

summary(lm(hrs_wk_writing ~ reviewsentiment, data = review_afinn))

# use BING lexicon to get intent ----
# this lexicon gets us 189 returned values
bing <- get_sentiments("bing")

writing_bing <- survey %>%      #creates new dataframe "writing_afinn"
  inner_join(get_sentiments(lexicon = "bing"), by = c("science_writing_open" = "word")) %>% #just joins words in AFINN lexicon
  rename(writesentiment = sentiment)

ggplot(aes(y = hrs_wk_writing, x = writesentiment), data = writing_bing) +
  theme_bw(base_size = 14) +
  geom_jitter(width = 0.1)

review_bing <- survey %>%      
  inner_join(get_sentiments(lexicon = "bing"), by = c("review_open" = "word")) %>% #just joins words in AFINN lexicon
  rename(reviewsentiment = sentiment)

ggplot(aes(y = hrs_wk_writing, x = reviewsentiment), data = review_bing) +
  theme_bw(base_size = 14) +
  geom_jitter(width = 0.1)

summary(lm(hrs_wk_writing ~ reviewsentiment, data = review_bing))

# use nrc lexicon to get intent ----
# this lexicon gets us 230 returned values
nrc <- get_sentiments("nrc")

writing_nrc <- survey %>%      #creates new dataframe "writing_afinn"
  inner_join(get_sentiments(lexicon = "nrc"), by = c("science_writing_open" = "word")) %>% #just joins words in AFINN lexicon
  rename(writesentiment = sentiment)

ggplot(aes(y = hrs_wk_writing, x = writesentiment), data = writing_nrc) +
  theme_bw(base_size = 14) +
  geom_violin() +
  geom_jitter(width = 0.1)

review_nrc <- survey %>%      
  inner_join(get_sentiments(lexicon = "nrc"), by = c("review_open" = "word")) %>% #just joins words in AFINN lexicon
  rename(reviewsentiment = sentiment)

ggplot(aes(y = hrs_wk_writing, x = reviewsentiment), data = review_nrc) +
  theme_bw(base_size = 14) +
  geom_boxplot() +
  geom_jitter(width = 0.1)

summary(lm(hrs_wk_writing ~ reviewsentiment, data = review_nrc))

        