# libraries
library(ggplot2)
library(dplyr)
library(ggpubr)
library(tidyr)
library(ggridges)
library(glue)
library(tidyverse)

# load data
survey <- read.csv('data/dataclean_Nov2.csv', header = TRUE)

# replace empty cells with NA
# transform all columns
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

# create new columns for total training and pubs
survey$trainingtot <- rowSums(survey[,c("graduate_yrs", "postdoc_yrs")], na.rm=TRUE)
survey$pubtotal <- rowSums(survey[,c("firstauthor_pubs", "coauthor_pubs")], na.rm=TRUE)

# create a new column for career stage
survey$stage <- ifelse(is.na(survey$postdoc_yrs), "grad", "postdoc")

# career interests ----

# change data frame for graphing
survey2 <- gather(survey[,c(11:17)], factor_key = TRUE)

# colored by density function and reordered by highest to lowest interest
all_career <- ggplot(aes(x = value, y = key2, fill = 0.5-abs(0.5-stat(ecdf))), data = survey2) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  scale_fill_gradientn(name = "Tail probability",
                       colours = c("#405364","#585b74","#6c5b7b","#966480","#c6798f", "#df858e", "#eda09c"),
                       values = c(1, 0.83, 0.66, 0.49, 0.32, 0.15, 0)) +
  theme_classic(base_size = 16) +
  xlim(0,10) +
  theme(panel.border = element_rect(fill = NA, size = 1),
        axis.line = element_blank()) +
  #scale_fill_viridis_c(name = "Tail probability", direction = -1) +
  ylab("Career path") +
  xlab("Interest level") +
  scale_y_discrete(labels = c('Teaching','R2 or R3',
                              'R1', 'Government',
                              'Industry or Data Science',
                              'Communication',
                              'NGO'))

ggsave(all_career, filename = glue("figures/career_interest_figure_{Sys.Date()}.png"), width = 7, height = 5, dpi=300)
