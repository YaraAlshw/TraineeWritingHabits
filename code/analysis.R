## Analysis code for Alshwairikh and Fanton et al.
# Code by F. Rowland and Y. Alshwairikh
# Last edit August 2022

# Load libraries
library(ggplot2)
library(viridis)
library(RColorBrewer)
library(dplyr)
library(ggpubr)
library(rstanarm)
library(easystats)
library(tidyr)
library(bayesplot)
library(bayestestR)
library(BayesFactor) 
library(shinystan)
library(cowplot)

# Load data ====
survey <- read.csv("data/dataclean_Jul22.csv", header = TRUE)

empty_as_na <- function(x){ # empty_as_na function did not exist so I found this code to create the function
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ifelse(as.character(x)!="", x, NA)}
  
# replace empty cells with NA
# transform all columns
survey <- survey %>% mutate_each(funs(empty_as_na)) 

# convert columns from character to factor
survey$writing_word <- as.factor(survey$writing_word)
survey$review_word <- as.factor(survey$review_word)

# # create new columns for total training and pubs
survey$trainingtot <- rowSums(survey[,c("graduate_yrs", "postdoc_yrs")], na.rm=TRUE)

#Set priors and scale for all Bayesian analyses
t_prior <- student_t(df = 1, location = 0, scale = 2.5)

#Analysis 1: first author pubs vs. time spent writing ====
model_bayes1a <- stan_glm(firstauthor_pubs ~ 
                            hrs_wk_writing,
                          iter = 20000,
                          prior = t_prior,
                          prior_intercept = t_prior,
                          cores = 3,
                          chains = 4,
                          warmup = 5000, 
                          data= survey, seed=111)
#interpret
describe_posterior(model_bayes1a, test = c("p_direction", "rope", "bayesfactor"))
summary(model_bayes1a, digits = 3)
posteriors_model_bayes1a <- posterior(model_bayes1a)

loo(model_bayes1a) # fit is good
# prior_summary(model_bayes1a) # if want to see priors we used

# shinystan to check for model fit
launch_shinystan(model_bayes1a)

# plot posterior fits to show uncertainty
fits <- model_bayes1a %>% 
  as_tibble() %>% 
  rename(intercept = `(Intercept)`) %>% 
  select(-sigma)
fits

# randomly plot 500 posterior draws
# aesthetic controllers
n_draws <- 200
alpha_level <- .15
col_draw <- "grey60"
col_median <-  "#405364"


# does writing more mean more papers? YES
writepubs <-
  ggplot(aes(x = hrs_wk_writing, y = firstauthor_pubs), data = survey) +
  geom_jitter(
    width = 0.2,
    pch = 21,
    aes(fill = trainingtot),
    size = 3,
    alpha = 0.7
  ) +
  # scale_fill_viridis(name = "Training total (yrs)") +
  scale_fill_gradientn(
    colours = c(
      "#EDA09C",
      "#DF858E",
      "#C6798F",
      "#966480",
      "#6C5B7B",
      "#585B74",
      "#405364"
    ),
    name = "Training total (yrs)"
  ) +
  theme_classic(base_size = 14) +
  theme(panel.border = element_rect(fill = NA, size = 1)) +
  geom_abline(
    aes(intercept = intercept, slope = hrs_wk_writing), 
    data = slice_sample(fits, n = n_draws), 
    color = col_draw, 
    alpha = alpha_level
  ) + 
  # Plot the median values in blue
  geom_abline(
    intercept = median(fits$intercept), 
    slope = median(fits$hrs_wk_writing), 
    size = 1, 
    color = col_median
  ) +
  xlab("Hrs per week devoted to writing") +
  ylab("First-authored publications")
print(writepubs)

# save figure
ggsave(writepubs, filename = "figures/time_firstauth.png", dpi = 300, height = 4, width = 6)


# Analysis 2: plan writing (binomial) and pub total ====
survey$plan_writing <- as.factor(survey$plan_writing) 

plan_model <- stan_glm(firstauthor_pubs ~ 0 +
                         plan_regular +
                         plan_deadline +
                         plan_no,
                       iter = 20000,
                       prior = t_prior,
                       prior_intercept = t_prior,
                       cores = 3,
                       chains = 4,
                       warmup = 10000, 
                       data = survey,
                       seed = 111,
                       family = gaussian(link = "log"))

bayestestR::describe_posterior(
  plan_model,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance", "bayesfactor"),
  centrality = "all"
)

summary(plan_model, digits = 3)
posteriors_plan_model <- posterior(plan_model)

loo(plan_model)
# prior_summary(plan_model)


# plot(plan_model) # check out posteriors
launch_shinystan(plan_model)


# does planning mean more papers?
posterior2 <- as.matrix(plan_model)

color_scheme_set("darkgray")
plan_plot <- bayesplot::mcmc_intervals(posterior2,
                                            pars = c("plan_regular", "plan_deadline",
                                                     "plan_no")) +
  scale_y_discrete(labels = c('Regularly schedule writing',
                              'Write before deadlines',
                              'No writing schedule')) +
  theme_classic(base_size = 14) +
  geom_vline(xintercept=0, linetype = "dotted", colour = "black", size = 1) +
  # set your own labels
  xlab("Posterior distribution of parameter") +
  theme(panel.border = element_rect(fill = NA, size = 1))
print(plan_plot)
ggsave(plan_plot, filename = "figures/planning_fig.png", dpi = 300, height = 5, width = 5)


#Analysis 3: writing tracking method (binomial) and first author pub total ====

# two models: (1) with all data and (2) with first author pubs = 0 removed
nonzero <- survey %>%
  dplyr::filter(firstauthor_pubs > 0)

model_bayes7 <- stan_glm(
  firstauthor_pubs ~
    0 +
    tracking_advisor +
    tracking_group +
    tracking_individ +
    tracking_no,
  iter = 20000,
  prior = t_prior,
  prior_intercept = t_prior,
  cores = 3,
  chains = 4,
  warmup = 10000,
  data = survey,
  # data = nonzero,
  family = gaussian(link = "log"),
  seed = 111
)


plot(model_bayes7)

bayestestR::describe_posterior(
  model_bayes7,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance", "bayesfactor"),
  centrality = "all"
)
summary(model_bayes7, digits = 3)


loo(model_bayes7, k_threshold = 0.7) #check if there are problems, values influencing the model
prior_summary(model_bayes7)

# check model fit
launch_shinystan(model_bayes7)

# plot model
posterior2 <- as.matrix(model_bayes7)
color_scheme_set("darkgray")
analysis2_plot <- bayesplot::mcmc_intervals(posterior2,
                             pars = c("tracking_advisor",
                                      "tracking_group",
                                      "tracking_individ",
                                      "tracking_no")
                             ) + # parameters of interest as they are in the model output
  scale_y_discrete(labels = c('Advisor check-ins',
                              'Writing group check-ins',
                              'Individual tracking',
                              'No tracking')) +
  theme_classic(base_size = 14) +
  geom_vline(xintercept=0, linetype = "dotted", colour = "black", size = 1) +
  # set your own labels
  xlab("Posterior distribution of parameter") +
  theme(panel.border = element_rect(fill = NA, size = 1))
print(analysis2_plot)
ggsave(analysis2_plot, filename = "figures/tracking_fig.png", dpi = 300, height = 5, width = 5)



### make combined plot for analysis 1 & 2----
multi <- cowplot::plot_grid(
                plan_plot,
                analysis2_plot,
                align = "hv",
                nrow = 2,
                ncol = 1,
                labels = c("a", "b"),
                label_size = 14)
print(multi)

ggsave(multi, filename = "figures/writingplanningmulti.png", dpi = 300, height = 6, width = 6)

# Analysis 4: time per week spent writing and attitude toward a) scientific writing, 2) peer review process ====
levels(survey$writing_word)
levels(survey$review_word)

attitude_model1 <- stan_glm(hrs_wk_writing ~ 0 + writing_word, 
                            iter = 10000,
                            cores = 3,
                            chains = 4,
                            prior = t_prior,
                            prior_intercept = t_prior,
                            warmup = 5000, 
                            seed = 111,
                            data = survey)

posteriors_attitude_model1 <- describe_posterior(attitude_model1, test = c("p_direction", "rope", "bayesfactor"))
loo(attitude_model1)
# prior_summary(attitude_model1)
summary(attitude_model1, digits = 3)

launch_shinystan(attitude_model1)


# setting the intercept to zero allows us to compare the groups easier
attitude_model2 <- stan_glm(hrs_wk_writing ~ 0 + review_word,
                            iter = 10000,
                            prior = t_prior,
                            prior_intercept = t_prior,
                            cores = 3,
                            chains = 4,
                            warmup = 5000, data = survey)

describe_posterior(attitude_model2, test = c("p_direction", "rope", "bayesfactor")) 
summary(attitude_model2)
loo(attitude_model2)
# prior_summary(attitude_model2)
plot(attitude_model2)

# explore model fit
launch_shinystan(attitude_model2)

#Analysis 5: first author pubs and sentiment towards a) scientific writing, 2) peer review process====

#a) scientific writing word
## This is cool! Those who feel negative have fewer pubs than those who feel positive
model_bayes4 <- stan_glm(firstauthor_pubs ~ 0 +
                           writing_word,
                         iter = 10000,
                         prior = t_prior,
                         prior_intercept = t_prior,
                         cores = 3,
                         chains = 4,
                         warmup = 5000,
                         data= survey, 
                         seed=111)

plot(model_bayes4)

bayestestR::describe_posterior(
  model_bayes4,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance"),
  centrality = "all"
)
summary(model_bayes4, digits = 3)

loo(model_bayes4) #check if there are problems, values influencing the model

launch_shinystan(model_bayes4)

#pd plot for analysis 5a
posterior4 <- as.matrix(model_bayes4)

color_scheme_set("darkgray")
senti_sci_plot <- bayesplot::mcmc_intervals(posterior4,
                                            pars = c("writing_wordnegative", "writing_wordneutral",
                                                     "writing_wordpositive")) +
  scale_y_discrete(labels = c('Negative sentiment',
                              'Neutral sentiment',
                              'Positive sentiment')) +
  theme_classic(base_size = 14) +
  geom_vline(xintercept=0, linetype = "dotted", colour = "black", size = 1) +
  # set your own labels
  xlab("Posterior distribution of parameter") +
  theme(panel.border = element_rect(fill = NA, size = 1))
print(senti_sci_plot)
ggsave(senti_sci_plot, filename = "figures/sentiment_sci_fig.png", dpi = 300, height = 5, width = 5)

#b) review process word
model_bayes4b <- stan_glm(firstauthor_pubs ~ 0 +
                           review_word,
                         iter = 10000,
                         prior = t_prior,
                         prior_intercept = t_prior,
                         cores = 3,
                         chains = 4,
                         warmup = 5000,
                         data= survey, seed=111)

plot(model_bayes4b)

bayestestR::describe_posterior(
  model_bayes4b,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance"),
  centrality = "all"
)
summary(model_bayes4b, digits = 3)
posteriors_model_bayes4b <- describe_posterior(model_bayes4b)

loo(model_bayes4b) #check if there are problems, values influencing the model
prior_summary(model_bayes4b)
summary(model_bayes4b, digits = 3)


# for a nicer table
print_md(posteriors_model_bayes4b, digits = 3)

launch_shinystan(model_bayes4b)

#pd plot for analysis 5b
posterior4b <- as.matrix(model_bayes4b)

color_scheme_set("darkgray")
senti_rev_plot <- bayesplot::mcmc_intervals(posterior4b,
                                            pars = c("review_wordnegative", "review_wordneutral",
                                                     "review_wordpositive")) +
  scale_y_discrete(labels = c('Negative sentiment',
                              'Neutral sentiment',
                              'Positive sentiment')) +
  theme_classic(base_size = 14) +
  geom_vline(xintercept=0, linetype = "dotted", colour = "black", size = 1) +
  # set your own labels
  xlab("Posterior distribution of parameter") +
  theme(panel.border = element_rect(fill = NA, size = 1))
print(senti_rev_plot)
ggsave(senti_rev_plot, filename = "figures/sentiment_rev_fig.png", dpi = 300, height = 5, width = 5)

# Make combined plot of boxplot for sentiment and first author pubs and pd figures ====
writing_box <-
  ggplot(aes(x = writing_word, y = firstauthor_pubs), 
         data = na.omit(survey[, c("firstauthor_pubs", "writing_word")])) +
  geom_boxplot(fill = c("#EDA09C", "#966480", "#585B74")) + theme_bw(base_size = 14) +
  xlab("Feelings about scientific writing") +
  ylab("First author publications") 

review_box <- ggplot(aes(x = review_word, y = firstauthor_pubs),
                     data = na.omit(survey[,c("firstauthor_pubs", "review_word")])) +
  geom_boxplot(fill=c("#EDA09C", "#966480", "#585B74")) + theme_bw(base_size = 14) +
  xlab("Feelings about peer review") +
  ylab("First author publications")  

#Create 4 panel figure of the pd plots and box plot for sentiment analysis
multi_panel <- ggarrange(common.legend = TRUE,
                         writing_box,
                         review_box,
                         senti_sci_plot,
                         senti_rev_plot,
                         align = "hv", 
                         nrow = 2,
                         ncol = 2,
                         labels = "AUTO")

print(multi_panel)

#Analysis 6: pubs total and Writing support groups====

#try stan_glm model
#summary(aov(lm(pubtotal ~ writing_support_group, data = survey)))
#summary(aov(lm(pubtotal ~ writing_support_group, data = grads)))
#grad.aov <- aov(lm(pubtotal ~ writing_support_group, data = grads))
#TukeyHSD(grad.aov)
#summary(aov(lm(pubtotal ~ writing_support_group, data = postdocs)))

model_bayes5 <- stan_glm(firstauthor_pubs ~ 
                           writing_support_group,
                         iter = 10000,
                         prior = t_prior,
                         prior_intercept = t_prior,
                         cores = 3,
                         chains = 4,
                         warmup = 5000,
                         data= survey, seed=111)

plot(model_bayes5)

bayestestR::describe_posterior(
  model_bayes5,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance"),
  centrality = "all"
)
summary(model_bayes5, digits = 3)
posteriors_model_bayes5 <- posterior(model_bayes5)

loo(model_bayes5) #check if there are problems, values influencing the model
prior_summary(model_bayes5)

launch_shinystan(model_bayes5)

#Analysis 7: Writing support groups, try Chi-squared test====
#bayesian chi squared test
#equal probability for all levels (i.e., null hypothesis is there's no difference)

x <- xtabs( ~ writing_word + writing_support_group, survey)
x
survey$writing_support_group <- as.factor(survey$writing_support_group)

x2 <- xtabs( ~ review_word + writing_support_group, survey)
x2

#Bayesian chi-square test from https://learningstatisticswithr.com/book/bayes.html#bayescontingency
#BayesFactor package

contingencyTableBF(x, sampleType = "poisson", seed = 111) #odds for alt hypothesis is 0.17%, so pretty much no relationship between sentiment towards scientific writing and having joined a writing group

contingencyTableBF(x2, sampleType = "poisson", seed = 111) #odds for alt hypothesis is 0.18%, so pretty much no relationship between sentiment towards peer reviews process and having joined a writing group


# boxplot for sentiment and first author pubs ====
writing_box <-
  ggplot(aes(x = writing_word, y = firstauthor_pubs), 
         data = na.omit(survey[, c("firstauthor_pubs", "writing_word")])) +
  geom_boxplot(fill = c("#EDA09C", "#966480", "#585B74")) + theme_bw(base_size = 14) +
  xlab("Feelings about scientific writing") +
  ylab("First author publications") 

review_box <- ggplot(aes(x = review_word, y = firstauthor_pubs),
                     data = na.omit(survey[,c("firstauthor_pubs", "review_word")])) +
  geom_boxplot(fill=c("#EDA09C", "#966480", "#585B74")) + theme_bw(base_size = 14) +
  xlab("Feelings about peer review") +
  ylab("First author publications")  

#Create 4 panel figure of the pd plots and box plot for sentiment analysis
multi_panel <- ggarrange(common.legend = TRUE,
                         writing_box,
                         review_box,
                         senti_sci_plot,
                         senti_rev_plot,
                          align = "hv", 
                          nrow = 2,
                          ncol = 2,
                          labels = "AUTO")

print(multi_panel)


#ggsave(combined_box, filename = "combined_box.png", dpi = 300, width = 8, height = 8)
ggsave(multi_panel, filename = "multi_panel.png", dpi = 300, width = 12, height = 8)


# ggplot for writing vs first author pubs ====
# does writing more mean more papers? YES
writepubs <- ggplot(aes(x = hrs_wk_writing, y = firstauthor_pubs), data = survey) + scale_fill_viridis() +
  geom_point(pch = 21, aes(fill = hrs_wk_writing), size = 4) +
  theme_bw(base_size = 14) +
  xlab("Hrs per week devoted to writing") +
  ylab("First author publications")

print(writepubs)
ggsave(writepubs, filename = "writepubs.png", dpi = 300, width = 10, height = 8)


# Make writing groups figure ====
# if else hell
survey$GoalSetting <- ifelse(survey[,39]=="Improved",1, ifelse(survey[,39]=="No Change", 0, -1))
survey$Reviews <- ifelse(survey[,40]=="Improved",1, ifelse(survey[,40]=="No Change", 0, -1))
survey$Collaboration <- ifelse(survey[,41]=="Improved",1, ifelse(survey[,41]=="No Change", 0, -1))
survey$Camaraderie <- ifelse(survey[,42]=="Improved",1, ifelse(survey[,42]=="No Change", 0, -1))
survey$Skills <- ifelse(survey[,43]=="Improved",1, ifelse(survey[,43]=="No Change", 0, -1))
survey$Starting <- ifelse(survey[,44]=="Improved",1, ifelse(survey[,44]=="No Change", 0, -1))
survey$TimeManagement <- ifelse(survey[,45]=="Improved",1, ifelse(survey[,45]=="No Change", 0, -1))
survey$Perfectionism <- ifelse(survey[,46]=="Improved",1, ifelse(survey[,46]=="No Change", 0, -1))
survey$Anxiety <- ifelse(survey[,47]=="Improved",1, ifelse(survey[,47]=="No Change", 0, -1))
survey$Imposter <- ifelse(survey[,48]=="Improved",1, ifelse(survey[,48]=="No Change", 0, -1))
survey$Quality <- ifelse(survey[,49]=="Improved",1, ifelse(survey[,49]=="No Change", 0, -1))
survey$Output <- ifelse(survey[,50]=="Improved",1, ifelse(survey[,50]=="No Change", 0, -1))

# reshape the data
groups_wide <- survey[,c(7, 76:87)]
groups_wide2 <- survey[,c(7, 39:50)]

# The arguments to gather():
# - data: Data object
# - key: Name of new key column (made from names of data columns)
# - value: Name of new value column
# - ...: Names of source columns that contain values
# - factor_key: Treat the new key column as a factor (instead of character vector)
groups_long <- gather(groups_wide2, key = factor, value = rating, Goal.setting:Writing.output, factor_key=TRUE)
groups_long

g <- groups_long %>%
  group_by(factor, rating) %>%
  summarise(cnt = n(), na.rm = TRUE) %>%
  mutate(freq = round(cnt / sum(cnt), 3), na.rm = TRUE) %>% 
  arrange(desc(freq))

# remove NAs
g2 <- as.data.frame(g[c(13:42),])

# Build plot
# use function likert() to plot likert data
g2 <- ggplot()+
  geom_bar(data = g2, aes(x = reorder(factor, cnt), y=cnt, fill=as.factor(rating)), position="stack", stat="identity")+
  coord_flip() + 
  ylab("Rating")+
  xlab("Factor")+
  scale_fill_brewer(palette="PRGn",
                    name  ="Response",
                    labels=c("Improved", "No Change", "Worsened"))+
  theme(legend.position="bottom") +
  theme_bw(base_size = 14) +
  xlab("Accountability group effect") +
  ylab("Number of respondents")
#scale_x_discrete(limits=c("StronglyAgree", "Agree", "DontKnow","Disagree","StronglyDisagree"))
g2

View()


