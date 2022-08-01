## Analysis code for Alshwairikh and Fanton et al.
# Last edit August 2022

# #install packages if needed
# install.packages("viridis")
# install.packages("tm")
# install.packages("ggpubr")
# install.packages("rstanarm")
# install.packages("easystats", repos = "https://easystats.r-universe.dev")
# install.packages("logspline")
# install.packages("BayesFactor")
# install.packages("shinystan")


# libraries
library(ggplot2)
library(viridis)
library(RColorBrewer)
# library(tm)
library(dplyr)
library(ggpubr)
library(rstanarm)
# library(remotes)
library(easystats)
library(tidyr)
# library(ggridges)
# library(glue)
library(bayesplot)
library(bayestestR)
# library(logspline)
# library(car)
library(BayesFactor) 
library(shinystan)

# Load data ====
survey <- read.csv("data/dataclean_Jul22.csv", header = TRUE)

empty_as_na <- function(x){ # empty_as_na function did not exist so I found this code to create the function
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ifelse(as.character(x)!="", x, NA)}
  
# replace empty cells with NA
# transform all columns
survey <- survey %>% mutate_each(funs(empty_as_na)) 

# convert columns from character to factor
str(survey)

survey$writing_word <- as.factor(survey$writing_word)
survey$review_word <- as.factor(survey$review_word)

survey$plan_writing <- as.factor(survey$plan_writing)
survey$writing_tracking_reco <- as.factor(survey$writing_tracking_reco)

# # create new columns for total training and pubs
survey$trainingtot <- rowSums(survey[,c("graduate_yrs", "postdoc_yrs")], na.rm=TRUE)
# survey$pubtotal <- rowSums(survey[,c("firstauthor_pubs", "coauthor_pubs")], na.rm=TRUE)
# 
# # create a new column for career stage
# survey$stage <- ifelse(is.na(survey$postdoc_yrs), "grad", "postdoc")



#Analysis 1: first author pubs vs. time spent writing ====

t_prior <- student_t(df = 1, location = 0, scale = 2.5)

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

plan_model <- stan_glm(firstauthor_pubs ~ plan_writing,
                       iter = 10000,
                       prior = t_prior,
                       prior_intercept = t_prior,
                       cores = 3,
                       chains = 4,
                       warmup = 5000, 
                       data = survey,
                       seed = 111,
                       family = gaussian(link = "log"))

describe_posterior(plan_model, test = c("p_direction", "rope", "bayesfactor"))

bayestestR::describe_posterior(
  plan_model,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance"),
  centrality = "all"
)

summary(plan_model, digits = 3)
posteriors_plan_model <- posterior(plan_model)

loo(plan_model)
prior_summary(plan_model)
summary(plan_model, digits = 3)
posterior_interval(
  plan_model,
  prob = 0.9)
plot(plan_model)
launch_shinystan(plan_model)

#Analysis 3: writing tracking method (binomial) and first author pub total ====

model_bayes7 <- stan_glm(
  firstauthor_pubs ~
    0 +
    tracking_advisor +
    tracking_group +
    tracking_elec +
    tracking_note +
    tracking_no,
  iter = 20000,
  prior = t_prior,
  prior_intercept = t_prior,
  cores = 3,
  chains = 4,
  warmup = 5000,
  data = survey,
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
posteriors_model_bayes7 <- posterior(model_bayes7)

loo(model_bayes7, k_threshold = 0.7) #check if there are problems, values influencing the model
prior_summary(model_bayes7)
summary(model_bayes7, digits = 3)
posterior_interval(
  model_bayes7,
  prob = 0.9)

launch_shinystan(model_bayes7)

#writing per week
model_bayes7b <- stan_glm(hrs_wk_writing ~ 
                            writing_tracking_reco,
                          iter = 10000,
                          cores = 3,
                          chains = 4,
                          warmup = 5000,
                          family = gaussian(link = "log"),
                          data= survey, seed=111)

plot(model_bayes7b)

bayestestR::describe_posterior(
  model_bayes7b,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance"),
  centrality = "all"
)
summary(model_bayes7b, digits = 3)
posteriors_model_bayes7b <- posterior(model_bayes7b)

loo(model_bayes7b) #check if there are problems, values influencing the model
prior_summary(model_bayes7b)
summary(model_bayes7b, digits = 3)
posterior_interval(
  model_bayes7b,
  prob = 0.9)


# Analysis 4: time per week spent writing and attitude toward writing and review ====
# plan writing model
## These may need to be recoded as -1, 0, and 1. I'm not sure stan_glm does it automatically

#survey$writing_word <- factor(survey$writing_word, levels = c("neutral", "negative", "positive")) #reorder the writing_word levels so the reference is "neutral" for lm functions
#survey$review_word <- factor(survey$review_word, levels = c("neutral", "negative", "positive")) #reorder the writing_word levels so the reference is "neutral" for lm functions   ##don't need to do this anymore b/c we set intercept to 0

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

describe_posterior(attitude_model1, test = c("p_direction", "rope", "bayesfactor")) 
posteriors_attitude_model1 <- describe_posterior(attitude_model1)
loo(attitude_model1)
prior_summary(attitude_model1)
summary(attitude_model1, digits = 3)
posterior_interval(
  attitude_model1,
  prob = 0.9)
plot(attitude_model1)
# for a nicer table
print_md(posteriors_attitude_model1, digits = 3)

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
posteriors_attitude_model2 <- describe_posterior(attitude_model2)
loo(attitude_model2)
prior_summary(attitude_model2)
summary(attitude_model2, digits = 3)
posterior_interval(
  attitude_model2,
  prob = 0.9)
plot(attitude_model2)

# for a nicer table
print_md(posteriors_attitude_model2, digits = 3)

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
posteriors_model_bayes4 <- describe_posterior(model_bayes4)

loo(model_bayes4) #check if there are problems, values influencing the model
prior_summary(model_bayes4)
summary(model_bayes4, digits = 3)
posterior_interval(
  model_bayes4,
  prob = 0.9)

# for a nicer table
print_md(posteriors_model_bayes4, digits = 3)

launch_shinystan(model_bayes4)
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
posterior_interval(
  model_bayes4b,
  prob = 0.9)

# for a nicer table
print_md(posteriors_model_bayes4b, digits = 3)

launch_shinystan(model_bayes4b)

# writing success (i.e., pubs) NOT linked to peer-review attitude
review_box <- ggplot(aes(x = review_word, y = firstauthor_pubs, fill = stage), 
                     data = na.omit(survey[,c("firstauthor_pubs", "review_word", "stage")])) +
  geom_boxplot() + theme_bw(base_size = 14) +
  xlab("Feelings about peer review") +
  ylab("First author publications")
print(review_box)

review_box2 <- ggplot(aes(x = review_word, y = firstauthor_pubs), 
                     data = na.omit(survey[,c("firstauthor_pubs", "review_word", "stage")])) +
  geom_boxplot() + theme_bw(base_size = 14) +
  xlab("Feelings about peer review") +
  ylab("First author publications")
print(review_box2)

writing_box <- ggplot(aes(y = firstauthor_pubs, x = writing_word), #, fill = stage),
                      data = na.omit(survey[,c("firstauthor_pubs", "writing_word", "stage")])) +
  geom_boxplot() + theme_bw(base_size = 14) +
  xlab("Feelings about writing process") +
  ylab("First author publications")
print(writing_box)

ggplot(aes(y = writing_word, x = stage, fill = stage), data = survey) +
  geom_boxplot() + 
  theme_bw(base_size = 14)

ggplot(survey, aes(fill = writing_word, x = stage)) + 
  geom_bar(position="stack", stat="identity")

ggplot(aes(x = review_word, y = pubtotal, fill = stage), 
       data = survey) +
  geom_boxplot() + theme_bw(base_size = 14) +
  xlab("Feelings about peer review") +
  ylab("Total publications")

ggarrange(common.legend = TRUE,
          writing_box, 
          review_box, 
          align = "hv", 
          nrow = 2,
          labels = "AUTO"
)

# Writing groups section ====
# make writing groups figure
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
library(tidyr)

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

require(dplyr)

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
summary(model_bayes5, digits = 3)
posterior_interval(
  model_bayes5,
  prob = 0.9)

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


#Electronic spreadsheets
#Electronic note taking applications
#Physical notebook
#Checking in with writing accountability/support group
#Checking in with advisor or mentor
#I do not track my writing progress
#other

# boxplot for sentiment and first author pubs ====
writing_box <- ggplot(aes(x = writing_word, y = firstauthor_pubs),               data = na.omit(survey[,c("firstauthor_pubs", "writing_word")])) +
  geom_boxplot(fill=c("#fde725", "#21918c", "#443983")) + theme_bw(base_size = 14) +
  xlab("Feelings about scientific writing") +
  ylab("First author publications") 

review_box <- ggplot(aes(x = review_word, y = firstauthor_pubs),               data = na.omit(survey[,c("firstauthor_pubs", "review_word")])) +
  geom_boxplot(fill=c("#fde725", "#21918c", "#443983")) + theme_bw(base_size = 14) +
  xlab("Feelings about peer review") +
  ylab("First author publications")  

combined_box <- ggarrange(common.legend = TRUE,
          writing_box,
          review_box,
          align = "hv", 
          nrow = 2,
          labels = "AUTO"
)

ggsave(combined_box, filename = "combined_box.png", dpi = 300, width = 8, height = 8)

### density plot
#sent_plot <- ggplot(aes(x = review_word, y = firstauthor_pubs),    #           data = na.omit(survey[,c("firstauthor_pubs", #"review_word")])) +
#  geom_hline(yintercept = 0.5, linetype = "dotted") +
#  ggdist::stat_halfeye(
#    adjust = 1,
#    normalize = "all",
#    position = position_dodge(width = 0.5),
#    ## set slab interval to show IQR and 95% data range
#    .width = c(.5, .95),
#    slab_alpha = 0.7) +
#  theme_bw(base_size = 14) +
#  theme(
#    panel.grid.major.y = element_blank(),
#    panel.grid.minor.y = element_blank(),
#    panel.grid.major.x = element_blank(),
#    panel.grid.minor.x = element_blank()
#  ) +
#  ylim(0, 20) +
#  ylab("First author publications") +
#  xlab("Sentiment towards peer review") +
#  ggtitle("Density Plot of setiment towards peer review")

#print(sent_plot)

#ggsave(fiveyrshift_plot, filename = "Output_Figures/FiveYrShifts.png", dpi = 300, width = 8, height = 5)

# Plotting histograms ====

#analysis 1
# for specifying what you want in posteriors
# plot of posteriors
posterior <- as.matrix(model_bayes1a)

# plot it

analysis1_plot <- mcmc_areas(posterior,
                           pars = "hrs_wk_writing",
                           prob = 0.95) + # parameters of interest as they are in the model output
  #plot_title +
  theme_bw(base_size = 16) +
  geom_vline(xintercept=0, linetype = "dotted", colour = "black", size = 1) +
  # set your own labels
  #xlab("Posterior distribution of parameter") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
print(analysis1_plot)

# analysis 2
posterior2 <- as.matrix(plan_model)

analysis2_plot <- mcmc_areas(posterior2,
                             pars = "plan_writing1",
                             prob = 0.95) + # parameters of interest as they are in the model output
  #plot_title +
  theme_bw(base_size = 16) +
  geom_vline(xintercept=0, linetype = "dotted", colour = "black", size = 1) +
  # set your own labels
  xlab("Posterior distribution of parameter") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
print(analysis2_plot)

#analysis 3
posterior3 <- as.matrix(model_bayes7)

analysis3_plot <- mcmc_areas(posterior3,
                             pars = "writing_tracking_reco1",
                             prob = 0.95) + # parameters of interest as they are in the model output
  #plot_title +
  theme_bw(base_size = 16) +
  geom_vline(xintercept=0, linetype = "dotted", colour = "black", size = 1) +
  # set your own labels
  #xlab("Posterior distribution of parameter") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
print(analysis3_plot)

#analysis 4
posterior4 <- as.matrix(attitude_model1)

analysis4_plot <- mcmc_areas(posterior4,
                             pars = c("writing_wordnegative","writing_wordneutral", "writing_wordpositive"),
                             prob = 0.95) + # parameters of interest as they are in the model output
  #plot_title +
  theme_bw(base_size = 16) +
  geom_vline(xintercept=0, linetype = "dotted", colour = "black", size = 1) +
  # set your own labels
  xlab("Posterior distribution of parameter") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
print(analysis4_plot)


#analysis 4b
posterior4b <- as.matrix(attitude_model2)

analysis4b_plot <- mcmc_areas(posterior4b,
                             pars = c("review_wordnegative","review_wordneutral", "review_wordpositive"),
                             prob = 0.95) + # parameters of interest as they are in the model output
  #plot_title +
  theme_bw(base_size = 16) +
  geom_vline(xintercept=0, linetype = "dotted", colour = "black", size = 1) +
  # set your own labels
  xlab("Posterior distribution of parameter") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
print(analysis4b_plot)


#analysis 5a
posterior5a <- as.matrix(model_bayes4)

analysis5a_plot <- mcmc_areas(posterior5a,
                              pars = c("writing_wordnegative","writing_wordneutral", "writing_wordpositive"),
                              prob = 0.95) + # parameters of interest as they are in the model output
  #plot_title +
  theme_bw(base_size = 16) +
  geom_vline(xintercept=0, linetype = "dotted", colour = "black", size = 1) +
  # set your own labels
  xlab("Posterior distribution of parameter") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
print(analysis5a_plot)


#analysis 5b
posterior5b <- as.matrix(model_bayes4b)

analysis5b_plot <- mcmc_areas(posterior5b,
                              pars = c("review_wordnegative","review_wordneutral", "review_wordpositive"),
                              prob = 0.95) + # parameters of interest as they are in the model output
  #plot_title +
  theme_bw(base_size = 16) +
  geom_vline(xintercept=0, linetype = "dotted", colour = "black", size = 1) +
  # set your own labels
  xlab("Posterior distribution of parameter") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
print(analysis5b_plot)

#figures for paper
print(analysis1_plot)
print(analysis2_plot)
print(analysis3_plot)
print(analysis5a_plot)

plots3 <- ggarrange(common.legend = TRUE,
                    analysis1_plot,
                    analysis2_plot,
                    analysis3_plot,
                    align = "hv", 
                    nrow = 1,
                    labels = "AUTO")

ggsave(plots3, filename = "plots3.png", dpi = 300, width = 16, height = 6)

print(analysis5a_plot)

ggsave(analysis5a_plot, filename = "analysis5a_plot.png", dpi = 300, width = 8, height = 8)


# ggplot for writing vs first author pubs ====
# does writing more mean more papers? YES
writepubs <- ggplot(aes(x = hrs_wk_writing, y = firstauthor_pubs), data = survey) + scale_fill_viridis() +
  geom_point(pch = 21, aes(fill = hrs_wk_writing), size = 4) +
  theme_bw(base_size = 14) +
  xlab("Hrs per week devoted to writing") +
  ylab("First author publications")

print(writepubs)
ggsave(writepubs, filename = "writepubs.png", dpi = 300, width = 10, height = 8)


#code from Freya
writepubs <- ggplot(aes(x = hrs_wk_writing, y = firstauthor_pubs), data = survey) +
  geom_point(pch = 21, aes(size = graduate_yrs, fill = postdoc_yrs), alpha = 0.5) +
  scale_fill_viridis() +
  theme_bw(base_size = 14) +
  xlab("Hrs per week devoted to writing") +
  ylab("All publications")
print(writepubs)

