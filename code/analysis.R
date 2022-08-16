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
library(ggrepel) #overlap text or labels onto plot
library(Rmisc) #basic stats (summarySE fx)

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
  xlab("Hours per week devoted to writing") +
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
#writing_box <-
#  ggplot(aes(x = writing_word, y = firstauthor_pubs), 
#         data = na.omit(survey[, c("firstauthor_pubs", #"writing_word")])) +
#  geom_boxplot(fill = c("#EDA09C", "#966480", "#585B74")) + theme_bw#(base_size = 14) +
#  xlab("Feelings about scientific writing") +
#  ylab("First author publications") 
#
#review_box <- ggplot(aes(x = review_word, y = firstauthor_pubs),
#                     data = na.omit(survey[,c("firstauthor_pubs", #"review_word")])) +
#  geom_boxplot(fill=c("#EDA09C", "#966480", "#585B74")) + theme_bw#(base_size = 14) +
#  xlab("Feelings about peer review") +
#  ylab("First author publications")  

#Create 2 panel figure of the pd plots (will add word cloud panels manually)
multi_panel <- ggarrange(common.legend = TRUE,
                         senti_sci_plot,
                         senti_rev_plot,
                         align = "hv", 
                         nrow = 1,
                         ncol = 2,
                         labels = "AUTO")

print(multi_panel)
ggsave(multi_panel, filename = "figures/sentiment_pd.png", dpi = 300, height = 4, width = 10)

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


# Writing groups figure ====
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



# Circular plot - Planning writing ====
plan_write <- read.csv('plan_write.csv', header=TRUE)

1b <- {
  y2 <- ggplot(plan_write) +
    # Make custom panel grid
    geom_hline(aes(yintercept = y), data.frame(y = c(0:6) * 25), color = "lightgrey") + 
    # add bars using geom_col instead geom_bar
    # reorder = data from smaller % to higher
    geom_col(aes(x = reorder((method), count), y = count, fill = percent), position = "dodge2", show.legend = TRUE, alpha = 0.9) +
    # dashed segments from 0 to 150 = y-axis scale
    geom_segment(
      aes(x = reorder((method), count), y = 0, xend = reorder((method), count), yend = 150), linetype = "dashed", color = "gray12") + 
    # make it circular
    coord_polar() +
    # y-axis center scale = so bars don't start in the center point
    scale_y_continuous(limits = c(-50, 150), expand = c(0, 0), breaks = seq(0, 150, by = 25)) +
    # colors fill and legend title
    # same color pattern
    #scale_fill_gradientn("Percentage",
    #colours = c("#DF858E", "#C6798F", "#966480", "#6C5B7B", "#585B74")) +
    #missing the last color to fit to 0-40
    scale_fill_gradientn("Percentage",
                         colours = c("#EDA09C","#DF858E", "#C6798F", "#966480", "#6C5B7B")) +
    # remove axis ticks and text
    theme(legend.position = "bottom",
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          #axis.text.x = element_text(color = "gray12", size = 12),
          axis.title = element_blank(),
          axis.ticks = element_blank()) +
    # make the guide for the fill discrete
    guides(fill = guide_colorsteps(barwidth = 15, barheight = 0.5, title.position = "top", title.hjust = 0.5)) +
    # annotate custom scale inside plot
    annotate(x = 5.5, y =  50, label =  "50", geom = "text", color = "gray12", size = 3.5) +
    annotate(x = 5.5, y = 100, label = "100", geom = "text", color = "gray12", size = 3.5) +
    annotate(x = 5.5, y = 150, label = "150", geom = "text", color = "gray12", size = 3.5) +
    # remove background and plot lines - it will show the custom panel grid made
    theme(text = element_text(color = "gray12"),
          panel.background = element_rect(fill = "white", color = "white"),
          panel.grid = element_blank(),
          panel.grid.major.x = element_blank() + theme_classic(base_size = 14))
  y2
  ggsave(plot=y2, "plan_write.png", width=6, height=6, dpi=1200)
}

View(1b)

# Circular plot - Tracking writing ====
track_write <- read.csv('track_write.csv', header=TRUE)
# Q. how do you track your writing progress? multiple choice
#1	Electronic spreadsheets
#2	Electronic note taking applications
#3	Physical notebook
#4	Checking in with writing accountability/support group
#5	Checking in with advisor or mentor
#6	I do not track my writing progress
#7	Other (please specify)
{
  y3 <- ggplot(track_write) +
    # Make custom panel grid
    geom_hline(aes(yintercept = y), data.frame(y = c(0:5) * 40), color = "lightgrey") + 
    # add bars using geom_col instead geom_bar
    # reorder = data from smaller % to higher
    geom_col(aes(x = reorder((track), count), y = count, fill = percent), position = "dodge2", show.legend = TRUE, alpha = 0.9) +
    # dashed segments from 0 to 150 = y-axis scale
    geom_segment(aes(x = reorder((track), count), y = 0, xend = reorder((track), count), yend = 200), linetype = "dashed", color = "gray12") +
    # make it circular
    coord_polar() +
    # y-axis center scale = so bars don't start in the center point
    scale_y_continuous(limits = c(-40, 200), expand = c(0, 0), breaks = seq(0, 200, by = 40)) +
    # colors fill and legend title
    scale_fill_gradientn("Percentage",
                         colours = c("#EDA09C","#DF858E", "#C6798F", "#966480", "#6C5B7B", "#585B74")) +
    # remove axis ticks and text
    theme(legend.position = "bottom",
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          #axis.text.x = element_text(color = "gray12", size = 12),
          axis.title = element_blank(),
          axis.ticks = element_blank()) +
    # make the guide for the fill discrete
    guides(fill = guide_colorsteps(barwidth = 15, barheight = 0.5, title.position = "top", title.hjust = 0.5)) +
    # annotate custom scale inside plot
    annotate(x = 7.5, y =  40, label =  "40", geom = "text", color = "gray12", size = 3.5) +
    annotate(x = 7.5, y = 120, label = "120", geom = "text", color = "gray12", size = 3.5) +
    annotate(x = 7.5, y = 200, label = "200", geom = "text", color = "gray12", size = 3.5) +
    # remove backgroung and plot lines - it will show the custom panel grid made
    theme(text = element_text(color = "gray12"),
          panel.background = element_rect(fill = "white", color = "white"),
          panel.grid = element_blank(),
          panel.grid.major.x = element_blank())
  y3
  ggsave(plot=y3, "write_track.png", width=6, height=6, dpi=1200)
}

#Bar plot for figure 4 - responses about writing group participation ====
# Fig 5 ----
# writing groups questions
# Fig 5A
# Q. Have you participated in a writing support group? (y/n)
# data
surv3 <- survey %>%
  select(Response_ID,writing_support,no_support,yes_support)
# writing_support = y/n
table(survey$writing_support)
# 124 yes, 163 no, 287 total
# 57% yes, 43% no
# data frame
sup <- data.frame(type=rep(c('Yes', 'No')),
                  count=c(124, 163),
                  survey=rep(c('wrt_support')))
# FIG 5A - plot
{
  m1 <- ggplot() +
    geom_col(aes(y=type, x=count, fill=type), data=sup, width=0.3, color="black") +
    #panel/border + ticks
    theme(axis.ticks.length=unit(-0.15, "cm"), 
          axis.line = element_blank(), 
          panel.border = element_rect(color = "black", fill=NA)) +
    #axis text-limits
    scale_x_continuous(limits=c(0, 200)) +
    #axis text and labels
    xlab("") +
    theme(axis.text.y = element_text(margin=margin(r=10), size=12, color="black"),
          axis.title.y = element_blank(),
          axis.text.x = element_text(margin=margin(t=10), size=12, color="black"),
          axis.title.x = element_text(margin=margin(t=10), size=12, color="black")) +
    # title
    #ggtitle("Have you participated in a writing support group?") +
    #theme(plot.title = element_text(size=16)) +
    #color yes = charcoal #405364, no = fuchsia #966480
    scale_fill_manual(values=(c("#405364", "#966480"))) +   
    #legend
    theme(legend.position = "none") +
    # plot
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA),
          strip.background = element_blank()) +
    # texts
    annotate("text", x=160, y=1.3, size=4, label="43%") +
    annotate("text", x=125, y=2.3, size=4, label="57%")
  m1
  #ggsave(plot=m1, device="png", "wrt_sup.png", width=8, height=3, dpi=200)
}
# Fig 5B
# Q. Why did you not participate in a writing group?
# no interest 66 - 32%
# dont know   62 - 30%
# use lab     41 - 20%
# not find    37 - 18%
# data
nosup <- data.frame(type=rep(c('4nointerest', '3dontkmow', '2lab', '1nofind')),
                    count=c(66, 62, 41, 37),
                    survey=rep(c('no_sup')))
# FIG 5B - plot
{
  m2 <- ggplot(aes(y=type, x=count), data=nosup) +
    geom_col(width=0.3, color="black") +
    #panel/border + ticks
    theme(axis.ticks.length=unit(-0.15, "cm"), 
          axis.line = element_blank(), 
          panel.border = element_rect(color = "black", fill=NA)) +
    #axis text-limits
    scale_x_continuous(limits=c(0,100)) +
    # title
    #ggtitle("Why did you not participate in a writing group?") +
    #theme(plot.title = element_text(size=16)) +
    #axis text and labels
    xlab(" ") +
    theme(axis.text.y = element_text(margin=margin(r=10), size=12, color="black"),
          axis.title.y = element_blank(),
          axis.text.x = element_text(margin=margin(t=10), size=12, color="black"),
          axis.title.x = element_text(margin=margin(t=10), size=12, color="black"))  +
    scale_y_discrete(labels=c("4nointerest"= "Not interested\n in joining one",
                              "3dontkmow"= "Did not know\n they existed",
                              "2lab"= "Had ample\n lab support",
                              "1nofind"= "Could not\n find one")) +
    # plot
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA),
          strip.background = element_blank()) +
    # texts
    annotate("text", x=65, y=4.3, size=4, label="32%") +
    annotate("text", x=61, y=3.3, size=4, label="30%") +
    annotate("text", x=40, y=2.3, size=4, label="20%") +
    annotate("text", x=36, y=1.3, size=4, label="18%") 
  m2
  #ggsave(plot=m2, device="png", "fig_nosupport.png", width=8, height=4, dpi=200)
}
# Fig 5C
# Q. Which type of writing support group have you participated?
# peer     80 - 37% 
# course   44 - 20%
# workshop 43 - 20%
# retreat  43 - 20%
# mentor   7  - 3%
# data
ysup <- data.frame(type=rep(c('5peer', '4course', '3workshop', '2retreat', '1mentor')),
                   count=c(80, 44, 43, 43, 7),
                   survey=rep(c('y_sup')))
# FIG 5C - plot
{
  m3 <- ggplot(aes(y=type, x=count), data=ysup) +
    geom_col(width=0.3, color="black") +
    #panel/border + ticks
    theme(axis.ticks.length=unit(-0.15, "cm"), 
          axis.line = element_blank(), 
          panel.border = element_rect(color = "black", fill=NA)) +
    #axis text-limits
    scale_x_continuous(limits=c(0,100)) +
    # title
    #ggtitle("Which type of writing group have you participated?") +
    #theme(plot.title = element_text(size=16)) +
    #axis text and labels
    xlab(" ") +
    theme(axis.text.y = element_text(margin=margin(r=10), size=12, color="black"),
          axis.title.y = element_blank(),
          axis.text.x = element_text(margin=margin(t=10), size=12, color="black"),
          axis.title.x = element_text(margin=margin(t=10), size=12, color="black")) +
    scale_y_discrete(labels=c("5peer"= "Peer group",
                              "4course"= "Course",
                              "3workshop"= "Workshop",
                              "2retreat" = "Writing Retreat/\nStudy Hall",
                              "1mentor" = "Mentoring \nprogram")) +
    # plot
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA),
          strip.background = element_blank()) +
    # texts
    annotate("text", x=07, y=1.3, size=4, label="3%") +
    annotate("text", x=44, y=2.3, size=4, label="20%") +
    annotate("text", x=43, y=3.3, size=4, label="20%") +
    annotate("text", x=43, y=4.3, size=4, label="20%") +
    annotate("text", x=80, y=5.3, size=4, label="37%")
  m3
  #ggsave(plot=m3, device="png", "Fig5C.png", width=6, height=4, dpi=1200)
}
# Fig 5D - writing group effects w/ 12 variables
wrt_group <- read.csv("data/wrtgr_effect_sum2.csv", header=TRUE)
# FIG 5D
{
  m4 <- ggplot() +
    geom_col(aes(y=type, x=count, fill=effec), data=wrt_group, width=0.3, color="black") +
    #panel/border + ticks
    theme(axis.ticks.length=unit(-0.15, "cm"), 
          axis.line = element_blank(), 
          panel.border = element_rect(color="black", fill=NA)) +
    #axis text-limits
    scale_x_continuous(limits=c(0,125)) +
    #title
    #ggtitle("How did your perspective on writing change through participating in a writing group?") +
    theme(plot.title = element_text(size=16)) +
    #axis text and labels
    xlab("Number of Answers") +
    theme(axis.text.y = element_text(margin=margin(r=10), size=12, color="black"),
          axis.title.y = element_blank(),
          axis.text.x = element_text(margin=margin(t=10), size=12, color="black"),
          axis.title.x = element_text(margin=margin(t=10), size=12, color="black")) +
    scale_y_discrete(labels=c("12output"= "Writing output",
                              "11starting" = "Less difficulty starting",
                              "10quality"= "Writing quality",
                              "09time"= "Time management",
                              "08goal"= "Goal setting",
                              "07anxiety" = "Overcoming anxiety",
                              "06perfect"= "Overcoming \nperfectionism paralysis",
                              "05skills" = "Technical writing skills",
                              "04camarad"= "Camaraderie",
                              "03reviews" = "Giving or \nreceiving reviews",
                              "02imposter" = "Overcoming \nimposter syndrome",
                              "01collab" = "Collaboration")) +
    #color better = charcoal #405364, same = #EDA09C, worse = fuchsia #966480
    scale_fill_manual(" ",
                      values=(c("#405364", "#EDA09C", "#966480"))) + 
    #legend
    #theme(legend.position = "none") +
    theme(legend.position = c(0.91, 0.12),
          legend.key.size = unit(0.50, 'cm')) + # x and y
    # plot
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA),
          strip.background = element_blank()) +
    # texts
    annotate("text", x=060, y=01.3, size=4, label="65%") +
    annotate("text", x=092, y=01.3, size=4, label="35%") +
    annotate("text", x=003, y=02.3, size=4, label="3%") +
    annotate("text", x=064, y=02.3, size=4, label="60%") +
    annotate("text", x=102, y=02.3, size=4, label="36%") +
    annotate("text", x=037, y=03.3, size=4, label="64%") +
    annotate("text", x=102, y=03.3, size=4, label="4%") +
    annotate("text", x=025, y=04.3, size=4, label="24%") +
    annotate("text", x=103, y=04.3, size=4, label="76%") +
    annotate("text", x=050, y=05.3, size=4, label="48%") +
    annotate("text", x=104, y=05.3, size=4, label="52%") +
    annotate("text", x=003, y=06.3, size=4, label="3%") +
    annotate("text", x=048, y=06.3, size=4, label="43%") +
    annotate("text", x=106, y=06.3, size=4, label="55%") +
    annotate("text", x=007, y=07.3, size=4, label="7%") +
    annotate("text", x=047, y=07.3, size=4, label="37%") +
    annotate("text", x=107, y=07.3, size=4, label="56%") +
    annotate("text", x=033, y=08.3, size=4, label="29%") +
    annotate("text", x=109, y=08.3, size=4, label="71%") +
    annotate("text", x=002, y=09.3, size=4, label="2%") +
    annotate("text", x=056, y=09.3, size=4, label="49%") +
    annotate("text", x=110, y=09.3, size=4, label="49%") +
    annotate("text", x=046, y=10.3, size=4, label="41%") +
    annotate("text", x=111, y=10.3, size=4, label="59%") +
    annotate("text", x=001, y=11.3, size=4, label="1%") +
    annotate("text", x=057, y=11.3, size=4, label="49%") +
    annotate("text", x=113, y=11.3, size=4, label="50%") +
    annotate("text", x=002, y=12.3, size=4, label="2%") +
    annotate("text", x=037, y=12.3, size=4, label="31%") +
    annotate("text", x=114, y=12.3, size=4, label="68%")
  m4
  #ggsave(plot=m4, device="png", "Fig5D.png", width=10, height=6, dpi=1200)
}
# creating panel 
{
  # x= 0-1, y= 0-1, width= 0-1, height= 0-1
  MM <- ggdraw() +
    draw_plot(m1, x=0.150, y=0.80, width=0.75, height =0.170) + # top
    draw_plot(m2, x=0.005, y=0.50, width=0.47, height =0.30) + # m. left
    draw_plot(m3, x=0.475, y=0.50, width=0.50, height =0.30) + # m. right
    draw_plot(m4, x=0.005, y=0.003, width=0.97, height =0.48) + # bottom
    draw_label(x=0.05, y=0.98, size=18, "A") +
    draw_label(x=0.47, y=0.98, size=14, "Have you participated in a writing support group?") +
    draw_label(x=0.05, y=0.81, size=18, "B") +
    draw_label(x=0.30, y=0.81, size=14, "Reason for not participating") +
    draw_label(x=0.50, y=0.81, size=18, "C") +
    draw_label(x=0.80, y=0.81, size=14, "Type of group, if participated") +
    draw_label(x=0.05, y=0.49, size=18, "D") +
    draw_label(x=0.57, y=0.49, size=14, "Change in perspective through participating in a writing group")
  
  ggsave(plot= MM, device = "png", "Fig4AD.png", width=8, height=10 , dpi=200)
}


# Bar plot for figure 5 - lab group involvment ====
# Fig 5A
# Q. Collaborated writing with your lab members? (y/n)
# Q. Lab members provide feedback? (y/n)
# Q. Has lab feedback improved writing? (y/n)
# lab_collab with NA
{
  # data frame
  # lab_collab = col
  table(surv1$col)
  # 55  NA, 109 No, 188 Yes total = 352
  # 15% NA, 30% No, 53% Yes
  col <- data.frame(type=rep(c('Yes', 'No', 'NA')),
                    count=c(188, 109, 55),
                    survey=rep(c('3lab_collab')))
  
  # lab_feedback = feed
  table(surv1$feed)
  # 55  NA, 91  No, 206 Yes total = 352
  # 15% NA, 25% No, 58% Yes
  feed <- data.frame(type=rep(c('Yes', 'No', 'NA')),
                     count=c(206, 91, 55),
                     survey=rep(c('2lab_feedback')))
  
  # lab_feedback_helped = help
  table(surv1$help)
  # 148 NA, 11 No, 193 Yes total = 352
  # 42% NA, 3% No, 54% Yes
  help <- data.frame(type=rep(c('Yes', 'No', 'NA')),
                     count=c(193, 11, 148),
                     survey=rep(c('1lab_feedback_helped')))
  # plot
  p1 <- ggplot() +
    geom_col(aes(y=survey, x=count, fill=type), data=col, position=position_stack(reverse=TRUE), width=0.3, color="black") +
    geom_col(aes(y=survey, x=count, fill=type), data=feed, position=position_stack(reverse = TRUE), width=0.3, color="black") +
    geom_col(aes(y=survey, x=count, fill=type), data=help, position=position_stack(reverse = TRUE), width=0.3, color="black") +
    #panel/border + ticks
    theme(axis.ticks.length=unit(-0.15, "cm"), 
          axis.line = element_blank(), 
          panel.border = element_rect(color = "black", fill=NA)) +
    #axis text-limits
    scale_x_continuous(limits=c(0, 400)) +
    #axis text and labels
    xlab("Number of Answers") +
    theme(axis.text.y = element_text(margin=margin(r=10), size=14, color="black"),
          axis.title.y = element_blank(),
          axis.text.x = element_text(margin=margin(t=10), size=14, color="black"),
          axis.title.x = element_text(margin=margin(t=10), size=14, color="black")) +
    scale_y_discrete(labels=c("3lab_collab"= "Writing collaboration\n with lab members?",
                              "2lab_feedback"= "Does lab\nprovide feedback?",
                              "1lab_feedback_helped"= "Has feedback\nimproved writing?")) +
    #color
    scale_fill_manual(values=(c("#AAACB0", "#fca311", "#277da1"))) +
    #legend
    theme(legend.position = c(0.90,0.07), #x,y
          legend.title = element_blank(),
          legend.key.size = unit(0.6, "cm"),
          legend.text = element_text(size=10),
          legend.background = element_blank()) +
    # plot
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA),
          strip.background = element_blank()) +
    #add text
    annotate("text", x=025, y=1.3, size=4, label="42%") +
    annotate("text", x=155, y=1.3, size=4, label="5%") +
    annotate("text", x=300, y=1.3, size=4, label="54%") +
    
    annotate("text", x=025, y=2.3, size=4, label="15%") +
    annotate("text", x=125, y=2.3, size=4, label="25%") +
    annotate("text", x=300, y=2.3, size=4, label="58%") +
    
    annotate("text", x=025, y=3.3, size=4, label="15%") +
    annotate("text", x=125, y=3.3, size=4, label="30%") +
    annotate("text", x=300, y=3.3, size=4, label="53%")
  p1
  #ggsave(plot=p1, device="png", "Fig_Lab1.png", width=6, height=4, dpi=200)
}
# lab_collab without NA
{
  # data frame
  # lab_collab = col1
  table(survey$lab_collab)
  # 109 No, 188 Yes total 297
  # 37% No, 63% Yes
  col1 <- data.frame(type=rep(c('Yes', 'No')),
                     count=c(188, 109),
                     survey=rep(c('3lab_collab')))
  
  # (2) lab_feedback = feed1
  table(survey$lab_feedback)
  # 91  No, 206 Yes
  # 31% No, 69% Yes
  feed1 <- data.frame(type=rep(c('Yes', 'No')),
                      count=c(206, 91),
                      survey=rep(c('2lab_feedback')))
  
  # (3) lab_feedback_helped = help1
  table(survey$lab_feedback_helped)
  # 11 No, 193 Yes
  # 5% No, 95% Yes
  help1 <- data.frame(type=rep(c('Yes', 'No')),
                      count=c(193, 11),
                      survey=rep(c('1lab_feedback_helped')))
}
# FIG 5A (plot w/out NAs)
{
  p2 <- ggplot() +
    geom_col(aes(y=survey, x=count, fill=type), data=col1, position=position_stack(reverse=TRUE), width=0.3, color="black") +
    geom_col(aes(y=survey, x=count, fill=type), data=feed1, position=position_stack(reverse = TRUE), width=0.3, color="black") +
    geom_col(aes(y=survey, x=count, fill=type), data=help1, position=position_stack(reverse = TRUE), width=0.3, color="black") +
    #panel/border + ticks
    theme(axis.ticks.length=unit(-0.15, "cm"), 
          axis.line = element_blank(), 
          panel.border = element_rect(color = "black", fill=NA)) +
    #axis text-limits
    scale_x_continuous(limits=c(0, 350)) +
    #axis text and labels
    xlab(" ") +
    theme(axis.text.y = element_text(margin=margin(r=10), size=12, color="black"),
          axis.title.y = element_blank(),
          axis.text.x = element_text(margin=margin(t=10), size=12, color="black"),
          axis.title.x = element_text(margin=margin(t=10), size=12, color="black")) +
    scale_y_discrete(labels=c("3lab_collab"= "Writing collaboration\n with lab members?",
                              "2lab_feedback"= "Does your lab\nprovide feedback?",
                              "1lab_feedback_helped"= "Has feedback\nimproved writing?")) +
    #color yes = charcoal #405364, no = fuchsia #966480
    scale_fill_manual(values=(c("#966480", "#405364"))) +   
    #legend
    theme(legend.position = c(0.90,0.15), #x,y
          legend.title = element_blank(),
          legend.key.size = unit(0.3, "cm"),
          legend.text = element_text(size=8),
          legend.background = element_blank()) +
    # plot
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA),
          strip.background = element_blank()) +
    # texts
    annotate("text", x=010, y=1.3, size=4, label="5%") +
    annotate("text", x=150, y=1.3, size=4, label="95%") +
    annotate("text", x=050, y=2.3, size=4, label="31%") +
    annotate("text", x=250, y=2.3, size=4, label="69%") +
    annotate("text", x=050, y=3.3, size=4, label="37%") +
    annotate("text", x=250, y=3.3, size=4, label="63%")
  p2
  #ggsave(plot=p2, device="png", "Fig_collab.png", width=8, height=4, dpi=200)
}
# Fig 5B - pi_involved
# Q. How is your advisor involved in your writing? (multiple options)
# data
inv <- data.frame(type=rep(c('6RevisingMajor', '5Planning', '4RevisingMinor','3Drafting', '2Writing', '1NotInvolved')),
                  count=c(255, 202, 185, 114, 51, 5),
                  survey=rep(c('PI_involved')))
# FIG 5B plot
{
  p5 <- ggplot(aes(y=type, x=count), data=inv) +
    geom_col(width=0.3, color="black") +
    #panel/border + ticks
    theme(axis.ticks.length=unit(-0.15, "cm"),
          axis.line = element_blank(),
          panel.border = element_rect(color = "black", fill=NA)) +
    #axis text-limits
    scale_x_continuous(limits=c(0,300)) +
    # title
    #ggtitle("How is your advisor involved in your writing?") +
    #theme(plot.title = element_text(size=16)) +
    #axis text and labels
    xlab(" ") +
    theme(axis.text.y = element_text(margin=margin(r=10), size=12, color="black"),
          axis.title.y = element_blank(),
          axis.text.x = element_text(margin=margin(t=10), size=12, color="black"),
          axis.title.x = element_text(margin=margin(t=10), size=12, color="black")) +
    scale_y_discrete(labels=c("6RevisingMajor"= "Major revisions",
                              "5Planning" = "Planning",
                              "4RevisingMinor"= "Minor revisions",
                              "3Drafting"= "Drafting and outlining",
                              "2Writing"= "Writing sections",
                              "1NotInvolved" = "Not involved in writing")) +
    # plot
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA),
          strip.background = element_blank()) +
    # texts
    annotate("text", x=255, y=6.3, size=4, label="31%") +
    annotate("text", x=202, y=5.3, size=4, label="25%") +
    annotate("text", x=185, y=4.3, size=4, label="23%") +
    annotate("text", x=114, y=3.3, size=4, label="14%") +
    annotate("text", x=051, y=2.3, size=4, label="6%")  +
    annotate("text", x=005, y=1.3, size=4, label="1%") 
  p5
  #ggsave(plot=p5, device="png", "Fig_advisor.png", width=8, height=4, dpi=200)
}
# number of revisions
# data
table(survey$PI_revisions)
# NA = 63
# 1     18  6%
# 1to3  85  29%
# 3to5  87  30%
# 5+    102 34%
# total 292 

# create new columns for total training, pubs, and career stage
survey$trainingtot <- rowSums(survey[,c("graduate_yrs", "postdoc_yrs")], na.rm=TRUE)
survey$pubtotal <- rowSums(survey[,c("firstauthor_pubs", "coauthor_pubs")], na.rm=TRUE)
survey$stage <- ifelse(is.na(survey$postdoc_yrs), "grad", "postdoc")
# remove NAs
surv2 <- survey %>%
  select(Response_ID, PI_revisions, stage) %>%
  mutate(rev=paste0(PI_revisions)) %>%
  mutate_all(na_if,"") %>%
  drop_na(rev) # NA 63

table(surv2$rev) 
table(surv2$stage) #grad 222 postdoc 70
# FIG 5C
{
  p3 <- ggplot(aes(y=rev), data=surv2) +
    geom_bar(width=0.3, color="black") +
    #panel/border + ticks
    theme(axis.ticks.length=unit(-0.15, "cm"), 
          axis.line = element_blank(), 
          panel.border = element_rect(color="black", fill=NA)) +
    #axis text-limits
    scale_x_continuous(limits=c(0,105)) +
    # title
    #ggtitle("How many round of revisions before submission?") +
    #theme(plot.title = element_text(size=16)) +
    #axis text and labels
    xlab("Number of Answers") +
    theme(axis.text.y = element_text(margin=margin(r=10), size=12, color="black"),
          axis.title.y = element_blank(),
          axis.text.x = element_text(margin=margin(t=10), size=12, color="black"),
          axis.title.x = element_text(margin=margin(t=10), size=12, color="black")) +
    # plot
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA),
          strip.background = element_blank()) +
    # texts
    annotate("text", x=18, y=1.3, size=4, label="6%") +
    annotate("text", x=85, y=2.3, size=4, label="29%") +
    annotate("text", x=87, y=3.3, size=4, label="30%") +
    annotate("text", x=100, y=4.3, size=4, label="34%") 
  p3
  #ggsave(plot=p3, device="png", "Fig_Rev.png", width=8, height=4, dpi=200)
}
# same plot divided by grad/postdoc
{
  s1 <- surv2 %>%
    filter(grepl('1to3', rev))
  table(s1$stage) #grad 66 postdoc 19
  s2 <- surv2 %>%
    filter(grepl('3to5', rev))
  table(s2$stage) #grad 64 postdoc 23
  s3 <- surv2 %>%
    filter(!grepl('3', rev)) %>%
    filter(grepl('5', rev))
  table(s3$stage) #grad 81 postdoc 21
  s4 <- surv2 %>%
    filter(!grepl('3', rev)) %>%
    filter(grepl('1', rev))
  table(s4$stage) #grad 11 postdoc 7
  
  p4 <- ggplot(aes(y=rev, fill=stage), data=surv2) +
    geom_bar(width=0.3, color="black") +
    #panel/border + ticks
    theme(axis.ticks.length=unit(-0.15, "cm"), 
          axis.line = element_blank(), 
          panel.border = element_rect(color = "black", fill=NA)) +
    #axis text-limits
    scale_x_continuous(limits=c(0,105)) +
    # title
    ggtitle("How many round of revisions before submission?") +
    #axis text and labels
    xlab("Number of Answers") +
    theme(axis.text.y = element_text(margin=margin(r=10), size=16, color="black"),
          axis.title.y = element_blank(),
          axis.text.x = element_text(margin=margin(t=10), size=16, color="black"),
          axis.title.x = element_text(margin=margin(t=10), size=16, color="black"))
  p4
  #ggsave(plot=p4, device="png", "Fig.png", width=8, height=4, dpi=200)
}
# creating panel
{
  # x= 0-1, y= 0-1, width= 0-1, height= 0-1
  PP <- ggdraw() +
    draw_plot(p2, x=0.022, y=0.70, width=0.935, height =0.25) + #top
    draw_plot(p5, x=0.005, y=0.45, width=0.950, height =0.25) + #middle
    draw_plot(p3, x=0.19, y=0.20, width=0.769, height =0.25) + #bottom
    draw_label(x=0.05, y=0.96, size=18, "A") +
    draw_label(x=0.38, y=0.71, size=14, "Advisor involvement") +
    draw_label(x=0.05, y=0.71, size=18, "B") +
    draw_label(x=0.50, y=0.46, size=14, "Rounds of revisions before submission") +
    draw_label(x=0.05, y=0.46, size=18, "C")
  ggsave(plot= PP, device = "png", "Fig4_lab5.png", width=7, height=10 , dpi=200)
}

#