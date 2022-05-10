#Copied from "covid-identity" Github repo by YAA on 4/29/2022. Will edit to remove non-relevant code

#install packages if needed
install.packages("viridis")
install.packages("tm")
install.packages("ggpubr")
install.packages("rstanarm")
install.packages("easystats", repos = "https://easystats.r-universe.dev")
install.packages("logspline")
install.packages("BayesFactor")
install.packages("shinystan")


# libraries
library(ggplot2) 
library(viridis)
library(RColorBrewer)
library(tm)
library(dplyr)
library(ggpubr)
library(rstanarm)
library(remotes)
library(easystats)
library(tidyr)
library(ggridges)
library(glue)
library(bayesplot)
library(bayestestR)
library(logspline)
library(car)
library(BayesFactor) 
library(shinystan)

# Load data ====
survey <- read.csv("data/dataclean_Nov2.csv", header = TRUE)

empty_as_na <- function(x){ # empty_as_na function did not exist so I found this code to create the function
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ifelse(as.character(x)!="", x, NA)}
  
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

# Make two dataframes for grads and postdocs ----
grads <- subset(survey, is.na(survey$postdoc_yrs))
postdocs <- subset(survey, !is.na(survey$postdoc_yrs))


# Demographics ----
survey %>%
  group_by(stage) %>%
  summarize(avgfirst = mean(firstauthor_pubs, na.rm = TRUE),
            avgco = mean(coauthor_pubs, na.rm = TRUE),
            sdfirst = sd(firstauthor_pubs, na.rm = TRUE),
            sdco = sd(coauthor_pubs, na.rm = TRUE))

# First_gen n = 272, grad = 194 respondents, postdoc = 78
survey %>%
  group_by(first_gen) %>%
  summarize(n())

# 193 grads, 140 female, 47 male, 6 non-binary-thirdgender/other
# 76 postdocs, 46 female, 28 male, 2 other
survey %>%
  group_by(gender_identity) %>%
  summarize(n())

# disability or health issue
# n = 193 grads, n = 77 postdocs
survey %>%
  group_by(condition) %>%
  summarize(n())

# first language English
# n = 199 grads, n = 78 postdocs
survey %>%
  group_by(first_language_english) %>%
  summarize(n())

# BIPOC
# n = 360, 53 = yes, 307 = no
survey %>%
  group_by(BIPOC) %>%
  summarize(n())

#report Rhat being lass than 1.01, record neff, want mean and median to be about the same, report median and 95 CI
#multiple regression, can add random effect (stan_gmler), X ` predcitors, iter start with 2000 then increase when model is right, 4 chains means you run it 4 times (keep as 4), need at least 20,000 for final product (chains*iter), cores is # cores used by computer, warmup discards the first 5000, usually throw away half of iter as warmup, order doesn't matter, set seed)

###-------This is where writing habits ms analysis start -Yara-----###

#Analysis 1: pubtotal vs. time spent writing ====
#Q for Freya: do we add any of the identity variables?
#figure out if we need to analyze by grad and post docs? or ok to keep all together
model_bayes1a <- stan_glm(pubtotal ~ 
                          hrs_wk_writing,
                        iter = 10000,
                        cores = 3,
                        chains = 4,
                        warmup = 5000, 
                        data= survey, seed=111) #All data

model_bayes1b <- stan_glm(pubtotal ~ 
                          hrs_wk_writing,
                        iter = 10000,
                        cores = 3,
                        chains = 4,
                        warmup = 5000, 
                        data= grads, seed=111) #All data

model_bayes1c <- stan_glm(pubtotal ~ 
                          hrs_wk_writing,
                        iter = 10000,
                        cores = 3,
                        chains = 4,
                        warmup = 5000, 
                        data= postdocs, seed=111) #All data

loo(model_bayes1a)
prior_summary(model_bayes1a)
summary(model_bayes1a, digits = 3)
posterior_interval(
  model_bayes1a,
  prob = 0.9)
plot(model_bayes1a)

launch_shinystan(model_bayes1a)

loo(model_bayes1b)
summary(model_bayes1b, digits = 3)
posterior_interval(
  model_bayes1b,
  prob = 0.9)

loo(model_bayes1c)
summary(model_bayes1c, digits = 3)
posterior_interval(
  model_bayes1c,
  prob = 0.9)

# for all data combined how does writing time relate to pub total
model_bayesx <- stan_glm(hrs_wk_writing ~ trainingtot, data = survey)
summary(model_bayesx, digits = 3)
posteriorsx <- describe_posterior(model_bayesx)

# for a nicer table
print_md(posteriorsx, digits = 3)

posteriorx <- as.matrix(model_bayesx)

# graph of relationship total pubs and total training
linpubs <- ggplot(aes(x = trainingtot, y = pubtotal), data = survey) +
  geom_point(aes(size = postdoc_yrs), alpha = 0.5) +
  scale_fill_viridis() +
  theme_bw(base_size = 14) +
  xlab("Total yrs as trainee (grad + postdoc)") +
  ylab("Total publications")

# Correlation between pubtotal and total training years
cor(survey$pubtotal, survey$trainingtot, method = "pearson")
cor(survey$pubtotal, survey$trainingtot, method = "spearman")

# relationship first author pubs and total training 
linpubs <- ggplot(aes(x = trainingtot, y = firstauthor_pubs), data = survey) +
  geom_point(pch = 21, aes(size = postdoc_yrs, fill = postdoc_yrs), alpha = 0.5) +
  scale_fill_viridis() +
  theme_bw(base_size = 14) +
  xlab("Total yrs as trainee (grad + postdoc)") +
  ylab("First-author publications")

# does writing more mean more papers? YES
writepubs <- ggplot(aes(x = hrs_wk_writing, y = pubtotal), data = survey) +
  geom_point(pch = 21, aes(size = graduate_yrs, fill = postdoc_yrs), alpha = 0.5) +
  scale_fill_viridis() +
  theme_bw(base_size = 14) +
  xlab("Hrs per week devoted to writing") +
  ylab("All publications")

writetrain <- ggplot(aes(y = hrs_wk_writing, x = trainingtot), data = survey) +
  geom_point(pch = 21, aes(size = graduate_yrs, fill = postdoc_yrs), alpha = 0.5) +
  scale_fill_viridis() +
  theme_bw(base_size = 14) +
  ylab("Hrs per week devoted to writing") +
  xlab("Yrs as trainee")

# hrs writing per week and training
#summary(lm(hrs_wk_writing ~ trainingtot, data = survey)) #yes - more training = more time
#summary(lm(hrs_wk_writing ~ trainingtot, data = grads)) #yes - more training = more time
#summary(lm(hrs_wk_writing ~ trainingtot, data = postdocs)) #no - not increasing writing #time with training

## total writing time and first author pubs
#summary(lm(firstauthor_pubs ~ hrs_wk_writing, data = survey)) #yes
#summary(lm(firstauthor_pubs ~ hrs_wk_writing, data = grads)) #no
#summary(lm(firstauthor_pubs ~ hrs_wk_writing, data = postdocs)) #marginal

## hrs per week and coauthor pubs
#summary(lm(coauthor_pubs ~ hrs_wk_writing, data = survey)) #no
#summary(lm(coauthor_pubs ~ hrs_wk_writing, data = grads)) #yes
#summary(lm(coauthor_pubs ~ hrs_wk_writing, data = postdocs)) #no

## hrs per week and all pubs
#summary(lm(pubtotal ~ hrs_wk_writing, data = survey)) #yes
#summary(lm(pubtotal ~ hrs_wk_writing, data = grads)) #yes
#summary(lm(pubtotal ~ hrs_wk_writing, data = postdocs)) #no


# Attitudes toward science writing and review 
summary(survey$writing_word)
summary(survey$review_word)

# Boxplots attitudes about writing and review 
# writing word connotation vs. time spent writing
survey$writing_word <- as.factor(survey$writing_word)
summary((lm(hrs_wk_writing ~ writing_word, data = survey, na.rm = TRUE)))
boxplot(hrs_wk_writing ~ writing_word, data = survey)

#New analysis 5.3.2022
# Analysis 4: Number of 1st author pubs vs. feelings about writing process -- repeated down below in Bayesian
#summary((lm(firstauthor_pubs ~ writing_word, data = survey, na.rm = TRUE)))
#boxplot(firstauthor_pubs ~ writing_word, data = survey)

# writing tracking 
#boxplot(hrs_wk_writing ~ plan_writing, data = survey)
#summary(lm(hrs_wk_writing ~ plan_writing, data = survey, na.rm = TRUE))

# plan writing model
plan_model <- stan_glm(hrs_wk_writing ~ plan_writing, 
                       iter = 10000,
                       cores = 3,
                       chains = 4,
                       warmup = 5000, data = survey)

summary(plan_model)
posteriors <- describe_posterior(plan_model)
# for a nicer table
print_md(posteriors, digits = 3)



# Analysis 2: writing attitude vs. plan writing ====
#2A scientific writing word
survey$plan_writing <- as.numeric(survey$plan_writing)
plan_model <- stan_glm(plan_writing ~ 1 + writing_word, #q for Freya: what is this "1"?
                           iter = 10000,
                           cores = 3,
                           chains = 4,
                           warmup = 5000, 
                           data = survey,
                           family = binomial)

describe_posterior(plan_model, test = c("p_direction", "rope", "bayesfactor"))
summary(plan_model, digits = 3)
posteriors_plan_model <- posterior(plan_model)

loo(plan_model)
prior_summary(plan_model)
summary(plan_model, digits = 3)
posterior_interval(
  plan_model,
  prob = 0.9)
plot(plan_model)

boxplot(hrs_wk_writing ~ writing_word, data = survey)

#2B: review word
plan_model2 <- stan_glm(plan_writing ~ 1 + review_word, #q for Freya: what is this "1"?
                       iter = 10000,
                       cores = 3,
                       chains = 4,
                       warmup = 5000, 
                       data = survey,
                       family = binomial)

describe_posterior(plan_model2, test = c("p_direction", "rope", "bayesfactor"))
summary(plan_model2, digits = 3)
posteriors_plan_model2 <- posterior(plan_model2)

loo(plan_model2)
prior_summary(plan_model2)
summary(plan_model2, digits = 3)
posterior_interval(
  plan_model2,
  prob = 0.9)
plot(plan_model2)

boxplot(hrs_wk_writing ~ review_word, data = survey)


#Q for Freya: did we end up using this function for anything?
# convert to probabilities
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

# positive probability of tracking writing
logit2prob(0.365)
logit2prob(0.064)
logit2prob(-0.824)

# Analysis 3: time per week spent writing and attitude toward writing and review ====
# plan writing model
attitude_model1 <- stan_glm(hrs_wk_writing ~ writing_word, 
                            iter = 10000,
                            cores = 3,
                            chains = 4,
                            warmup = 5000, data = survey)
summary(attitude_model1)
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


attitude_model2 <- stan_glm(hrs_wk_writing ~ review_word,
                            iter = 10000,
                            cores = 3,
                            chains = 4,
                            warmup = 5000, data = survey)
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

#Analysis 4: first author pubs and sentiment towards a) scientific writing, 2) peer review process====

#a) scientific writing word
model_bayes4 <- stan_glm(pubtotal ~ 
                           writing_word,
                         iter = 10000,
                         cores = 3,
                         chains = 4,
                         warmup = 5000,
                         data= survey, seed=111)

plot(model_bayes4)

# describe posteriors
# 93% of posterior is negative
bayestestR::describe_posterior(
  model_bayes4,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance"),
  centrality = "all"
)

model_loo <- loo(model_bayes4) #check if there are problems, values influencing the model
summary(model_bayes4, digits = 3)
launch_shinystan(model_bayes4)

#b) review process word
model_bayes4b <- stan_glm(pubtotal ~ 
                           graduate_yrs +
                           review_word,
                         iter = 10000,
                         cores = 3,
                         chains = 4,
                         warmup = 5000,
                         data= survey, seed=111)

plot(model_bayes4b)

# describe posteriors
# 93% of posterior is negative
bayestestR::describe_posterior(
  model_bayes4b,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance"),
  centrality = "all"
)

model_loo <- loo(model_bayes4b) #check if there are problems, values influencing the model
summary(model_bayes4b, digits = 3)
launch_shinystan(model_bayes4b)


#writing attitude IS linked to first-author pubs for grads
#summary(lm(firstauthor_pubs ~ writing_word, data = survey)) # yes
#summary(lm(firstauthor_pubs ~ writing_word, data = grads)) # yes
#summary(lm(firstauthor_pubs ~ writing_word, data = postdocs)) #nope

## writing attitude not linked to co-author pubs
#summary(lm(coauthor_pubs ~ writing_word, data = survey)) # no
#summary(lm(coauthor_pubs ~ writing_word, data = grads)) # yes
#summary(lm(coauthor_pubs ~ writing_word, data = postdocs)) # no

# peer review attitude not linked to total pubs
#summary(lm(pubtotal ~ review_word, data = survey))
#summary(lm(pubtotal ~ review_word, data = grads))
#summary(lm(pubtotal ~ review_word, data = postdocs))

## peer review attitude not linked to first-author pubs
#summary(lm(firstauthor_pubs ~ review_word, data = survey))
#summary(lm(firstauthor_pubs ~ review_word, data = grads))
#summary(lm(firstauthor_pubs ~ review_word, data = postdocs))

## peer review attitude not linked to co-author pubs
#summary(lm(coauthor_pubs ~ review_word, data = survey))
#summary(lm(coauthor_pubs ~ review_word, data = grads))
#summary(lm(coauthor_pubs ~ review_word, data = postdocs))

# writing success (i.e., pubs) NOT linked to peer-review attitude
review_box <- ggplot(aes(x = review_word, y = pubtotal, fill = stage), 
                     data = na.omit(survey[,c("pubtotal", "review_word", "stage")])) +
  geom_boxplot() + theme_bw(base_size = 14) +
  xlab("Feelings about peer review") +
  ylab("Total publications")

writing_box <- ggplot(aes(y = pubtotal, x = writing_word, fill = stage),
                      data = na.omit(survey[,c("pubtotal", "writing_word", "stage")])) +
  geom_boxplot() + theme_bw(base_size = 14) +
  xlab("Feelings about writing process") +
  ylab("Total publications")

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

#Analysis 5: Writing support groups, try Chi-squared test====
#bayesian chi squared test
#equal probability for all levels (i.e., null hypothesis is there's no difference)

x <- xtabs( ~ writing_word + writing_support_group, survey)
x
survey$writing_support_group <- as.factor(survey$writing_support_group)

x2 <- xtabs( ~ review_word + writing_support_group, survey)
x2

#Bayesian chi-squared
contingencyTableBF(x, sampleType = "poisson") #odds for alt hypothesis is 0.17%, so pretty much no relationship between sentiment towards scientific writing and having joined a writing group

contingencyTableBF(x2, sampleType = "poisson") #odds for alt hypothesis is 0.18%, so pretty much no relationship between sentiment towards peer reviews process and having joined a writing group


#Analysis 6: pubs total and Writing support groups====
#try stan_glm model
#summary(aov(lm(pubtotal ~ writing_support_group, data = survey)))
#summary(aov(lm(pubtotal ~ writing_support_group, data = grads)))
#grad.aov <- aov(lm(pubtotal ~ writing_support_group, data = grads))
#TukeyHSD(grad.aov)
#summary(aov(lm(pubtotal ~ writing_support_group, data = postdocs)))


model_bayes6 <- stan_glm(pubtotal ~ 
                           writing_support_group,
                         iter = 10000,
                         cores = 3,
                         chains = 4,
                         warmup = 5000,
                         data= survey, seed=111)

plot(model_bayes6)

bayestestR::describe_posterior(
  model_bayes6,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance"),
  centrality = "all"
)
summary(model_bayes6, digits = 3)
posteriors_model_bayes6 <- posterior(model_bayes6)

loo(model_bayes6) #check if there are problems, values influencing the model
prior_summary(model_bayes6)
summary(model_bayes6, digits = 3)
posterior_interval(
  model_bayes6,
  prob = 0.9)

