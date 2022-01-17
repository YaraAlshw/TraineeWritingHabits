# survey questions lab & writing-group support figs

# library
{
  library(ggplot2)
  #theme_set(theme_classic()) #remove ugly background
  library(ggrepel) #overlap text or labels onto plot
  library(cowplot) #makes multiple panels plot
  library(ggpubr) # anotation on multipanel
  library(tidyr) #tydiverse
  library(dplyr) #data/colunms manipulation from tydiverse
  library(Rmisc) #basic stats (summarySE fx)
}
survey <- read.csv('dataclean_May11.csv', header=TRUE)
# Fig 1 ----
# 1 A writing location - multiple choice
wrt_location <- read.csv('write_location_sum.csv', header=TRUE)
{# Q. where do you write? 
  #1	Home (off-campus residence or dormitory)
  #2	University provided office or lab space
  #3	On-campus spaces (e.g. graduate student centers, affinity group centers)
  #4	University or public libraries
  #5	Coffee shops/restaurants
  #6	Other (please specify)
  #7	I would like to write at an office, but my university does not provide me with office space
}
# data frame
# 55  NA, 109 No, 188 Yes total = 352
# 15% NA, 30% No, 53% Yes
wrt_location <- data.frame(type=rep(c('Yes', 'No', 'NA')),
                  count=c(188, 109, 55),
                  survey=rep(c('3lab_collab')))


# 1A plot
{
  y1 <- ggplot(wrt_location) +
    # custom panel grid
    geom_hline(aes(yintercept = y), data.frame(y = c(0:5) * 50), color = "lightgrey") +
    # add bars using geom_col instead geom_bar
    # reorder = data from smaller % to higher
    geom_col(aes( x = reorder((location), count), y = count, fill = percent), position = "dodge2", show.legend = TRUE, alpha = 0.9) +
    # dashed segments from 0 to 250 = y-axis scale
    geom_segment(aes(x = reorder((location), count),
                     y = 0, xend = reorder((location), count), yend = 250),
                 linetype = "dashed", color = "gray12") +
    # make it circular
    coord_polar() +
    # y-axis center scale = so bars don't start in the center point
    scale_y_continuous(limits = c(-50, 250), expand = c(0, 0), breaks = c(0, 50, 100, 150, 200, 250)) +
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
    annotate(x = 7.5, y = 050, label =  "50", geom = "text", color = "gray12", size = 3.5) +
    annotate(x = 7.5, y = 150, label = "150", geom = "text", color = "gray12", size = 3.5) +
    annotate(x = 7.5, y = 250, label = "250", geom = "text", color = "gray12", size = 3.5) +
    # remove background and plot lines - it will show the custom panel grid made
    theme(text = element_text(color = "gray12"),
          panel.background = element_rect(fill = "white", color = "white"),
          panel.grid = element_blank(),
          panel.grid.major.x = element_blank())
  y1
#ggsave(plot=y1, "Fig1A_write_location.png", width=6, height=6, dpi=200)
}
# 1B writing schedule
wrt_time <- read.csv('write_schedule_sum.csv', header=TRUE)
{# Q. how often do you write? multiple choice
  #1	I set a specific amount of time each week
  #2	I set aside specific days each week
  #3	I set aside large blocks of time before deadlines
  #4	I do not track or schedule my writing time
  #5	I set aside large blocks of time (e.g., during weekends or breaks)
}
# 1B plot
{
  y2 <- ggplot(wrt_time) +
    # Make custom panel grid
    geom_hline(aes(yintercept = y), data.frame(y = c(0:6) * 25), color = "lightgrey") + 
    # add bars using geom_col instead geom_bar
    # reorder = data from smaller % to higher
    geom_col(aes(x = reorder((schedule), count), y = count, fill = percent), position = "dodge2", show.legend = TRUE, alpha = 0.9) +
    # dashed segments from 0 to 150 = y-axis scale
    geom_segment(
      aes(x = reorder((schedule), count), y = 0, xend = reorder((schedule), count), yend = 150), linetype = "dashed", color = "gray12") + 
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
          panel.grid.major.x = element_blank())
  y2
  ggsave(plot=y2, "Fig1B-noblue_write_time.png", width=6, height=6, dpi=1200)
}
# 1C how tracking progress
wrt_prog <- read.csv('write_progress_sum.csv', header=TRUE)
{# Q. how do you track your writing progress? multiple choice
  #1	Electronic spreadsheets
  #2	Electronic note taking applications
  #3	Physical notebook
  #4	Checking in with writing accountability/support group
  #5	Checking in with advisor or mentor
  #6	I do not track my writing progress
  #7	Other (please specify)
}
# 1C plot
{
  y3 <- ggplot(wrt_prog) +
    # Make custom panel grid
    geom_hline(aes(yintercept = y), data.frame(y = c(0:5) * 40), color = "lightgrey") + 
    # add bars using geom_col instead geom_bar
    # reorder = data from smaller % to higher
    geom_col(aes(x = reorder((progress), count), y = count, fill = percent), position = "dodge2", show.legend = TRUE, alpha = 0.9) +
    # dashed segments from 0 to 150 = y-axis scale
    geom_segment(aes(x = reorder((progress), count), y = 0, xend = reorder((progress), count), yend = 200), linetype = "dashed", color = "gray12") +
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
  #ggsave(plot=y3, "Fig1C_write_track.png", width=6, height=6, dpi=1200)
}

# Fig 2 ----
# writing challenges
wrt_challg <- read.csv('write_challange_sum.csv', header=TRUE)
{# Q. what are some of the challenges you face with writing (pre-COVID-19)? multiple choices/answers
  #1	I find it difficult to start a new writing project
  #2	I have trouble fitting writing into my schedule
  #3	I have too many other obligations
  #4	I do not receive adequate feedback on my writing
  #5	My perfectionism with writing hinders my progress
  #6	I get easily distracted whenever I try to write
}
# 2 plot
{
  k1 <- ggplot(wrt_challg) +
    # Make custom panel grid
    geom_hline(aes(yintercept = y), data.frame(y = c(0:5) * 40), color = "lightgrey") + 
    # add bars using geom_col instead geom_bar
    # reorder = data from smaller % to higher
    geom_col(aes(x = reorder((challenge), count), y = count, fill = percent), position = "dodge2", show.legend = TRUE, alpha = 0.9) +
    # dashed segments from 0 to 150 = y-axis scale
    geom_segment(aes(x = reorder((challenge), count), y = 0, xend = reorder((challenge), count), yend = 200), linetype = "dashed", color = "gray12") +
    # make it circular
    coord_polar() +
    # y-axis center scale = so bars don't start in the center point
    scale_y_continuous(limits = c(-50, 200), expand = c(0, 0), breaks = seq(0, 200, by = 40)) +
    # colors fill and legend title
    scale_fill_gradientn("Percentage",
                         colours = c("#EDA09C","#DF858E", "#C6798F", "#966480", "#6C5B7B", "#585B74")) +
    #remove axis ticks and text
    theme(legend.position = "bottom",
          axis.text.y = element_blank(),
          #axis.text.x = element_text(color = "gray12", size = 12),
          axis.text.x = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank()) +
    # make the guide for the fill discrete
    guides(fill = guide_colorsteps(barwidth = 15, barheight = 0.5, title.position = "top", title.hjust = 0.5)) +
    # annotate custom scale inside plot
    annotate(x = 6.5, y =  40, label =  "40", geom = "text", color = "gray12", size = 3.5) +
    annotate(x = 6.5, y = 120, label = "120", geom = "text", color = "gray12", size = 3.5) +
    annotate(x = 6.5, y = 200, label = "200", geom = "text", color = "gray12", size = 3.5) +
    # remove backgroung and plot lines - it will show the custom panel grid made
    theme(text = element_text(color = "gray12"),
          panel.background = element_rect(fill = "white", color = "white"),
          panel.grid = element_blank(),
          panel.grid.major.x = element_blank())
  k1
  #ggsave(plot=k1, "Fig2_wrt-challenge.png", width=6, height=6, dpi=1200)
}

# Fig 4 ----
# data
surv1 <- survey %>%
  select(Response_ID,lab_feedback,lab_feedback_helped,lab_collab) %>%
  mutate(feed=paste0(lab_feedback)) %>%
  mutate(help=paste0(lab_feedback_helped)) %>%
  mutate(col=paste0(lab_collab)) %>%
  select(-lab_feedback,-lab_feedback_helped,-lab_collab)
# Fig 4 A
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
# FIG 4A (plot w/out NAs)
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
# Fig 4B - pi_involved
# Q. How is your advisor involved in your writing? (multiple options)
# data
inv <- data.frame(type=rep(c('6RevisingMajor', '5Planning', '4RevisingMinor','3Drafting', '2Writing', '1NotInvolved')),
                  count=c(255, 202, 185, 114, 51, 5),
                  survey=rep(c('PI_involved')))
# FIG 4B plot
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
# FIG 4C
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
    scale_fill_manual(values=(c("#966480", "#405364"))) +   
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
                              "3dontkmow"= "Did not know\n the existence",
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
wrt_group <- read.csv('wrtgr_effect_sum2.csv', header=TRUE)
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
    draw_label(x=0.30, y=0.81, size=14, "Rason for not participating") +
    draw_label(x=0.50, y=0.81, size=18, "C") +
    draw_label(x=0.80, y=0.81, size=14, "Type of group, if participated") +
    draw_label(x=0.05, y=0.49, size=18, "D") +
    draw_label(x=0.57, y=0.49, size=14, "Change in perspective through participating in a writing group")
    
  ggsave(plot= MM, device = "png", "Fig5AD_1.png", width=8, height=10 , dpi=200)
}
  