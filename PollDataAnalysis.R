#=============================================================================================================#
# Script created by Meghan Duffy, duffymeg@umich.edu
# Script created in version R 3.3.0 
# This script is for analyzing data related to the Dynamic Ecology poll on
# last and corresponding authorship practices in ecology
#=============================================================================================================#

# Import data
polldata <- read.csv("AuthorshipPollResults.csv", na.strings=".")

#require(devtools)
#install_github('jbryer/likert', force = TRUE)

# load libraries needed to run code below
library(ggplot2)
library(dplyr)
library(cowplot)
library(knitr)
library(rmarkdown)
library(pander)
require(likert)
# ls("package:likert")
# detach("package:reshape", unload=TRUE) #just incase its loaded
library("reshape2")
library(magrittr)

colnames(polldata)

# First, get overall info on respondents 
PrimaryResearchArea <-
  polldata %>%
  filter(!is.na(PrimaryResearch)) %>%
  group_by(PrimaryResearch) %>%
  summarise(n=n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 0))

kable(PrimaryResearchArea, caption = "Primary Research Area of Respondents")

BasicAppliedSplit <-
  polldata %>%
  filter(!is.na(BasicApplied)) %>%
  group_by(BasicApplied) %>%
  summarise(n=n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 0))

kable(BasicAppliedSplit)

Interdisciplinarity <-
  polldata %>%
  filter(!is.na(Interdisciplinary)) %>%
  group_by(Interdisciplinary) %>%
  summarise(n=n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 0))

Reorder <- function(x, ordering=c(1,3,5,4,2))
    factor(x, levels(x)[ordering])

Interdisciplinarity$Interdisciplinary <- Reorder(Interdisciplinarity$Interdisciplinary)
kable(Interdisciplinarity[order(Interdisciplinarity$Interdisciplinary), ])

YearsPostPhD <-
  polldata %>%
  filter(!is.na(YearssincePhD)) %>%
  group_by(YearssincePhD) %>%
  summarise(n=n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 0))

Reorder <- function(x, ordering=c(2,6,3,4,5,1,7))
  factor(x, levels(x)[ordering])

YearsPostPhD$YearssincePhD <- Reorder(YearsPostPhD$YearssincePhD)
kable(YearsPostPhD[order(YearsPostPhD$YearssincePhD), ])
#This one has problem of rendering as dates instead of year ranges. Need to fix that.

Continent <-
  polldata %>%
  filter(!is.na(WhereLive)) %>%
  group_by(WhereLive) %>%
  summarise(n=n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 0))

kable(Continent)

Department <-
  polldata %>%
  filter(!is.na(Dept01)) %>%
  group_by(Dept01) %>%
  summarise(n=n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 0))

kable(Department)

# Next, looking at 4 main questions

# calculate frequency of each of the different responses, then plot
# starting with the question "For ecology papers, do you consider the last author to be the senior author?"
LastSeniorSum <- 
  polldata %>%
  filter(!is.na(LastSenior01)) %>%
  group_by(LastSenior01) %>%
  summarise(n=n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 0))

LastSeniorSum
#Note: most frequent response (option 6) is 43%
#Sum of all the "yes" options = 86%

as.factor(LastSeniorSum$LastSenior01)

ggplot(LastSeniorSum,aes(x=LastSenior01,y=rel.freq,fill=LastSenior01)) +
  geom_bar(stat="identity") 

require(cowplot)
lastseniorplot <- ggplot(LastSeniorSum,aes(x=LastSenior01,y=rel.freq,fill=LastSenior01)) +
  geom_bar(stat="identity") +
  ylab("Percent of responses") +
  theme(legend.position="none") +
  coord_flip() +
  theme(axis.text.y = element_blank()) +
  annotate("text", x = 1, y = 9, label = "No", hjust=0, size=3) +
  annotate("text", x = 2, y = 9, label = "It depends, but probably no", hjust=0, size=3) +
  annotate("text", x = 3, y = 9, label = "Not sure, but probably no", hjust=0, size=3) +
  annotate("text", x = 4, y = 9, label = "Not sure, but probably yes", hjust=0, size=3) +
  annotate("text", x = 5, y = 01, label = "It depends, but probably yes", hjust=0, size=3) +
  annotate("text", x = 6, y = 01, label = "Yes", hjust=0, size=3) +
  ggtitle("For ecology papers, \ndo you consider the last author \nto be the senior author?") +
  theme(plot.title=element_text(size=12)) +
  theme(axis.title.y=element_blank()) +
  theme(axis.title.x=element_text(size=12)) +
  theme(axis.ticks.y = element_blank())

save_plot("lastseniorplot.jpg", lastseniorplot)

# Next up: the question "Which of the following statements most closely
# matches the current norms in ecology in terms of who is corresponding author?"
CurrentCorrespondingSum <- 
  polldata %>%
  filter(!is.na(CorrespondingCurrent01)) %>%
  group_by(CorrespondingCurrent01) %>%
  summarise(n=n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 0))

CurrentCorrespondingSum
#most frequent answer (option 5) = 54%
#option 3 = 19%, option 1 = 16%

currentcorrespondingplot <- ggplot(CurrentCorrespondingSum,aes(x=CorrespondingCurrent01,y=rel.freq)) +
  geom_bar(stat="identity") +
  ylab("Percent of responses") +
  theme(legend.position="none") +
  coord_flip() +
  theme(axis.text.y = element_blank()) +
  annotate("text", x = 1, y = 16.7, label = "The corresponding author is the person that has taken \nresponsibility for fielding questions about the paper \npost-publication",hjust=0,size=2.5) +
  annotate("text", x = 2, y = 3.5, label = "The corresponding author is the person with the most stable contact info \nand/or internet access",hjust=0,size=2.5) +
  annotate("text", x = 3, y = 19.5, label = "The corresponding author is usually the person who \nuploaded the files (usually the first author)",hjust=0,size=2.5) +
  annotate("text", x = 4, y = 7.6, label = "The corresponding author is usually the senior author",hjust=0,size=2.5) +
  annotate("text", x = 5, y = 1, color="white", label = "The corresponding author uploaded the files, managed the revisions and \nwrote the response to reviewers, and took responsibility for the paper after \npublication",hjust=0,size=2.5) +
  ggtitle("Which of the following statements most \nclosely matches the current norms in \necology in terms of who is corresponding author?") +
  theme(plot.title=element_text(size=12)) +
  theme(axis.title.y=element_blank()) +
  theme(axis.title.x=element_text(size=12)) +
  theme(axis.ticks.y = element_blank())

save_plot("currentcorrespondingplot.jpg", currentcorrespondingplot)

# Q3: "Which of the following statements would be best practice in terms of who is corresponding author?"
BestCorrespondingSum <- 
  polldata %>%
  filter(!is.na(CorrespondingBest01)) %>%
  group_by(CorrespondingBest01) %>%
  summarise(n=n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 0))

BestCorrespondingSum
#most frequent answer (option 5) = 61%
#option 1 = 24%

bestcorrespondingplot <- ggplot(BestCorrespondingSum,aes(x=CorrespondingBest01,y=rel.freq)) +
  geom_bar(stat="identity") +
  ylab("Percent of responses") +
  theme(legend.position="none") +
  coord_flip() +
  theme(axis.text.y = element_blank()) +
  annotate("text", x = 1, y = 24.5, label = "The corresponding author should be the person \nthat has taken responsibility for fielding questions \nabout the paper post-publication",hjust=0,size=2.5) +
  annotate("text", x = 2, y = 5, label = "The corresponding author should be the person with the most stable \ncontact info and/or internet access",hjust=0,size=2.5) +
  annotate("text", x = 3, y = 9, label = "The corresponding author should be whichever person uploaded \nthe files (usually the first author)",hjust=0,size=2.5) +
  annotate("text", x = 4, y = 5, label = "The corresponding author should be the senior author",hjust=0,size=2.5) +
  annotate("text", x = 5, y = 1, color="white", label = "The corresponding author should be the person who uploaded the files, \nmanaged the revisions and wrote the response to reviewers, \nand took responsibility for the paper after publication",hjust=0,size=2.5) +
  ggtitle("Which of the following statements would be the \nbest practice in terms of who is \ncorresponding author?") +
  theme(plot.title=element_text(size=12)) +
  theme(axis.title.y=element_blank()) +
  theme(axis.title.x=element_text(size=12)) +
  theme(axis.ticks.y = element_blank())

save_plot("bestcorrespondingplot.jpg", bestcorrespondingplot)


# Q4: "If someone includes a statement on his/her CV indicating they have used a first/last author emphasis, do you pay attention to that?"
CVStatementSum <- 
  polldata %>%
  filter(!is.na(CVStatement01)) %>%
  group_by(CVStatement01) %>%
  summarise(n=n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 0))

CVStatementSum
#most people haven't seen such a statement (options 2+3 = 70%)
#most people either pay attention to such a statement or think they would: (options 3+4 = 71%)
#but the proportion of people who would ignore such a statement isn't exactly trivial!

CVstatementplot <- ggplot(CVStatementSum,aes(x=CVStatement01,y=rel.freq,fill=CVStatement01)) +
  geom_bar(stat="identity") +
  ylab("Percent of responses") +
  theme(legend.position="none") +
  coord_flip() +
  theme(axis.text.y = element_blank()) +
  annotate("text", x = 1, y = 1, color="white", label = "No",hjust=0,size=2.5) +
  annotate("text", x = 2, y = 1, color="white", label = "I have never seen this, but \nwould probably not pay \nattention to it",hjust=0,size=2.5) +
  annotate("text", x = 3, y = 1, color="black", label = "I have never seen this, but would probably pay attention to it",hjust=0,size=2.5) +
  annotate("text", x = 4, y = 1, color="black", label = "Yes",hjust=0,size=2.5) +
  ggtitle("If someone includes a statement on his/her \nCV indicating they have used a first/last \nauthor emphasis, do you pay attention to that?") +
  theme(plot.title=element_text(size=12)) +
  theme(axis.title.y=element_blank()) +
  theme(axis.title.x=element_text(size=12)) +
  theme(axis.ticks.y = element_blank())

save_plot("CVstatementplot.jpg", CVstatementplot)

# Now moving on to more interesting analyses that take advantage of the other info on respondents 

# Starting with Q1: "For ecology papers, do you consider the last author to be the senior author?":
# Q1.1: Is there a difference in responses for ecologists vs. others?
polldata$ecology01 <- ifelse(polldata$PrimaryResearch01 <= 3, 1, 0)

LastSeniorSumEcology <- 
  polldata %>%
  filter(polldata$ecology01==1,!is.na(LastSenior01)) %>%
  group_by(LastSenior01) %>%
  summarise(n=n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 0))

LastSeniorSumEcology

LastSeniorSumNotEcology <- 
  polldata %>%
  filter(polldata$ecology01==0,!is.na(LastSenior01)) %>%
  group_by(LastSenior01) %>%
  summarise(n=n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 0))

LastSeniorSumNotEcology

#Comparing the various "yes" options in the two areas:
# Ecologists: 85%
# Not ecologists: 89%
# So, overall, not a big difference

# Looking at slightly different grouping: Are molecular folks different?
LastSeniorSumMolecular <- 
  polldata %>%
  filter(polldata$PrimaryResearch01==2 | polldata$PrimaryResearch01==4,!is.na(LastSenior01)) %>%
  group_by(LastSenior01) %>%
  summarise(n=n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 0))

LastSeniorSumMolecular

LastSeniorSumOrganismal <- 
  polldata %>%
  filter(polldata$PrimaryResearch01==1 | polldata$PrimaryResearch01==5,!is.na(LastSenior01)) %>%
  group_by(LastSenior01) %>%
  summarise(n=n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 0))

LastSeniorSumOrganismal

# Big difference in full on "yes" answers:
# molecular-types: 62%
# organismal/field-types: 39%

# Q1.2: Is there a difference in responses related to years since PhD?
polldata$youngold <- ifelse(polldata$PhD01 < 3, "young",
                            ifelse(polldata$PhD01 == 3, "middle",
                            ifelse(polldata$PhD01>3,"old","neither"
                                   )))

# the above is putting everyone within 5 years of PhD in the "young" category, and everyone 11 or more years post-PhD in the "old" category.
# I'm sure I'll catch some flack for those category names. :)

LastSeniorSumOld <- 
  polldata %>%
  filter(polldata$youngold=='old',!is.na(LastSenior01)) %>%
  group_by(LastSenior01) %>%
  summarise(n=n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 0))

LastSeniorSumOld

LastSeniorSumYoung <- 
  polldata %>%
  filter(polldata$youngold=='young',!is.na(LastSenior01)) %>%
  group_by(LastSenior01) %>%
  summarise(n=n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 0))

LastSeniorSumYoung

# Summary: 
# "Yes" answers in "old" group: 79
# "Yes" answers in "young" group: 88

# But that's making it more discrete than necessary. What about a correlation?

# First, make a table 

# what I want to do:
# 1. subset data by year since Phd, then calculate percentages that gave each answer (6 possible answers)
# 2. make a table that has each of the above as a row of data (maybe add column for n for that subset, too)

LastSeniorSums <- 
  polldata %>%
  filter(!is.na(LastSenior01&!is.na(PhD01))) %>%
  group_by(LastSenior01,PhD01) %>%
  summarise(n=n()) 

LastSeniorSumPhd1 <-
  LastSeniorSums %>%
  subset(PhD01 == 1) %>%
  
  mutate(rel.freq = round(100 * n/sum(n), 0))

# I abandoned the above approach after learning about the Likert package


# Q1.3: Is there a difference related to home department (with guess that EEB dept folks might have more traditional view)?

polldata$depttype <- ifelse(polldata$Dept01 == 1, "eeb",
                            ifelse(polldata$Dept01 == 2, "bio",
                            ifelse(polldata$Dept01 == 3, "natres","other"
                            )))

LastSeniorSumEEB <- 
  polldata %>%
  filter(polldata$depttype=='eeb',!is.na(LastSenior01)) %>%
  group_by(LastSenior01) %>%
  summarise(n=n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 0))

LastSeniorSumEEB

LastSeniorSumBio <- 
  polldata %>%
  filter(polldata$depttype=='bio',!is.na(LastSenior01)) %>%
  group_by(LastSenior01) %>%
  summarise(n=n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 0))

LastSeniorSumBio

LastSeniorSumNatRes <- 
  polldata %>%
  filter(polldata$depttype=='natres',!is.na(LastSenior01)) %>%
  group_by(LastSenior01) %>%
  summarise(n=n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 0))

LastSeniorSumNatRes

# Summary:
# Yes answers for EEB departments: 90
# Yes answers for Bio departments: 90
# Yes answers for nat res departments: 76
# So, no difference between EEB and Bio (I thought there would be one), but Nat Res seems lower

# Q1.4: Is there a difference related to whether do basic vs. applied research?

LastSeniorSumBasic <- 
  polldata %>%
  filter(polldata$BasicApplied01==1,!is.na(LastSenior01)) %>%
  group_by(LastSenior01) %>%
  summarise(n=n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 0))

LastSeniorSumBasic

LastSeniorSumApplied <- 
  polldata %>%
  filter(polldata$BasicApplied01==2,!is.na(LastSenior01)) %>%
  group_by(LastSenior01) %>%
  summarise(n=n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 0))

LastSeniorSumApplied

#Summary:
# Yes answers for basic: 89
# Yes answers for applied: 82

# Q1.5: Is there a difference related to how often the person does interdisciplinary research?

LastSeniorSumInterdisciplinary <- 
  polldata %>%
  filter(polldata$Inter01==4 | polldata$Inter01==5,!is.na(LastSenior01)) %>%
  group_by(LastSenior01) %>%
  summarise(n=n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 0))

LastSeniorSumInterdisciplinary

LastSeniorSumNotInterdisciplinary <- 
  polldata %>%
  filter(polldata$Inter01==1 | polldata$Inter01==2,!is.na(LastSenior01)) %>%
  group_by(LastSenior01) %>%
  summarise(n=n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 0))

LastSeniorSumNotInterdisciplinary

#interdisciplinary "yes" type answers: 83 (43% straight "yes")
#non-interdisciplinary "yes" type answers: 88 (43% straight "yes")

# Q1.6: Is there a difference related to where the person lives?
LastSeniorSumNA <- 
  polldata %>%
  filter(polldata$WhereLive01==5,!is.na(LastSenior01)) %>%
  group_by(LastSenior01) %>%
  summarise(n=n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 0))

LastSeniorSumNA

LastSeniorSumEurope <- 
  polldata %>%
  filter(polldata$WhereLive01==4,!is.na(LastSenior01)) %>%
  group_by(LastSenior01) %>%
  summarise(n=n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 0))

LastSeniorSumEurope

LastSeniorSumAustralia <- 
  polldata %>%
  filter(polldata$WhereLive01==3,!is.na(LastSenior01)) %>%
  group_by(LastSenior01) %>%
  summarise(n=n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 0))

LastSeniorSumAustralia

LastSeniorSumSA <- 
  polldata %>%
  filter(polldata$WhereLive01==6,!is.na(LastSenior01)) %>%
  group_by(LastSenior01) %>%
  summarise(n=n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 0))

LastSeniorSumSA

#Summary:
#North America: 82% said some form of yes (36% straight "yes")
#Europe: 95% said some form of yes (58% straight "yes")
#Australia: 82% said some form of yes (49% straight "yes")
#South America (but with caution of relatively small sample size): 93% (37%)


# Code from Rayna Harris that looks at Likert-style data and shows responses
## subsetting only the likert data, but first filter by the grouping variables of interest
interdisc <-
  polldata %>%
  filter(Interdisciplinary != "NA", LastSenior != "NA", WhereLive != "NA", BasicApplied != "NA") %>% 
  select(Interdisciplinary) 

## ordering the factors
interdisc$Interdisciplinary <- factor(interdisc$Interdisciplinary, 
                                      c("Never",
                                        "Rarely", 
                                        "Sometimes", 
                                        "Often",
                                        "Always"))

## subsetting the likert data AND the grouping variables
interdisc_grouping <-
  polldata %>%
  filter(Interdisciplinary != "NA", LastSenior != "NA", WhereLive != "NA", BasicApplied != "NA") %>%
  select(LastSenior, youngold, WhereLive, BasicApplied) 

## ordering the grouping factors
interdisc_grouping$LastSenior <- factor(interdisc_grouping$LastSenior, 
                                        c("No",
                                          "It depends, but probably no", 
                                          "Not sure, but probably no", 
                                          "Not sure, but probably yes",
                                          "It depends, but probably yes",
                                          "Yes"))
interdisc_grouping$youngold <- factor(interdisc_grouping$youngold, 
                                      c("young",
                                        "middle", 
                                        "old"))

## individual plots!
likert_1 <- likert(interdisc, grouping = interdisc_grouping$youngold)
plot(likert_1)

likert_2 <- likert(interdisc, grouping = interdisc_grouping$WhereLive)
plot(likert_2)

likert_3 <- likert(interdisc, grouping = interdisc_grouping$LastSenior)
plot(likert_3) 

likert_4 <- likert(interdisc, grouping = interdisc_grouping$BasicApplied)
plot(likert_4)

## cowplot!!
likert_1.mpg <- plot(likert_1)
likert_2.mpg <- plot(likert_2)
likert_3.mpg <- plot(likert_3)
likert_4.mpg <- plot(likert_4)
likertplot1 <- plot_grid(likert_1.mpg, likert_2.mpg, likert_3.mpg, likert_4.mpg, labels = c("A", "B", "C", "D"), ncol = 2)

save_plot("likertplot1.jpg", likertplot1)
#Hmm, the above plot looks pretty awful (in terms of the way things are arranged), but, since it's not the main plot of interest, I'm moving on with Rayna's code anyway

## subsetting only the corresponding author data, but first filter by the grouping variables of interest
## then renaming factors, renaming question
corresponding <-
  polldata %>%
  filter(CorrespondingBest01 != "NA", LastSenior != "NA", 
         WhereLive != "NA", BasicApplied != "NA", 
         depttype != "NA", PrimaryResearch != "NA") %>% 
  select(CorrespondingBest01) 
corresponding$CorrespondingBest01 <- factor(corresponding$CorrespondingBest01, 
                                            c("1", "2", "3", "4", "5"))
levels(corresponding$CorrespondingBest01) <- c("Fields Questions", "Most Stable", "Uploaded Files", "Senior Author", "Revisions, Reviews")
colnames(corresponding)[1] <- "Who is the Corresponding Author?"

## subsetting the likert data AND the grouping variables
corresponding_grouping <-
  polldata %>%
  filter(CorrespondingBest01 != "NA", LastSenior != "NA", 
         WhereLive != "NA", BasicApplied != "NA", 
         depttype != "NA", PrimaryResearch != "NA") %>%
  select(LastSenior, youngold, 
         WhereLive, BasicApplied, 
         depttype, PrimaryResearch) 

## ordering the grouping factors
## then renaming the primary resech responses
corresponding_grouping$LastSenior <- factor(corresponding_grouping$LastSenior, 
                                            c("No",
                                              "It depends, but probably no", 
                                              "Not sure, but probably no", 
                                              "Not sure, but probably yes",
                                              "It depends, but probably yes",
                                              "Yes"))
corresponding_grouping$youngold <- factor(corresponding_grouping$youngold, 
                                          c("young",
                                            "middle", 
                                            "old"))
levels(corresponding_grouping$PrimaryResearch)
levels(corresponding_grouping$PrimaryResearch) <- c("Biology Other", "EEB Comp", "EEB Field", "EEB Mol Ecol", "Evol Mol", "Evol Organismal", "Other")

## plots##
likert_11 <- likert(corresponding, grouping = corresponding_grouping$youngold)
plot(likert_11)
likert_21 <- likert(corresponding, grouping = corresponding_grouping$LastSenior)
plot(likert_21)
likert_31 <- likert(corresponding, grouping = corresponding_grouping$BasicApplied)
plot(likert_31)
likert_41 <- likert(corresponding, grouping = corresponding_grouping$WhereLive)
plot(likert_41)
likert_51 <- likert(corresponding, grouping = corresponding_grouping$depttype)
plot(likert_51)
likert_61 <- likert(corresponding, grouping = corresponding_grouping$PrimaryResearch)
plot(likert_61)

## cowplot !
likert_11.mpg <- plot(likert_11)
likert_21.mpg <- plot(likert_21)
likert_31.mpg <- plot(likert_31)
likert_41.mpg <- plot(likert_41)
likert_51.mpg <- plot(likert_51)
likert_61.mpg <- plot(likert_61)
plot_grid(likert_11.mpg, likert_21.mpg, likert_31.mpg, likert_41.mpg, likert_51.mpg, likert_61.mpg, 
          labels = c("A", "B", "C", "D", "E", "F"), ncol = 2, nrow = 3,
          rel_widths = c(0.4, 0.6))


# Updating the above to focus on question 1 instead (related to last == senior)

## first, making a "molecular" variable
polldata$molecular <- ifelse(polldata$PrimaryResearch01 == 2 | polldata$PrimaryResearch == 4, "molecular",
                             ifelse(polldata$PrimaryResearch01 == 1 | polldata$PrimaryResearch == 5,"organismal","other"
                             ))

## make an ecology vs. evolution variable:
polldata$ecoevo <- ifelse(polldata$PrimaryResearch01 < 4, "ecology",
                             ifelse(polldata$PrimaryResearch01 == 4 | polldata$PrimaryResearch == 5,"evolution","other"
                             ))


## subsetting only the last author data, but first filter by the grouping variables of interest
## then renaming factors, renaming question
lastdata <-
  polldata %>%
  filter(CorrespondingBest01 != "NA", LastSenior != "NA", 
         WhereLive != "NA", BasicApplied != "NA", 
         depttype != "NA", PrimaryResearch != "NA", Inter01 != "NA") %>% 
  select(LastSenior) 

# order the data
lastdata$LastSenior <- factor(lastdata$LastSenior, 
                                      c("No",
                                        "Not sure, but probably no",
                                        "It depends, but probably no",
                                        "It depends, but probably yes",
                                        "Not sure, but probably yes",
                                        "Yes"))

# changing the column name to the question
colnames(lastdata)[1] <- "Is the last author the senior author?"

## subsetting the likert data AND the grouping variables
lastdata_grouping <-
  polldata %>%
  filter(CorrespondingBest01 != "NA", LastSenior != "NA", 
         WhereLive != "NA", BasicApplied != "NA", 
         depttype != "NA", PrimaryResearch != "NA", Inter01 != "NA") %>%
  select(molecular, 
         WhereLive, BasicApplied, 
         depttype, PrimaryResearch,
         ecoevo, Interdisciplinary, YearssincePhD) 

## ordering the grouping factors
## then renaming the primary research responses
lastdata_grouping$molecular <- factor(lastdata_grouping$molecular, 
                                            c("molecular",
                                              "organismal", 
                                              "other"))
lastdata_grouping$youngold <- factor(lastdata_grouping$youngold, 
                                          c("young",
                                            "middle", 
                                            "old"))
lastdata_grouping$Interdisciplinary <- factor(lastdata_grouping$Interdisciplinary, 
                                     c("Always",
                                       "Often",
                                       "Sometimes",
                                       "Rarely",
                                       "Never"))

levels(lastdata_grouping$PrimaryResearch) <- c("Biology Other", "Comp Ecol", "Field Ecol", "Lab Mol Ecol", "Mol Evol", "Organismal Evol", "Other")

lastdata_grouping$YearssincePhD <- lastdata_grouping %>%
  use_series(YearssincePhD) %>%
  plyr::mapvalues(., c(">20","0 (current students should choose this)","10-Jun","15-Nov", "16-20", "5-Jan", "I do not have a PhD and am not a current student"), c(">20", "0","6-10","11-15", "16-20", "1-5", "no PhD, not student"))

lastdata_grouping$PrimaryResearch <- lastdata_grouping %>%
  use_series(PrimaryResearch) %>%
  plyr::mapvalues(., c("Biology other than EEB","Ecology (primarily computational-based)",
                       "Ecology (primarily field-based)","Ecology (primarily wet-lab based, including molecular ecology)", 
                       "Evolutionary biology (primarily molecular)", "Evolutionary biology (primarily organismal)", "Outside biology"), 
                  c("Biology Other", "Comp Ecol","Field Ecol","Lab Mol Ecol", "Mol Evol", "Organismal Evol", "Other"))


lastdata_grouping$YearssincePhD <- factor(lastdata_grouping$YearssincePhD, 
                                     c("0",
                                       "1-5",
                                       "6-10",
                                       "11-15",
                                       "16-20",
                                       ">20", 
                                       "no PhD, not student"))

## plots##

#colnames(lastdata)[1] <- "Is the last author the senior author?"
#likert_12 <- likert(lastdata, grouping = lastdata_grouping$youngold)
#plot(likert_12)

colnames(lastdata)[1] <- "Geographic location"
likert_22 <- likert(lastdata, grouping = lastdata_grouping$WhereLive)
plot(likert_22)
colnames(lastdata)[1] <- "Department type"
likert_32 <- likert(lastdata, grouping = lastdata_grouping$depttype)
plot(likert_32)
colnames(lastdata)[1] <- "Basic v. Applied"
likert_42 <- likert(lastdata, grouping = lastdata_grouping$BasicApplied)
plot(likert_42)
colnames(lastdata)[1] <- "Primary Research Area"
likert_52 <- likert(lastdata, grouping = lastdata_grouping$PrimaryResearch)
plot(likert_52)
colnames(lastdata)[1] <- "Molecular vs. Organismal"
likert_62 <- likert(lastdata, grouping = lastdata_grouping$molecular)
plot(likert_62)
colnames(lastdata)[1] <- "Frequency of interdisciplinary research?"
likert_72 <- likert(lastdata, grouping = lastdata_grouping$Interdisciplinary)
plot(likert_72)
colnames(lastdata)[1] <- "Ecology vs. Evolution"
likert_82 <- likert(lastdata, grouping = lastdata_grouping$ecoevo)
plot(likert_82)
colnames(lastdata)[1] <- "Years since PhD"
likert_92 <- likert(lastdata, grouping = lastdata_grouping$YearssincePhD)
plot(likert_92)

likert_92.mpg <- plot(likert_92) + theme(legend.position = "none")
likert_52.mpg <- plot(likert_52) + theme(legend.position = "none")
likert_82.mpg <- plot(likert_82) + theme(legend.position = "none")
likert_62.mpg <- plot(likert_62) + theme(legend.position = "none")
likert_42.mpg <- plot(likert_42)
likert_32.mpg <- plot(likert_32) + theme(legend.position = "none")
likert_72.mpg <- plot(likert_72)
likert_22.mpg <- plot(likert_22) + theme(legend.position = "none")

lastplotgrid4panelv2 <- plot_grid(likert_92.mpg, likert_52.mpg, likert_32.mpg, likert_42.mpg, 
                            labels = c("A", "B", "C", "D"), ncol = 1, nrow = 4,
                            rel_heights = c(0.3, 0.3, 0.2, 0.2))

save_plot("lastplotgrid4panelv2.jpg", lastplotgrid4panelv2, base_width = 8.5, base_height = 15)

lastplotgrid2panelv2 <- plot_grid(likert_22.mpg, likert_72.mpg, 
                            labels = c("A", "B"), ncol = 1, nrow = 2,
                            rel_heights = c(0.5, 0.5))

save_plot("lastplotgrid2panelv2.jpg", lastplotgrid2panelv2, base_width = 8.5, base_height = 10)


# would be ideal if could get better aligned, but I think this is tricky to do


## Now looking at question 4 and how responses there relate to those for question 1

## subsetting only the statement data, but first filter by the grouping variables of interest
## then renaming factors, renaming question
statementdata <-
  polldata %>%
  filter(CVStatement01 != "NA", LastSenior != "NA", 
         WhereLive != "NA", BasicApplied != "NA", 
         depttype != "NA", PrimaryResearch != "NA") %>% 
  select(CVStatement) 

# order the data
statementdata$CVStatement <- factor(statementdata$CVStatement, 
                              c("No",
                                "I have never seen this, but would probably not pay attention to it",
                                "I have never seen this, but would probably pay attention to it",
                                "Yes"))

statementdata$CVStatement <- statementdata %>%
  use_series(CVStatement) %>%
  plyr::mapvalues(., c("No","I have never seen this, but would probably not pay attention to it","I have never seen this, but would probably pay attention to it","Yes"), c("No","Not seen, no","Not seen, yes","Yes"))

# changing the column name to the question
colnames(statementdata)[1] <- "Do you pay attention to a CV statement?"

## subsetting the likert data AND the grouping variables
statementdata_grouping <-
  polldata %>%
  filter(CVStatement01 != "NA", LastSenior != "NA", 
         WhereLive != "NA", BasicApplied != "NA", 
         depttype != "NA", PrimaryResearch != "NA") %>%
  select(molecular, youngold, 
         WhereLive, BasicApplied, 
         depttype, PrimaryResearch, LastSenior, LastSenior01) 

## ordering the grouping factors
## then renaming the primary research responses
statementdata_grouping$molecular <- factor(statementdata_grouping$molecular, 
                                      c("molecular",
                                        "organismal", 
                                        "other"))
statementdata_grouping$youngold <- factor(statementdata_grouping$youngold, 
                                     c("young",
                                       "middle", 
                                       "old"))
levels(statementdata_grouping$PrimaryResearch)
levels(statementdata_grouping$PrimaryResearch) <- c("Biology Other", "Comp Ecol", "Field Ecol", "Lab Mol Ecol", "Mol Evol", "Organismal Evol", "Other")
statementdata_grouping$LastSenior <- factor(statementdata_grouping$LastSenior, 
                                            c("No",
                                              "Not sure, but probably no",
                                              "It depends, but probably no",
                                              "It depends, but probably yes",
                                              "Not sure, but probably yes",
                                              "Yes"))

statementdata_grouping$LastSeniorYesNo <- ifelse(statementdata_grouping$LastSenior01 < 4, "No", "Yes")

## Plots
## plots##
likert_13 <- likert(statementdata, grouping = statementdata_grouping$youngold)
plot(likert_13)
likert_23 <- likert(statementdata, grouping = statementdata_grouping$WhereLive)
plot(likert_23)
likert_33 <- likert(statementdata, grouping = statementdata_grouping$depttype)
plot(likert_33)
likert_43 <- likert(statementdata, grouping = statementdata_grouping$BasicApplied)
plot(likert_43)
likert_53 <- likert(statementdata, grouping = statementdata_grouping$PrimaryResearch)
plot(likert_53)
likert_63 <- likert(statementdata, grouping = statementdata_grouping$molecular)
plot(likert_63)
likert_73 <- likert(statementdata, grouping = statementdata_grouping$LastSenior)
plot(likert_73)
likert_83 <- likert(statementdata, grouping = statementdata_grouping$LastSeniorYesNo)
plot(likert_83)


likert_13.mpg <- plot(likert_13) + theme(legend.position = "none")
likert_23.mpg <- plot(likert_23) + theme(legend.position = "none")
likert_33.mpg <- plot(likert_33) + theme(legend.position = "none")
likert_43.mpg <- plot(likert_43) + theme(legend.position = "none")
likert_53.mpg <- plot(likert_53) + theme(legend.position = "none")
likert_63.mpg <- plot(likert_63) + theme(legend.position = "none")
likert_73.mpg <- plot(likert_73) 
likert_83.mpg <- plot(likert_83) 
statementplotgrid <- plot_grid(likert_13.mpg, likert_23.mpg, likert_33.mpg, likert_43.mpg, likert_53.mpg, likert_63.mpg, likert_73.mpg, likert_83.mpg,
                          labels = c("A", "B", "C", "D", "E", "F", "G", "H"), ncol = 2, nrow = 4,
                          rel_widths = c(0.5, 0.5),
                          rel_heights = c(0.3, 0.3, 0.35))

save_plot("statementplotgrid.jpg", statementplotgrid, base_width = 17, base_height = 15)


# Looking more simply: what if just take the people who said straight "Yes" or "No" to Q1. What are their splits for Q4?
lastnotseniorsummary <- 
  polldata %>%
  subset(LastSenior01 == 1) %>%
  group_by(CVStatement) %>%
  summarise(n=n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 0))

lastisseniorsummary <- 
  polldata %>%
  subset(LastSenior01 == 6) %>%
  filter(!is.na(CVStatement01)) %>%
  group_by(CVStatement) %>%
  summarise(n=n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 0))

lastunsuresummary <-
  polldata %>%
  subset(LastSenior01 > 1 & LastSenior01 < 6) %>%
  filter(!is.na(CVStatement01)) %>%
  group_by(CVStatement) %>%
  summarise(n=n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 0))

# Figuring out how to plot:
lastnotsenior <- 
  polldata %>%
  subset(LastSenior01 == 1)

lnsstatementdata <-
  lastnotsenior %>%
  filter(CVStatement01 != "NA", LastSenior != "NA", 
         WhereLive != "NA", BasicApplied != "NA", 
         depttype != "NA", PrimaryResearch != "NA") %>% 
  select(CVStatement) 

lnsstatementdata$CVStatement <- factor(lnsstatementdata$CVStatement, 
                                    c("No",
                                      "I have never seen this, but would probably not pay attention to it",
                                      "I have never seen this, but would probably pay attention to it",
                                      "Yes"))

lnsstatementdata$CVStatement <- lnsstatementdata %>%
  use_series(CVStatement) %>%
  plyr::mapvalues(., c("No","I have never seen this, but would probably not pay attention to it","I have never seen this, but would probably pay attention to it","Yes"), c("No","Not seen, no","Not seen, yes","Yes"))

colnames(lnsstatementdata)[1] <- "Do you pay attention to a CV statement?"

## subsetting the likert data AND the grouping variables
lnsstatementdata_grouping <-
  lastnotsenior %>%
  filter(CVStatement01 != "NA", LastSenior != "NA", 
         WhereLive != "NA", BasicApplied != "NA", 
         depttype != "NA", PrimaryResearch != "NA") %>%
  select(LastSenior) 

lastnotseniorlikert <- likert(lnsstatementdata, grouping = lnsstatementdata_grouping$LastSenior)
plot(lastnotseniorlikert)

# Plot above works, but maybe isn't that useful



