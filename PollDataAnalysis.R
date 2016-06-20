#=============================================================================================================#
# Script created by Meghan Duffy, duffymeg@umich.edu
# Script created in version R 3.3.0 
# This script is for analyzing data related to the Dynamic Ecology poll on
# last and corresponding authorship practices in ecology
#=============================================================================================================#

# Import data
polldata <- read.csv("AuthorshipPollResults.csv", na.strings=".")

# load libraries needed to run code below
library(ggplot2)
library(dplyr)
library(cowplot)
library(knitr)
library(rmarkdown)
library(pander)

colnames(polldata)
as.factor(polldata$LastSenior01)

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


Reorder <- function(x, ordering=c(1, 3, 5, 4, 2))
    factor(x, levels(x)[ordering])

Interdisciplinarity$Interdisciplinary <- Reorder(Interdisciplinarity$Interdisciplinary)

kable(Interdisciplinarity)


YearsPostPhD <-
  polldata %>%
  filter(!is.na(YearssincePhD)) %>%
  group_by(YearssincePhD) %>%
  summarise(n=n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 0))

# reuse `Reorder` function here but with a different `ordering` argument
kable(YearsPostPhD)
#Another one I'll want to reorder once I've figured that out.  


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

# Whoa! Huge difference in full on "yes" answers:
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

# There is an elegant way to do this, I'm sure, but I don't know it and I'm just going
# with the brute force approach:



  mutate(rel.freq = round(100 * n/sum(n), 0))


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





