## Author: Sam Freis
## Date: 1/09/23
## Purpose: Importing and exploring PGA tournament data from 2019 - 2022
## Title: PGAanalysis.R

dev.off()
rm(list=ls())
cat("\014") 

library(tidyverse)
library(readxl)
library(psych)
library(car)
library(forcats)
library(lme4)

# data pulled from: https://www.advancedsportsanalytics.com/pga-overview

# Reading in Data
pga <- read_csv("rawdat/ASA All PGA Raw Data - Tourn Level.csv", na = c("", "NA", "999"))

# look at var names and info
names(pga)
str(pga)
head(pga)

# drop the empty variables
pga$`Unnamed: 2`<-NULL
pga$`Unnamed: 3`<-NULL
pga$`Unnamed: 4`<-NULL

# look for other NAs
pga[rowSums(is.na(pga))==0,]

# subset to numeric vars and describe
numVARS<-c("hole_par",
          "strokes",
          "hole_DKP",
          "hole_FDP",
          "hole_SDP",
          "streak_DKP",
          "streak_FDP",
          "streak_SDP",
          "pos",
          "finish_DKP",
          "finish_FDP",
          "finish_SDP",
          "total_DKP",
          "total_FDP",
          "total_SDP",
          "sg_putt",
          "sg_arg",
          "sg_app",
          "sg_ott",
          "sg_t2g",
          "sg_total"
)
numITEMS<-pga[numVARS]
describe(numITEMS)

# Research Questions: 
## 1. Who appeared in the most tournaments since 2019
### 1a. What is the average number of tournament appearances? 

# count players' appearances
app<-as.data.frame(table(pga$Player_initial_last)) 
app<-app[ order(-app$Freq), ]
head(app)

describe(app$Freq)

ggplot(app, aes(x=Freq))+
  geom_histogram(color="#FFFFFF", fill="#003C80")+
  scale_x_continuous(breaks = seq(0, 90, by = 10))+
  scale_y_continuous(breaks = seq(0, 90, len = 10))+
  labs(title="Player Tournament Appearances (2019-2022)",x="Frequency", y = "Count")+
  theme_minimal()

# [for report] name player with most appearance, give mean, describe trend of many players appearing under 20 times 

## 2. Who won the most tournaments since 2019
winners <- pga[ which(pga$pos==1), ]

wincount<-winners %>% count(Player_initial_last)
wincount<-wincount[ order(-wincount$n), ]

ggplot(winners, aes(x = fct_infreq(Player_initial_last)))+
  geom_bar(color="#FFFFFF", fill="#CCA600")+
  labs(title="Player Tournament Wins (2019-2022)",x="Player", y = "Count")+
  theme_minimal()+
  theme(legend.position = "bottom", axis.text.x = element_text(angle=60, size=7, hjust = 1))
  
# [for report] name players with most wins

## 3. Examining max streak DKP, FDP and SDP
streakVARS<-c(
           "streak_DKP",
           "streak_FDP",
           "streak_SDP"
)
streakITEMS<-pga[streakVARS]
describe(streakITEMS)

## 4. Info on strokes

## plot by cut
## scale strokes by round
pga$strokesperround<-(pga$strokes/pga$n_rounds)
pga$made_cut<-as.factor(pga$made_cut)

ggplot(pga, aes(x=made_cut, y=strokesperround, fill=made_cut)) + 
  geom_boxplot()+
  theme_bw()+
  labs(title="Stokes per Round by Cut", y = "Strokes per Round", x = "")+
  scale_fill_discrete(name = "Made Cut", labels = c("No", "Yes"))+
  theme(legend.position="bottom")

## strokes and par 
ggplot(pga, aes(x=hole_par, y=strokes)) + 
  geom_point()+
  geom_smooth(method=lm)+
  labs(y = "Total Strokes", x = "Total Hole Par")+
  theme_bw()  

## correlation matrix of numeric items
res <- cor(numITEMS, use="pairwise.complete.obs")
round(res, 2)
library(corrplot)
corrplot(res, type = "lower", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# distributions of strokes gained
p1<-ggplot(pga, aes(sg_putt))+
  geom_histogram(color="darkgreen", fill="lightgreen")+
  labs(x="Putt")+
  theme_bw()

p2<-ggplot(pga, aes(sg_arg))+
  geom_histogram(color="darkgreen", fill="lightgreen")+
  labs(x="ARG")+
  theme_bw()

p3<-ggplot(pga, aes(sg_app))+
  geom_histogram(color="darkgreen", fill="lightgreen")+
  labs(x="App")+
  theme_bw()

p4<-ggplot(pga, aes(sg_ott))+
  geom_histogram(color="darkgreen", fill="lightgreen")+
  labs(x="OTT")+
  theme_bw()

p5<-ggplot(pga, aes(sg_t2g))+
  geom_histogram(color="darkgreen", fill="lightgreen")+
  labs(x="t2g")+
  theme_bw()

p6<-ggplot(pga, aes(sg_total))+
  geom_histogram(color="darkgreen", fill="lightgreen")+
  labs(x="Total")+
  theme_bw()

library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2,
             top = "Strokes Gained"
)

sgVARS<-c(
           "strokes",
           "pos",
           "sg_putt",
           "sg_arg",
           "sg_app",
           "sg_ott"

)
sgITEMS<-pga[sgVARS]
describe(sgITEMS)

res2 <- cor(sgITEMS, use="pairwise.complete.obs")
round(res2, 2)
library(corrplot)
corrplot(res2, type = "lower", order = "hclust", 
         tl.col = "black", tl.srt = 45)

## strokes gained predicting pos 
## account for clustering within player and tournament 
pga$playerid<-as.factor(pga$`player id`)
pga$tourid<-as.factor(pga$`tournament id`)

m1<- lmer(pos ~ sg_ott+sg_app+sg_arg+sg_putt+(1|playerid:tourid), data=pga)
summary(m1)

