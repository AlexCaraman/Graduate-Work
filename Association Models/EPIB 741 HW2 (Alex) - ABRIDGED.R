### Loading data, libraries 
library(ggplot2) # for generating plots
library(tidyverse)
library(haven) # for reading sas data
library(cowplot) # for plot grids
library(rstatix)
data1 <- read_sas("C:/Users/jacma/OneDrive/school work/UMD/EPIB 741 Association Models/chpt2_nhefs.sas7bdat")
data1$death <- as.factor(data1$death) # so R treats these variables as categorical
data1$dep <- as.factor(data1$dep)
data1$sex <- as.factor(data1$sex)
data1$race <- as.factor(data1$race)
data1$marital <- as.factor(data1$marital)
data1$area <- as.factor(data1$area)
data1$income <- as.factor(data1$income) # (<10, 10–<12, 12–<13, 13–<15, and ≥15)
data1$paffect_cat <- cut(data1$paffect,
                         breaks=c(-Inf, 9, 12, 13, 15, Inf), 
                         labels=c("very low","low","average","high","very high"))

### Variable encodings
# sex: 1=male, 0=female
# age: in years
# race: 1=white, 0=other (this may be the other way around)
# eduy: in years
# marital: 0=married, 1=never married, 2=widowed/divorced/separated
# area: 0=rural, 1=city, 2=suburb
# income: (family), 0=up to 4k, 1=4-6k, 2=6-10k, 3=10k-20k, 4=20k-35k, 5=35k+
# bmi3: 0=18.5-24.9, 1=<18.5, 2=25.91-29.92, 3=29.92+

### EDA
apply(data1[,c(2,6,8:12,18)],2,table) # counts for each of the categorical variables

plot_grid(
  ggplot(data=data1) + # plot for age, calculates and displays mean and sd
    geom_histogram(mapping=aes(x=age),bins=50) +
    labs(title="Distribution of Age",
         subtitle=paste("Mean =",
                        mean(data1$age) %>% round (3),
                        "Std Dev =",
                        sd(data1$age) %>% round (3)),
         tag="Figure 1a"),
  ggplot(data=data1) + # plot for education, calculates and displays mean and sd
    geom_histogram(mapping=aes(x=eduy),bins=50) +
    labs(title="Distribution of Education",
         subtitle=paste("Mean =",
                        mean(data1$eduy,na.rm=T) %>% round (3),
                        "Std Dev =",
                        sd(data1$eduy,na.rm=T) %>% round (3)),
         tag="Figure 1b"),
  ggplot(data=data1) + # plot for paffect, calculates and displays mean and sd
    geom_histogram(mapping=aes(x=paffect),bins=50) + # centered has same shape, of course
    labs(title="Distribution of pAffect",
         subtitle=paste("Mean =",
                        mean(data1$paffect,na.rm=T) %>% round (3),
                        "Std Dev =",
                        sd(data1$paffect,na.rm=T) %>% round (3)),
         tag="Figure 1c"),
  nrow=2,ncol=2)

# cross-tabulations, replace $dep with $death to get Gerry's table 1
xtabs(~data1$dep+data1$sex) # higher proportion of depressed people are male (sex=0)
xtabs(~data1$dep+data1$race) # slightly higher proportion of depressed are race0
xtabs(~data1$dep+data1$marital)
xtabs(~data1$dep+data1$area)
xtabs(~data1$dep+data1$income)

### Bivariate Analysis
# these lines re-label the categorical variables so their plots are more interpretable
data1$dep <- forcats::fct_recode(data1$dep, "no" = "0", "yes" = "1")
data1$death <- forcats::fct_recode(data1$death, "no" = "0", "yes" = "1")
data1$sex <- forcats::fct_recode(data1$sex, "female" = "0", "male" = "1")
data1$race <- forcats::fct_recode(data1$race, "other" = "0", "white" = "1")
data1$marital <- forcats::fct_recode(data1$marital, "married" = "0", "never married" = "1",
                                     "divorced/separated/widowed"="2")
data1$area <- forcats::fct_recode(data1$area, "rural" = "0", "city" = "1","suburb"="2")
data1$income <- forcats::fct_recode(data1$income, "<4k" = "0", "4-6k" = "1",
                                    "6-10k"="2","10-20k"="3","20-35k"="4",">35k"="5")

# paffect vs other categorical variables
plot_grid(
  ggplot(data1) +
    geom_density(mapping=aes(x=paffect,fill=dep,group=dep),alpha=0.5) +
    labs(title="Distribution of pAffect: Depressed and Not",fill="Depression"),
  ggplot(data1) +
    geom_density(mapping=aes(x=paffect,fill=death,group=death),alpha=0.5) +
    labs(title="Distribution of pAffect: Death and Not",fill="Death"),
  ggplot(data1) +
    geom_density(mapping=aes(x=paffect,fill=sex,group=sex),alpha=0.5) +
    labs(title="Distribution of pAffect: Male and Female",fill="Sex"),
  ggplot(data1) +
    geom_density(mapping=aes(x=paffect,fill=race,group=race),alpha=0.5) +
    labs(title="Distribution of pAffect: White and Non-White",fill="Race"),
  ggplot(data1) +
    geom_density(mapping=aes(x=paffect,fill=marital,group=marital),alpha=0.5) +
    labs(title="Distribution of pAffect: Marital Status",fill="Marital Status"),
  ggplot(data1) +
    geom_density(mapping=aes(x=paffect,fill=area,group=area),alpha=0.5) +
    labs(title="Distribution of pAffect: Residence",fill="Residence Area"),
  ggplot(data1) +
    geom_density(mapping=aes(x=paffect,fill=income,group=income),alpha=0.5) +
    labs(title="Distribution of pAffect Across Income Levels",fill="Income"),
  nrow=4,ncol=2)

# distribution of age across positive affect categories
ggplot(data1 %>% na.omit()) +
  geom_density(mapping=aes(x=age,fill=paffect_cat,group=paffect_cat),alpha=0.5)
ggplot(data1 %>% na.omit()) +
  geom_histogram(mapping=aes(x=age,fill=paffect_cat,group=paffect_cat),alpha=0.5)
