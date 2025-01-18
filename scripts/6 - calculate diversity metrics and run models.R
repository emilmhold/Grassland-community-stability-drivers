## calculate diversity metrics and run models
## Author: Emily H
## Created: January 11, 2025
## Last edited: January 11, 2025

#install.packages("tidyverse")
#install.packages("vegan")
#install.packages("lme4")
#install.packages("lmerTest")
#install.packages("car")
#install.packages("cowplot")
#install.packages("sjPlot")
#install.packages("sjmisc")

library(tidyverse)
library(vegan)
library(lme4)
library(lmerTest)
library(car)
library(cowplot)
library(sjPlot)
library(sjmisc)

setwd("/Users/emilyholden/Documents/GitHub/Analyses/Resources, diversity, and traits drive community stability")

#### import data ####
## import 2023 species richness and productivity data
no.spp <- read_rds("output/richness23.rds") %>%
  dplyr::select(Plot, Light, Nutrients, Thin, Block, richness, Productivity) %>%
    rename(richness23 = richness) %>%
  mutate(Plot = as.numeric(Plot))
str(no.spp)

## import 2021 species richness data
richness21 <- read_rds("output/richness21.rds") %>%
  dplyr::select(Plot, richness) %>%
  rename(richness21 = richness) %>%
  mutate(Plot = as.numeric(Plot))
str(richness21)

## import FD data
fdiv <- read_rds("output/July 2021 FD.rds") %>%
  dplyr::select(Plot, FRic)
str(fdiv)

## import TBD data 
gain.loss <- read_rds("output/bcd matrix.rds")
str(gain.loss)

## join data frames
diversity.df <- no.spp %>%
  left_join(richness21, by = "Plot") %>% #join with richness21 df
  left_join(fdiv, by = "Plot") %>% #join with FDiv df
  left_join(gain.loss, by = "Plot")
str(diversity.df)

#### models ####
## losses models
div.losses.lmm <- lmer(spp.losses ~ richness21*FRic + (1|Block), data = diversity.df)
qqnorm(resid(div.losses.lmm))
qqline(resid(div.losses.lmm))
shapiro.test(resid(div.losses.lmm))
summary(div.losses.lmm)
Anova(div.losses.lmm)

## gains models
div.gains.lmm <- lmer(spp.gains ~ richness21*FRic + (1|Block), data = diversity.df)
qqnorm(resid(div.gains.lmm))
qqline(resid(div.gains.lmm))
shapiro.test(resid(div.gains.lmm))
summary(div.gains.lmm)
Anova(div.gains.lmm) #richness21 is sig

## turnover models
div.turnover.lmm <- lmer(turnover ~ richness21*FRic + (1|Block), data = diversity.df)
qqnorm(resid(div.turnover.lmm))
qqline(resid(div.turnover.lmm))
shapiro.test(resid(div.turnover.lmm))
summary(div.turnover.lmm)
Anova(div.turnover.lmm)

## productivity models
div.productivity.lmm <- lmer(Productivity ~ richness21*FRic + (1|Block), data = diversity.df)
qqnorm(resid(div.productivity.lmm))
qqline(resid(div.productivity.lmm))
shapiro.test(resid(div.productivity.lmm))
summary(div.productivity.lmm)
Anova(div.productivity.lmm)
