# AIC for models
# Author: Emily H
# created: December 24, 2024
#last edited: January 11, 2025
#install.packages("tidyverse")
#install.packages("AICcmodavg")
#install.packages("rsq")
#install.packages("arm")

library(tidyverse)
library(AICcmodavg)
library(rsq)
library(arm)

setwd("/Users/emilyholden/Documents/GitHub/Analyses/Resources, diversity, and traits drive community stability")

#### import and format data ####
#### losses ####
#define list of models
loss.models <- list(spp.losses, div.losses.lmm, CWM.losses.lmm, social.losses.lmm)

#specify model names
loss.mod.names <- c('Resources',  'Diversity metrics', 'Structural traits', 'Social traits')

##K: The number of parameters in the model.
##AICc: The AIC value of the model. The lowercase ‘c’ indicates that the AIC has been calculated from the AIC corrected for small sample sizes.
##Delta_AICc: The difference between the AIC of the best model compared to the current model being compared.
##AICcWt: The proportion of the total predictive power that can be found in the model.
##Cum.Wt: The cumulative sum of the AIC weights.

loss.AIC<- aictab(REML=F, cand.set = loss.models, #my models
                  modnames = loss.mod.names, #the names
                  sort = TRUE)
loss.AIC <- loss.AIC %>%
  dplyr::select(Modnames, K, AICc, Delta_AICc, AICcWt, Res.LL) %>%
  rename(Model = Modnames,
         '# of parameters' = K) %>%
  mutate_if(is.numeric, round, digits = 3)

View(loss.AIC)

#write table to .txt file
write.table(loss.AIC, file = "output/AIC loss output.txt", sep = ",", quote = FALSE, row.names = F)

#### gains ####
#define list of models
gain.models <- list(spp.gains, div.gains.lmm, CWM.gains.lmm, social.gains.lmm)

#specify model names
gain.mod.names <- c('Resources',  'Diversity metrics', 'Structural traits', 'Social traits')

gain.AIC<-aictab(REML=F, cand.set = gain.models, #my models
                 modnames = gain.mod.names, #the names
                 sort = TRUE)

gain.AIC<-gain.AIC %>% 
  dplyr::select(Modnames, K, AICc, Delta_AICc, AICcWt, Res.LL) %>%
  rename(Model = Modnames,
         '# of parameters' = K) %>%
  mutate_if(is.numeric, round, digits = 3)
str(gain.AIC)

#write table to .txt file
write.table(gain.AIC, file = "output/AIC gain output.txt", sep = ",", quote = FALSE, row.names = F)

#### turnover ####
#define list of models
turnover.models <- list(spp.turnover, div.turnover.lmm, CWM.turnover.lmm, social.turnover.lmm)

#specify model names
turnover.mod.names <- c('Resources',  'Diversity metrics', 'Structural traits', 'Social traits')

turnover.AIC<-aictab(REML=F, cand.set = turnover.models, #my models
                     modnames = turnover.mod.names, #the names
                     sort = TRUE)

turnover.AIC<-turnover.AIC %>% 
  dplyr::select(Modnames, K, AICc, Delta_AICc, AICcWt, Res.LL) %>%
  rename(Model = Modnames,
         '# of parameters' = K) %>%
  mutate_if(is.numeric, round, digits = 3)

str(turnover.AIC)

#write table to .txt file
write.table(turnover.AIC, file = "output/AIC turnover output.txt", sep = ",", quote = FALSE, row.names = F)
