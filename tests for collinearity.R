## Measure collinearity among predictors
## Author: Emily H
## Created: October 28, 2025
## Last edited: November 17, 2025

#install.packages("tidyverse")
#install.packages("car")
#install.packages("corrplot")
#install.packages("Hmisc")
#install.packages("performance")
#install.packages("lme4")

library(tidyverse)
library(car)
library(corrplot)
library(Hmisc)
library(performance)
library(lme4)

setwd("/Users/emilyholden/Documents/GitHub/Analyses/Resources, diversity, and traits drive community stability")

#### import data ####
## import social metric file
social.metrics <- read_rds("output/CWM coocurrences.rds") %>%
  rename(Gregariousness = total.pos.cooccurrence,
         Reclusiveness = total.neg.cooccurrence) %>%
  dplyr::select(Plot, Gregariousness, Reclusiveness, spp.losses, spp.gains)
str(social.metrics)

## import FD data
fdiv <- read_rds("output/July 2021 FD.rds") %>%
  dplyr::select(Plot, FDiv)
str(fdiv)

##import trait .rds file
gain.loss.new <- read_rds("output/gain.loss.rds") %>%
  dplyr::select(Plot, Block, Light, Nutrients, Thin, Max.Height, SLA, RTD, richness) %>%
  full_join(social.metrics, by = "Plot") %>%
  left_join(fdiv, by = "Plot") %>%
  dplyr::mutate(Nutrients = replace(Nutrients, Nutrients == 'No fert', 'Not fertilized')) %>%
  rename(Richness = richness) %>%
  mutate(Resource_treatment = as.factor(paste0(Light, "_", Nutrients))) ## make new column to differentiate the shape of points among treatments
str(gain.loss.new)

#### look for correlations among predictors ####
## scale trait values
dat <- gain.loss.new %>%
  mutate(across(c(Max.Height, SLA, RTD), ~ scale(.x, center = TRUE, scale = FALSE))) %>%
  mutate(across(c(Gregariousness, Reclusiveness), ~ scale(.x, center = TRUE, scale = FALSE))) %>%
  mutate(across(c(Richness, FDiv), ~ scale(.x, center = TRUE, scale = FALSE)))

## isolate variables
corr.df2 <- dat %>%
  dplyr::select(Max.Height, SLA, RTD, Richness, Gregariousness, Reclusiveness, FDiv)

## default corrplot
corrplot(cor(corr.df2))

## calculate correlation matrix and p-values
cor.results <- rcorr(as.matrix(corr.df2))

## extract correlation matrix and p-values
cor_matrix <- cor.results$r
p_values <- cor.results$P

## mask insignificant correlations
significant.cor <- cor_matrix
significant.cor[p_values > 0.05] <- NA

## plot only the significant correlations
png("figures/predictors correlation plot.png", width = 1000, height = 1000)
corrplot(significant.cor,
         na.label = " ",
         tl.col = "black",
         tl.cex = 2,   # increase the text labels
         cl.cex = 1.5)   # increase color legend text
dev.off()

#### check VIF for models ####
## trait models 
CWM.gains.lmm <- lmer(spp.gains ~ Max.Height*SLA*RTD + (1|Block), data = dat)
check_collinearity(CWM.gains.lmm)

CWM.losses.lmm <- lmer(spp.losses ~ Max.Height*SLA*RTD + (1|Block), data = dat)
check_collinearity(CWM.losses.lmm)

## resource models 
spp.gains <- lmer(spp.gains ~ Nutrients*Light + (1|Block), data = gain.loss.new)
check_collinearity(spp.gains) #low correlation

spp.losses <- lmer(spp.losses ~ Nutrients*Light + (1|Block), data = gain.loss.new)
check_collinearity(spp.losses) #low correlation

## social models
social.losses.lmm <- lmer(spp.losses ~ Gregariousness*Reclusiveness + (1|Block), data = dat)
check_collinearity(social.losses.lmm)

social.gains.lmm <- lmer(spp.gains ~ Gregariousness*Reclusiveness + (1|Block), data = dat)
check_collinearity(social.gains.lmm)

##diversity models
div.losses.lmm <- lmer(spp.losses ~ Richness*FDiv + (1|Block), data = dat)
check_collinearity(div.losses.lmm)

div.gains.lmm <- lmer(spp.gains ~ Richness*FDiv + (1|Block), data = dat)
check_collinearity(div.gains.lmm)

#### make table for export ####
## write function to extract values
extract_collinearity <- function(models, model_names = NULL, save_path = NULL) {
  # If no model names are given, assign generic ones
  if (is.null(model_names)) {
    model_names <- paste0("model_", seq_along(models))
  }
  
  # Check names length
  if (length(model_names) != length(models)) {
    stop("`model_names` must have the same length as `models`.")
  }
  
  # Run check_collinearity on each model
  collin_list <- map(models, check_collinearity)
  
  # Convert to data frames and add model name
  collin_df <- map2_dfr(collin_list, model_names, ~
                          as.data.frame(.x) %>%
                          mutate(Model = .y) %>%
                          relocate(Model)
  )
}

## make list of models
models <- list(CWM.gains.lmm, spp.gains, social.gains.lmm, div.losses.lmm, 
               CWM.losses.lmm, spp.losses, social.losses.lmm, div.gains.lmm)

## make table
collin.table <- extract_collinearity(models) %>%
  mutate(VIF.interval = VIF - VIF_CI_low,
         Tolerance.interval = Tolerance - Tolerance_CI_low) %>%
  mutate(VIF.CI = paste0(round(VIF, 3)," ± ", round(VIF.interval, 3))) %>%
  mutate(Tolerance.CI = paste0(round(Tolerance, 3)," ± ", round(Tolerance.interval, 3))) %>%
  dplyr::select(Model, Term, VIF.CI, Tolerance.CI) %>%
  rename("VIF ± 95% CI" = VIF.CI,
         "Tolerance ± 95% CI" = Tolerance.CI)
str(collin.table)

## export table
write.csv(collin.table, "output/VIF table for models.csv", row.names = FALSE)
