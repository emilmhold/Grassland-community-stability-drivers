## light penetration in ambient vs tie-back plots
## Author: Emily H
## Created: June 6, 2024
## Last edited: January 18, 2024

#install.packages("tidyverse")
#install.packages("lme4)
#install.packages("lmerTest")
#install.packages("car")
#install.packages("emmeans")

library(tidyverse)
library(lme4)
library(lmerTest)
library(car)
library(emmeans)

setwd("/Users/emilyholden/Documents/GitHub/Analyses/Resources, diversity, and traits drive community stability")

#### import data ####
#read in PAR data
PAR21.new <- read_rds("output/PAR21 new.rds")
str(PAR21.new)

# read in trt data
trts <- read.csv("data/Plot treatments.csv") %>%
  dplyr::select(!X)
str(trts)

# merge dfs
dat <- merge(trts, PAR21.new, by = "Plot") %>% 
  filter(Thin == "Not thinned")
str(dat)

# calculate means and SEs for Table S1
sum.dat <- dat %>%
  dplyr::group_by(Light, Nutrients) %>%
  dplyr::summarise(mean.light.penetration = mean(True.light.penetration),
                   se.light.penetration = sd(True.light.penetration)/sqrt(length(True.light.penetration))) %>%
  mutate_if(is.numeric, round, digits = 3)
print(sum.dat)

#### model ####
# test for significant differences
light.model <- lmer(True.light.penetration ~ Light + (1|Block), data = dat)
Anova(light.model)
emmeans(light.model, pairwise ~ Light, adjust = "Tukey") #differences between tie-backs and shade plots

nutrient.model <- lmer(True.light.penetration ~ Nutrients + (1|Block), data = dat)
Anova(nutrient.model)

#### plot ####
# prep light data
light.sum <- dat %>%
  dplyr::group_by(Light) %>%
  dplyr::summarise(mean.light.penetration = mean(True.light.penetration),
                   se.light.penetration = sd(True.light.penetration)/sqrt(length(True.light.penetration)))

##graph
light.trt.plot <- ggplot(light.sum, aes(x = Light, y = mean.light.penetration, fill=Light)) +
  geom_col() +
  geom_errorbar(aes(ymin=mean.light.penetration-se.light.penetration, ymax=mean.light.penetration+se.light.penetration), position = "dodge") + 
  labs(x = "Light treatment",
       y = "Proportional light penetration") + 
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  scale_fill_grey() +
  theme_classic(base_size = 20) + 
  annotate("text", x = -Inf, y = Inf, label = bquote(bold("p = 0.010")), hjust = -0.1, vjust = 1.1, size = 5)
light.trt.plot

##export
ggsave(filename = "light penetration by light trt.png", 
       light.trt.plot,
       path = "figures/",
       width = 8,
       height = 6,
       units = "in"
)
