## Make figures for paper
## Author: Emily H
## Created: June 6, 2024
## Last edited: November 24, 2025

#install.packages("tidyverse")
#install.packages("scales")
#install.packages("cowplot")

library(tidyverse)
library(scales)
library(cowplot)

setwd("/Users/emilyholden/Documents/GitHub/Analyses/Resources, diversity, and traits drive community stability")

#### import data ####
## import social metric file
social.metrics <- read_rds("output/CWM coocurrences.rds") %>%
  rename(gregariousness = total.pos.cooccurrence,
         isolationism = total.neg.cooccurrence) %>%
  dplyr::select(Plot, gregariousness, isolationism, spp.losses, spp.gains, turnover, Productivity)
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

nutrients.for.plots <- gain.loss.new %>%
  group_by(Nutrients) %>%
  summarise(mean.gains = mean(spp.gains),
            se.gains = sd(spp.gains)/sqrt(length(spp.gains)),
            mean.losses = mean(spp.losses),
            se.losses = sd(spp.losses)/sqrt(length(spp.losses)),
            mean.turnover = mean(turnover),
            se.turnover = sd(turnover)/sqrt(length(turnover)),
            mean.productivity = mean(Productivity),
            se.productivity = sd(Productivity)/sqrt(length(Productivity))
            )
str(nutrients.for.plots)
##find how many times more productive fertilized plots were
# 426/361

light.for.plots <- gain.loss.new %>%
  group_by(Light) %>%
  summarise(mean.gains = mean(spp.gains),
            se.gains = sd(spp.gains)/sqrt(length(spp.gains)),
            mean.losses = mean(spp.losses),
            se.losses = sd(spp.losses)/sqrt(length(spp.losses)),
            mean.turnover = mean(turnover),
            se.turnover = sd(turnover)/sqrt(length(turnover)),
            mean.productivity = mean(Productivity),
            se.productivity = sd(Productivity)/sqrt(length(Productivity))
            )
str(light.for.plots)

#### gains figures ####
height.gains.plot <- ggplot(gain.loss.new, aes(x = Max.Height, y = spp.gains, shape = Resource_treatment)) +
  geom_point() + 
  labs(y = " ",
       x = " ") +
  scale_fill_manual(values = "#1F618D") +
  theme_classic(base_size = 20) + 
  annotate("text", x = Inf, y = Inf, label = bquote(bold("p = 0.235")), hjust = 1.1, vjust = 1.1, size = 5) +
  theme(legend.position = "none")
height.gains.plot

SLA.gains.plot <- ggplot(gain.loss.new, aes(x = SLA, y = spp.gains, shape = Resource_treatment)) +
  geom_point() +
  labs(y = " ",
       x = " ") +
  theme_classic(base_size = 20) +
  annotate("text", x = Inf, y = Inf, label = bquote(bold("p = 0.674")), hjust = 1.1, vjust = 1.1, size = 5) +
  theme(legend.position = "none")
SLA.gains.plot

RTD.gains.plot <- ggplot(gain.loss.new, aes(x = RTD, y = spp.gains, shape = Resource_treatment)) +
  geom_point() + 
  labs(y = " ",
       x = " ") +
  theme_classic(base_size = 20) +
  annotate("text", x = Inf, y = Inf, label = bquote(bold("p = 0.258")), hjust = 1.1, vjust = 1.1, size = 5) +
  theme(legend.position = "none")
RTD.gains.plot

richness.gains.plot <- ggplot(gain.loss.new, aes(x = Richness, y = spp.gains, shape = Resource_treatment)) +
  geom_point() + 
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, colour = "blue") +
  labs(y = " ",
       x = " ") +
  theme_classic(base_size = 20) +
  annotate("text", x = Inf, y = Inf, label = bquote(bold("p = 0.046")), hjust = 1.1, vjust = 1.1, size = 5) +
  theme(legend.position = "none")
richness.gains.plot

FDiv.gains.plot <- ggplot(gain.loss.new, aes(x = FDiv, y = spp.gains, shape = Resource_treatment)) +
  geom_point() + 
  labs(y = " ",
       x = " ") +
  theme_classic(base_size = 20) +
  annotate("text", x = Inf, y = Inf, label = bquote(bold("p = 0.294")), hjust = 1.1, vjust = 1.1, size = 5) +
  theme(legend.position = "none")
FDiv.gains.plot

nutrients.gains.plot <- ggplot(nutrients.for.plots, aes(x = Nutrients, y = mean.gains, fill=Nutrients)) +
  geom_col() +
  geom_errorbar(aes(ymin=mean.gains-se.gains, ymax=mean.gains+se.gains), position = "dodge") + 
  labs(x = " ",
       y = "Relative contribution of \nspecies gains") + 
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  scale_fill_manual(values = c("#1F618D", "#DAF7A6")) +
  theme_classic(base_size = 20) + 
  annotate("text", x = -Inf, y = Inf, label = bquote(bold("p = 0.569")), hjust = -0.1, vjust = 1.1, size = 5)
nutrients.gains.plot

light.gains.plot <- ggplot(light.for.plots, aes(x = Light, y = mean.gains, fill = Light)) +
  geom_col() +
  geom_errorbar(aes(ymin=mean.gains-se.gains, ymax=mean.gains+se.gains), position = "dodge") + 
  labs(x = " ",
       y = " ") + 
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  scale_fill_manual(values = c("#FFC300", "#FF5733", "#900C3F")) +
  #geom_signif(stat="identity",aes(x=signif, xend = signif, y=y, yend = y, annotation="*"), size = 5) +
  theme_classic(base_size = 20) +
  annotate("text", x = Inf, y = Inf, label = bquote(bold("p = 0.903")), hjust = 1.1, vjust = 1.1, size = 5)
light.gains.plot

#### losses figures ####
height.losses.plot <- ggplot(gain.loss.new, aes(x = Max.Height, y = spp.losses, shape = Resource_treatment)) +
  geom_point() + 
  labs(y = " ",
       x = "CWM Maximum height \n(cm)") +
  theme_classic(base_size = 20) + 
  annotate("text", x = Inf, y = Inf, label = bquote(bold("p = 0.307")), hjust = 1.1, vjust = 1.1, size = 5) +
  theme(legend.position = "none")
height.losses.plot

SLA.losses.plot <- ggplot(gain.loss.new, aes(x = SLA, y = spp.losses, shape = Resource_treatment)) +
  geom_point() + 
  labs(y = " ",
       x = "CWM SLA \n(cm\u00B2/g)") +
  theme_classic(base_size = 20) +
  annotate("text", x = Inf, y = Inf, label = bquote(bold("p = 0.507")), hjust = 1.1, vjust = 1.1, size = 5) +
  theme(legend.position = "none")
SLA.losses.plot

RTD.losses.plot <- ggplot(gain.loss.new, aes(x = RTD, y = spp.losses, shape = Resource_treatment)) +
  geom_point() + 
  labs(y = " ",
       x = "CWM RTD \n(mg/cm\u00B2)") +
  theme_classic(base_size = 20) +
  annotate("text", x = Inf, y = Inf, label = bquote(bold("p = 0.786")), hjust = 1.1, vjust = 1.1, size = 5) +
  theme(legend.position = "none")
RTD.losses.plot

richness.losses.plot <- ggplot(gain.loss.new, aes(x = Richness, y = spp.losses, shape = Resource_treatment)) +
  geom_point() + 
  labs(y = " ",
       x = "2021 richness \n") +
  theme_classic(base_size = 20) +
  annotate("text", x = Inf, y = Inf, label = bquote(bold("p = 0.428")), hjust = 1.1, vjust = 1.1, size = 5) +
  theme(legend.position = "none")
  richness.losses.plot

FDiv.losses.plot <- ggplot(gain.loss.new, aes(x = FDiv, y = spp.losses, shape = Resource_treatment)) +
  geom_point() + 
  labs(y = " ",
       x = "FDiv \n") +
  theme_classic(base_size = 20) +
  annotate("text", x = Inf, y = Inf, label = bquote(bold("p = 0.434")), hjust = 1.1, vjust = 1.1, size = 5) +
  theme(legend.position = "none")
FDiv.losses.plot

nutrients.losses.plot <- ggplot(nutrients.for.plots, aes(x = Nutrients, y = mean.losses, fill = Nutrients)) +
  geom_col() +
  geom_errorbar(aes(ymin=mean.losses-se.losses, ymax=mean.losses+se.losses), position = "dodge") + 
  labs(x = "Nutrient treatment \n",
       y = "Relative contribution of \nspecies losses") + 
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  scale_fill_manual(values = c("#1F618D", "#DAF7A6")) +
  #geom_signif(stat="identity",aes(x=signif, xend = signif, y=y, yend = y, annotation="*"), size = 5) +
  theme_classic(base_size = 20) + 
  annotate("text", x = 1, y = nutrients.for.plots$mean.losses[1] + 0.04, label = "a", size = 6) +
  annotate("text", x = 2, y = nutrients.for.plots$mean.losses[2] + 0.04, label = "b", size = 6) +
  annotate("text", x = Inf, y = Inf, label = bquote(bold("p < 0.001")), hjust = 1, vjust = 1.1, size = 5)
  #annotate("text", x = 1.5, y = max(nutrients.for.plots$mean.losses) + 0.03, label = "*", size = 6, fontface = "bold")
nutrients.losses.plot

light.losses.plot <- ggplot(light.for.plots, aes(x = Light, y = mean.losses, fill = Light)) +
  geom_col() +
  geom_errorbar(aes(ymin=mean.losses-se.losses, ymax=mean.losses+se.losses), position = "dodge") + 
  labs(x = "Light treatment\n",
       y = " ") + 
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  scale_fill_manual(values = c("#FFC300", "#FF5733", "#900C3F")) +
  theme_classic(base_size = 20) + 
  annotate("text", x = Inf, y = Inf, label = bquote(bold("p = 0.289")), hjust = 1.1, vjust = 1, size = 5)
light.losses.plot

#### put it together ####
## extract the legend
legend <- cowplot::get_legend(ggplot(gain.loss.new, aes(x = Richness, y = spp.gains, shape = Resource_treatment)) +
    geom_point() +
  scale_shape_discrete(name = "Resource treatment",
                       drop = FALSE) +  # force legend for all levels
    theme_classic(base_size = 20) +
    theme(legend.text = element_text(size = 16),
          legend.direction = "horizontal") +
  guides(shape = guide_legend(nrow = 1)))

## make figure
traits.resources.multiplot <- cowplot::plot_grid(
  cowplot::plot_grid(
    nutrients.gains.plot + theme(legend.position = "none"), 
    light.gains.plot + theme(legend.position = "none"), 
    richness.gains.plot + theme(legend.position = "none"), 
    FDiv.gains.plot, height.gains.plot, SLA.gains.plot, RTD.gains.plot,
    nutrients.losses.plot + theme(legend.position = "none"), 
    light.losses.plot + theme(legend.position = "none"), 
    richness.losses.plot + theme(legend.position = "none"), 
    FDiv.losses.plot, height.losses.plot, SLA.losses.plot, RTD.losses.plot, 
    ncol = 7,
    labels = 'auto',
    label_size = 16
  ),
  legend,                      # add the legend below
  ncol = 1,                    # stack vertically
  rel_heights = c(1, 0.1)     # adjust the legend height
)
traits.resources.multiplot

## add "Figure 1" title
title <- ggdraw() + 
  draw_label("Figure 1", x = 0, hjust = 0, size = 20)

final_plot <- plot_grid(
  title,
  traits.resources.multiplot,
  ncol = 1,
  rel_heights = c(0.05, 1)
)
final_plot

ggsave(filename = "traits and resource plots.jpeg", 
       final_plot,
       path = "figures/",
       width = 27,
       height = 15,
       units = "in"
)

#### make loss ~ light figure ####
#read in PAR data
PAR21.new <- read_rds("output/PAR21 new.rds")
str(PAR21.new)

#join with gain.loss.new df
light.loss.df <- gain.loss.new %>%
  left_join(PAR21.new, by = "Plot")
str(light.loss.df)

## plot
light.loss.plot <- ggplot(light.loss.df, aes(x = True.light.penetration, y = spp.losses)) +
  geom_point() + 
  labs(y = "Relative contribution of \nspecies losses",
       x = "Proportional light penetration") +
  annotate("text", x = Inf, y = Inf, label = bquote(bold("p = 0.81")), hjust = 1, vjust = 1, size = 5) +
  theme_classic(base_size = 20) 
light.loss.plot

##export
ggsave(filename = "light loss plot.png", 
       light.loss.plot,
       path = "figures/",
       width = 8,
       height = 6,
       units = "in"
)

