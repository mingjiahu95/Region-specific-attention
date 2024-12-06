

library(data.table)
library(ggplot2)
library(plyr)
library(dplyr)
library(purrr)
library(gridExtra)
library(tidyr)
source('utils.R')

# ---- Load the Data ----
propcorr_upper = c(0.883064516,0.786290323,0.334677419,0.943452367,0.903125,0.70625,0.979761905,0.985416667,0.9375)
propcorr_lower = c(0.686635945,0.66733871,0.387096774,0.613095238,0.629166667,0.345833333,0.74744898,0.748883929,0.640625)
condition = factor(c(2,2,2,1,1,1,3,3,3),levels = c(1,2,3),labels = c("4CAT","2CAT","4VL & 4RH"))
itemtype = factor(c(1,2,3,1,2,3,1,2,3),levels = c(1,2,3),labels = c("Training","Other Transfer","Critical Transfer"))
propcorr_upper_df = data.frame(Condition = condition,
                               ItemType = itemtype,
                               PropCorr = propcorr_upper)
propcorr_lower_df = data.frame(Condition = condition,
                               ItemType = itemtype,
                               PropCorr = propcorr_lower)


#---- plot the data ----
p = ggplot(propcorr_upper_df, aes(x = ItemType, y = PropCorr, fill = Condition)) +
    geom_col(position = position_dodge(.8),color = "black",width = .6) +
    geom_text(aes(label = round(PropCorr,3)),size = 4,vjust = -.4, position = position_dodge(.8)) +
    xlab("Item Type") +
    ylab("Mean Proportion Correct") +
    ylim(0,1) +
    theme_bw(base_size = 14) + 
    theme(plot.title = element_text(hjust = .5),
          plot.subtitle = element_text(hjust = .5),
          axis.text.x = element_text(size = 12))

ggsave("propcorr_upper_median.jpg",plot = p,width = 10, height = 6)

p = ggplot(propcorr_lower_df, aes(x = ItemType, y = PropCorr, fill = Condition)) +
  geom_col(position = position_dodge(.8),color = "black",width = .6) +
  geom_text(aes(label = round(PropCorr,3)),size = 4,vjust = -.4, position = position_dodge(.8)) +
  xlab("Item Type") +
  ylab("Mean Proportion Correct") +
  ylim(0,1) +
  theme_bw(base_size = 14) + 
  theme(plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5),
        axis.text.x = element_text(size = 12))

ggsave("propcorr_lower_median.jpg",plot = p,width = 10, height = 6)