library(readr)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(anomalize)
library(outliers)
library(plotly)

wname <- getwd() # set back to working directory
dname <- paste(wname,"economicdata",sep="/") # will open data file in workikng directory
oname <- paste(wname,"outputs",sep="/") # will open outputs file in workikng directory
pname<-paste(wname,"plots",sep="/") # will open plots file in workikng directory
fname<-paste(wname,"functions",sep="/") # will open functions file in workikng directory

# color pallette generated using https://coolors.co/
colors <- c('#26547c', '#c43b5b', '#e8be5d', '#05b083', '#2f2e2e')

op <- function(tbls) {
  semi_join(tbls[[1]], tbls[[2]], by = "X1")
}

# read in the data 
setwd(dname) #tells the program to look in the data folder

# auxillary files
mint_sim <- read_csv("economic-v6/MINT_v6_simulation_output.txt")

production_cost <- read_csv("economic-v6/productioncost_v6.csv")
price <- read_csv("economic-v6/price_v6.csv")
sim_production_cost <- read_csv("economic-v6/simproductioncost.csv")
sim_price <- read_csv("economic-v6/simprice_v6.csv")
cycles_data <- read_csv("economic-v6/cyclesdata_v6.csv")
calib_data <- read_csv("economic-v6/calibdata_v6.csv")
calib_output <- read_csv("economic-v6/MINT_v6_calibration_output.txt")

# 1000 runs of economic model
data <- read_csv("results_summary_bycrop_new.csv")

# 275 runs of economic model by Deborah only changing C2 one crop at a time
data2 <- read_csv("results_summary_bycrop.csv")

# 275 runs of economic model by Deborah only changing C2 one crop at a time, added run ID
data3 <- read_csv("results_summary_bycrop_byrun.csv")


### visualize original results data
# boxplots
ggplot(data = data, mapping = aes(x = `crop`, y = `land area (ha)`)) + 
  geom_boxplot(mapping = aes(color = crop)) + 
  theme_hc() + 
  scale_color_hc()+
  ggtitle("Land Use (All Crops)")

ggplot(data = data, mapping = aes(x = `crop`, y = `Nfert (kg/ha)`)) + 
  geom_boxplot(mapping = aes(color = crop)) + 
  theme_hc() + 
  scale_color_hc()+
  ylim(0, 500)
  ggtitle("Fertilizer Use (All Crops)")

ggplot(data = data, mapping = aes(x = `crop`, y = `yield (kg/ha)`)) + 
  geom_boxplot(mapping = aes(color = crop)) + 
  theme_hc() + 
  scale_color_hc()+
  ggtitle("Yield (All Crops)")

ggplot(data = data, mapping = aes(x = `crop`, y = `production (kg)`)) + 
  geom_boxplot(mapping = aes(color = crop)) + 
  theme_hc() + 
  scale_color_hc()+
  ggtitle("Production (All Crops)")


# distribution plots
ggplot(data = data, mapping = aes(x = `land area (ha)`)) + 
  geom_freqpoly(mapping = aes(color = crop)) + 
  theme_hc() + 
  scale_color_hc()+
  ggtitle("Distribution of Land Use per Crop")

ggplot(data = data, mapping = aes(x = `Nfert (kg/ha)`)) + 
  geom_freqpoly(mapping = aes(color = crop)) + 
  theme_hc() + 
  scale_color_hc()+
  ggtitle("Distribution of Fertilizer per Crop")

ggplot(data = data, mapping = aes(x = `yield (kg/ha)`)) + 
  geom_freqpoly(mapping = aes(color = crop)) + 
  theme_hc() + 
  scale_color_hc()+
  ggtitle("Distribution of Yield per Crop")

ggplot(data = data, mapping = aes(x = `production (kg)`)) + 
  geom_freqpoly(mapping = aes(color = crop)) + 
  theme_hc() + 
  scale_color_hc()+
  ggtitle("Distribution of Production per Crop")


# violin plots
ggplot(data = data, mapping = aes(x = `crop`, y = `land area (ha)`)) +
  geom_violin(mapping = aes(color = crop)) + 
  theme_hc() + 
  scale_color_hc()+
  ggtitle("Land Use per Crop")

ggplot(data = data, mapping = aes(x = `crop`, y = `Nfert (kg/ha)`)) +
  geom_violin(mapping = aes(color = crop)) + 
  theme_hc() + 
  scale_color_hc()+
  ggtitle("Fertilizer Use per Crop")

ggplot(data = data, mapping = aes(x = `crop`, y = `yield (kg/ha)`)) +
  geom_violin(mapping = aes(color = crop)) + 
  theme_hc() + 
  scale_color_hc()+
  ggtitle("Yield per Crop")

ggplot(data = data, mapping = aes(x = `crop`, y = `production (kg)`)) + 
  geom_violin(mapping = aes(color = crop)) + 
  theme_hc() + 
  scale_color_hc()+
  ggtitle("Production per Crop")


# Scatterplots - these don't run yet
ggplot(data = data, mapping = aes(x = reorder(`Nfert (kg/ha)`, crop, FUN = median), y = reorder(`yield (kg/ha)`, crop, FUN = median))) + 
  geom_point(mapping = aes(color = crop)) + 
  theme_hc() + 
  scale_color_hc()+
  ggtitle("Fertilizer vs. Yield (All Crops)")

ggplot(data = data, mapping = aes(x = `land area (ha)`, y = `yield (kg/ha)`)) + 
  geom_point(mapping = aes(color = crop)) + 
  theme_hc() + 
  scale_color_hc()+
  ggtitle("Land Use vs. Yield (All Crops)")

ggplot(data = data$crop, mapping = aes(x = `Nfert (kg/ha)`, y = `yield (kg/ha)`)) + 
  geom_point(mapping = aes(color = crop)) + 
  theme_hc() + 
  scale_color_hc()+
  ggtitle("Fertilizer vs. Yield (All Crops)")
