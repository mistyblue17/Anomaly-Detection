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
data <- read_csv("results_summary_bycrop_new_tweaked.csv")

# 275 runs of economic model by Deborah only changing C2 one crop at a time
data2 <- read_csv("results_summary_bycrop.csv")

# 275 runs of economic model by Deborah only changing C2 one crop at a time, added run ID
data3 <- read_csv("results_summary_bycrop_byrun.csv")


### re-classify fertilizer treaments to 0 (no fertilizer) or non-0 (fertilizer applied) ###
data$fertclass <- data$c1
data$fertclass[data$c2 > 0] <- "Fert Subsidy"
data$fertclass[data$c2 < 0] <- "Fert Subsidy"
data$fertclass[data$c2 == 0] <- "No Fert Subsidy"
data <- data %>% group_by(run_ID)

data$flag <- "valid"

# assign inputs and outputs to variables
# in1 <- data$p
# in2 <- data$c1
# in3 <- data$c2
# out1 <- data$`land area (ha)`
# out2 <- data$`yield (kg/ha)`
# out3 <- data$`Nfert (kg/ha)`
# out4 <- data$`production (kg)`
# name <- data$crop

### check for results that defy the laws of physics by exceeding total available land to use ###
# identify total land
total_land <- sum(calib_data$xbar1)

# filter by c1, c2, p combinations
landcheck <- data %>%
  group_by(run_ID) %>%
  summarize(landsum = sum(`land area (ha)`)) 

  
aggregate(data$ID, by = "crop", apply(flag_underuse_anomaly))
flag_list <- c()
# %>%
  # filter(sum < (total_land - 1)) %>%
  # within(data, levels(flag)[levels(flag) == "valid"] <- "invalid")
#   mutate(flag = replace(flag, flag == "valid", "invalid"))

# find runs that over use total available land -> anomalous
overuse <- landcheck %>%
  filter (landsum > (total_land + 1)) 
  
    # within(data, levels(flag)[levels(flag) == "valid"] <- "invalid")

  
data$flag[which(underuse$flag == "invalid")] <- "invalid"
  # mutate(select(sum < (total_land - 1)), replace = "invalid"]
  
    
# find runs that under use total available land -> not anomalous b/c doesn't defy physics but could be interesting to analyze further
underuse <- landcheck %>%
  filter (landsum < (total_land - 1))
underuse$flag <- "invalid"

flag_underuse_anomaly <- function(data, underuse) {
  if (underuse$run_ID %in% data$run_ID) {
    data$flag[which] = "invalid"
  } else {
    data$flag = "valid"
  }
}   
    # rename_if(landsum, landsum < (total_land - 1), ~flag(.) =="invalid")
    
    # data$flag <- "invalid" 
  # if (-group_cols(underuse$c("run_ID", "c1", "c2", "p") == data$c("run_ID", "c1", "c2", "p"))) {
  #   data$flag <-  "invalid"
  # }
}

data[]

data <- flag_underuse_anomaly(data = data, underuse = underuse)
data$flag[which(data$run_ID %in% overuse$run_ID)] <- "invalid"  ### FINAL CORRECT SCRIPT THAT WORKS
data$flag[which(data$run_ID %in% underuse$run_ID)] <- "invalid" ### FINAL CORRECT SCRIPT THAT WORKS

data$flag <- data %>% select_if(data, underuse$c("run_ID", "c1", "c2", "p") == data$c("run_ID", "c1", "c2", "p"), replace(data$flag, data$flag == "valid", "invalid"))

data[which(underuse$c("run_ID", "c1", "c2", "p") == data$c("run_ID", "c1", "c2", "p")), "flag"] <- "invalid"

data <- inner_join(data, underuse, op = mutate_if(flag, underuse$c("run_ID", "c1", "c2", "p") == data$c("run_ID", "c1", "c2", "p"), data$flag = "invalid"))


  within(data, levels(flag)[levels(flag) == "valid"] <- "invalid")
         
data <- data %>% mutate_at(vars(-group_cols(c("run_ID", "p", "c1", "c2", "p"))), data$flag <- "invalid")


df <- df %>% mutate(height = replace(height, height == 20, NA))
data$flag <- underuse[ select(sum < (total_land - 1)), replace(flag, flag == "valid", "invalid")]

data$flag[underuse] <- "invalid"

data <- flag_underuse_anomaly(data = data)
data$flag[ which(underuse %in% data)] <- "invalid" 

data$flag <- data %>% select_if(data, underuse$c("run_ID", "c1", "c2", "p") == data$c("run_ID", "c1", "c2", "p"), replace(data$flag, data$flag == "valid", "invalid"))

data[which(underuse$c("run_ID", "c1", "c2", "p") == data$c("run_ID", "c1", "c2", "p")), "flag"] <- "invalid"

data <- inner_join(data, underuse, op = mutate_if(flag, underuse$c("run_ID", "c1", "c2", "p") == data$c("run_ID", "c1", "c2", "p"), data$flag = "invalid"))


within(data, levels(flag)[levels(flag) == "valid"] <- "invalid")

data <- data %>% mutate_at(vars(-group_cols(c("run_ID", "p", "c1", "c2", "p"))), data$flag <- "invalid")


df <- df %>% mutate(height = replace(height, height == 20, NA))
data$flag <- underuse[ select(sum < (total_land - 1)), replace(flag, flag == "valid", "invalid")]
