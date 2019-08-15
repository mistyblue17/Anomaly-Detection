library(readr)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(anomalize)
library(outliers)
library(plotly)
library(cluster)
library(factoextra)

setwd("~/MINT/Economic")
wname <- getwd() # set back to working directory
dname <- paste(wname,"economicdata",sep="/") # will open data file in workikng directory
oname <- paste(wname,"outputs",sep="/") # will open outputs file in workikng directory
pname<-paste(wname,"plots",sep="/") # will open plots file in workikng directory
fname<-paste(wname,"functions",sep="/") # will open functions file in workikng directory

# color pallette generated using https://coolors.co/
colors <- c('#26547c', '#c43b5b', '#e8be5d', '#05b083', '#2f2e2e')

############################# IMPORT & PREPARE DATA #############################
# read in the data 
setwd(dname) #tells the program to look in the data folder

calib_data <- read_csv("economic-v6/calibdata_v6.csv")
data <- read_csv("results_summary_bycrop_withfeelings.csv")

# return to working directory
setwd(wname)

### add coloumns for anomaly flags and reason flag was generated
data$flag <- "valid"
data$reason <- NA

### re-classify fertilizer treaments to 0 (no fertilizer) or non-0 (fertilizer applied)
data$fertclass <- data$c1
data$fertclass[data$c2 > 0] <- "Fert Subsidy"
data$fertclass[data$c2 < 0] <- "Fert Subsidy"
data$fertclass[data$c2 == 0] <- "No Fert Subsidy"
data <- data %>% group_by(run_ID)

### subset by crop
cassava_results <- subset(data, crop=='cassava') 
groundnuts_results <- subset(data, crop=='groundnuts')
maize_results <- subset(data, crop=='maize')
sesame_results <- subset(data, crop=='sesame')
sorghum_results <- subset(data, crop=='sorghum')



####################### FLAG INVALID RUNS ####################
### check for results that defy the laws of physics by exceeding total available land to use ###
# identify total land
total_land <- sum(calib_data$xbar1)

# group by run ID and find sum of land area from each crop for that run
landcheck <- data %>%
  group_by(run_ID) %>%
  summarize(landsum = sum(`land area (ha)`)) 

# find runs that over use total available land, flag as invalid, and add reason for flag
overuse <- landcheck %>%
  filter (landsum > (total_land + 1))

data$flag[which(data$run_ID %in% overuse$run_ID)] <- "invalid" 
data$reason[which(data$run_ID %in% overuse$run_ID)] <- paste(data$reason, "land overuse", sep = ";")

# find runs that under use total available land -> not anomalous b/c doesn't defy physics but could be interesting to analyze further
underuse <- landcheck %>%
  filter (landsum < (total_land - 1))

data$reason[which(data$run_ID %in% underuse$run_ID)] <- paste(data$reason, "land underuse", sep = ";") 



####################### MAD STATISTICAL OUTLIER REMOVAL #############################
### set MAD threshold, to be multiplied by MAD, values beyond threshold * MAD are flagged as statistical outliers 
mad_thresh <- 9

### calculate median absolute deviation (MAD) by output variable for each crop
### CASSAVA ###
### calculate median absolute deviation by output variable using cassava data subset
mad_cassava_land <- mad_thresh * mad(cassava_results$`land area (ha)`, na.rm = TRUE)
mad_cassava_yield <- mad_thresh * mad(cassava_results$`yield (kg/ha)`, na.rm = TRUE)
mad_cassava_prod <- mad_thresh * mad(cassava_results$`production (kg)`, na.rm = TRUE)
mad_cassava_fert <- mad_thresh * mad(cassava_results$`Nfert (kg/ha)`, na.rm = TRUE)
mad_cassava_fertuse <- mad_thresh * mad(cassava_results$`Nuse (kg)`, na.rm = TRUE)


# compare variables to MAD to determine if outlier
mad_outliers_cassava_land <- cassava_results %>%
  group_by(run_ID) %>%
  filter(`land area (ha)` > mad_cassava_land)
mad_outliers_cassava_yield <- cassava_results %>%
  group_by(run_ID) %>%
  filter(`yield (kg/ha)` > mad_cassava_yield)
mad_outliers_cassava_prod <- cassava_results %>%
  group_by(run_ID) %>%
  filter(`production (kg)` > mad_cassava_prod)
  ### identifies 6652 obs out of 6655 obs as outliers, skip flagging or else all runs will be invalid
mad_outliers_cassava_fert <- cassava_results %>%
  group_by(run_ID) %>%
  filter(`Nfert (kg/ha)` > mad_cassava_fert)
mad_outliers_cassava_fertuse <- cassava_results %>%
  group_by(run_ID) %>%
  filter(`Nuse (kg)` > mad_cassava_fertuse)


# flag outliers in original dataframe and add reason "MAD outlier" -> only flag yield and fert since those are the variables we are comparing in clusters
# data$flag[which(data$run_ID %in% mad_outliers_cassava_land$run_ID)] <- "invalid" 
# data$reason[which(data$run_ID %in% mad_outliers_cassava_land$run_ID)] <- paste(data$reason, "MAD land outlier", sep = ";")

data$flag[which(data$run_ID %in% mad_outliers_cassava_yield$run_ID)] <- "invalid" 
data$reason[which(data$run_ID %in% mad_outliers_cassava_yield$run_ID)] <- paste(data$reason, "MAD yield outlier", sep = ";")

# skip otherwise all runs will be flagged as invalid with MAD production outlier
# data$flag[which(data$run_ID %in% mad_outliers_cassava_prod$run_ID)] <- "invalid" 
# data$reason[which(data$run_ID %in% mad_outliers_cassava_prod$run_ID)] <- paste(data$reason, "MAD production outlier", sep = ";")

data$flag[which(data$run_ID %in% mad_outliers_cassava_fert$run_ID)] <- "invalid" 
data$reason[which(data$run_ID %in% mad_outliers_cassava_fert$run_ID)] <- paste(data$reason, "MAD fert outlier", sep = ";")

# data$flag[which(data$run_ID %in% mad_outliers_cassava_fertuse$run_ID)] <- "invalid" 
# data$reason[which(data$run_ID %in% mad_outliers_cassava_fertuse$run_ID)] <- paste(data$reason, "MAD fert use outlier", sep = ";")


### GROUNDNUTS ###
### calculate median absolute deviation by output variable using groundnuts data subset
mad_groundnuts_land <- mad_thresh * mad(groundnuts_results$`land area (ha)`, na.rm = TRUE)
mad_groundnuts_yield <- mad_thresh * mad(groundnuts_results$`yield (kg/ha)`, na.rm = TRUE)
mad_groundnuts_prod <- mad_thresh * mad(groundnuts_results$`production (kg)`, na.rm = TRUE)
mad_groundnuts_fert <- mad_thresh * mad(groundnuts_results$`Nfert (kg/ha)`, na.rm = TRUE)
mad_groundnuts_fertuse <- mad_thresh * mad(groundnuts_results$`Nuse (kg)`, na.rm = TRUE)


# compare variables to MAD to determine if outlier
mad_outliers_groundnuts_land <- groundnuts_results %>%
  group_by(run_ID) %>%
  filter(`land area (ha)` > mad_groundnuts_land)
mad_outliers_groundnuts_yield <- groundnuts_results %>%
  group_by(run_ID) %>%
  filter(`yield (kg/ha)` > mad_groundnuts_yield)
mad_outliers_groundnuts_prod <- groundnuts_results %>%
  group_by(run_ID) %>%
  filter(`production (kg)` > mad_groundnuts_prod)
### identifies 6655 obs out of 6655 obs as outliers, skip flagging or else all runs will be invalid
mad_outliers_groundnuts_fert <- groundnuts_results %>%
  group_by(run_ID) %>%
  filter(`Nfert (kg/ha)` > mad_groundnuts_fert)
mad_outliers_groundnuts_fertuse <- groundnuts_results %>%
  group_by(run_ID) %>%
  filter(`Nuse (kg)` > mad_groundnuts_fertuse)


# flag outliers in original dataframe and add reason "MAD outlier" -> only flag yield and fert since those are the variables we are comparing in clusters
# data$flag[which(data$run_ID %in% mad_outliers_groundnuts_land$run_ID)] <- "invalid" 
# data$reason[which(data$run_ID %in% mad_outliers_groundnuts_land$run_ID)] <- paste(data$reason, "MAD land outlier", sep = ";")

data$flag[which(data$run_ID %in% mad_outliers_groundnuts_yield$run_ID)] <- "invalid" 
data$reason[which(data$run_ID %in% mad_outliers_groundnuts_yield$run_ID)] <- paste(data$reason, "MAD yield outlier", sep = ";")

# skip otherwise all runs will be flagged as invalid with MAD production outlier
# data$flag[which(data$run_ID %in% mad_outliers_groundnuts_prod$run_ID)] <- "invalid" 
# data$reason[which(data$run_ID %in% mad_outliers_groundnuts_prod$run_ID)] <- paste(data$reason, "MAD production outlier", sep = ";")

data$flag[which(data$run_ID %in% mad_outliers_groundnuts_fert$run_ID)] <- "invalid" 
data$reason[which(data$run_ID %in% mad_outliers_groundnuts_fert$run_ID)] <- paste(data$reason, "MAD fert outlier", sep = ";")

# data$flag[which(data$run_ID %in% mad_outliers_groundnuts_fertuse$run_ID)] <- "invalid" 
# data$reason[which(data$run_ID %in% mad_outliers_groundnuts_fertuse$run_ID)] <- paste(data$reason, "MAD fert use outlier", sep = ";")


### MAIZE ###
### calculate median absolute deviation by output variable using maize data subset
mad_maize_land <- mad_thresh * mad(maize_results$`land area (ha)`, na.rm = TRUE)
mad_maize_yield <- mad_thresh * mad(maize_results$`yield (kg/ha)`, na.rm = TRUE)
mad_maize_prod <- mad_thresh * mad(maize_results$`production (kg)`, na.rm = TRUE)
mad_maize_fert <- mad_thresh * mad(maize_results$`Nfert (kg/ha)`, na.rm = TRUE)
mad_maize_fertuse <- mad_thresh * mad(maize_results$`Nuse (kg)`, na.rm = TRUE)


# compare variables to MAD to determine if outlier
mad_outliers_maize_land <- maize_results %>%
  group_by(run_ID) %>%
  filter(`land area (ha)` > mad_maize_land)
mad_outliers_maize_yield <- maize_results %>%
  group_by(run_ID) %>%
  filter(`yield (kg/ha)` > mad_maize_yield)
mad_outliers_maize_prod <- maize_results %>%
  group_by(run_ID) %>%
  filter(`production (kg)` > mad_maize_prod)
### identifies 6655 obs out of 6655 obs as outliers, skip flagging or else all runs will be invalid
mad_outliers_maize_fert <- maize_results %>%
  group_by(run_ID) %>%
  filter(`Nfert (kg/ha)` > mad_maize_fert)
mad_outliers_maize_fertuse <- maize_results %>%
  group_by(run_ID) %>%
  filter(`Nuse (kg)` > mad_maize_fertuse)


# flag outliers in original dataframe and add reason "MAD outlier" -> only flag yield and fert since those are the variables we are comparing in clusters
# data$flag[which(data$run_ID %in% mad_outliers_maize_land$run_ID)] <- "invalid" 
# data$reason[which(data$run_ID %in% mad_outliers_maize_land$run_ID)] <- paste(data$reason, "MAD land outlier", sep = ";")

data$flag[which(data$run_ID %in% mad_outliers_maize_yield$run_ID)] <- "invalid" 
data$reason[which(data$run_ID %in% mad_outliers_maize_yield$run_ID)] <- paste(data$reason, "MAD yield outlier", sep = ";")

# skip otherwise all runs will be flagged as invalid with MAD production outlier
# data$flag[which(data$run_ID %in% mad_outliers_maize_prod$run_ID)] <- "invalid" 
# data$reason[which(data$run_ID %in% mad_outliers_maize_prod$run_ID)] <- paste(data$reason, "MAD production outlier", sep = ";")

data$flag[which(data$run_ID %in% mad_outliers_maize_fert$run_ID)] <- "invalid" 
data$reason[which(data$run_ID %in% mad_outliers_maize_fert$run_ID)] <- paste(data$reason, "MAD fert outlier", sep = ";")

# data$flag[which(data$run_ID %in% mad_outliers_maize_fertuse$run_ID)] <- "invalid" 
# data$reason[which(data$run_ID %in% mad_outliers_maize_fertuse$run_ID)] <- paste(data$reason, "MAD fert use outlier", sep = ";")


### SESAME ###
### calculate median absolute deviation by output variable using sesame data subset
mad_sesame_land <- mad_thresh * mad(sesame_results$`land area (ha)`, na.rm = TRUE)
mad_sesame_yield <- mad_thresh * mad(sesame_results$`yield (kg/ha)`, na.rm = TRUE)
mad_sesame_prod <- mad_thresh * mad(sesame_results$`production (kg)`, na.rm = TRUE)
mad_sesame_fert <- mad_thresh * mad(sesame_results$`Nfert (kg/ha)`, na.rm = TRUE)
mad_sesame_fertuse <- mad_thresh * mad(sesame_results$`Nuse (kg)`, na.rm = TRUE)


# compare variables to MAD to determine if outlier
mad_outliers_sesame_land <- sesame_results %>%
  group_by(run_ID) %>%
  filter(`land area (ha)` > mad_sesame_land)
mad_outliers_sesame_yield <- sesame_results %>%
  group_by(run_ID) %>%
  filter(`yield (kg/ha)` > mad_sesame_yield)
mad_outliers_sesame_prod <- sesame_results %>%
  group_by(run_ID) %>%
  filter(`production (kg)` > mad_sesame_prod)
### identifies 6655 obs out of 6655 obs as outliers, skip flagging or else all runs will be invalid
mad_outliers_sesame_fert <- sesame_results %>%
  group_by(run_ID) %>%
  filter(`Nfert (kg/ha)` > mad_sesame_fert)
mad_outliers_sesame_fertuse <- sesame_results %>%
  group_by(run_ID) %>%
  filter(`Nuse (kg)` > mad_sesame_fertuse)


# flag outliers in original dataframe and add reason "MAD outlier" -> only flag yield and fert since those are the variables we are comparing in clusters
# data$flag[which(data$run_ID %in% mad_outliers_sesame_land$run_ID)] <- "invalid" 
# data$reason[which(data$run_ID %in% mad_outliers_sesame_land$run_ID)] <- paste(data$reason, "MAD land outlier", sep = ";")

data$flag[which(data$run_ID %in% mad_outliers_sesame_yield$run_ID)] <- "invalid" 
data$reason[which(data$run_ID %in% mad_outliers_sesame_yield$run_ID)] <- paste(data$reason, "MAD yield outlier", sep = ";")

# skip otherwise all runs will be flagged as invalid with MAD production outlier
# data$flag[which(data$run_ID %in% mad_outliers_sesame_prod$run_ID)] <- "invalid" 
# data$reason[which(data$run_ID %in% mad_outliers_sesame_prod$run_ID)] <- paste(data$reason, "MAD production outlier", sep = ";")

data$flag[which(data$run_ID %in% mad_outliers_sesame_fert$run_ID)] <- "invalid" 
data$reason[which(data$run_ID %in% mad_outliers_sesame_fert$run_ID)] <- paste(data$reason, "MAD fert outlier", sep = ";")

# data$flag[which(data$run_ID %in% mad_outliers_sesame_fertuse$run_ID)] <- "invalid" 
# data$reason[which(data$run_ID %in% mad_outliers_sesame_fertuse$run_ID)] <- paste(data$reason, "MAD fert use outlier", sep = ";")


### SORGHUM ###
### calculate median absolute deviation by output variable using sorghum data subset
mad_sorghum_land <- mad_thresh * mad(sorghum_results$`land area (ha)`, na.rm = TRUE)
mad_sorghum_yield <- mad_thresh * mad(sorghum_results$`yield (kg/ha)`, na.rm = TRUE)
mad_sorghum_prod <- mad_thresh * mad(sorghum_results$`production (kg)`, na.rm = TRUE)
mad_sorghum_fert <- mad_thresh * mad(sorghum_results$`Nfert (kg/ha)`, na.rm = TRUE)
mad_sorghum_fertuse <- mad_thresh * mad(sorghum_results$`Nuse (kg)`, na.rm = TRUE)

# compare variables to MAD to determine if outlier
mad_outliers_sorghum_land <- sorghum_results %>%
  group_by(run_ID) %>%
  filter(`land area (ha)` > mad_sorghum_land)
mad_outliers_sorghum_yield <- sorghum_results %>%
  group_by(run_ID) %>%
  filter(`yield (kg/ha)` > mad_sorghum_yield)
mad_outliers_sorghum_prod <- sorghum_results %>%
  group_by(run_ID) %>%
  filter(`production (kg)` > mad_sorghum_prod)
### identifies 6655 obs out of 6655 obs as outliers, skip flagging or else all runs will be invalid
mad_outliers_sorghum_fert <- sorghum_results %>%
  group_by(run_ID) %>%
  filter(`Nfert (kg/ha)` > mad_sorghum_fert)
mad_outliers_sorghum_fertuse <- sorghum_results %>%
  group_by(run_ID) %>%
  filter(`Nuse (kg)` > mad_sorghum_fertuse)


# flag outliers in original dataframe and add reason "MAD outlier" -> only flag yield and fert since those are the variables we are comparing in clusters
# data$flag[which(data$run_ID %in% mad_outliers_sorghum_land$run_ID)] <- "invalid" 
# data$reason[which(data$run_ID %in% mad_outliers_sorghum_land$run_ID)] <- paste(data$reason, "MAD land outlier", sep = ";")

data$flag[which(data$run_ID %in% mad_outliers_sorghum_yield$run_ID)] <- "invalid" 
data$reason[which(data$run_ID %in% mad_outliers_sorghum_yield$run_ID)] <- paste(data$reason, "MAD yield outlier", sep = ";")

# skip otherwise all runs will be flagged as invalid with MAD production outlier
# data$flag[which(data$run_ID %in% mad_outliers_sorghum_prod$run_ID)] <- "invalid" 
# data$reason[which(data$run_ID %in% mad_outliers_sorghum_prod$run_ID)] <- paste(data$reason, "MAD production outlier", sep = ";")

data$flag[which(data$run_ID %in% mad_outliers_sorghum_fert$run_ID)] <- "invalid" 
data$reason[which(data$run_ID %in% mad_outliers_sorghum_fert$run_ID)] <- paste(data$reason, "MAD fert outlier", sep = ";")

# data$flag[which(data$run_ID %in% mad_outliers_sorghum_fertuse$run_ID)] <- "invalid" 
# data$reason[which(data$run_ID %in% mad_outliers_sorghum_fertuse$run_ID)] <- paste(data$reason, "MAD fert use outlier", sep = ";")

hist(sorghum_subruns$diff_yield)
# % distribution under a certain amount, want to retain about 80% of data
# mad x6 and DBSCAN give about the same results

### calculate median absolute deviation (MAD) by output variable for all crops
### ALL CROPS ###
### calculate median absolute deviation by output variable using land area variable
mad_all_land <- mad_thresh * mad(data$`land area (ha)`, na.rm = TRUE)
mad_all_yield <- mad_thresh * mad(data$`yield (kg/ha)`, na.rm = TRUE)
mad_all_prod <- mad_thresh * mad(data$`production (kg)`, na.rm = TRUE)
mad_all_fert <- mad_thresh * mad(data$`Nfert (kg/ha)`, na.rm = TRUE)
mad_all_fertuse <- mad_thresh * mad(data$`Nuse (kg)`, na.rm = TRUE)

# compare variables to MAD to determine if outlier
mad_outliers_all_land <- data %>%
  group_by(run_ID) %>%
  filter(`land area (ha)` > mad_all_land)
mad_outliers_all_yield <- data %>%
  group_by(run_ID) %>%
  filter(`yield (kg/ha)` > mad_all_yield)
mad_outliers_all_prod <- data %>%
  group_by(run_ID) %>%
  filter(`production (kg)` > mad_all_prod)
### identifies 6494 obs out of 6655 obs as outliers, skip flagging or else all runs will be invalid
mad_outliers_all_fert <- data %>%
  group_by(run_ID) %>%
  filter(`Nfert (kg/ha)` > mad_all_fert)
mad_outliers_all_fertuse <- data %>%
  group_by(run_ID) %>%
  filter(`Nuse (kg)` > mad_all_fertuse)

# flag outliers in original dataframe and add reason "MAD outlier" -> only flag yield and fert since those are the variables we are comparing in clusters
# data$flag[which(data$run_ID %in% mad_outliers_all_land$run_ID)] <- "invalid" 
# data$reason[which(data$run_ID %in% mad_outliers_all_land$run_ID)] <- paste(data$reason, "MAD land outlier", sep = ";")

data$flag[which(data$run_ID %in% mad_outliers_all_yield$run_ID)] <- "invalid" 
data$reason[which(data$run_ID %in% mad_outliers_all_yield$run_ID)] <- paste(data$reason, "MAD yield outlier", sep = ";")

# skip otherwise all runs will be flagged as invalid with MAD production outlier
# data$flag[which(data$run_ID %in% mad_outliers_all_prod$run_ID)] <- "invalid" 
# data$reason[which(data$run_ID %in% mad_outliers_all_prod$run_ID)] <- paste(data$reason, "MAD production outlier", sep = ";")

data$flag[which(data$run_ID %in% mad_outliers_all_fert$run_ID)] <- "invalid" 
data$reason[which(data$run_ID %in% mad_outliers_all_fert$run_ID)] <- paste(data$reason, "MAD fert outlier", sep = ";")

# data$flag[which(data$run_ID %in% mad_outliers_all_fertuse$run_ID)] <- "invalid" 
# data$reason[which(data$run_ID %in% mad_outliers_all_fertuse$run_ID)] <- paste(data$reason, "MAD fert use outlier", sep = ";")

########################## END MAD OUTLIER DETECTION #########################

### Remove outliers from data
data_clean_allcrop <- subset(data, flag=="valid")
data_clean <- data_clean_allcrop

####################### BEGIN CLUSTERING ANALYSIS ##################################
### CASSAVA ###
cassava_results_select <- subset(data_clean, crop=="cassava")[ , c("run_ID", "p", "c1", "c2", "land area (ha)", "yield (kg/ha)", "production (kg)", "Nfert (kg/ha)", "Nuse (kg)")] 

# find baseline runs
cassava_0 <- subset(data, crop=='cassava') %>%
  filter(p == 0, c1 == 0, c2 == 0)
cassava_0_land <- mean(cassava_0$`land area (ha)`)
cassava_0_yield <- mean(cassava_0$`yield (kg/ha)`)
cassava_0_prod <- mean(cassava_0$`production (kg)`)
cassava_0_fert <- mean(cassava_0$`Nfert (kg/ha)`)
cassava_0_fertuse <- mean(cassava_0$`Nuse (kg)`)

# isolate runs given subsidy
cassava_subruns <- anti_join(cassava_results_select, cassava_0)

# calculate difference between subsidy runs and baseline runs for each result variable
cassava_subruns$diff_land <- cassava_subruns$`land area (ha)`- cassava_0_land
cassava_subruns$diff_yield <- cassava_subruns$`yield (kg/ha)`- cassava_0_yield
cassava_subruns$diff_prod <- cassava_subruns$`production (kg)`- cassava_0_prod
cassava_subruns$diff_fert <- cassava_subruns$`Nfert (kg/ha)`- cassava_0_fert
cassava_subruns$diff_fertuse <- cassava_subruns$`Nuse (kg)`- cassava_0_fertuse

# select variables for clustering
# reference to convert "run_ID" column to rownames so that it will be ignored during clustering while preserving results that can then be returned to original dataframe:
# https://rdrr.io/cran/textshape/man/column_to_rownames.html
cassava_subruns_select <- cassava_subruns[ , c("run_ID", "diff_yield", "diff_fert")] %>% 
  remove_rownames %>% 
  column_to_rownames(var = "run_ID") %>% 
  as.data.frame()

### load dataframe for clustering
df <- cassava_subruns_select
df <- scale(df)
head(df)

### determine optimal number clusters using gap method
set.seed(123)
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25, K.max = 10, B = 50)

# print the result
# print(gap_stat, method = "firstmax")
# reference to extract output from print text: 
# http://r.789695.n4.nabble.com/extract-printed-value-from-a-function-td3322451.html
gs_print <- capture.output(gap_stat)
gs_outputstr <- gs_print[4]
optclusters <- as.numeric(strsplit(gs_outputstr, ": ")[[1]][2]) 


### run k-means clustering
cassava_kcluster <- kmeans(df, centers = optclusters, nstart = 25)
# str(cassava_kcluster)
fviz_cluster(cassava_kcluster, data = df, geom = "point")
cassava_kcluster_df <- cassava_kcluster[1] %>%
  as.data.frame() %>% 
  rownames_to_column (var = "run_ID")


### PAM clustering
# require(cluster)
# pam_cluster <- pam(df, optclusters)
# # Visualize pam clustering
# fviz_cluster(pam_cluster, geom = "point", ellipse.type = "norm")



### GROUNDNUTS ###
groundnuts_results_select <- subset(data_clean, crop=="groundnuts")[ , c("run_ID", "p", "c1", "c2", "land area (ha)", "yield (kg/ha)", "production (kg)", "Nfert (kg/ha)", "Nuse (kg)")] 

# find baseline runs
groundnuts_0 <- subset(data, crop=='groundnuts') %>%
  filter(p == 0, c1 == 0, c2 == 0)
groundnuts_0_land <- mean(groundnuts_0$`land area (ha)`)
groundnuts_0_yield <- mean(groundnuts_0$`yield (kg/ha)`)
groundnuts_0_prod <- mean(groundnuts_0$`production (kg)`)
groundnuts_0_fert <- mean(groundnuts_0$`Nfert (kg/ha)`)
groundnuts_0_fertuse <- mean(groundnuts_0$`Nuse (kg)`)

# isolate runs given subsidy
groundnuts_subruns <- anti_join(groundnuts_results_select, groundnuts_0)

# calculate difference between subsidy runs and baseline runs for each result variable
groundnuts_subruns$diff_land <- groundnuts_subruns$`land area (ha)`- groundnuts_0_land
groundnuts_subruns$diff_yield <- groundnuts_subruns$`yield (kg/ha)`- groundnuts_0_yield
groundnuts_subruns$diff_prod <- groundnuts_subruns$`production (kg)`- groundnuts_0_prod
groundnuts_subruns$diff_fert <- groundnuts_subruns$`Nfert (kg/ha)`- groundnuts_0_fert
groundnuts_subruns$diff_fertuse <- groundnuts_subruns$`Nuse (kg)`- groundnuts_0_fertuse

# select variables for clustering
# reference to convert "run_ID" column to rownames so that it will be ignored during clustering while preserving results that can then be returned to original dataframe:
# https://rdrr.io/cran/textshape/man/column_to_rownames.html
groundnuts_subruns_select <- groundnuts_subruns[ , c("run_ID", "diff_yield", "diff_fert")] %>% 
  remove_rownames %>% 
  column_to_rownames(var = "run_ID") %>% 
  as.data.frame()

### load dataframe for clustering
df <- groundnuts_subruns_select
df <- scale(df)
head(df)

### determine optimal number clusters using gap method
set.seed(123)
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25, K.max = 10, B = 50)

# print the result
# print(gap_stat, method = "firstmax")
# reference to extract output from print text: 
# http://r.789695.n4.nabble.com/extract-printed-value-from-a-function-td3322451.html
gs_print <- capture.output(gap_stat)
gs_outputstr <- gs_print[4]
optclusters <- as.numeric(strsplit(gs_outputstr, ": ")[[1]][2]) 


### run k-means clustering
groundnuts_kcluster <- kmeans(df, centers = optclusters, nstart = 25)
# str(groundnuts_kcluster)
fviz_cluster(groundnuts_kcluster, data = df, geom = "point")
groundnuts_kcluster_df <- groundnuts_kcluster[1] %>%
  as.data.frame() %>% 
  rownames_to_column (var = "run_ID")


### PAM clustering
# require(cluster)
# pam_cluster <- pam(df, optclusters)
# # Visualize pam clustering
# fviz_cluster(pam_cluster, geom = "point", ellipse.type = "norm")



### MAIZE ###
maize_results_select <- subset(data_clean, crop=="maize")[ , c("run_ID", "p", "c1", "c2", "land area (ha)", "yield (kg/ha)", "production (kg)", "Nfert (kg/ha)", "Nuse (kg)")] 

# find baseline runs
maize_0 <- subset(data, crop=='maize') %>%
  filter(p == 0, c1 == 0, c2 == 0)
maize_0_land <- mean(maize_0$`land area (ha)`)
maize_0_yield <- mean(maize_0$`yield (kg/ha)`)
maize_0_prod <- mean(maize_0$`production (kg)`)
maize_0_fert <- mean(maize_0$`Nfert (kg/ha)`)
maize_0_fertuse <- mean(maize_0$`Nuse (kg)`)

# isolate runs given subsidy
maize_subruns <- anti_join(maize_results_select, maize_0)

# calculate difference between subsidy runs and baseline runs for each result variable
maize_subruns$diff_land <- maize_subruns$`land area (ha)`- maize_0_land
maize_subruns$diff_yield <- maize_subruns$`yield (kg/ha)`- maize_0_yield
maize_subruns$diff_prod <- maize_subruns$`production (kg)`- maize_0_prod
maize_subruns$diff_fert <- maize_subruns$`Nfert (kg/ha)`- maize_0_fert
maize_subruns$diff_fertuse <- maize_subruns$`Nuse (kg)`- maize_0_fertuse


# select variables for clustering
# reference to convert "run_ID" column to rownames so that it will be ignored during clustering while preserving results that can then be returned to original dataframe:
# https://rdrr.io/cran/textshape/man/column_to_rownames.html
maize_subruns_select <- maize_subruns[ , c("run_ID", "diff_yield", "diff_fert")] %>% 
  remove_rownames %>% 
  column_to_rownames(var = "run_ID") %>% 
  as.data.frame()

### load dataframe for clustering
df <- maize_subruns_select
df <- scale(df)
head(df)

### determine optimal number clusters using gap method
set.seed(123)
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25, K.max = 10, B = 50)

# print the result
# print(gap_stat, method = "firstmax")
# reference to extract output from print text: 
# http://r.789695.n4.nabble.com/extract-printed-value-from-a-function-td3322451.html
gs_print <- capture.output(gap_stat)
gs_outputstr <- gs_print[4]
optclusters <- as.numeric(strsplit(gs_outputstr, ": ")[[1]][2]) 


### run k-means clustering
maize_kcluster <- kmeans(df, centers = optclusters, nstart = 25)
# str(maize_kcluster)
fviz_cluster(maize_kcluster, data = df, geom = "point")
maize_kcluster_df <- maize_kcluster[1] %>%
  as.data.frame() %>% 
  rownames_to_column (var = "run_ID")


### PAM clustering
# require(cluster)
# pam_cluster <- pam(df, optclusters)
# # Visualize pam clustering
# fviz_cluster(pam_cluster, geom = "point", ellipse.type = "norm")



### SESAME ###
sesame_results_select <- subset(data_clean, crop=="sesame")[ , c("run_ID", "p", "c1", "c2", "land area (ha)", "yield (kg/ha)", "production (kg)", "Nfert (kg/ha)", "Nuse (kg)")] 

# find baseline runs
sesame_0 <- subset(data, crop=='sesame') %>%
  filter(p == 0, c1 == 0, c2 == 0)
sesame_0_land <- mean(sesame_0$`land area (ha)`)
sesame_0_yield <- mean(sesame_0$`yield (kg/ha)`)
sesame_0_prod <- mean(sesame_0$`production (kg)`)
sesame_0_fert <- mean(sesame_0$`Nfert (kg/ha)`)
sesame_0_fertuse <- mean(sesame_0$`Nuse (kg)`)

# isolate runs given subsidy
sesame_subruns <- anti_join(sesame_results_select, sesame_0)

# calculate difference between subsidy runs and baseline runs for each result variable
sesame_subruns$diff_land <- sesame_subruns$`land area (ha)`- sesame_0_land
sesame_subruns$diff_yield <- sesame_subruns$`yield (kg/ha)`- sesame_0_yield
sesame_subruns$diff_prod <- sesame_subruns$`production (kg)`- sesame_0_prod
sesame_subruns$diff_fert <- sesame_subruns$`Nfert (kg/ha)`- sesame_0_fert
sesame_subruns$diff_fertuse <- sesame_subruns$`Nuse (kg)`- sesame_0_fertuse

# Select variables for clustering
# Reference to convert "run_ID" column to rownames so that it will be ignored during clustering while preserving results that can then be returned to original dataframe:
# https://rdrr.io/cran/textshape/man/column_to_rownames.html
sesame_subruns_select <- sesame_subruns[ , c("run_ID", "diff_yield", "diff_fert")] %>% 
  remove_rownames %>% 
  column_to_rownames(var = "run_ID") %>% 
  as.data.frame()


### load df for clustering
df <- sesame_subruns_select
df <- scale(df)
head(df)

### determine optimal number clusters using gap method
set.seed(123)
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25, K.max = 10, B = 50)

# print the result
# print(gap_stat, method = "firstmax")
# reference to extract output from print text: 
# http://r.789695.n4.nabble.com/extract-printed-value-from-a-function-td3322451.html
gs_print <- capture.output(gap_stat)
gs_outputstr <- gs_print[4]
optclusters <- as.numeric(strsplit(gs_outputstr, ": ")[[1]][2]) 


### run k-means clustering
sesame_kcluster <- kmeans(df, centers = optclusters, nstart = 25)
# str(sesame_kcluster)
fviz_cluster(sesame_kcluster, data = df, geom = "point")
sesame_kcluster_df <- sesame_kcluster[1] %>%
  as.data.frame() %>% 
  rownames_to_column (var = "run_ID")


# ### PAM clustering
# require(cluster)
# pam_cluster <- pam(df, optclusters)
# # Visualize pam clustering
# fviz_cluster(pam_cluster, geom = "point", ellipse.type = "norm")



### SORGHUM ###
sorghum_results_select <- subset(data_clean, crop=="sorghum")[ , c("run_ID", "p", "c1", "c2", "land area (ha)", "yield (kg/ha)", "production (kg)", "Nfert (kg/ha)", "Nuse (kg)")] 

# find baseline runs
sorghum_0 <- subset(data, crop=='sorghum') %>%
  filter(p == 0, c1 == 0, c2 == 0)
sorghum_0_land <- mean(sorghum_0$`land area (ha)`)
sorghum_0_yield <- mean(sorghum_0$`yield (kg/ha)`)
sorghum_0_prod <- mean(sorghum_0$`production (kg)`)
sorghum_0_fert <- mean(sorghum_0$`Nfert (kg/ha)`)
sorghum_0_fertuse <- mean(sorghum_0$`Nuse (kg)`)

# isolate runs given subsidy
sorghum_subruns <- anti_join(sorghum_results_select, sorghum_0)

# calculate difference between subsidy runs and baseline runs for each result variable
sorghum_subruns$diff_land <- sorghum_subruns$`land area (ha)`- sorghum_0_land
sorghum_subruns$diff_yield <- sorghum_subruns$`yield (kg/ha)`- sorghum_0_yield
sorghum_subruns$diff_prod <- sorghum_subruns$`production (kg)`- sorghum_0_prod
sorghum_subruns$diff_fert <- sorghum_subruns$`Nfert (kg/ha)`- sorghum_0_fert
sorghum_subruns$diff_fertuse <- sorghum_subruns$`Nuse (kg)`- sorghum_0_fertuse

# select variables for clustering
# reference to convert "run_ID" column to rownames so that it will be ignored during clustering while preserving results that can then be returned to original dataframe:
# https://rdrr.io/cran/textshape/man/column_to_rownames.html
sorghum_subruns_select <- sorghum_subruns[ , c("run_ID", "diff_yield", "diff_fert")] %>% 
  remove_rownames %>% 
  column_to_rownames(var = "run_ID") %>% 
  as.data.frame()


### load dataframe for clustering
df <- sorghum_subruns_select
df <- scale(df)
head(df)

### determine optimal number clusters using gap method
set.seed(123)
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25, K.max = 10, B = 50)

# print the result
# print(gap_stat, method = "firstmax")
# reference to extract output from print text: 
# http://r.789695.n4.nabble.com/extract-printed-value-from-a-function-td3322451.html
gs_print <- capture.output(gap_stat)
gs_outputstr <- gs_print[4]
optclusters <- as.numeric(strsplit(gs_outputstr, ": ")[[1]][2]) 


### run k-means clustering
sorghum_kcluster <- kmeans(df, centers = optclusters, nstart = 25)
# str(sorghum_kcluster)
fviz_cluster(sorghum_kcluster, data = df, geom = "point")
sorghum_kcluster_df <- sorghum_kcluster[1] %>%
  as.data.frame() %>% 
  rownames_to_column (var = "run_ID")


### PAM clustering
# require(cluster)
# pam_cluster <- pam(df, optclusters)
# # Visualize pam clustering
# fviz_cluster(pam_cluster, geom = "point", ellipse.type = "norm")

################################END CLUSTERING###########################

### Output original data frame with anomaly information appended to outputs directory
#View(data)
setwd(oname)
write.csv(data, file = "results_summary_bycrop_withfeelingsA.csv")
