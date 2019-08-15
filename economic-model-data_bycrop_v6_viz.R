library(readr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(dbscan)


setwd("~/MINT/Economic/Anomaly-Detection")
wname <- getwd() # set back to working directory
dname <- paste(wname,"economicdata",sep="/") # will open data file in workikng directory
oname <- paste(wname,"outputs",sep="/") # will open outputs file in workikng directory
pname<-paste(wname,"plots",sep="/") # will open plots file in workikng directory
fname<-paste(wname,"functions",sep="/") # will open functions file in workikng directory


############################# IMPORT & PREPARE DATA #############################
### read in the data 
setwd(dname) #tells the program to look in the data folder

calib_data <- read_csv("economic-v6/calibdata_v6.csv")
data <- read_csv("results_summary_bycrop_update.csv")

# return to working directory
setwd(wname)

### add coloumns for anomaly flags and reason flag was generated
data$flag <- "valid"
data$reason <- NA
data <- data %>% group_by(run_ID)



####################### FLAG INVALID RUNS ####################
### check for results that defy the laws of physics by exceeding total available land to use ###
### identify total land
total_land <- sum(calib_data$xbar1)

### group by run ID and find sum of land area from each crop for that run
landcheck <- data %>%
  group_by(run_ID) %>%
  summarize(landsum = sum(`land area (ha)`)) 

### find runs that over use total available land, flag as invalid, and add reason for flag
overuse <- landcheck %>%
  filter (landsum > (total_land + 1))

data$flag[which(data$run_ID %in% overuse$run_ID)] <- "invalid" 
data$reason[which(data$run_ID %in% overuse$run_ID)] <- paste(data$reason, "land overuse", sep = ";")

### find runs that under use total available land -> not anomalous b/c doesn't defy physics but could be interesting to analyze further
underuse <- landcheck %>%
  filter (landsum < (total_land - 1))

data$reason[which(data$run_ID %in% underuse$run_ID)] <- paste(data$reason, "land underuse", sep = ";") 

########################## END INVALID RUNS CHECK #########################


### Remove invalid runs from data
data_valid <- subset(data, flag=="valid")


####################### MAD STATISTICAL OUTLIER REMOVAL #############################
### set MAD threshold, to be multiplied by MAD, values beyond threshold * MAD are flagged as statistical outliers 
mad_thresh <- 9

### calculate median absolute deviation (MAD) for each output variable across all crops
mad_all_land <- mad_thresh * mad(data_valid$`land area (ha)`, na.rm = TRUE)
mad_all_yield <- mad_thresh * mad(data_valid$`yield (kg/ha)`, na.rm = TRUE)
mad_all_prod <- mad_thresh * mad(data_valid$`production (kg)`, na.rm = TRUE)
mad_all_fert <- mad_thresh * mad(data_valid$`Nfert (kg/ha)`, na.rm = TRUE)
mad_all_fertuse <- mad_thresh * mad(data_valid$`Nuse (kg)`, na.rm = TRUE)

### compare variables to MAD to determine if outlier
mad_outliers_all_land <- data_valid %>%
  group_by(run_ID) %>%
  filter(`land area (ha)` > mad_all_land)
mad_outliers_all_yield <- data_valid %>%
  group_by(run_ID) %>%
  filter(`yield (kg/ha)` > mad_all_yield)
mad_outliers_all_prod <- data_valid %>%
  group_by(run_ID) %>%
  filter(`production (kg)` > mad_all_prod)
### identifies 6494 obs out of 6655 obs as outliers, skip flagging or else all runs will be invalid
mad_outliers_all_fert <- data_valid %>%
  group_by(run_ID) %>%
  filter(`Nfert (kg/ha)` > mad_all_fert)
mad_outliers_all_fertuse <- data_valid %>%
  group_by(run_ID) %>%
  filter(`Nuse (kg)` > mad_all_fertuse)

### visualize outliers for yield and fert with histogram
# ggplot(data=mad_outliers_all_yield, aes(mad_outliers_all_yield$crop)) + geom_histogram()
# hist(mad_outliers_all_yield$crop)
# 
# ggplot(data=mad_outliers_all_fert, aes(mad_outliers_all_fert$crop)) + geom_histogram()
# hist(mad_outliers_all_fert$crop)

### flag outliers in original dataframe and add reason "MAD outlier" -> only flag yield and fert since those are the variables we are comparing in clusters
# land area
# data$flag[which(data$run_ID %in% mad_outliers_all_land$run_ID)] <- "invalid" 
# data$reason[which(data$run_ID %in% mad_outliers_all_land$run_ID)] <- paste(data$reason, "MAD land outlier", sep = ";")
# yield
data$flag[which(data$run_ID %in% mad_outliers_all_yield$run_ID)] <- "invalid" 
data$reason[which(data$run_ID %in% mad_outliers_all_yield$run_ID)] <- paste(data$reason, "MAD yield outlier", sep = ";")
# production
# skip otherwise all runs will be flagged as invalid with MAD production outlier
# data$flag[which(data$run_ID %in% mad_outliers_all_prod$run_ID)] <- "invalid" 
# data$reason[which(data$run_ID %in% mad_outliers_all_prod$run_ID)] <- paste(data$reason, "MAD production outlier", sep = ";")
# fert application
data$flag[which(data$run_ID %in% mad_outliers_all_fert$run_ID)] <- "invalid" 
data$reason[which(data$run_ID %in% mad_outliers_all_fert$run_ID)] <- paste(data$reason, "MAD fert outlier", sep = ";")
# fert use
# data$flag[which(data$run_ID %in% mad_outliers_all_fertuse$run_ID)] <- "invalid" 
# data$reason[which(data$run_ID %in% mad_outliers_all_fertuse$run_ID)] <- paste(data$reason, "MAD fert use outlier", sep = ";")

########################## END MAD OUTLIER DETECTION #########################


### remove outliers from data
data_clean <- subset(data, flag=="valid")

### prepare data for cluster analysis
### select variables for data analysis
cassava_results <- subset(data_clean, crop=="cassava")[ , c("run_ID", "p", "c1", "c2", "fertilizer subsidy (%)", "land area (ha)", "yield (kg/ha)", "production (kg)", "Nfert (kg/ha)", "Nuse (kg)")] 
groundnuts_results <- subset(data_clean, crop=="groundnuts")[ , c("run_ID", "p", "c1", "c2", "fertilizer subsidy (%)", "land area (ha)", "yield (kg/ha)", "production (kg)", "Nfert (kg/ha)", "Nuse (kg)")] 
maize_results <- subset(data_clean, crop=="maize")[ , c("run_ID", "p", "c1", "c2", "fertilizer subsidy (%)", "land area (ha)", "yield (kg/ha)", "production (kg)", "Nfert (kg/ha)", "Nuse (kg)")] 
sesame_results <- subset(data_clean, crop=="sesame")[ , c("run_ID", "p", "c1", "c2", "fertilizer subsidy (%)", "land area (ha)", "yield (kg/ha)", "production (kg)", "Nfert (kg/ha)", "Nuse (kg)")] 
sorghum_results <- subset(data_clean, crop=="sorghum")[ , c("run_ID", "p", "c1", "c2", "fertilizer subsidy (%)", "land area (ha)", "yield (kg/ha)", "production (kg)", "Nfert (kg/ha)", "Nuse (kg)")] 

### find baseline runs
# cassava
cassava_0 <- subset(data, crop=='cassava') %>%
  filter(p == 0, c1 == 0, c2 == 0)
cassava_0_land <- mean(cassava_0$`land area (ha)`)
cassava_0_yield <- mean(cassava_0$`yield (kg/ha)`)
cassava_0_prod <- mean(cassava_0$`production (kg)`)
cassava_0_fert <- mean(cassava_0$`Nfert (kg/ha)`)
cassava_0_fertuse <- mean(cassava_0$`Nuse (kg)`)
# groundnuts
groundnuts_0 <- subset(data, crop=='groundnuts') %>%
  filter(p == 0, c1 == 0, c2 == 0)
groundnuts_0_land <- mean(groundnuts_0$`land area (ha)`)
groundnuts_0_yield <- mean(groundnuts_0$`yield (kg/ha)`)
groundnuts_0_prod <- mean(groundnuts_0$`production (kg)`)
groundnuts_0_fert <- mean(groundnuts_0$`Nfert (kg/ha)`)
groundnuts_0_fertuse <- mean(groundnuts_0$`Nuse (kg)`)
# maize
maize_0 <- subset(data, crop=='maize') %>%
  filter(p == 0, c1 == 0, c2 == 0)
maize_0_land <- mean(maize_0$`land area (ha)`)
maize_0_yield <- mean(maize_0$`yield (kg/ha)`)
maize_0_prod <- mean(maize_0$`production (kg)`)
maize_0_fert <- mean(maize_0$`Nfert (kg/ha)`)
maize_0_fertuse <- mean(maize_0$`Nuse (kg)`)
# sesame
sesame_0 <- subset(data, crop=='sesame') %>%
  filter(p == 0, c1 == 0, c2 == 0)
sesame_0_land <- mean(sesame_0$`land area (ha)`)
sesame_0_yield <- mean(sesame_0$`yield (kg/ha)`)
sesame_0_prod <- mean(sesame_0$`production (kg)`)
sesame_0_fert <- mean(sesame_0$`Nfert (kg/ha)`)
sesame_0_fertuse <- mean(sesame_0$`Nuse (kg)`)
# sorghum
sorghum_0 <- subset(data, crop=='sorghum') %>%
  filter(p == 0, c1 == 0, c2 == 0)
sorghum_0_land <- mean(sorghum_0$`land area (ha)`)
sorghum_0_yield <- mean(sorghum_0$`yield (kg/ha)`)
sorghum_0_prod <- mean(sorghum_0$`production (kg)`)
sorghum_0_fert <- mean(sorghum_0$`Nfert (kg/ha)`)
sorghum_0_fertuse <- mean(sorghum_0$`Nuse (kg)`)

### isolate runs given subsidy
cassava_subruns <- anti_join(cassava_results, cassava_0)
groundnuts_subruns <- anti_join(groundnuts_results, groundnuts_0)
maize_subruns <- anti_join(maize_results, maize_0)
sesame_subruns <- anti_join(sesame_results, sesame_0)
sorghum_subruns <- anti_join(sorghum_results, sorghum_0)

### calculate difference between subsidy runs and baseline runs for each result variable
# cassava
cassava_subruns$diff_land <- cassava_subruns$`land area (ha)`- cassava_0_land
cassava_subruns$diff_yield <- cassava_subruns$`yield (kg/ha)`- cassava_0_yield
cassava_subruns$diff_prod <- cassava_subruns$`production (kg)`- cassava_0_prod
cassava_subruns$diff_fert <- cassava_subruns$`Nfert (kg/ha)`- cassava_0_fert
cassava_subruns$diff_fertuse <- cassava_subruns$`Nuse (kg)`- cassava_0_fertuse
# groundnuts
groundnuts_subruns$diff_land <- groundnuts_subruns$`land area (ha)`- groundnuts_0_land
groundnuts_subruns$diff_yield <- groundnuts_subruns$`yield (kg/ha)`- groundnuts_0_yield
groundnuts_subruns$diff_prod <- groundnuts_subruns$`production (kg)`- groundnuts_0_prod
groundnuts_subruns$diff_fert <- groundnuts_subruns$`Nfert (kg/ha)`- groundnuts_0_fert
groundnuts_subruns$diff_fertuse <- groundnuts_subruns$`Nuse (kg)`- groundnuts_0_fertuse
# maize
maize_subruns$diff_land <- maize_subruns$`land area (ha)`- maize_0_land
maize_subruns$diff_yield <- maize_subruns$`yield (kg/ha)`- maize_0_yield
maize_subruns$diff_prod <- maize_subruns$`production (kg)`- maize_0_prod
maize_subruns$diff_fert <- maize_subruns$`Nfert (kg/ha)`- maize_0_fert
maize_subruns$diff_fertuse <- maize_subruns$`Nuse (kg)`- maize_0_fertuse
# sesame
sesame_subruns$diff_land <- sesame_subruns$`land area (ha)`- sesame_0_land
sesame_subruns$diff_yield <- sesame_subruns$`yield (kg/ha)`- sesame_0_yield
sesame_subruns$diff_prod <- sesame_subruns$`production (kg)`- sesame_0_prod
sesame_subruns$diff_fert <- sesame_subruns$`Nfert (kg/ha)`- sesame_0_fert
sesame_subruns$diff_fertuse <- sesame_subruns$`Nuse (kg)`- sesame_0_fertuse
# sorghum
sorghum_subruns$diff_land <- sorghum_subruns$`land area (ha)`- sorghum_0_land
sorghum_subruns$diff_yield <- sorghum_subruns$`yield (kg/ha)`- sorghum_0_yield
sorghum_subruns$diff_prod <- sorghum_subruns$`production (kg)`- sorghum_0_prod
sorghum_subruns$diff_fert <- sorghum_subruns$`Nfert (kg/ha)`- sorghum_0_fert
sorghum_subruns$diff_fertuse <- sorghum_subruns$`Nuse (kg)`- sorghum_0_fertuse

### recombine crop subsets of runs given fert subsidy to create full data set
join1 <- full_join(cassava_subruns, maize_subruns)
join2 <- full_join(join1, groundnuts_subruns)
join3 <- full_join(join2, sesame_subruns)
data_subruns <- full_join(join3, sorghum_subruns)



####################### BEGIN CLUSTERING ANALYSIS ##################################
### CASSAVA ###
### select variables for clustering
# reference to convert "run_ID" column to rownames so that it will be ignored during clustering while preserving results that can then be returned to original dataframe:
# https://rdrr.io/cran/textshape/man/column_to_rownames.html
cassava_subruns_select <- cassava_subruns[ , c("run_ID", "diff_yield", "diff_fert")] %>% 
  remove_rownames %>% 
  column_to_rownames(var = "run_ID") %>% 
  as.data.frame()

### find suitable eps parameter using a k-NN plot for k = dim + 1
# Look for the knee!
# reference: https://stackoverflow.com/questions/12893492/choosing-eps-and-minpts-for-dbscan-r
# use 2 * dim or ln(dim) for minPts if # of variables > 2, min points = k
kNNdist_cassava <- sort(kNNdist(cassava_subruns_select, k = (length(cassava_subruns_select)) + 1))
kNNdistplot(cassava_subruns_select, k = (length(cassava_subruns_select)) + 1)

### calculate second derivative to find maximum curvature of "knee" of plot to obtain eps value, eps = second_d
### needs work so eps is automated, currently eps is found from looking at kNNdistplot and approximating max curvature, then testing +/- from that value until # of clusters is as few as possible and # in each cluster doesn't change
# mid <- 1:length(kNNdist_cassava) 
# deriv <- function(x, y) diff(y) / diff(x)
# middle_pts <- function(x) x[-1] - diff(x) / 2
# second_d <- deriv(middle_pts(mid), deriv(mid, kNNdist_cassava))
# smooth_second_d <- loess(second_d ~ midpts, data.frame(second_d = second_d, midpts = middle_pts(middle_pts(mid))), model = T)

### run DBSCAN analysis
res_dbscan <- dbscan(cassava_subruns_select, eps = 74, minPts = (length(cassava_subruns_select)) + 1)
res_dbscan

### visualize relationships between variables & clusters
pairs(cassava_subruns_select, col = res_dbscan$cluster + 1L)
## plot clusters and add noise (cluster 0) as crosses.
plot(cassava_subruns_select, col=res_dbscan$cluster)
points(cassava_subruns_select[res_dbscan$cluster==0,], pch = 3, col = "grey")
hullplot(cassava_subruns_select, res_dbscan, main = "Cassava DBSCAN Clusters")

### add dbscan cluster values to cluster dataframe
cassava_dbscan_df <- res_dbscan[1] %>%
  as.data.frame() %>% 
  rownames_to_column (var = "run_ID")

### return clustering results to original cassava dataset 
cassava_subruns_select$cluster <- res_dbscan$cluster
cassava_subruns_select <- cassava_subruns_select %>% 
  as.data.frame() %>% 
  rownames_to_column (var = "run_ID")
cassava_results$cluster <- NA
cassava_results$cluster[which(cassava_results$run_ID %in% cassava_subruns_select$run_ID)] <- cassava_subruns_select[ , 4] # select last column, which is cluster column

### isolate outliers flagged by DBSCAN
cassava_outliers <- subset(cassava_results, cluster==0)

### visualize outliers in relation to p, c1, c2 inputs
ggplot(cassava_outliers, aes(x = c1, y = c2, color = p)) +
  geom_point(size = 5) +
  scale_color_gradientn(colours = rainbow(5)) +
  geom_text_repel(aes(label = p), color = "black", size = 5)


### GROUNDNUTS ###
### select variables for clustering
# reference to convert "run_ID" column to rownames so that it will be ignored during clustering while preserving results that can then be returned to original dataframe:
# https://rdrr.io/cran/textshape/man/column_to_rownames.html
groundnuts_subruns_select <- groundnuts_subruns[ , c("run_ID", "diff_yield", "diff_fert")] %>% 
  remove_rownames %>% 
  column_to_rownames(var = "run_ID") %>% 
  as.data.frame()

### find suitable eps parameter using a k-NN plot for k = dim + 1
# Look for the knee!
# reference: https://stackoverflow.com/questions/12893492/choosing-eps-and-minpts-for-dbscan-r
# use 2 * dim or ln(dim) for min points, min points = k
kNNdist_groundnuts <- sort(kNNdist(groundnuts_subruns_select, k = (length(groundnuts_subruns_select)) + 1))
kNNdistplot(groundnuts_subruns_select, k = (length(groundnuts_subruns_select)) + 1)

### calculate second derivative to find maximum curvature of "knee" of plot to obtain eps value, eps = second_d
# mid <- 1:length(kNNdist_groundnuts) 
# deriv <- function(x, y) diff(y) / diff(x)
# middle_pts <- function(x) x[-1] - diff(x) / 2
# second_d <- deriv(middle_pts(mid), deriv(mid, kNNdist_groundnuts))
# smooth_second_d <- loess(second_d ~ midpts, data.frame(second_d = second_d, midpts = middle_pts(middle_pts(mid))), model = T)

### run DBSCAN analysis
res_dbscan <- dbscan(groundnuts_subruns_select, eps = 57, minPts = (length(groundnuts_subruns_select)) + 1)
res_dbscan

### visualize relationships between variables & clusters
pairs(groundnuts_subruns_select, col = res_dbscan$cluster + 1L)
### plot clusters and add noise (cluster 0) as crosses.
plot(groundnuts_subruns_select, col=res_dbscan$cluster)
points(groundnuts_subruns_select[res_dbscan$cluster==0,], pch = 3, col = "grey")

hullplot(groundnuts_subruns_select, res_dbscan, main = "Groundnuts DBSCAN Clusters")

### add dbscan cluster values to cluster dataframe
groundnuts_dbscan_df <- res_dbscan[1] %>%
  as.data.frame() %>% 
  rownames_to_column (var = "run_ID")

### return clustering results to original groundnuts dataset 
groundnuts_subruns_select$cluster <- res_dbscan$cluster
groundnuts_subruns_select <- groundnuts_subruns_select %>% 
  as.data.frame() %>% 
  rownames_to_column (var = "run_ID")
groundnuts_results$cluster <- NA
groundnuts_results$cluster[which(groundnuts_results$run_ID %in% groundnuts_subruns_select$run_ID)] <- groundnuts_subruns_select[ , 4] 

### isolate outliers flagged by DBSCAN
groundnuts_outliers <- subset(groundnuts_results, cluster==0)

### visualize outliers in relation to p, c1, c2 inputs
ggplot(groundnuts_outliers, aes(x = c1, y = c2, color = p)) +
  geom_point(size = 5) +
  scale_color_gradientn(colours = rainbow(5)) +
  geom_text_repel(aes(label = p), color = "black", size = 5)


### MAIZE ###
# select variables for clustering
# reference to convert "run_ID" column to rownames so that it will be ignored during clustering while preserving results that can then be returned to original dataframe:
# https://rdrr.io/cran/textshape/man/column_to_rownames.html
maize_subruns_select <- maize_subruns[ , c("run_ID", "diff_yield", "diff_fert")] %>% 
  remove_rownames %>% 
  column_to_rownames(var = "run_ID") %>% 
  as.data.frame()

### find suitable eps parameter using a k-NN plot for k = dim + 1
# Look for the knee!
# reference: https://stackoverflow.com/questions/12893492/choosing-eps-and-minpts-for-dbscan-r
# use 2 * dim or ln(dim) for min points, min points = k
kNNdist_maize <- sort(kNNdist(maize_subruns_select, k = (length(maize_subruns_select)) + 1))
kNNdistplot(maize_subruns_select, k = (length(maize_subruns_select)) + 1)

### calculate second derivative to find maximum curvature of "knee" of plot to obtain eps value, eps = second_d
# mid <- 1:length(kNNdist_maize) 
# deriv <- function(x, y) diff(y) / diff(x)
# middle_pts <- function(x) x[-1] - diff(x) / 2
# second_d <- deriv(middle_pts(mid), deriv(mid, kNNdist_maize))
# smooth_second_d <- loess(second_d ~ midpts, data.frame(second_d = second_d, midpts = middle_pts(middle_pts(mid))), model = T)

### run DBSCAN analysis
res_dbscan <- dbscan(maize_subruns_select, eps = 68, minPts = (length(maize_subruns_select)) + 1)
res_dbscan

### visualize relationships between variables & clusters
pairs(maize_subruns_select, col = res_dbscan$cluster + 1L)
### plot clusters and add noise (cluster 0) as crosses.
plot(maize_subruns_select, col=res_dbscan$cluster)
points(mazie_subruns_select[res_dbscan$cluster==0,], pch = 3, col = "grey")

hullplot(maize_subruns_select, res_dbscan, main = "Maize DBSCAN Clusters")

### add dbscan cluster values to cluster dataframe
maize_dbscan_df <- res_dbscan[1] %>%
  as.data.frame() %>% 
  rownames_to_column (var = "run_ID")

### return clustering results to original maize dataset 
maize_subruns_select$cluster <- res_dbscan$cluster
maize_subruns_select <- maize_subruns_select %>% 
  as.data.frame() %>% 
  rownames_to_column (var = "run_ID")
maize_results$cluster <- NA
maize_results$cluster[which(maize_results$run_ID %in% maize_subruns_select$run_ID)] <- maize_subruns_select[ , 4] 

### isolate outliers flagged by DBSCAN
maize_outliers <- subset(maize_results, cluster==0)

### visualize outliers in relation to p, c1, c2 inputs
ggplot(maize_outliers, aes(x = c1, y = c2, color = p)) +
  geom_point(size = 5) +
  scale_color_gradientn(colours = rainbow(5)) +
  geom_text_repel(aes(label = p), color = "black", size = 5)


### SESAME ###
### Select variables for clustering
# Reference to convert "run_ID" column to rownames so that it will be ignored during clustering while preserving results that can then be returned to original dataframe:
# https://rdrr.io/cran/textshape/man/column_to_rownames.html
sesame_subruns_select <- sesame_subruns[ , c("run_ID", "diff_yield", "diff_fert")] %>%   remove_rownames %>% 
  column_to_rownames(var = "run_ID") %>% 
  as.data.frame()

### find suitable eps parameter using a k-NN plot for k = dim + 1
# Look for the knee!
# reference: https://stackoverflow.com/questions/12893492/choosing-eps-and-minpts-for-dbscan-r
# use 2 * dim or ln(dim) for min points, min points = k
kNNdist_sesame <- sort(kNNdist(sesame_subruns_select, k = (length(sesame_subruns_select)) + 1))
kNNdistplot(sesame_subruns_select, k = (length(sesame_subruns_select)) + 1)

### calculate second derivative to find maximum curvature of "knee" of plot to obtain eps value, eps = second_d
# mid <- 1:length(kNNdist_sesame) 
# deriv <- function(x, y) diff(y) / diff(x)
# middle_pts <- function(x) x[-1] - diff(x) / 2
# second_d <- deriv(middle_pts(mid), deriv(mid, kNNdist_sesame))
# smooth_second_d <- loess(second_d ~ midpts, data.frame(second_d = second_d, midpts = middle_pts(middle_pts(mid))), model = T)

### run DBSCAN analysis
res_dbscan <- dbscan(sesame_subruns_select, eps = 45, minPts = (length(sesame_subruns_select)) + 1)
res_dbscan

### visualize relationships between variables & clusters
pairs(sesame_subruns_select, col = res_dbscan$cluster + 1L)
### plot clusters and add noise (cluster 0) as crosses.
plot(sesame_subruns_select, col=res_dbscan$cluster)
points(sesame_subruns_select[res_dbscan$cluster==0,], pch = 3, col = "grey")

hullplot(sesame_subruns_select, res_dbscan, main = "Sesame DBSCAN Clusters")

### add dbscan cluster values to cluster dataframe
sesame_dbscan_df <- res_dbscan[1] %>%
  as.data.frame() %>% 
  rownames_to_column (var = "run_ID")

### return clustering results to original sesame dataset 
sesame_subruns_select$cluster <- res_dbscan$cluster
sesame_subruns_select <- sesame_subruns_select %>% 
  as.data.frame() %>% 
  rownames_to_column (var = "run_ID")
sesame_results$cluster <- NA
sesame_results$cluster[which(sesame_results$run_ID %in% sesame_subruns_select$run_ID)] <- sesame_subruns_select[ , 4] 

### isolate outliers flagged by DBSCAN
sesame_outliers <- subset(sesame_results, cluster==0)

### visualize outliers in relation to p, c1, c2 inputs
ggplot(sesame_outliers, aes(x = c1, y = c2, color = p)) +
  geom_point(size = 5) +
  scale_color_gradientn(colours = rainbow(5)) +
  geom_text_repel(aes(label = p), color = "black", size = 5)


### SORGHUM ###
### select variables for clustering
# reference to convert "run_ID" column to rownames so that it will be ignored during clustering while preserving results that can then be returned to original dataframe:
# https://rdrr.io/cran/textshape/man/column_to_rownames.html
sorghum_subruns_select <- sorghum_subruns[ , c("run_ID", "diff_yield", "diff_fert")] %>% 
  remove_rownames %>% 
  column_to_rownames(var = "run_ID") %>% 
  as.data.frame()

### find suitable eps parameter using a k-NN plot for k = dim + 1
# Look for the knee!
# reference: https://stackoverflow.com/questions/12893492/choosing-eps-and-minpts-for-dbscan-r
# use 2 * dim or ln(dim) for min points, min points = k
kNNdist_sorghum <- sort(kNNdist(sorghum_subruns_select, k = (length(sorghum_subruns_select)) + 1))
kNNdistplot(sorghum_subruns_select, k = (length(sorghum_subruns_select)) + 1)

### calculate second derivative to find maximum curvature of "knee" of plot to obtain eps value, eps = second_d
# mid <- 1:length(kNNdist_sorghum) 
# deriv <- function(x, y) diff(y) / diff(x)
# middle_pts <- function(x) x[-1] - diff(x) / 2
# second_d <- deriv(middle_pts(mid), deriv(mid, kNNdist_sorghum))
# smooth_second_d <- loess(second_d ~ midpts, data.frame(second_d = second_d, midpts = middle_pts(middle_pts(mid))), model = T)

### run DBSCAN analysis
res_dbscan <- dbscan(sorghum_subruns_select, eps = 85, minPts = (length(sorghum_subruns_select)) + 1)
res_dbscan

### visualize relationships between variables & clusters
pairs(sorghum_subruns_select, col = res_dbscan$cluster + 1L)
### plot clusters and add noise (cluster 0) as crosses.
plot(sorghum_subruns_select, col=res_dbscan$cluster)
points(sorghum_subruns_select[res_dbscan$cluster==0,], pch = 3, col = "grey")

hullplot(sorghum_subruns_select, res_dbscan, main = "Sorghum DBSCAN Clusters")

### add dbscan cluster values to cluster dataframe
sorghum_dbscan_df <- res_dbscan[1] %>%
  as.data.frame() %>% 
  rownames_to_column (var = "run_ID")

### return clustering results to original sorghum dataset and original dataframe
sorghum_subruns_select$cluster <- res_dbscan$cluster
sorghum_subruns_select <- sorghum_subruns_select %>% 
  as.data.frame() %>% 
  rownames_to_column (var = "run_ID")
sorghum_results$cluster <- NA
sorghum_results$cluster[which(sorghum_results$run_ID %in% sorghum_subruns_select$run_ID)] <- sorghum_subruns_select[ , 4] 

### isolate outliers flagged by DBSCAN
sorghum_outliers <- subset(sorghum_results, cluster==0)

### visualize outliers in relation to p, c1, c2 inputs
ggplot(sorghum_outliers, aes(x = c1, y = c2, color = p)) +
  geom_point(size = 5) +
  scale_color_gradientn(colours = rainbow(5)) +
  geom_text_repel(aes(label = p), color = "black", size = 5)

### ALL CROPS ###
### select variables for clustering
# reference to convert "run_ID" column to rownames so that it will be ignored during clustering while preserving results that can then be returned to original dataframe:
# https://rdrr.io/cran/textshape/man/column_to_rownames.html
allcrops_subruns_select <- data_subruns[ , c("run_ID", "diff_yield", "diff_fert")] %>%
  remove_rownames %>% 
  column_to_rownames(var = "run_ID") %>% 
  as.data.frame()

### find suitable eps parameter using a k-NN plot for k = dim + 1
# Look for the knee!
# reference: https://stackoverflow.com/questions/12893492/choosing-eps-and-minpts-for-dbscan-r
# use 2 * dim or ln(dim) for min points, min points = k
kNNdist_allcrops <- sort(kNNdist(allcrops_subruns_select, k = (length(allcrops_subruns_select)) + 1))
kNNdistplot(allcrops_subruns_select, k = (length(allcrops_subruns_select)) + 1)

### calculate second derivative to find maximum curvature of "knee" of plot to obtain eps value, eps = second_d
# mid <- 1:length(kNNdist_allcrops) 
# deriv <- function(x, y) diff(y) / diff(x)
# middle_pts <- function(x) x[-1] - diff(x) / 2
# second_d <- deriv(middle_pts(mid), deriv(mid, kNNdist_allcrops))
# smooth_second_d <- loess(second_d ~ midpts, data.frame(second_d = second_d, midpts = middle_pts(middle_pts(mid))), model = T)

### run DBSCAN analysis
res_dbscan <- dbscan(allcrops_subruns_select, eps = 55, minPts = (length(allcrops_subruns_select)) + 1)
res_dbscan

### visualize relationships between variables & clusters
pairs(allcrops_subruns_select, col = res_dbscan$cluster + 1L)

### plot clusters and add noise (cluster 0) as crosses.
plot(allcrops_subruns_select, col=res_dbscan$cluster)
points(allcrops_subruns_select[res_dbscan$cluster==0,], pch = 3, col = "grey")

hullplot(allcrops_subruns_select, res_dbscan, main = "All Crops DBSCAN Clusters")

### add dbscan cluster values to cluster dataframe
allcrops_dbscan_df <- res_dbscan[1] %>%
  as.data.frame() %>% 
  rownames_to_column (var = "run_ID")

### return clustering results to original allcrops dataset and original dataframe
allcrops_subruns_select$cluster <- res_dbscan$cluster
allcrops_subruns_select <- allcrops_subruns_select %>% 
  as.data.frame() %>% 
  rownames_to_column (var = "run_ID")
data_subruns$cluster <- NA
data_subruns$cluster[which(data_subruns$run_ID %in% allcrops_subruns_select$run_ID)] <- allcrops_subruns_select[ , 4] 

### isolate outliers flagged by DBSCAN
allcrops_outliers <- subset(data_subruns, cluster==0)

### visualize outliers in relation to p, c1, c2 inputs
ggplot(allcrops_outliers, aes(x = c1, y = c2, color = p)) +
  geom_point(size = 5) +
  scale_color_gradientn(colours = rainbow(5)) +
  geom_text_repel(aes(label = p), color = "black", size = 5)


######################## END CLUSTERING #########################


################# OUTPUT CLUSTERING RESULTS DATA #################

### combine crop cluster dataframes and join with orginal dataframe
join1 <- full_join(cassava_outliers, maize_outliers)
join2 <- full_join(join1, groundnuts_outliers)
join3 <- full_join(join2, sesame_outliers)
cluster_outliers <- full_join(join3, sorghum_outliers)

### flag runs with outlier clusters as invalid in original dataframe
cluster_outliers_select <- cluster_outliers[ , c(1,11)]
data$flag_cluster <- NA
data$flag_cluster[which(data$run_ID %in% cluster_outliers_select$run_ID)] <- "invalid"

### Output original data frame with anomaly information appended to outputs directory
# View(data)
setwd(oname)
write.csv(data, file = "results_summary_bycrop_updateA.csv")
write.csv(cluster_outliers, file = "outliers_summary.csv")


##################### PREDICT NEW DATA POINTS CLUSTERING ########################

### load new run data
setwd(dname)
newdata <- read_csv("MINT_v6_simulation_output_newrun1.csv")

# return to working directory
setwd(wname)

### trim crops_subruns_select (original data frames used to train clusters) so that columns match those analyzed by DBSCAN
allcrops_subruns_select <- allcrops_subruns_select[ , c("diff_yield", "diff_fert")]
cassava_subruns_select <- cassava_subruns_select[ , c("diff_yield", "diff_fert")]
groundnuts_subruns_select <- groundnuts_subruns_select[ , c("diff_yield", "diff_fert")]
maize_subruns_select <- maize_subruns_select[ , c("diff_yield", "diff_fert")]
sesame_subruns_select <- sesame_subruns_select[ , c("diff_yield", "diff_fert")]
sorghum_subruns_select <- sorghum_subruns_select[ , c("diff_yield", "diff_fert")]

### subset new data by crop
newdata_cassava <- subset(newdata, crop == "cassava")
newdata_groundnuts <- subset(newdata, crop == "groundnuts")
newdata_maize <- subset(newdata, crop == "maize")
newdata_sesame <- subset(newdata, crop == "sesame")
newdata_sorghum <- subset(newdata, crop == "sorghum")

### normalize yield and fert variables using differnce from baseline for each crop
newdata_cassava$diff_yield <- newdata_cassava$`yield (kg/ha)` - cassava_0_yield
newdata_cassava$diff_fert <- newdata_cassava$`Nfert (kg/ha)`- cassava_0_fert
newdata_groundnuts$diff_yield <- newdata_groundnuts$`yield (kg/ha)` - groundnuts_0_yield
newdata_groundnuts$diff_fert <- newdata_groundnuts$`Nfert (kg/ha)`- groundnuts_0_fert
newdata_maize$diff_yield <- newdata_maize$`yield (kg/ha)` - maize_0_yield
newdata_maize$diff_fert <- newdata_maize$`Nfert (kg/ha)`- maize_0_fert
newdata_sesame$diff_yield <- newdata_sesame$`yield (kg/ha)` - sesame_0_yield
newdata_sesame$diff_fert <- newdata_sesame$`Nfert (kg/ha)`- sesame_0_fert
newdata_sorghum$diff_yield <- newdata_sorghum$`yield (kg/ha)` - sorghum_0_yield
newdata_sorghum$diff_fert <- newdata_sorghum$`Nfert (kg/ha)`- sorghum_0_fert

### recombine normalized crop subsets to create full data set
join1 <- full_join(newdata_cassava, newdata_maize)
join2 <- full_join(join1, newdata_groundnuts)
join3 <- full_join(join2, newdata_sesame)
newdata <- full_join(join3, newdata_sorghum)

### select columns to be compared to DBSCAN cluster results, must match columns used to define/train clusters
newdata_select <- newdata[ , c("diff_yield", "diff_fert")]
newdata_cassava_select <- newdata_cassava[ , c("diff_yield", "diff_fert")]
newdata_groundnuts_select <- newdata_groundnuts[ , c("diff_yield", "diff_fert")]
newdata_maize_select <- newdata_maize[ , c("diff_yield", "diff_fert")]
newdata_sesame_select <- newdata_sesame[ , c("diff_yield", "diff_fert")]
newdata_sorghum_select <- newdata_sorghum[ , c("diff_yield", "diff_fert")]

### predict cluster membership for new data points
### (Note: 0 means it is predicted as noise)
predict(res_dbscan, newdata_select, data = allcrops_subruns_select)
predict(res_dbscan, newdata_cassava_select, data = cassava_subruns_select)
predict(res_dbscan, newdata_groundnuts_select, data = groundnuts_subruns_select)
predict(res_dbscan, newdata_maize_select, data = maize_subruns_select)
predict(res_dbscan, newdata_sesame_select, data = sesame_subruns_select)
predict(res_dbscan, newdata_sorghum_select, data = sorghum_subruns_select)

################### END OUTPUT CLUSTERING RESULTS DATA ##############