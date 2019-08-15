library(readr)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(anomalize)
library(outliers)
library(plotly)
library(cluster)
library(factoextra)

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


### re-classify fertilizer treaments to 0 (no fertilizer) or non-0 (fertilizer applied) ###
data$fertclass <- data$c1
data$fertclass[data$c2 > 0] <- "Fert Subsidy"
data$fertclass[data$c2 < 0] <- "Fert Subsidy"
data$fertclass[data$c2 == 0] <- "No Fert Subsidy"
data <- data %>% group_by(run_ID)

### add coloumns for anomaly flags and reason flag was generated
data$flag <- "valid"
data$reason <- NA

### Subset by crop
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



############################# BEGIN CLUSTERING ANALYSIS #######################################
### cassava ###
cassava_results_select <- cassava_results[ , c("run_ID", "p", "c1", "c2", "land area (ha)", "yield (kg/ha)", "production (kg)", "Nfert (kg/ha)")] 

# find baseline runs
cassava_0 <- subset(data, crop=='cassava') %>%
  filter(p == 0, c1 == 0, c2 == 0)
cassava_0_land <- mean(cassava_0$`land area (ha)`)
cassava_0_yield <- mean(cassava_0$`yield (kg/ha)`)
cassava_0_prod <- mean(cassava_0$`production (kg)`)
cassava_0_fert <- mean(cassava_0$`Nfert (kg/ha)`)

# isolate runs given subsidy
cassava_subruns <- anti_join(cassava_results_select, cassava_0)

# calculate difference between subsidy runs and baseline runs for each result variable
cassava_subruns$diff_land <- cassava_subruns$`land area (ha)`- cassava_0_land
cassava_subruns$diff_yield <- cassava_subruns$`yield (kg/ha)`- cassava_0_yield
cassava_subruns$diff_prod <- cassava_subruns$`production (kg)`- cassava_0_prod
cassava_subruns$diff_fert <- cassava_subruns$`Nfert (kg/ha)`- cassava_0_fert

# Select variables for clustering
# Reference to convert "run_ID" column to rownames so that it will be ignored during clustering while preserving results that can then be returned to original dataframe:
# https://rdrr.io/cran/textshape/man/column_to_rownames.html
cassava_subruns_select <- cassava_subruns[ , c("run_ID", "diff_yield", "diff_fert")] %>% 
  remove_rownames %>% 
  column_to_rownames(var = "run_ID") %>% 
  as.data.frame()


### Clustering ###
df <- cassava_subruns_select
df <- scale(df)
head(df)

### Determine optimal number clusters using gap method
set.seed(123)
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25, K.max = 10, B = 50)

# Print the result
# reference to extract output from print text: 
# http://r.789695.n4.nabble.com/extract-printed-value-from-a-function-td3322451.html
# print(gap_stat, method = "firstmax")
gs_print <- capture.output(gap_stat)
gs_outputstr <- gs_print[4]
optclusters <- as.numeric(strsplit(gs_outputstr, ": ")[[1]][2]) 

### run k-means clustering
cassava_kcluster <- kmeans(df, centers = optclusters, nstart = 25)
fviz_cluster(cassava_kcluster, data = df, geom = "point")
cassava_kcluster_df <- cassava_kcluster[1] %>%
  as.data.frame() %>% 
  rownames_to_column (var = "run_ID")


# ### PAM clustering
# require(cluster)
# pam_cluster <- pam(df, optclusters)
# # Visualize pam clustering
# fviz_cluster(pam_cluster, geom = "point", ellipse.type = "norm")

### Return Cluster assignments to original dataframe ###
data$cluster_val[which(data$run_ID %in% cassava_kcluster_df$run_ID)] <- cassava_kcluster_df[ , 2] 
cassava_0 <- subset(data, crop=='cassava') %>%
  filter(p == 0, c1 == 0, c2 == 0)
cassava_results_select <- cassava_results[ , c("run_ID", "p", "c1", "c2", "land area (ha)", "yield (kg/ha)", "production (kg)", "Nfert (kg/ha)", "cluster_val")] 
cassava_subruns <- anti_join(cassava_results_select, cassava_0)


# plot cluster values vs. p, c1, c2 using plotly package to create 3D scatterplot
p_cluster_val <- plot_ly(cassava_subruns, x = ~p, y = ~c1, z = ~c2, color = ~cluster_val, colors = colors) %>%
  add_markers() %>%
  layout(title = 'Cluster Values Assigned to Result Generated by p, c1, c2 Combination',
         scene = list(xaxis = list(title = 'Land Price (c1)', range = c(-50, 50)),
                      yaxis = list(title = 'Fertilizer Price (c2)', range = c(-50, 50)),
                      zaxis = list(title = 'Market Crop Price (p)', range = c(-50, 50))))
p_cluster_val
link_cluster_val = api_create(p_cluster_val, filename="cluster_val")
link_cluster_val

cassava_subruns_cluster1 <- subset(cassava_subruns, cluster_val==1)
p_cluster_val1 <- plot_ly(cassava_subruns_cluster1, x = ~p, y = ~c1, z = ~c2, colors = colors[1]) %>%
  add_markers() %>%
  layout(title = 'Cluster Value 1 Assigned to Results Generated by p, c1, c2 Combination',
         scene = list(xaxis = list(title = 'Land Price (c1)', range = c(-50, 50)),
                      yaxis = list(title = 'Fertilizer Price (c2)', range = c(-50, 50)),
                      zaxis = list(title = 'Market Crop Price (p)', range = c(-50, 50))))
p_cluster_val1
link_cluster_val1 = api_create(p_cluster_val1, filename="cluster_val1")
link_cluster_val1

cassava_subruns_cluster2 <- subset(cassava_subruns, cluster_val==2)
p_cluster_val2 <- plot_ly(cassava_subruns_cluster2, x = ~p, y = ~c1, z = ~c2, colors = colors[2]) %>%
  add_markers() %>%
  layout(title = 'Cluster Value 2 Assigned to Results Generated by p, c1, c2 Combination',
         scene = list(xaxis = list(title = 'Land Price (c1)', range = c(-50, 50)),
                      yaxis = list(title = 'Fertilizer Price (c2)', range = c(-50, 50)),
                      zaxis = list(title = 'Market Crop Price (p)', range = c(-50, 50))))
p_cluster_val2
link_cluster_val2 = api_create(p_cluster_val2, filename="cluster_val2")
link_cluster_val2

cassava_subruns_cluster3 <- subset(cassava_subruns, cluster_val==3)
p_cluster_val3 <- plot_ly(cassava_subruns_cluster3, x = ~p, y = ~c1, z = ~c2, colors = colors[3]) %>%
  add_markers() %>%
  layout(title = 'Cluster Value 3 Assigned to Results Generated by p, c1, c2 Combination',
         scene = list(xaxis = list(title = 'Land Price (c1)', range = c(-50, 50)),
                      yaxis = list(title = 'Fertilizer Price (c2)', range = c(-50, 50)),
                      zaxis = list(title = 'Market Crop Price (p)', range = c(-50, 50))))
p_cluster_val3
link_cluster_val3 = api_create(p_cluster_val3, filename="cluster_val3")
link_cluster_val3

cassava_subruns_cluster4 <- subset(cassava_subruns, cluster_val==4)
p_cluster_val4 <- plot_ly(cassava_subruns_cluster4, x = ~p, y = ~c1, z = ~c2, colors = colors[4]) %>%
  add_markers() %>%
  layout(title = 'Cluster Value 4 Assigned to Results Generated by p, c1, c2 Combination',
         scene = list(xaxis = list(title = 'Land Price (c1)', range = c(-50, 50)),
                      yaxis = list(title = 'Fertilizer Price (c2)', range = c(-50, 50)),
                      zaxis = list(title = 'Market Crop Price (p)', range = c(-50, 50))))
p_cluster_val4
link_cluster_val4 = api_create(p_cluster_val4, filename="cluster_val4")
link_cluster_val4

cassava_subruns_cluster5 <- subset(cassava_subruns, cluster_val==5)
p_cluster_val5 <- plot_ly(cassava_subruns_cluster5, x = ~p, y = ~c1, z = ~c2, colors = colors[4]) %>%
  add_markers() %>%
  layout(title = 'Cluster Value 5 Assigned to Results Generated by p, c1, c2 Combination',
         scene = list(xaxis = list(title = 'Land Price (c1)', range = c(-50, 50)),
                      yaxis = list(title = 'Fertilizer Price (c2)', range = c(-50, 50)),
                      zaxis = list(title = 'Market Crop Price (p)', range = c(-50, 50))))
p_cluster_val5
link_cluster_val5 = api_create(p_cluster_val5, filename="cluster_val5")
link_cluster_val5



################################################################################################
### groundnuts
groundnuts_results <- subset(data, crop=='groundnuts')
groundnuts_results_select <- groundnuts_results[ , c("run_ID", "p", "c1", "c2", "land area (ha)", "yield (kg/ha)", "production (kg)", "Nfert (kg/ha)")] 

# find baseline runs
groundnuts_0 <- subset(data, crop=='groundnuts') %>%
  filter(p == 0, c1 == 0, c2 == 0)
groundnuts_0_land <- mean(groundnuts_0$`land area (ha)`)
groundnuts_0_yield <- mean(groundnuts_0$`yield (kg/ha)`)
groundnuts_0_prod <- mean(groundnuts_0$`production (kg)`)
groundnuts_0_fert <- mean(groundnuts_0$`Nfert (kg/ha)`)

# isolate runs given subsidy
groundnuts_subruns <- anti_join(groundnuts_results_select, groundnuts_0)

# calculate difference between subsidy runs and baseline runs for each result variable
groundnuts_subruns$diff_land <- groundnuts_subruns$`land area (ha)`- groundnuts_0_land
groundnuts_subruns$diff_yield <- groundnuts_subruns$`yield (kg/ha)`- groundnuts_0_yield
groundnuts_subruns$diff_prod <- groundnuts_subruns$`production (kg)`- groundnuts_0_prod
groundnuts_subruns$diff_fert <- groundnuts_subruns$`Nfert (kg/ha)`- groundnuts_0_fert

# Select variables for clustering
# Reference to convert "run_ID" column to rownames so that it will be ignored during clustering while preserving results that can then be returned to original dataframe:
# https://rdrr.io/cran/textshape/man/column_to_rownames.html
groundnuts_subruns_select <- groundnuts_subruns[ , c("run_ID", "diff_yield", "diff_fert")] %>% 
  remove_rownames %>% 
  column_to_rownames(var = "run_ID") %>% 
  as.data.frame()


### Clustering ###
df <- groundnuts_subruns_select
df <- scale(df)
head(df)

### Determine optimal number clusters using gap method
set.seed(123)
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25, K.max = 10, B = 50)

# Print the result
# reference to extract output from print text: 
# http://r.789695.n4.nabble.com/extract-printed-value-from-a-function-td3322451.html
# print(gap_stat, method = "firstmax")
gs_print <- capture.output(gap_stat)
gs_outputstr <- gs_print[4]
optclusters <- as.numeric(strsplit(gs_outputstr, ": ")[[1]][2]) 


### run k-means clustering
groundnuts_kcluster <- kmeans(df, centers = optclusters, nstart = 25)
str(groundnuts_kcluster)
fviz_cluster(groundnuts_kcluster, data = df, geom = "point")
groundnuts_kcluster_df <- groundnuts_kcluster[1] %>%
  as.data.frame() %>% 
  rownames_to_column (var = "run_ID")


### PAM clustering
require(cluster)
pam_cluster <- pam(df, optclusters)
# Visualize pam clustering
fviz_cluster(pam_cluster, geom = "point", ellipse.type = "norm")



##################################################################################
### maize
maize_results <- subset(data, crop=='maize')
maize_results_select <- maize_results[ , c("run_ID", "p", "c1", "c2", "land area (ha)", "yield (kg/ha)", "production (kg)", "Nfert (kg/ha)")] 

# find baseline runs
maize_0 <- subset(data, crop=='maize') %>%
  filter(p == 0, c1 == 0, c2 == 0)
maize_0_land <- mean(maize_0$`land area (ha)`)
maize_0_yield <- mean(maize_0$`yield (kg/ha)`)
maize_0_prod <- mean(maize_0$`production (kg)`)
maize_0_fert <- mean(maize_0$`Nfert (kg/ha)`)

# isolate runs given subsidy
maize_subruns <- anti_join(maize_results_select, maize_0)

# calculate difference between subsidy runs and baseline runs for each result variable
maize_subruns$diff_land <- maize_subruns$`land area (ha)`- maize_0_land
maize_subruns$diff_yield <- maize_subruns$`yield (kg/ha)`- maize_0_yield
maize_subruns$diff_prod <- maize_subruns$`production (kg)`- maize_0_prod
maize_subruns$diff_fert <- maize_subruns$`Nfert (kg/ha)`- maize_0_fert

# Select variables for clustering
# Reference to convert "run_ID" column to rownames so that it will be ignored during clustering while preserving results that can then be returned to original dataframe:
# https://rdrr.io/cran/textshape/man/column_to_rownames.html
maize_subruns_select <- maize_subruns[ , c("run_ID", "diff_yield", "diff_fert")] %>% 
  remove_rownames %>% 
  column_to_rownames(var = "run_ID") %>% 
  as.data.frame()


### Clustering ###
df <- maize_subruns_select
df <- scale(df)
head(df)

### Determine optimal number clusters using gap method
set.seed(123)
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)

# Print the result
# reference to extract output from print text: 
# http://r.789695.n4.nabble.com/extract-printed-value-from-a-function-td3322451.html
# print(gap_stat, method = "firstmax")
gs_print <- capture.output(gap_stat)
gs_outputstr <- gs_print[4]
optclusters <- as.numeric(strsplit(gs_outputstr, ": ")[[1]][2]) 


### run k-means clustering
maize_kcluster <- kmeans(df, centers = optclusters, nstart = 25)
str(maize_kcluster)
fviz_cluster(maize_kcluster, data = df, geom = "point")
maize_kcluster_df <- maize_kcluster[1] %>%
  as.data.frame() %>% 
  rownames_to_column (var = "run_ID")


### PAM clustering
require(cluster)
pam_cluster <- pam(df, optclusters)
# Visualize pam clustering
fviz_cluster(pam_cluster, geom = "point", ellipse.type = "norm")



##################################################################################
### sesame
sesame_results <- subset(data, crop=='sesame')
sesame_results_select <- sesame_results[ , c("run_ID", "p", "c1", "c2", "land area (ha)", "yield (kg/ha)", "production (kg)", "Nfert (kg/ha)")] 

# find baseline runs
sesame_0 <- subset(data, crop=='sesame') %>%
  filter(p == 0, c1 == 0, c2 == 0)
sesame_0_land <- mean(sesame_0$`land area (ha)`)
sesame_0_yield <- mean(sesame_0$`yield (kg/ha)`)
sesame_0_prod <- mean(sesame_0$`production (kg)`)
sesame_0_fert <- mean(sesame_0$`Nfert (kg/ha)`)

# isolate runs given subsidy
sesame_subruns <- anti_join(sesame_results_select, sesame_0)

# calculate difference between subsidy runs and baseline runs for each result variable
sesame_subruns$diff_land <- sesame_subruns$`land area (ha)`- sesame_0_land
sesame_subruns$diff_yield <- sesame_subruns$`yield (kg/ha)`- sesame_0_yield
sesame_subruns$diff_prod <- sesame_subruns$`production (kg)`- sesame_0_prod
sesame_subruns$diff_fert <- sesame_subruns$`Nfert (kg/ha)`- sesame_0_fert

# Select variables for clustering
# Reference to convert "run_ID" column to rownames so that it will be ignored during clustering while preserving results that can then be returned to original dataframe:
# https://rdrr.io/cran/textshape/man/column_to_rownames.html
sesame_subruns_select <- sesame_subruns[ , c("run_ID", "diff_yield", "diff_fert")] %>% 
  remove_rownames %>% 
  column_to_rownames(var = "run_ID") %>% 
  as.data.frame()


### Clustering ###
df <- sesame_subruns_select
df <- scale(df)
head(df)

### Determine optimal number clusters using gap method
set.seed(123)
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)

# Print the result
# reference to extract output from print text: 
# http://r.789695.n4.nabble.com/extract-printed-value-from-a-function-td3322451.html
# print(gap_stat, method = "firstmax")
gs_print <- capture.output(gap_stat)
gs_outputstr <- gs_print[4]
optclusters <- as.numeric(strsplit(gs_outputstr, ": ")[[1]][2]) 


### run k-means clustering
sesame_kcluster <- kmeans(df, centers = optclusters, nstart = 25)
str(sesame_kcluster)
fviz_cluster(sesame_kcluster, data = df, geom = "point")
sesame_kcluster_df <- sesame_kcluster[1] %>%
  as.data.frame() %>% 
  rownames_to_column (var = "run_ID")


### PAM clustering
require(cluster)
pam_cluster <- pam(df, optclusters)
# Visualize pam clustering
fviz_cluster(pam_cluster, geom = "point", ellipse.type = "norm")



##################################################################################
### sorghum
sorghum_results <- subset(data, crop=='sorghum')
sorghum_results_select <- sorghum_results[ , c("run_ID", "p", "c1", "c2", "land area (ha)", "yield (kg/ha)", "production (kg)", "Nfert (kg/ha)")] 

# find baseline runs
sorghum_0 <- subset(data, crop=='sorghum') %>%
  filter(p == 0, c1 == 0, c2 == 0)
sorghum_0_land <- mean(sorghum_0$`land area (ha)`)
sorghum_0_yield <- mean(sorghum_0$`yield (kg/ha)`)
sorghum_0_prod <- mean(sorghum_0$`production (kg)`)
sorghum_0_fert <- mean(sorghum_0$`Nfert (kg/ha)`)

# isolate runs given subsidy
sorghum_subruns <- anti_join(sorghum_results_select, sorghum_0)

# calculate difference between subsidy runs and baseline runs for each result variable
sorghum_subruns$diff_land <- sorghum_subruns$`land area (ha)`- sorghum_0_land
sorghum_subruns$diff_yield <- sorghum_subruns$`yield (kg/ha)`- sorghum_0_yield
sorghum_subruns$diff_prod <- sorghum_subruns$`production (kg)`- sorghum_0_prod
sorghum_subruns$diff_fert <- sorghum_subruns$`Nfert (kg/ha)`- sorghum_0_fert

# Select variables for clustering
# Reference to convert "run_ID" column to rownames so that it will be ignored during clustering while preserving results that can then be returned to original dataframe:
# https://rdrr.io/cran/textshape/man/column_to_rownames.html
sorghum_subruns_select <- sorghum_subruns[ , c("run_ID", "diff_yield", "diff_fert")] %>% 
  remove_rownames %>% 
  column_to_rownames(var = "run_ID") %>% 
  as.data.frame()


### Clustering ###
df <- sorghum_subruns_select
df <- scale(df)
head(df)

### Determine optimal number clusters using gap method
set.seed(123)
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)

# Print the result
# reference to extract output from print text: 
# http://r.789695.n4.nabble.com/extract-printed-value-from-a-function-td3322451.html
# print(gap_stat, method = "firstmax")
gs_print <- capture.output(gap_stat)
gs_outputstr <- gs_print[4]
optclusters <- as.numeric(strsplit(gs_outputstr, ": ")[[1]][2]) 


### run k-means clustering
sorghum_kcluster <- kmeans(df, centers = optclusters, nstart = 25)
str(sorghum_kcluster)
fviz_cluster(sorghum_kcluster, data = df, geom = "point")
sorghum_kcluster_df <- sorghum_kcluster[1] %>%
  as.data.frame() %>% 
  rownames_to_column (var = "run_ID")


### PAM clustering
require(cluster)
pam_cluster <- pam(df, optclusters)
# Visualize pam clustering
fviz_cluster(pam_cluster, geom = "point", ellipse.type = "norm")


### All crops ###
data_subruns <- full_join(cassava_subruns, groundnuts_subruns)
data_subruns <- full_join(data_subruns, maize_subruns)
data_subruns <- full_join(data_subruns, sesame_subruns)
data_subruns <- full_join(data_subruns, sorghum_subruns)


# Select variables for clustering
# Reference to convert "run_ID" column to rownames so that it will be ignored during clustering while preserving results that can then be returned to original dataframe:
# https://rdrr.io/cran/textshape/man/column_to_rownames.html
data_subruns_select <- data_subruns[ , c("run_ID", "diff_yield", "diff_fert")] %>% 
  remove_rownames %>% 
  column_to_rownames(var = "run_ID") %>% 
  as.data.frame()


### Clustering ###
df <- data_subruns_select
df <- scale(df)
head(df)

### Determine optimal number clusters using gap method
set.seed(123)
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25, K.max = 10, B = 50)

# Print the result
# reference to extract output from print text: 
# http://r.789695.n4.nabble.com/extract-printed-value-from-a-function-td3322451.html
print(gap_stat, method = "firstmax")
gs_print <- capture.output(gap_stat)
gs_outputstr <- gs_print[4]
optclusters <- as.numeric(strsplit(gs_outputstr, ": ")[[1]][2]) 


### run k-means clustering
allcrops_kcluster <- kmeans(df, centers = 2, nstart = 25)
fviz_cluster(allcrops_kcluster, data = df, geom = "point")
allcrops_kcluster_df <- cassava_kcluster[1] %>%
  as.data.frame() %>% 
  rownames_to_column (var = "run_ID")


### PAM clustering
require(cluster)
pam_cluster <- pam(df, optclusters)
# Visualize pam clustering
fviz_cluster(pam_cluster, geom = "point", ellipse.type = "norm")


### Return Cluster assignments to original dataframe ###
data$cluster_val[which(data$run_ID %in% cassava_kcluster_df$run_ID)] <- cassava_kcluster_df[ , 2] 
cassava_0 <- subset(data, crop=='cassava') %>%
  filter(p == 0, c1 == 0, c2 == 0)
cassava_results_select <- cassava_results[ , c("run_ID", "p", "c1", "c2", "land area (ha)", "yield (kg/ha)", "production (kg)", "Nfert (kg/ha)", "cluster_val")] 
cassava_subruns <- anti_join(cassava_results_select, cassava_0)


# plot land anomalies using plotly package to create 3D scatterplot
p_cluster_val <- plot_ly(cassava_subruns, x = ~p, y = ~c1, z = ~c2, color = ~cluster_val, colors = colors) %>%
  add_markers() %>%
  layout(title = 'Cluster Values Assigned to Result Generated by p, c1, c2 Combination',
         scene = list(xaxis = list(title = 'Land Price (c1)', range = c(-50, 50)),
                      yaxis = list(title = 'Fertilizer Price (c2)', range = c(-50, 50)),
                      zaxis = list(title = 'Market Crop Price (p)', range = c(-50, 50))))
p_cluster_val
link_cluster_val = api_create(p_cluster_val, filename="cluster_val")
link_cluster_val

cassava_subruns_cluster1 <- subset(cassava_subruns, cluster_val==1)
p_cluster_val1 <- plot_ly(cassava_subruns_cluster1, x = ~p, y = ~c1, z = ~c2, colors = colors[1]) %>%
  add_markers() %>%
  layout(title = 'Cluster Value 1 Assigned to Results Generated by p, c1, c2 Combination',
         scene = list(xaxis = list(title = 'Land Price (c1)', range = c(-50, 50)),
                      yaxis = list(title = 'Fertilizer Price (c2)', range = c(-50, 50)),
                      zaxis = list(title = 'Market Crop Price (p)', range = c(-50, 50))))
p_cluster_val1
link_cluster_val1 = api_create(p_cluster_val1, filename="cluster_val1")
link_cluster_val1

cassava_subruns_cluster2 <- subset(cassava_subruns, cluster_val==2)
p_cluster_val2 <- plot_ly(cassava_subruns_cluster2, x = ~p, y = ~c1, z = ~c2, colors = colors[2]) %>%
  add_markers() %>%
  layout(title = 'Cluster Value 2 Assigned to Results Generated by p, c1, c2 Combination',
         scene = list(xaxis = list(title = 'Land Price (c1)', range = c(-50, 50)),
                      yaxis = list(title = 'Fertilizer Price (c2)', range = c(-50, 50)),
                      zaxis = list(title = 'Market Crop Price (p)', range = c(-50, 50))))
p_cluster_val2
link_cluster_val2 = api_create(p_cluster_val2, filename="cluster_val2")
link_cluster_val2

cassava_subruns_cluster3 <- subset(cassava_subruns, cluster_val==3)
p_cluster_val3 <- plot_ly(cassava_subruns_cluster3, x = ~p, y = ~c1, z = ~c2, colors = colors[3]) %>%
  add_markers() %>%
  layout(title = 'Cluster Value 3 Assigned to Results Generated by p, c1, c2 Combination',
         scene = list(xaxis = list(title = 'Land Price (c1)', range = c(-50, 50)),
                      yaxis = list(title = 'Fertilizer Price (c2)', range = c(-50, 50)),
                      zaxis = list(title = 'Market Crop Price (p)', range = c(-50, 50))))
p_cluster_val3
link_cluster_val3 = api_create(p_cluster_val3, filename="cluster_val3")
link_cluster_val3

cassava_subruns_cluster4 <- subset(cassava_subruns, cluster_val==4)
p_cluster_val4 <- plot_ly(cassava_subruns_cluster4, x = ~p, y = ~c1, z = ~c2, colors = colors[4]) %>%
  add_markers() %>%
  layout(title = 'Cluster Value 4 Assigned to Results Generated by p, c1, c2 Combination',
         scene = list(xaxis = list(title = 'Land Price (c1)', range = c(-50, 50)),
                      yaxis = list(title = 'Fertilizer Price (c2)', range = c(-50, 50)),
                      zaxis = list(title = 'Market Crop Price (p)', range = c(-50, 50))))
p_cluster_val4
link_cluster_val4 = api_create(p_cluster_val4, filename="cluster_val4")
link_cluster_val4

cassava_subruns_cluster5 <- subset(cassava_subruns, cluster_val==5)
p_cluster_val5 <- plot_ly(cassava_subruns_cluster5, x = ~p, y = ~c1, z = ~c2, colors = colors[4]) %>%
  add_markers() %>%
  layout(title = 'Cluster Value 5 Assigned to Results Generated by p, c1, c2 Combination',
         scene = list(xaxis = list(title = 'Land Price (c1)', range = c(-50, 50)),
                      yaxis = list(title = 'Fertilizer Price (c2)', range = c(-50, 50)),
                      zaxis = list(title = 'Market Crop Price (p)', range = c(-50, 50))))
p_cluster_val5
link_cluster_val5 = api_create(p_cluster_val5, filename="cluster_val5")
link_cluster_val5


################################END CLUSTERING############################################################
