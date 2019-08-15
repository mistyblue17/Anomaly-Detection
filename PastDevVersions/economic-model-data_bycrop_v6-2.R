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


### re-classify fertilizer treaments to 0 (no fertilizer) or non-0 (fertilizer applied) ###
data$fertclass <- data$c1
data$fertclass[data$c2 > 0] <- "Fert Subsidy"
data$fertclass[data$c2 < 0] <- "Fert Subsidy"
data$fertclass[data$c2 == 0] <- "No Fert Subsidy"
data <- data %>% group_by(run_ID)


### check for results that defy the laws of physics by exceeding total available land to use ###
# identify total land
total_land <- sum(calib_data$xbar1)

# filter by c1, c2, p combinations
landcheck <- data %>%
  group_by(run_ID, c1, c2, p) %>%
  summarize(sum = sum(`land area (ha)`)) 
 
# find runs that over use total available land -> anomalous
overuse <- landcheck %>%
  filter (sum > (total_land + 1)) 
  
# find runs that under use total available land -> not anomalous b/c doesn't defy physics but could be interesting to analyze further
underuse <- landcheck %>%
  filter (sum < (total_land - 1))


### subset by crop ###
#cassava
cassava_results <- subset(data, crop=='cassava')

cassava_land_summary <- summary(cassava_results$`land area (ha)`)
cassava_yield_summary <- summary(cassava_results$`yield (kg/ha)`)
cassava_prod_summary <- summary(cassava_results$`production (kg)`)
cassava_fert_summary <- summary(cassava_results$`Nfert (kg/ha)`)

cassava_0 <- subset(data, crop=='cassava') %>%
  filter(p == 0, c1 == 0, c2 == 0)
cassava2_0 <- subset(data, crop=='cassava') %>%
  filter(p == 0, c1 == 0, c2 == 0)
compare_tbls(cassava_0, cassava2_0, op)

mad_cassava_land <- mad(cassava_results$`land area (ha)`, na.rm = TRUE)
mad_cassava_yield <- mad(cassava_results$`yield (kg/ha)`, na.rm = TRUE)
mad_cassava_fert <- mad(cassava_results$`production (kg)`, na.rm = TRUE)
mad_cassava_prod <- mad(cassava_results$`Nfert (kg/ha)`, na.rm = TRUE)

# groundnuts
groundnuts_results <- subset(data, crop=='groundnuts')

groundnuts_land_summary <- summary(groundnuts_results$`land area (ha)`)
groundnuts_yield_summary <- summary(groundnuts_results$`yield (kg/ha)`)
groundnuts_prod_summary <- summary(groundnuts_results$`production (kg)`)
groundnuts_fert_summary <- summary(groundnuts_results$`Nfert (kg/ha)`)

groundnuts_0 <- subset(data, crop=='groundnuts') %>%
  filter(p == 0, c1 == 0, c2 == 0)
groundnuts2_0 <- subset(data, crop=='groundnuts') %>%
  filter(p == 0, c1 == 0, c2 == 0)

mad_groundnuts_land <- mad(groundnuts_results$`land area (ha)`, na.rm = TRUE)
mad_groundnuts_yield <- mad(groundnuts_results$`yield (kg/ha)`, na.rm = TRUE)
mad_groundnuts_fert <- mad(groundnuts_results$`production (kg)`, na.rm = TRUE)
mad_groundnuts_prod <- mad(groundnuts_results$`Nfert (kg/ha)`, na.rm = TRUE)

# maize
maize_results <- subset(data, crop=='maize')

maize_land_summary <- summary(maize_results$`land area (ha)`)
maize_yield_summary <- summary(maize_results$`yield (kg/ha)`)
maize_fert_summary <- summary(maize_results$`Nfert (kg/ha)`)
maize_prod_summary <- summary(maize_results$`production (kg)`)

maize_0 <- subset(data, crop=='maize') %>%
  filter(p == 0, c1 == 0, c2 == 0)
maize2_0 <- subset(data, crop=='maize') %>%
  filter(p == 0, c1 == 0, c2 == 0)

mad_maize_land <- mad(maize_results$`land area (ha)`, na.rm = TRUE)
mad_maize_yield <- mad(maize_results$`yield (kg/ha)`, na.rm = TRUE)
mad_maize_prod <- mad(maize_results$`Nfert (kg/ha)`, na.rm = TRUE)
mad_maize_fert <- mad(maize_results$`production (kg)`, na.rm = TRUE)

# sesame
sesame_results <- subset(data, crop=='sesame')

sesame_land_summary <- summary(sesame_results$`land area (ha)`)
sesame_yield_summary <- summary(sesame_results$`yield (kg/ha)`)
sesame_fert_summary <- summary(sesame_results$`Nfert (kg/ha)`)
sesame_prod_summary <- summary(sesame_results$`production (kg)`)

sesame_0 <- subset(data, crop=='sesame') %>%
  filter(p == 0, c1 == 0, c2 == 0)
sesame2_0 <- subset(data, crop=='sesame') %>%
  filter(p == 0, c1 == 0, c2 == 0)

mad_sesame_land <- mad(sesame_results$`land area (ha)`, na.rm = TRUE)
mad_sesame_yield <- mad(sesame_results$`yield (kg/ha)`, na.rm = TRUE)
mad_sesame_prod <- mad(sesame_results$`Nfert (kg/ha)`, na.rm = TRUE)
mad_sesame_fert <- mad(sesame_results$`production (kg)`, na.rm = TRUE)

# sorghum
sorghum_results <- subset(data, crop=='sorghum')

sorghum_land_summary <- summary(sorghum_results$`land area (ha)`)
sorghum_yield_summary <- summary(sorghum_results$`yield (kg/ha)`)
sorghum_fert_summary <- summary(sorghum_results$`Nfert (kg/ha)`)
sorghum_prod_summary <- summary(sorghum_results$`production (kg)`)

sorghum_0 <- subset(data, crop=='sorghum') %>%
  filter(p == 0, c1 == 0, c2 == 0)
sorghum2_0 <- subset(data, crop=='sorghum') %>%
  filter(p == 0, c1 == 0, c2 == 0)

mad_sorghum_land <- mad(sorghum_results$`land area (ha)`, na.rm = TRUE)
mad_sorghum_yield <- mad(sorghum_results$`yield (kg/ha)`, na.rm = TRUE)
mad_sorghum_prod <- mad(sorghum_results$`Nfert (kg/ha)`, na.rm = TRUE)
mad_sorghum_fert <- mad(sorghum_results$`production (kg)`, na.rm = TRUE)

merge1 <- merge(cassava_0, groundnuts_0)
merge2 <- merge(maize_0, sesame_0)
merge3 <- merge(merge2, sorghum_0)
crops_0 <- merge(merge1, merge3)

### preliminary anomaly screening of all crops by output variable ###
# detect land anomalies using anomalize package 
land_anomalies <- anomalize(data = data(group_by(run_ID)), target = `land area (ha)`, method = "gesd", alpha = 0.05, verbose = TRUE)
land_anomalousruns <- subset(land_anomalies$anomalized_tbl, anomaly == "Yes")
min_landarea <- min(land_anomalousruns$`land area (ha)`)
max_landarea <- max(land_anomalousruns$`land area (ha)`)
mad_all_land <- mad(data$`land area (ha)`, na.rm = TRUE)

# plot land anomalies using plotly package to create 3D scatterplot
p_land_anomalies3d <- plot_ly(land_anomalousruns, x = ~c1, y = ~c2, z = ~p, color = ~crop, colors = colors) %>%
  add_markers() %>%
  layout(title = 'Runs with Anomalous Values in Land Use Area',
         scene = list(xaxis = list(title = 'Land Price (c1)', range = c(-40, 50)),
                      yaxis = list(title = 'Fertilizer Price (c2)', range = c(-40, 50)),
                      zaxis = list(title = 'Market Crop Price (p)', range = c(-40, 50))),
         annotations = list(
           x = 1.13,
           y = 1.05,
           text = 'Crop',
           xref = 'paper',
           yref = 'paper',
           showarrow = FALSE
         ))
p_land_anomalies3d
link_land_anomalies3d = api_create(p_land_anomalies3d, filename="land_anomalies3d")
link_land_anomalies3d

# plot land anomalies varying marker diameter to reflect value of land area (4d)
p_land_anomalies4d <- plot_ly(land_anomalousruns, x = ~c1, y = ~c2, z = ~p, color = ~crop, size = ~`land area (ha)`, colors = colors, type = 'scatter3d', mode = 'markers',
    marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(5, 150),
    text = ~paste('Crop:', crop, '<br>Land Use Area (ha):', `land area (ha)`)) %>%
    layout(title = 'Runs with Anomalous Values in Land Use Area',
        scene = list(
            xaxis = list(title = 'Land Price (c1)',
                gridcolor = 'rgb(255, 255, 255)',
                range = c(-40, 50),
                zerolinewidth = 1,
                ticklen = 5,
                gridwith = 2),
            yaxis = list(title = 'Fertilizer Price (c2)',
                gridcolor = 'rgb(255, 255, 255)',
                range = c(-40, 50),
                zerolinewidth = 1,
                ticklen = 5,
                gridwith = 2),
            zaxis = list(title = 'Market Crop Price (p)',
                gridcolor = 'rgb(255, 255, 255)',
                range = c(-40, 50),
                zerolinewidth = 1,
                ticklen = 5,
                gridwith = 2),
            paper_bgcolor = 'rgb(243, 243, 243)',
            plot_bgcolor = 'rgb(243, 243, 243)'))
link_land_anomalies4d = api_create(p_land_anomalies, filename="land_anomalies4d")
link_land_anomalies4d


# yield anomalies
yield_anomalies <- anomalize(data = data, target = `yield (kg/ha)`, method = "gesd", alpha = 0.05, verbose = TRUE)
yield_anomalousruns <- subset(yield_anomalies$anomalized_tbl, anomaly == "Yes") 
mad_all_yield <- mad(data$`yield (kg/ha)`, na.rm = TRUE)

p_yield_anomalies3d <- plot_ly(yield_anomalousruns, x = ~c1, y = ~c2, z = ~p, color = ~crop, colors = colors) %>%
  add_markers() %>%
  layout(title = 'Runs with Anomalous Values in Yield',
         scene = list(xaxis = list(title = 'Land Price (c1)', range = c(-40, 50)),
                      yaxis = list(title = 'Fertilizer Price (c2)', range = c(-40, 50)),
                      zaxis = list(title = 'Market Crop Price (p)', range = c(-40, 50))),
         annotations = list(
           x = 1.13,
           y = 1.05,
           text = 'Crop',
           xref = 'paper',
           yref = 'paper',
           showarrow = FALSE
         ))
p_yield_anomalies3d
link_yield_anomalies3d = api_create(p_yield_anomalies3d, filename="yield_anomalies3d")
link_yield_anomalies3d


# production anomalies
prod_anomalies <- anomalize(data = data, target = `production (kg)`, method = "gesd", alpha = 0.05, verbose = TRUE)
prod_anomalousruns <- subset(prod_anomalies$anomalized_tbl, anomaly == "Yes")
mad_all_prod <- mad(data$`production (kg)`, na.rm = TRUE)

# plot production anomalies
p_prod_anomalies3d <- plot_ly(prod_anomalousruns, x = ~c1, y = ~c2, z = ~p, color = ~crop, colors = colors) %>%
  add_markers() %>%
  layout(title = 'Runs with Anomalous Values in Production',
         scene = list(xaxis = list(title = 'Land Price (c1)', range = c(-40, 50)),
                      yaxis = list(title = 'Fertilizer Price (c2)', range = c(-40, 50)),
                      zaxis = list(title = 'Market Crop Price (p)', range = c(-40, 50))),
         annotations = list(
           x = 1.13,
           y = 1.05,
           text = 'Crop',
           xref = 'paper',
           yref = 'paper',
           showarrow = FALSE
         ))
p_prod_anomalies3d
link_prod_anomalies3d = api_create(p_prod_anomalies3d, filename="prod_anomalies3d")
link_prod_anomalies3d


# fertilizer anomalies
fert_anomalies <- anomalize(data = data, target = `Nfert (kg/ha)`, method = "iqr", alpha = 0.05, verbose = TRUE)
fert_anomalousruns <- subset(fert_anomalies$anomalized_tbl, anomaly == "Yes")
mad_all_fert <- mad(data$`Nfert (kg/ha)`, na.rm = TRUE)

# plot fertilizer anomalies
p_fert_anomalies3d <- plot_ly(fert_anomalousruns, x = ~c1, y = ~c2, z = ~p, color = ~crop, colors = colors) %>%
  add_markers() %>%
  layout(title = 'Runs with Anomalous Values in Fertilizer',
         scene = list(xaxis = list(title = 'Land Price (c1)', range = c(-40, 50)),
                      yaxis = list(title = 'Fertilizer Price (c2)', range = c(-40, 50)),
                      zaxis = list(title = 'Market Crop Price (p)', range = c(-40, 50))),
         annotations = list(
           x = 1.13,
           y = 1.05,
           text = 'Crop',
           xref = 'paper',
           yref = 'paper',
           showarrow = FALSE
         ))
p_fert_anomalies3d
link_fert_anomalies3d = api_create(p_fert_anomalies3d, filename="fert_anomalies3d")
link_fert_anomalies3d

# join tables of anomalous runs to find overlaps, such as records that have both anomalous yield and anomalous fert values
#resource: https://stat545.com/bit001_dplyr-cheatsheet.html#semi_joinsuperheroes-publishers
#semi_join(x, y): Return all rows from x where there are matching values in y, keeping just columns from x. A semi join differs from an inner join because an inner join will return one row of x for each matching row of y, where a semi join will never duplicate rows of x. This is a filtering join.

# x = land with 101 obs, y = prod with 1000 obs, returns 33 obs indicating overlapping anomalies in land and prod  
semi_land_prod <- semi_join(land_anomalousruns, prod_anomalousruns)
# x = land with 101 obs, y = fert with 162 obs, returns 0 obs indicating land anomalies are not duplicates of fert anomlaies 
semi_land_fert <- semi_join(land_anomalousruns, fert_anomalousruns)
# x = land with 101 obs, y = yield with 775 obs, returns 0 obs indicating land anomalies are not duplicates of yield anomlaies 
semi_land_yield <- semi_join(land_anomalousruns, yield_anomalousruns)

# x = yield with 775 obs, y = land with 101 obs, returns 0 obs indicating no overlapping anomalies in yield and land with land 
semi_yield_land <- semi_join(yield_anomalousruns, land_anomalousruns)
# x = yield with 775 obs, y = fert with 162 obs, returns 127 obs indicating anomalies in yield and fert 
semi_yield_fert <- semi_join(yield_anomalousruns, fert_anomalousruns)
# x = semi_yield_fert with 775 obs, y = prod with 1000 obs, returns 364 obs indicating anomalies in yield and prod
semi_yield_prod <- semi_join(yield_anomalousruns, prod_anomalousruns)

# x = prod with 1000 obs, y = yield with 775 obs, returns 364 obs indicating overlapping anomalies in prod and yield 
semi_prod_yield <- semi_join(prod_anomalousruns, yield_anomalousruns)
# x = prod with 1000 obs, y = fert with 162 obs, returns 53 obs indicating overlapping anomalies in prod and fert 
semi_prod_fert <- semi_join(prod_anomalousruns, fert_anomalousruns)
# x = prod with 1000 obs, y = land with 101 obs, returns 33 obs indicating anomalies in yield and fert 
semi_prod_land <- semi_join(prod_anomalousruns, land_anomalousruns)

# x = fert with 162 obs, y = prod with 1000 obs, returns 53 obs indicating overlapping anomalies in fert and prod  
semi_fert_prod <- semi_join(fert_anomalousruns, prod_anomalousruns)
# x = fert with 162 obs, y = land with 101 obs, returns 0 obs indicating land anomalies are not duplicates of fert anomlaies 
semi_fert_land <- semi_join(fert_anomalousruns, land_anomalousruns)
# x = fert with 162 obs, y = yield with 775 obs, returns 127 obs indicating anomalies in  fert and yield
semi_fert_yield <- semi_join(fert_anomalousruns, yield_anomalousruns)

# x = semi_prod_yield with 364 obs, y = fert with 162 obs, returns 42 obs indicating overlapping anomalies in prod, yield, and fert 
semi_prod_yield_fert <- semi_join(semi_prod_yield, fert_anomalousruns)
# x = semi_prod_yield_fert with 42 obs, y = land with 101 obs, returns 0 obs indicating no overlapping anomalies in prod, yield, fert with land 
semi_prod_yield_fert_land <- semi_join(semi_prod_yield_fert, land_anomalousruns)

# x = semi_yield_land with 53 obs, y = semi_prod_land with 33 obs, returns 0 obs indicating land anomalies are not uniquie between yield and prod anomlaies 
semi_yield_land_prod_land <- semi_join(semi_yield_land, semi_prod_land)

#inner_join(x, y): Return all rows from x where there are matching values in y, and all columns from x and y. If there are multiple matches between x and y, all combination of the matches are returned. This is a mutating join.



# full_join(x, y): Return all rows and all columns from both x and y. Where there are not matching values, returns NA for the one missing. This is a mutating join.
# combines yield and land anomalies since there is no overlap identified
full_yield_land <- full_join(yield_anomalousruns,land_anomalousruns)
# x = join_yield_land with 775 obs, y = fert with 162 obs, join_anomalousrun ends up with 

# combines yield, land, and fert anomalies into 1 table
fulljoin_unique_anomalousruns <- full_join(full_yield_land, fert_anomalousruns)

# plot all anomalies
p_allunique_anomalies3d <- plot_ly(fulljoin_unique_anomalousruns, x = ~c1, y = ~c2, z = ~p, color = ~crop, colors = colors) %>%
  add_markers() %>%
  layout(title = 'All Unique Runs with Anomalous Values',
         scene = list(xaxis = list(title = 'Land Price (c1)', range = c(-40, 50)),
                      yaxis = list(title = 'Fertilizer Price (c2)', range = c(-40, 50)),
                      zaxis = list(title = 'Market Crop Price (p)', range = c(-40, 50))),
         annotations = list(
           x = 1.13,
           y = 1.05,
           text = 'Crop',
           xref = 'paper',
           yref = 'paper',
           showarrow = FALSE
         ))
p_allunique_anomalies3d
link_allunique_anomalies3d = api_create(p_allunique_anomalies3d, filename="allunique_anomalies3d")
link_allunique_anomalies3d


