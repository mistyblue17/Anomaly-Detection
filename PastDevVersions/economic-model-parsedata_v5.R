library(readr)

wname <- getwd() # set back to working directory
dname <- paste(wname,"economicdata",sep="/") # will open data file in workikng directory
oname <- paste(wname,"outputs",sep="/") # will open outputs file in workikng directory
pname<-paste(wname,"plots",sep="/") # will open plots file in workikng directory
fname<-paste(wname,"functions",sep="/") # will open functions file in workikng directory

# Your first step is to read in the data 
setwd(dname) #tells the program to look in the data folder
economic_land_use <- read_csv("economicdata/economic-v5-stan-00df82c1-7ff4-4d3f-9875-289d40daad39/economic-land-use")
View(economic_land_use)
production_cost <- read_csv("economicdata/economic-v5-stan-00df82c1-7ff4-4d3f-9875-289d40daad39/production-cost")
View(production_cost)
price <- read_csv("economicdata/economic-v5-stan-00df82c1-7ff4-4d3f-9875-289d40daad39/production-cost")
View(price)
sim_production_cost <- read_csv("economicdata/economic-v5-stan-00df82c1-7ff4-4d3f-9875-289d40daad39/sim-production-cost")
View(sim_production_cost)
sim_price <- read_csv("economicdata/economic-v5-stan-00df82c1-7ff4-4d3f-9875-289d40daad39/sim-price")
View(sim_price)
supply_elasticity <- read_csv("economicdata/economic-v5-stan-00df82c1-7ff4-4d3f-9875-289d40daad39/supply-elasticity")
View(supply_elasticity)
land_input <- read_csv("economicdata/economic-v5-stan-00df82c1-7ff4-4d3f-9875-289d40daad39/land-input")
View(land_input)
cycles_data <- read_csv("economicdata/economic-v5-stan-00df82c1-7ff4-4d3f-9875-289d40daad39/cycles-data")
View(cycles_data)

dat.files  <- list.files(path=dname,
                         recursive=T,
                         pattern="economic-land-use"
                         ,full.names=T)
readDatFile <- function(f) {
  dat.fl <- read.csv(f)
}
data.files <- sapply(dat.files, readDatFile)
View(data.files)
