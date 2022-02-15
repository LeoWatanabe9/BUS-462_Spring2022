############################################################
# BUS 462 | Spring 2021 
# Group Project 
# Team Omega
# I pledge on my honor that I have neither received nor given unauthorized assistance on this deliverable.
############################################################

#### PREAMBLE : ## Clearing buffers ####
cat("\014")  # Clear Console
rm(list = ls(all.names = TRUE))# clear all
gc()

# libraries
require(data.table)
require(stargazer)
require(ggplot2)
require(PerformanceAnalytics)
require(dyplr)
require(pastecs)
require(pscl)
require(nnet)
require(ggcorrplot)
require(jtools)
require(ggstance)
require(broom.mixed)
require(sqldf)
require(tidyr)

#load data
lap_times <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/lap_times.csv")
pit_stops <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/pit_stops.csv")
qualifying <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/qualifying.csv")
constructor_results <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/constructor_results.csv")
constructor_standings <-fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/constructor_standings.csv")
driver_standings <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/driver_standings.csv")
races <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/races.csv")
results <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/results.csv")
status <- fread("https://raw.githubusercontent.com/C-H-Y-I-N-G/BUS462/main/data/status.csv")
# races table replace seasons table
# driver standings table replace drivers table
# constructor results and constructor standings replaces constructors table

# organize races and results tables for merging
races <- subset(races, select = -c(round,name,date,time,url))#drop columns
results <- subset(results, select = -c(positionText,positionOrder,time,number))
names(results)[names(results) == "position"] <- "finishing_position"
names(results)[names(results) == "points"] <- "finishing_points"
names(results)[names(results) == "milliseconds"] <- "finishing_milliseconds"
names(results)[names(results) == "laps"] <- "lap_finished"
dt <- merge(races,results,by="raceId") #merge 2 data frame

# merge status table
dt <- merge(dt,status,by="statusId")

# organize driver standings tables for merging
driver_standings <- subset(driver_standings, select = -c(positionText))
names(driver_standings)[names(driver_standings) == "position"] <- "season_position"
names(driver_standings)[names(driver_standings) == "points"] <- "season_points"
dt <- merge(dt,driver_standings,by=c("driverId","raceId"))

# organize constructor standings and constructor results tables for merging
constructor_standings <- subset(constructor_standings, select = -c(positionText))
names(constructor_standings)[names(constructor_standings) == "position"] <- "constructor_position"
names(constructor_standings)[names(constructor_standings) == "points"] <- "constructor_points"
names(constructor_standings)[names(constructor_standings) == "wins"] <- "constructor_wins"
constructor_results <- subset(constructor_results, select = -c(status))
names(constructor_results)[names(constructor_results) == "points"] <- "constructorResults_points"
dt <- merge(dt,constructor_standings, by=c("constructorId","raceId"))
dt <- merge(dt,constructor_results, by=c("constructorId","raceId"))

# organize qualifying tables for merging
#include position and qs for models 
#NOTE: changed q1-3 times to milliseconds in Excel
qualifying <- subset(qualifying, select = -c(number,q1,q2,q3,q1_formatted,q2_formatted,q3_formatted))
names(qualifying)[names(qualifying) == "position"] <- "qualifying_position"
#code to omit nas without losing all qualifying positions that did not make it to round 3
#first convert to int
qualifying$q1_milliseconds <- as.integer(qualifying$q1_milliseconds)
qualifying$q2_milliseconds <- as.integer(qualifying$q2_milliseconds)
qualifying$q3_milliseconds <- as.integer(qualifying$q3_milliseconds)

#convert na to 0 for qmean calc
qualifying$q1_milliseconds[is.na(qualifying$q1_milliseconds)] <- 0
qualifying$q2_milliseconds[is.na(qualifying$q2_milliseconds)] <- 0
qualifying$q3_milliseconds[is.na(qualifying$q3_milliseconds)] <- 0


#calculate qmean
qualifying$qmean <- (qualifying$q1_milliseconds+qualifying$q2_milliseconds+qualifying$q3_milliseconds)/3
dt <- merge(dt,qualifying,by=c("driverId","raceId","constructorId"))

# organize pit stop tables for merging
pit_stops <- subset(pit_stops, select = -c(duration,time))#duration has same number as milliseconds
names(pit_stops)[names(pit_stops) == "stop"] <- "pit_stop"
names(pit_stops)[names(pit_stops) == "lap"] <- "pit_stops_lap"
names(pit_stops)[names(pit_stops) == "milliseconds"] <- "pit_stops_milliseconds"
dt <- merge(dt,pit_stops, by=c("driverId","raceId"))

# organize lap times tables for merging
lap_times <- subset(lap_times, select = -c(time))
names(lap_times)[names(lap_times) == "position"] <- "lap_times_position" #could cut this
names(lap_times)[names(lap_times) == "lap"] <- "lap_times_lap"
names(lap_times)[names(lap_times) == "milliseconds"] <- "lap_times_milliseconds"
dt <- merge(dt,lap_times, by=c("driverId","raceId"))


dt[duplicated(dt)]#check duplication
dt[!duplicated(dt)]#remove duplication

dt <- subset(dt, year > 2014)#shrink the table to year 2015-2020

#DATA MERGING COMPLETE

#look at data
head(dt)
#View(dt)

#check data type of columns
str(dt)

#convert milliseconds from chr to num
dt$finishing_milliseconds <- as.numeric(dt$finishing_milliseconds) 

#convert columns to int
dt$rank <- as.integer(dt$rank)
dt$finishing_position <- as.integer(dt$finishing_position) #convert  to integer



#convert fastest lap speed
dt$fastestLapSpeed <- as.numeric(dt$fastestLapSpeed)

#Create new columns with seconds instead of milliseconds for easier interpretation of key variables
dt$pit_stops_seconds <- (dt$pit_stops_milliseconds)/1000
dt$lap_times_seconds <- (dt$lap_times_milliseconds)/1000
dt$finishing_seconds <- (dt$finishing_milliseconds)/1000

#create function that checks if any NAs are in a column
check_na <- function(my_col){
  any(is.na(my_col))
}

#apply function to each column in the set
apply(dt, 2, check_na)

#omit nas 
dt <- na.omit(dt)


#Create new variable for podium, will be primary dv
dt$podium <- ifelse(dt$finishing_position>3,0,1)


View(dt)#check completed table


