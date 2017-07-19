library(DT)
library(shiny)
library(googleVis)
library(dplyr)
library(ggplot2)
library(readr)
library(leaflet)
library(leaflet.extras)
library(shinydashboard)
library(lubridate)


load("moto1.rda")

# 
# loading dataset
# moto1 <- read_csv("~/Downloads/shinyDashBoard/NYPD_Motor_Vehicle_Collisions.csv", col_types = cols(TIME = col_time(format = "%H:%M")))
# # converting column names to lowercase
# colnames(moto1) <- tolower(colnames(moto1))
# # Discarding columns who has maximum number of NA's and assign it to new variable
# moto1 <- select(moto1,c(1,2,3,4,5,6,8,9,11,12,13,14,15,16,17,18,19,24,25))
# # Removing all the rows who has NA's exist
# moto1 <- na.omit(moto1)
# # mutate
# moto1 <- moto1 %>% mutate(day = weekdays(as.Date(date,format = "%m/%d/%Y")),year = year(as.Date(date,format = "%m/%d/%Y")))
# moto1$date1 <- as.Date(moto1$date, format = "%m/%d/%Y")
# moto1 <- head(moto1,400000)
choice1 = unique(moto1$borough)
choice = unique(moto1$`vehicle type code 1`)
#
#
choice2 <- colnames(moto1)[-22]
factor <- unique(moto1$`contributing factor vehicle 1`)
