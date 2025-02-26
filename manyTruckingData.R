# get all the shit
# for the truckers multiple
# answer all questions from other one
# show trucker pay in bar?? chart

library(ggplot2)
library(dplyr)
library(tidyverse)
library(readxl)
library(tidyr)

rm(list = ls())

setwd("C:/Users/tomma/Data332/")

dfTruck0001 <- read_excel("TruckerData/truck data 0001.xlsx", sheet = 2, skip = 3, .name_repair = 'universal')
dfTruck0369 <- read_excel("TruckerData/truck data 0369.xlsx", sheet = 2, skip = 3, .name_repair = 'universal')
dfTruck1226 <- read_excel("TruckerData/truck data 1226.xlsx", sheet = 2, skip = 3, .name_repair = 'universal')
dfTruck1442 <- read_excel("TruckerData/truck data 1442.xlsx", sheet = 2, skip = 3, .name_repair = 'universal')
dfTruck1478 <- read_excel("TruckerData/truck data 1478.xlsx", sheet = 2, skip = 3, .name_repair = 'universal')
dfTruck1539 <- read_excel("TruckerData/truck data 1539.xlsx", sheet = 2, skip = 3, .name_repair = 'universal')
dfTruck1796 <- read_excel("TruckerData/truck data 1769.xlsx", sheet = 2, skip = 3, .name_repair = 'universal')
dfPay <- read_excel("TruckerData/Driver Pay Sheet.xlsx",.name_repair = 'universal')


df <- rbind(dfTruck0001,dfTruck0369,dfTruck1226,dfTruck1442,
            dfTruck1478,dfTruck1539,dfTruck1796)

dfStartingPivot <- df %>%
  group_by(Truck.ID) %>%
  summarize(count = n())

dfTotalMilesDriven <- df %>%
  group_by(Truck.ID) %>%
  summarize(totalMiles = sum(Odometer.Ending - Odometer.Beginning))


dfTotalPayDriver <- dfTotalMilesDriven %>%
  inner_join(dfPay, by = c("Truck.ID" = "Truck.ID")) %>%
  mutate(totalPay = labor_per_mil * totalMiles)


