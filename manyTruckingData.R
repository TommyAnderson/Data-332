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


dfTrucks <- rbind(dfTruck0001,dfTruck0369,dfTruck1226,dfTruck1442,
            dfTruck1478,dfTruck1539,dfTruck1796)

dfLocations <- dfTrucks %>%
  group_by(Truck.ID) %>%
  summarize(unique_locations = n_distinct(c(Starting.Location, Delivery.Location)))

dfTotalMilesDriven <- dfTrucks %>%
  group_by(Truck.ID) %>%
  summarize(Total_Miles = sum(Odometer.Ending - Odometer.Beginning))

dfMerged <- dfTotalMilesDriven %>%
  inner_join(dfPay, by = "Truck.ID") %>%
  mutate(Total_Pay = labor_per_mil * Total_Miles)


ggplot(dfMerged, aes(x = reorder(Truck.ID, Total_Pay), y = Total_Pay, fill = factor(Truck.ID))) +
  geom_bar(stat = "identity") +
  labs(title = "Total Pay Per Trucker", x = "Truck ID", y = "Total Pay ($)") +
  theme_minimal() +
  theme(legend.position = "none")

