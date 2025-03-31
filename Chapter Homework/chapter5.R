library(readxl)
rm(list = ls())
setwd("C:/Users/tomma/Data332/Chapters/")

deck <- read.csv("deck.csv")
deck2 <- deck

vec <- c(0, 0, 0, 0, 0, 0)
vec
vec[1]
vec[1] <- 1000
vec
vec[c(1, 3, 5)] <- c(1, 1, 1)
vec
vec[4:6] <- vec[4:6] + 1
vec
vec[7] <- 0
vec

deck2$new <- 1:52
head(deck2)
deck2$new <- NULL
head(deck2)
deck2[c(13, 26, 39, 52), ]
deck2[c(13, 26, 39, 52), 3]
deck2$value[c(13, 26, 39, 52)]

deck2$value[c(13, 26, 39, 52)] <- 14
head(deck2, 13)
# deck3 <- shuffle(deck)
# Needs shuffle function from chapter 4

vec[c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)]
1 > 2
1 > c(0, 1, 2)
c(1, 2, 3) == c(3, 2, 1)
1 %in% c(3, 4, 5)
#p 81