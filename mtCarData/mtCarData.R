library(dplyr)
library(ggplot2)
rm(list = ls())

cyl_counts <- table(mtcars$cyl)

barplot(cyl_counts,
        main = "Number of Cars by Cylinder Count",
        xlab = "Number of Cylinders",
        ylab = "Count of Cars",
        col = "skyblue",
        border = "black")

mtcars$cyl <- as.factor(mtcars$cyl)
ggplot(mtcars, aes(x = cyl, fill = cyl)) +
  geom_bar() +
  labs(title = "Number of Cars by Cylinder Count",
       x = "Number of Cylinders",
       y = "Count of Cars") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()

geom_bar(stat = "identity")

mpg_by_cyl <- aggregate(mpg ~ cyl, data = mtcars, FUN = mean)

ggplot(mpg_by_cyl, aes(x = cyl, y = mpg, fill = cyl)) +
  geom_bar(stat = "identity") +
  labs(title = "Average MPG by Cylinder Count",
       x = "Number of Cylinders",
       y = "Mean MPG") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  geom_text(aes(label = round(mpg, 1)), vjust = -0.5)

ggplot(mpg_by_cyl, aes(x = cyl, y = mpg, fill = cyl)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(mpg, 1)), vjust = -0.5) +
  labs(title = "Average MPG by Cylinder Count",
       x = "Number of Cylinders",
       y = "Mean MPG") +
  theme_minimal()







