## ----setup, include=FALSE------------------------------------------------------------------------------------------
#knitr::opts_chunk$set(echo = TRUE, warning=FALSE, message=FALSE)
setwd("/Users/bf2383/Downloads")
knitr::purl("PSY503_lab2.Rmd")


## ----eval=FALSE----------------------------------------------------------------------------------------------------
## install.packages('tinytex')
## tinytex::install_tinytex()
## # to uninstall TinyTeX, run tinytex::uninstall_tinytex()


## ------------------------------------------------------------------------------------------------------------------
library(tidyverse)


## ------------------------------------------------------------------------------------------------------------------
compute_SS <- function(x) {
  n <- length(x)
  mean_x <- mean(x)
  sum_squared_diff <- sum((x - mean_x)^2)
  return(sum_squared_diff)
}
vector_a <- c(5, 5, 5, 5, 5)
vector_b <- c(1, 3, 5, 7, 9)



## ----plots---------------------------------------------------------------------------------------------------------
library(datasauRus)
library(MetBrewer)
lab2_datasaurus<- datasaurus_dozen

# Produce graphs for the different changes below

#a r plot using facets in ggplot
ggplot(lab2_datasaurus, aes(x = x, y = y)) +
  geom_point() +
  facet_wrap(~ dataset) +
  theme_minimal()

#b plotting each dataset in different color
ggplot(lab2_datasaurus, aes(x = x, y = y, color = dataset)) +
  geom_point() +
  facet_wrap(~ dataset) +
  theme_minimal() +
  scale_color_met_d(name = "Archambault")

#c plotting in different shades of grey
ggplot(lab2_datasaurus, aes(x = x, y = y, color = dataset)) +
  geom_point() +
  facet_wrap(~ dataset) +
  theme_minimal() +
  scale_colour_grey(start = 0.2, end = 0.8)

#d changing shape of plotted points
ggplot(lab2_datasaurus, aes(x = x, y = y, color = dataset, shape = dataset)) +
  scale_shape_manual(values = 1:13) +
  geom_point() +
  facet_wrap(~ dataset) +
  theme_minimal() +
  scale_colour_grey(start = 0.2, end = 0.8)

#e box plot
ggplot(lab2_datasaurus, aes(x = dataset, y = y)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1.5)) 



## ------------------------------------------------------------------------------------------------------------------
mean_datasaurus <- lab2_datasaurus %>% 
    group_by(dataset) %>% 
    summarize(
      mean_x    = mean(x)
    )

print (mean_datasaurus)


## ------------------------------------------------------------------------------------------------------------------
summary_datasaurus <- lab2_datasaurus %>%
  group_by(dataset) %>%
  summarize(
    mean_y = mean(y),
    median_x = median(x),
    median_y = median(y),
    variance_x = var(x),
    variance_y = var(y),
    correlation_xy = cor(x, y)
  )
print(summary_datasaurus)


## ------------------------------------------------------------------------------------------------------------------
# calculating the mean of x and y
mean_values <- lab2_datasaurus %>%
  group_by(dataset) %>%
  summarize(
    mean_x = mean(x),
    mean_y = mean(y)
  )
ggplot(lab2_datasaurus, aes(x = x, y = y)) +
  geom_point() + # Scatter plot of the data points
  geom_point(data = mean_values, aes(x = mean_x, y = mean_y), color = "magenta", size = 5) + 
  facet_wrap(~ dataset) +
  theme_minimal() +
  labs(
    x = "X values",
    y = "Y values"
  )


## ------------------------------------------------------------------------------------------------------------------
knitr::purl("/Users/bf2383/Downloads/PSY503_lab2.Rmd", output = "PSY503_lab2.R")

