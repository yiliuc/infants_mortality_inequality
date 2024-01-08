# Import the data_cleaning.R so that we have the analysed data in this paper
source("codes/data_cleaning.R")

# Import all the libraries used in this paper
library(janitor)
library(tidyverse)
library(dplyr)
library(knitr)
library(kableExtra)
library(broom)
library(yaml)
library(ggplot2)

## -----------------------------------------------------------------------------------------------

# Fit the OLS regression model in this paper
data$White <- ifelse(data$race == "White", 1, 0)
data$HighIncome <- ifelse(data$pctile > 50, 1, 0)
data$White_HighIncome <- data$White * data$HighIncome
model <- lm(crude_rate ~ pctile*race + pctile*HighIncome + pctile*White_HighIncome, data = data)


## -----------------------------------------------------------------------------------------------

# Convert regression summary to a tidy data frame
tidy_model <- tidy(model)

# Add stars to estimates
tidy_model$estimate <- round(tidy_model$estimate, 3)
tidy_model$std.error <- round(tidy_model$std.error, 3)
tidy_model$statistic <- round(tidy_model$statistic, 3)
tidy_model$p.value <- round(tidy_model$p.value, 3)

## -----------------------------------------------------------------------------------------------

data$fitted_values <- predict(model, newdata = data)
plot1 <- ggplot(data, aes(x = pctile, y = crude_rate, color = race)) +
  stat_summary(fun = mean, geom = "point", size = 1) +
  geom_line(data = data[data$pctile <= 50, ], 
            aes(y = fitted_values, group = race), size = 1) +
  geom_line(data = data[data$pctile > 50, ], 
            aes(y = fitted_values, group = race), size = 1) +
  geom_vline(aes(xintercept = 50), 
             linetype = "dashed", color = "black", size = 1) +
  labs(title = "Figure 1: Average infant mortality rate by race across different household \nincome levels",
       x = "Income Percentile",
       y = "Infants Mortality per 100,000") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("Black or African American" = "#F8766D", 
                                "White" = "#00BFC4"),
                     name = "Race")