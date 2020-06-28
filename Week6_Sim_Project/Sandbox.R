library(broom)
library(plotly)
library(data.table)
setwd("C:/gitprojects/UIUC_MCSDS_STAT420/Week6_Sim_Project")
getwd()
x_predictors_df <- read.csv("study_1.csv")

group <- c(1:7)

brn <- sample(1:10,20,replace = T)
period <- c(101:120)

df1[CJ(group, period), on = .(period)]
