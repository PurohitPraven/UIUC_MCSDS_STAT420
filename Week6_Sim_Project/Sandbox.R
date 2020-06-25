
setwd("C:/gitprojects/UIUC_MCSDS_STAT420/Week6_Sim_Project")
getwd()
x_predictors_df <- read.csv("study_1.csv")

sample_size <- 25
mu <- 0
num_simul <- 1#2000
set.seed(5)

#Significant model
beta_0 <- 3
beta_1 <- 1
beta_2 <- 1
beta_3 <- 1

sim_f_stats_value <- rep(0,num_simul)
sim_f_stats_numdf <- rep(0,num_simul)
sim_f_stats_dendf <- rep(0,num_simul)
sim_pvals <- rep(0,num_simul)
sim_r2s <- rep(0,num_simul)
sim_df <- rep(0,num_simul)
sig <- 1
#for (i in 1:num_simul){
  epsilon <- rnorm(sample_size, mean=mu, sd=sig)
  x_predictors_df$y <- beta_0 + x_predictors_df$x1*beta_1 + x_predictors_df$x2*beta_2 + x_predictors_df$x3*beta_3 + epsilon
  m <- lm(y~., data = x_predictors_df)
  m_null <- lm(y~1, data = x_predictors_df)
  sim_f_stats_value[i] <- summary(m)$fstatistic[1]
  sim_f_stats_numdf[i] <- summary(m)$fstatistic[2]
  sim_f_stats_dendf[i] <- summary(m)$fstatistic[3]
  
  sim_pvals[i] <- glance(m)$p.value
  sim_r2s[i] <- glance(m)$r.squared
  sim_df[i] <- glance(m)$df
#}
df_1 <- data.frame("f_stat_value"=sim_f_stats_value,"f_stat_numdf"=sim_f_stats_numdf,"f_stat_dendf"=sim_f_stats_dendf, "pval"=sim_pvals, "r2"=sim_r2s)


library(ggplot2)
library(gridExtra)

ggplot(data = df_1, aes(x=df_1$f_stat_value)) + 
  geom_histogram(aes(y=stat(density), color=I("orange")), binwidth = 2) +
  ggtitle("fstat-Significant model-sigma 1") +
  theme_linedraw() +
  stat_function(fun = df, args = list(x=mean(df_1$f_stat_value), df1=mean(df_1$f_stat_numdf), df2=mean(df_1$f_stat_dendf)), geom = "line", col="red", size=2)

sig <- 5
for (i in 1:num_simul){
  epsilon <- rnorm(sample_size, mean=mu, sd=sig)
  x_predictors_df$y <- beta_0 + x_predictors_df$x1*beta_1 + x_predictors_df$x2*beta_2 + x_predictors_df$x3*beta_3 + epsilon
  m <- lm(y~., data = x_predictors_df)
  sim_f_stats_value[i] <- summary(m)$fstatistic[1]
  sim_f_stats_numdf[i] <- summary(m)$fstatistic[2]
  sim_f_stats_dendf[i] <- summary(m)$fstatistic[3]
  
  sim_pvals[i] <- glance(m)$p.value
  sim_r2s[i] <- glance(m)$r.squared
  sim_df[i] <- glance(m)$df
}
df_1 <- data.frame("f_stat_value"=sim_f_stats_value,"f_stat_numdf"=sim_f_stats_numdf,"f_stat_dendf"=sim_f_stats_dendf, "pval"=sim_pvals, "r2"=sim_r2s)

ggplot(data = df_1, aes(x=df_1$f_stat_value)) + 
  geom_histogram(aes(y=stat(density), color=I("orange")), binwidth = 2) +
  ggtitle("fstat-Significant model-sigma 1") +
  theme_linedraw() +
  stat_function(fun = df, args = list(x=mean(df_1$f_stat_value), df1=mean(df_1$f_stat_numdf), df2=mean(df_1$f_stat_dendf)), geom = "line", col="red", size=2)


sig <- 10
for (i in 1:num_simul){
  epsilon <- rnorm(sample_size, mean=mu, sd=sig)
  x_predictors_df$y <- beta_0 + x_predictors_df$x1*beta_1 + x_predictors_df$x2*beta_2 + x_predictors_df$x3*beta_3 + epsilon
  m <- lm(y~., data = x_predictors_df)
  sim_f_stats_value[i] <- summary(m)$fstatistic[1]
  sim_f_stats_numdf[i] <- summary(m)$fstatistic[2]
  sim_f_stats_dendf[i] <- summary(m)$fstatistic[3]
  
  sim_pvals[i] <- glance(m)$p.value
  sim_r2s[i] <- glance(m)$r.squared
  sim_df[i] <- glance(m)$df
}
df_1 <- data.frame("f_stat_value"=sim_f_stats_value,"f_stat_numdf"=sim_f_stats_numdf,"f_stat_dendf"=sim_f_stats_dendf, "pval"=sim_pvals, "r2"=sim_r2s)

ggplot(data = df_1, aes(x=df_1$f_stat_value)) + 
  geom_histogram(aes(y=stat(density), color=I("orange")), binwidth = 2) +
  ggtitle("fstat-Significant model-sigma 1") +
  theme_linedraw() +
  stat_function(fun = df, args = list(x=mean(df_1$f_stat_value), df1=mean(df_1$f_stat_numdf), df2=mean(df_1$f_stat_dendf)), geom = "line", col="red", size=2)
