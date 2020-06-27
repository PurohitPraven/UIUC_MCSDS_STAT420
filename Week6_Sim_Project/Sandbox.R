library(broom)
library(plotly)
setwd("C:/gitprojects/UIUC_MCSDS_STAT420/Week6_Sim_Project")
getwd()
x_predictors_df <- read.csv("study_1.csv")

sample_size <- 25
mu <- 0
num_simul <- 2000
set.seed(5)

#Significant model
beta_0 <- 3
beta_1 <- 1
beta_2 <- 1
beta_3 <- 1

sim_f_stat <- rep(0,num_simul)
sim_f_stat_glance <- rep(0,num_simul)
sim_f_stat_anova <- rep(0,num_simul)
sim_pval <- rep(0,num_simul)
sim_calc_pval <- rep(0,num_simul)
sim_anova_pval <- rep(0,num_simul)
sim_r2s <- rep(0,num_simul)
sim_sig <- rep(0,num_simul)

sim_null_f_stat <- rep(0,num_simul)
sim_null_f_stat_glance <- rep(0,num_simul)
sim_null_f_stat_anova <- rep(0,num_simul)
sim_null_pval <- rep(0,num_simul)
sim_null_calc_pval <- rep(0,num_simul)
sim_null_anova_pval <- rep(0,num_simul)
sim_null_r2s <- rep(0,num_simul)
sim_null_sigma <- rep(0,num_simul)

sig <- 1
set.seed(19732103)

for (i in 1:num_simul){
  epsilon <- rnorm(sample_size, mean=mu, sd=sig)
  
  x_predictors_df$y <- beta_0 + x_predictors_df$x1*beta_1 + x_predictors_df$x2*beta_2 + x_predictors_df$x3*beta_3 + epsilon
  
  m <- lm(y~., data = x_predictors_df)
  sim_f_stat_glance[i] <- glance(m)$statistic
  sim_pval[i] <- glance(m)$p.value
  sim_r2s[i] <- glance(m)$r.squared
  
  
  x_predictors_df <- read.csv("study_1.csv")
  epsilon <- rnorm(sample_size, mean=mu, sd=sig)
  
  x_predictors_df$y <- beta_0 + epsilon
  
  m <- lm(y~., data = x_predictors_df)
  sim_null_f_stat_glance[i] <- glance(m)$statistic
  sim_null_pval[i] <- glance(m)$p.value
  sim_null_r2s[i] <- glance(m)$r.squared
  
}
  
  
print(paste("fstat = ", mean(sim_f_stat_glance), "pval = ", mean(sim_pval[i]), " r2 = ", sim_r2s[i]))
  
print(paste("Null model fstat = ", mean(sim_null_f_stat_glance), "pval = ", mean(sim_null_pval), " r2 = ", sim_null_r2s[i]))
  


[1] "fstat =  41.4126281782561 pval =  4.31128439001723e-10  r2 =  0.886429219744925"
>   
[1] "Null model fstat =  1.08658907597403 pval =  0.500369718463895  r2 =  0.0416981782554686"
> 


#df_1 <- data.frame("f_stat_value"=sim_f_stats_value,"f_stat_numdf"=sim_f_stats_numdf,"f_stat_dendf"=sim_f_stats_dendf, "pval"=sim_pvals, "r2"=sim_r2s)


# library(ggplot2)
# library(gridExtra)
# 
# ggplot(data = df_1, aes(x=df_1$f_stat_value)) + 
#   geom_histogram(aes(y=stat(density), color=I("orange")), binwidth = 2) +
#   ggtitle("fstat-Significant model-sigma 1") +
#   theme_linedraw() +
#   stat_function(fun = df, args = list(x=mean(df_1$f_stat_value), df1=mean(df_1$f_stat_numdf), df2=mean(df_1$f_stat_dendf)), geom = "line", col="red", size=2)
# 
# sig <- 5
# for (i in 1:num_simul){
#   epsilon <- rnorm(sample_size, mean=mu, sd=sig)
#   x_predictors_df$y <- beta_0 + x_predictors_df$x1*beta_1 + x_predictors_df$x2*beta_2 + x_predictors_df$x3*beta_3 + epsilon
#   m <- lm(y~., data = x_predictors_df)
#   sim_f_stats_value[i] <- summary(m)$fstatistic[1]
#   sim_f_stats_numdf[i] <- summary(m)$fstatistic[2]
#   sim_f_stats_dendf[i] <- summary(m)$fstatistic[3]
#   
#   sim_pvals[i] <- glance(m)$p.value
#   sim_r2s[i] <- glance(m)$r.squared
#   sim_df[i] <- glance(m)$df
# }
# df_1 <- data.frame("f_stat_value"=sim_f_stats_value,"f_stat_numdf"=sim_f_stats_numdf,"f_stat_dendf"=sim_f_stats_dendf, "pval"=sim_pvals, "r2"=sim_r2s)
# 
# ggplot(data = df_1, aes(x=df_1$f_stat_value)) + 
#   geom_histogram(aes(y=stat(density), color=I("orange")), binwidth = 2) +
#   ggtitle("fstat-Significant model-sigma 1") +
#   theme_linedraw() +
#   stat_function(fun = df, args = list(x=mean(df_1$f_stat_value), df1=mean(df_1$f_stat_numdf), df2=mean(df_1$f_stat_dendf)), geom = "line", col="red", size=2)
# 
# 
# sig <- 10
# for (i in 1:num_simul){
#   epsilon <- rnorm(sample_size, mean=mu, sd=sig)
#   x_predictors_df$y <- beta_0 + x_predictors_df$x1*beta_1 + x_predictors_df$x2*beta_2 + x_predictors_df$x3*beta_3 + epsilon
#   m <- lm(y~., data = x_predictors_df)
#   sim_f_stats_value[i] <- summary(m)$fstatistic[1]
#   sim_f_stats_numdf[i] <- summary(m)$fstatistic[2]
#   sim_f_stats_dendf[i] <- summary(m)$fstatistic[3]
#   
#   sim_pvals[i] <- glance(m)$p.value
#   sim_r2s[i] <- glance(m)$r.squared
#   sim_df[i] <- glance(m)$df
# }
# df_1 <- data.frame("f_stat_value"=sim_f_stats_value,"f_stat_numdf"=sim_f_stats_numdf,"f_stat_dendf"=sim_f_stats_dendf, "pval"=sim_pvals, "r2"=sim_r2s)
# 
# ggplot(data = df_1, aes(x=df_1$f_stat_value)) + 
#   geom_histogram(aes(y=stat(density), color=I("orange")), binwidth = 2) +
#   ggtitle("fstat-Significant model-sigma 1") +
#   theme_linedraw() +
#   stat_function(fun = df, args = list(x=mean(df_1$f_stat_value), df1=mean(df_1$f_stat_numdf), df2=mean(df_1$f_stat_dendf)), geom = "line", col="red", size=2)
