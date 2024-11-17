###################################################
##       Generate performance metrics plots      ##
##            for simulation experiment          ##
##                                               ##
##               Anthony Charsley                ##
##                 October 2024                  ##
###################################################



rm(list=ls())



##############
#  Packages  #
##############

library(tidyverse)



#################
#  Directories  #
#################

EM_path <- paste0(getwd(), "/Models/EMs")
res_path <- file.path(getwd(),"Results","Simulation_experiment_results")
dir.create(res_path, showWarnings = F)



############################
# Load performance metrics #
############################

perf_OM_1a <- readRDS(file.path(EM_path, "Perf_metrics_OM_1a.RDS"))
perf_OM_2a <- readRDS(file.path(EM_path, "Perf_metrics_OM_2a.RDS"))
perf_OM_3a <- readRDS(file.path(EM_path, "Perf_metrics_OM_3a.RDS"))
perf_OM_3b <- readRDS(file.path(EM_path, "Perf_metrics_OM_3b.RDS"))
perf_OM_4a <- readRDS(file.path(EM_path, "Perf_metrics_OM_4a.RDS"))



##############
# Build data #
##############

# Operating models
scenarios <- c("OM_1a", 
               "OM_2a", 
               "OM_3a", "OM_3b",
               "OM_4a")

# Estimating models
data_sample <- c("sample_rand1n",
                 "sample_rand2n",
                 "sample_rand5n",
                 
                 "sample_unsuithab1n",
                 "sample_unsuithab2n",
                 "sample_unsuithab5n",
                 
                 "sample_nearroads1n",
                 "sample_nearroads2n",
                 "sample_nearroads5n",
                 
                 "sample_unsuithab_nearroads1n",
                 "sample_unsuithab_nearroads2n",
                 "sample_unsuithab_nearroads5n")

pm_list <- list()

start_time <- Sys.time()
for(om in scenarios){ #om = "OM_1a"
  
  print(om)
  
  if(om == "OM_1a"){df <- perf_OM_1a ; OM_lab <- "Random generation (1n)"} 
  if(om == "OM_2a"){df <- perf_OM_2a ; OM_lab <- "Unsuitable habitat (1n)"} 
  if(om == "OM_3a"){df <- perf_OM_3a ; OM_lab <- "Near roads (1n)"} 
  if(om == "OM_3b"){df <- perf_OM_3b ; OM_lab <- "Near roads (5n)"} 
  if(om == "OM_4a"){df <- perf_OM_4a ; OM_lab <- "Unsuitable habitat and near roads (1n)"} 
  
  for(em in data_sample){ #em = "sample_rand1n"
    
    if(em == "sample_rand1n"){EM_lab <- "Random generation (1n)"}
    if(em == "sample_rand2n"){EM_lab <- "Random generation (2n)"}
    if(em == "sample_rand5n"){EM_lab <- "Random generation (5n)"}
    
    if(em == "sample_unsuithab1n"){EM_lab <- "Unsuitable habitat (1n)"}
    if(em == "sample_unsuithab2n"){EM_lab <- "Unsuitable habitat (2n)"}
    if(em == "sample_unsuithab5n"){EM_lab <- "Unsuitable habitat (5n)"}
    
    if(em == "sample_nearroads1n"){EM_lab <- "Near roads (1n)"}
    if(em == "sample_nearroads2n"){EM_lab <- "Near roads (2n)"}
    if(em == "sample_nearroads5n"){EM_lab <- "Near roads (5n)"}
    
    if(em == "sample_unsuithab_nearroads1n"){EM_lab <- "Unsuitable habitat and near roads (1n)"}
    if(em == "sample_unsuithab_nearroads2n"){EM_lab <- "Unsuitable habitat and near roads (2n)"}
    if(em == "sample_unsuithab_nearroads5n"){EM_lab <- "Unsuitable habitat and near roads (5n)"}
    
    ## Build pm data
    x <- 100-length(df$RMSE[[em]])
    pm_df <- data.frame("Operating_model" = rep(OM_lab, 100),
                        "Estimating_model" = rep(EM_lab, 100),
                        "RMSE" = c(unlist(df$RMSE[[em]]), rep(NA,x)),
                        "Bias" = c(unlist(df$Bias[[em]]), rep(NA,x)),
                        "Spread" = c(unlist(df$Spread[[em]]), rep(NA,x)),
                        "Bias_JT" = c(unlist(df$Bias_JT[[em]]), rep(NA,x)),
                        "Coverage50" = c(unlist(df$Coverage_50[[em]]), rep(NA,x)),
                        "Coverage80" = c(unlist(df$Coverage_80[[em]]), rep(NA,x)),
                        "Coverage95" = c(unlist(df$Coverage_95[[em]]), rep(NA,x))#,
                        #"AUC" = c(unlist(df$AUC[[em]]), rep(NA,x))
                        )
    
    
    pm_list[[paste0(om,"_",em)]] <- pm_df
  }
  
  rm(df); rm(EM_lab)
  
}
time = Sys.time() - start_time ; print(paste0("Performance metrics run time: ", time))



#################################
# Fix up data for use in ggplot #
#################################

pm_all_df <- do.call(rbind, pm_list)

# Convert to factors and set leveling
#table(pm_all_df$Operating_model, is.na(pm_all_df$Bias), useNA = "ifany")
pm_all_df$Operating_model <- factor(pm_all_df$Operating_model, 
                                    levels = c("Random generation (1n)","Unsuitable habitat (1n)",
                                               "Near roads (1n)","Near roads (5n)",
                                               "Unsuitable habitat and near roads (1n)"))
#table(pm_all_df$Operating_model, is.na(pm_all_df$Bias), useNA = "ifany")

#table(pm_all_df$Estimating_model, is.na(pm_all_df$Bias), useNA = "ifany")
pm_all_df$Estimating_model <- factor(pm_all_df$Estimating_model,
                                     levels = c("Random generation (1n)",
                                                "Random generation (2n)",
                                                "Random generation (5n)",
                                                
                                                "Unsuitable habitat (1n)",
                                                "Unsuitable habitat (2n)",
                                                "Unsuitable habitat (5n)",
                                                
                                                "Near roads (1n)",
                                                "Near roads (2n)",
                                                "Near roads (5n)",
                                                
                                                "Unsuitable habitat and near roads (1n)",
                                                "Unsuitable habitat and near roads (2n)",
                                                "Unsuitable habitat and near roads (5n)"))
#table(pm_all_df$Estimating_model, is.na(pm_all_df$Bias), useNA = "ifany")

## Change names of variables
pm_all_df <- pm_all_df %>%
  rename("Operating model" = "Operating_model", "Estimating model" = "Estimating_model",
         "Coverage 50%" = "Coverage50", "Coverage 80%" = "Coverage80", "Coverage 95%" = "Coverage95")

## Remove NAs
pm_all_df <- pm_all_df[complete.cases(pm_all_df),]



##################
# Build boxplots #
##################

## RMSE
rmse_plot <- pm_all_df %>%
  ggplot(aes(x=`Operating model`, y=RMSE, fill=`Estimating model`)) +
  geom_boxplot() #+
  #geom_hline(yintercept=0, linetype="dashed")

ggsave(filename = file.path(res_path, "RMSE_boxplots.jpeg"), rmse_plot, 
       width = 30, height = 15, units = "cm")

# pm_all_df %>%
#   ggplot(aes(x=Estimating_model, y=RMSE, fill=Estimating_model)) +
#   geom_boxplot() +
#   facet_wrap(~Operating_model)


## Bias
bias_plot <- pm_all_df %>%
  ggplot(aes(x=`Operating model`, y=Bias, fill=`Estimating model`)) +
  geom_boxplot() +
  geom_hline(yintercept=0, linetype="dashed")

ggsave(filename = file.path(res_path, "Bias_boxplots.jpeg"), bias_plot, 
       width = 30, height = 15, units = "cm")


## Spread
spread_plot <- pm_all_df %>%
  ggplot(aes(x=`Operating model`, y=Spread, fill=`Estimating model`)) +
  geom_boxplot() +
  geom_hline(yintercept=1, linetype="dashed")

ggsave(filename = file.path(res_path, "Spread_boxplots.jpeg"), spread_plot, 
       width = 30, height = 15, units = "cm")


## Coverage - 50
coverage50_plot <- pm_all_df %>%
  ggplot(aes(x=`Operating model`, y=`Coverage 50%`, fill=`Estimating model`)) +
  geom_boxplot() +
  geom_hline(yintercept=50, linetype="dashed")

ggsave(filename = file.path(res_path, "Coverage50_boxplots.jpeg"), coverage50_plot, 
       width = 30, height = 15, units = "cm")


## Coverage - 80
coverage80_plot <- pm_all_df %>%
  ggplot(aes(x=`Operating model`, y=`Coverage 80%`, fill=`Estimating model`)) +
  geom_boxplot() +
  geom_hline(yintercept=80, linetype="dashed")

ggsave(filename = file.path(res_path, "Coverage80_boxplots.jpeg"), coverage80_plot, 
       width = 30, height = 15, units = "cm")

## Coverage - 95
coverage95_plot <- pm_all_df %>%
  ggplot(aes(x=`Operating model`, y=`Coverage 95%`, fill=`Estimating model`)) +
  geom_boxplot() +
  geom_hline(yintercept=95, linetype="dashed")

ggsave(filename = file.path(res_path, "Coverage95_boxplots.jpeg"), coverage95_plot, 
       width = 30, height = 15, units = "cm")

## Bias - JT
bias_plot <- pm_all_df %>%
  ggplot(aes(x=`Operating model`, y=Bias_JT, fill=`Estimating model`)) +
  geom_boxplot() +
  geom_hline(yintercept=1, linetype="dashed")

ggsave(filename = file.path(res_path, "Bias_JT_boxplots.jpeg"), bias_plot, 
       width = 30, height = 15, units = "cm")



# ## AUC
# AUC_plot <- pm_all_df %>%
#   ggplot(aes(x=`Operating model`, y=AUC, fill=`Estimating model`)) +
#   geom_boxplot() +
#   geom_hline(yintercept=1, linetype="dashed")
# 
# ggsave(filename = file.path(res_path, "AUC_boxplots.jpeg"), AUC_plot, 
#        width = 30, height = 15, units = "cm")
