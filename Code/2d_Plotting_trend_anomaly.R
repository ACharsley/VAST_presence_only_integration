###################################################
#                Trend anomaly                    #
#                                                 #
#               Anthony Charsley                  #
#               19th March 2024                   #
###################################################


rm(list=ls())



##############
#  Packages  #
##############

library(plyr)
library(tidyverse)



####################
#  Call functions  #
####################

source(paste0(getwd(), "/Code/funcs.R"))


########################
#  Modelling scenario  #
########################

#scenario <- "1a" #"2a" "3a" "3b" "4a"

sce = c("Taranaki data", "1a", "2a", "3a", "3b", "4a")
data_list <- list()

## Loop over all scenarios ##
for(scenario in sce){
  
  print(scenario)
  
  ##############
  # Model path #
  ##############
  
  #Set model path
  model_path <- paste0(getwd(), "/Models")
  
  #Set path
  if(scenario == "Taranaki data"){
    path <- file.path(model_path, "Taranaki_data_model")
  }else{
    path <- file.path(model_path, paste0("Model_", scenario))
  }
  
  #Figures path
  path_figs <- file.path(path, "Figures")
  
  
  
  ###################
  #  Load model fit #
  ###################
  
  #Model fit
  load(file.path(path, "Fit.RData"))
  
  
  
  #############
  # Make plot #
  #############
  
  POE <- Fit$Report$R1_gct[,1,]
  yrs <- Fit$year_labels
  
  #POEs averaged
  POE_ave_s <- colMeans(POE)
  #plot(POE_ave_s~yrs, type='l')
  
  POE_ave_s_t <-mean(colMeans(POE))
  
  #Anomaly
  I_t <- POE_ave_s - POE_ave_s_t
  #plot(I_t~yrs, type='l')
  
  # Add to dataframe and list
  I_t_df <- data.frame("Years" = yrs, "Anomaly" = I_t, "Scenario" = scenario)
  data_list[[scenario]] <- I_t_df
  
  # Plot
  trend_anomaly_plot <- ggplot(I_t_df, aes(x = Years, y=Anomaly, group = 1)) + 
    geom_line( color="steelblue",linewidth=1) + 
    geom_point() +
    geom_hline(yintercept = 0) +
    xlab("Year") + ylab("Trend anomaly") +
    scale_x_continuous(breaks = seq(1978, 2022, 2), minor_breaks = seq(1978, 2022, 1)) +
    scale_y_continuous(breaks = seq(round_any(min(I_t),0.1,f=floor), round_any(max(I_t),0.1,f=ceiling), 0.05),
                       minor_breaks = seq(round_any(min(I_t),0.1,f=floor), round_any(max(I_t),0.1,f=ceiling), 0.05)) +
    #ggtitle("") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          axis.text = element_text(size = rel(1.25)),
          axis.title = element_text(size = rel(1.5)),
          plot.title = element_text(size = rel(1.5)))
  
  ggsave(file.path(path_figs, "Trend_anomaly.jpeg"), trend_anomaly_plot)

}


I_t_df_all <- do.call(rbind, data_list)

