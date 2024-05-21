###################################################
#                Trend anomaly                    #
#                                                 #
#               Anthony Charsley                  #
#                  May 2024                       #
###################################################


rm(list=ls())



##############
#  Packages  #
##############

library(tidyverse)


########################
#  Modelling scenario  #
########################

scenario <- "3a"


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

## Historic baseline period ##

POE <- Fit$Report$R1_gct[,1,]
yrs <- Fit$year_labels

#POEs averaged
POE_ave_s <- colMeans(POE)
#plot(POE_ave_s~yrs, type='l')

POE_ave_s_t <-mean(colMeans(POE))

#Anomaly
I_t <- POE_ave_s - POE_ave_s_t
#plot(I_t~yrs, type='l')
####

## Future scenario 1 - increasing trend ##
yrs_future1 <- c(2023:2100)
I_t_future1 <- c(I_t[45] + seq(from=0.01, to=0.6, length.out=length(yrs_future1))+rnorm(n=length(yrs_future1) ,mean=0.01,sd=0.1))

# Add to dataframe and list
I_t_df_future1 <- data.frame("Years" = c(yrs,yrs_future1), "Anomaly" = c(I_t,I_t_future1))

# Plot
trend_anomaly_plot_future1 <- ggplot(I_t_df_future1, aes(x = Years, y=Anomaly, group = 1)) + 
  geom_line( color="steelblue",linewidth=1) + 
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 2024, colour="red", linetype='dashed') +
  xlab("Year") + ylab("Trend anomaly") +
  #scale_x_continuous(breaks = seq(1978, 2022, 2), minor_breaks = seq(1978, 2022, 1)) +
  #scale_y_continuous(breaks = seq(round_any(min(I_t),0.1,f=floor), round_any(max(I_t),0.1,f=ceiling), 0.05),
  #                   minor_breaks = seq(round_any(min(I_t),0.1,f=floor), round_any(max(I_t),0.1,f=ceiling), 0.05)) +
  #ggtitle("") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text = element_text(size = rel(1.25)),
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5)))

ggsave(file.path(path_figs, "Trend_anomaly_future1.jpeg"), trend_anomaly_plot_future1)


####


## Future scenario 2 - stable trend ##
yrs_future2 <- c(2023:2100)
I_t_future2 <- c(rnorm(n=length(yrs_future2) ,mean=0,sd=0.05))

# Add to dataframe and list
I_t_df_future2 <- data.frame("Years" = c(yrs,yrs_future2), "Anomaly" = c(I_t,I_t_future2))

# Plot
trend_anomaly_plot_future2 <- ggplot(I_t_df_future2, aes(x = Years, y=Anomaly, group = 1)) + 
  geom_line( color="steelblue",linewidth=1) + 
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 2024, colour="red", linetype='dashed') +
  xlab("Year") + ylab("Trend anomaly") +
  #scale_x_continuous(breaks = seq(1978, 2022, 2), minor_breaks = seq(1978, 2022, 1)) +
  #scale_y_continuous(breaks = seq(round_any(min(I_t),0.1,f=floor), round_any(max(I_t),0.1,f=ceiling), 0.05),
  #                   minor_breaks = seq(round_any(min(I_t),0.1,f=floor), round_any(max(I_t),0.1,f=ceiling), 0.05)) +
  #ggtitle("") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text = element_text(size = rel(1.25)),
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5)))

ggsave(file.path(path_figs, "Trend_anomaly_future2.jpeg"), trend_anomaly_plot_future2)


####


## Future scenario 3 - decreasing trend ##
yrs_future3 <- c(2023:2100)
I_t_future3 <- c(I_t[45] + seq(from=0.01, to=-0.6, length.out=length(yrs_future3))+rnorm(n=length(yrs_future3) ,mean=0.01,sd=0.1))

# Add to dataframe and list
I_t_df_future3 <- data.frame("Years" = c(yrs,yrs_future3), "Anomaly" = c(I_t,I_t_future3))

# Plot
trend_anomaly_plot_future3 <- ggplot(I_t_df_future3, aes(x = Years, y=Anomaly, group = 1)) + 
  geom_line( color="steelblue",linewidth=1) + 
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 2024, colour="red", linetype='dashed') +
  xlab("Year") + ylab("Trend anomaly") +
  #scale_x_continuous(breaks = seq(1978, 2022, 2), minor_breaks = seq(1978, 2022, 1)) +
  #scale_y_continuous(breaks = seq(round_any(min(I_t),0.1,f=floor), round_any(max(I_t),0.1,f=ceiling), 0.05),
  #                   minor_breaks = seq(round_any(min(I_t),0.1,f=floor), round_any(max(I_t),0.1,f=ceiling), 0.05)) +
  #ggtitle("") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text = element_text(size = rel(1.25)),
        axis.title = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(1.5)))

ggsave(file.path(path_figs, "Trend_anomaly_future3.jpeg"), trend_anomaly_plot_future3)


####

