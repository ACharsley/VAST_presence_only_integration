

rm(list=ls())



##############
#  Packages  #
##############

library(tidyverse)



#################
#  Directories  #
#################

VAST_input_data_dir <- paste0(getwd(), "/Data_processed/VAST_input_data")
fig_dir <- paste0(getwd(), "/Data_processed/Figures")
covariate_plot_dir <- file.path(fig_dir, "Covariate_plots")
data_taranaki_dir <- "./Data_processed"



########################################################
#  Load VAST input data and raw network covariate data #
########################################################

network_type = "full"

if(network_type == "downstream"){
  VAST_input_data <- readRDS(file.path(VAST_input_data_dir, "VAST_input_data_ds.rds"))
}
if(network_type == "full"){
  VAST_input_data <- readRDS(file.path(VAST_input_data_dir, "VAST_input_data.rds"))
}

raw_network_covs <- readRDS(file.path(data_taranaki_dir, "Taranaki_network.rds"))



####################################
#  Covariate data and network data #
####################################

# Covariate data
covariate_df <- VAST_input_data[["Taranaki data"]]$covariate_data
covariate_df <- as.data.frame(covariate_df)
covars_all <- colnames(covariate_df)[4:ncol(covariate_df)]

#raw covariate data
REC_covs <- c("loc_elev", "FWENZ_SegRipShade", "MeanFlowCumecs", "FWENZ_segSubstrate", "local_twarm")
raw_covs <- raw_network_covs %>%
  select(Lat, Lon, all_of(REC_covs))


#Network data
network = VAST_input_data[["Taranaki data"]]$network


#####################
#  Covariate plots  #
#####################

# Processed covariate data
for(cov in covars_all){# cov = "Barrier_present"
  
  print(cov)
  
  data_to_plot <- covariate_df %>%
    mutate("Covariate" = covariate_df[,cov])
  
  if(cov %in% c("std_Years_since_barrier", "Barrier_present")){
    
    catchmap4 <- ggplot(data_to_plot) +
      geom_point(aes(x = Lon, y = Lat, col=Covariate), alpha=0.6) +
      facet_wrap(~Year) +
      scale_colour_distiller(palette = "RdYlGn", direction = -1) +
      xlab("Longitude (°E)") + ylab("Latitude (°N)") +
      #ggtitle(paste0("Catchment map of ", cov)) + 
      #theme_bw(base_size = 14)
      labs(colour = "") +
      theme(axis.title=element_text(size = rel(1.5),face="bold"),
            axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1, size=6),
            axis.text.y=element_text(size=6),
            legend.text=element_text(size = rel(1.5)))
    ggsave(file.path(covariate_plot_dir, paste0("Final_covariate_map - ", cov,".png")), catchmap4)
    
  }else{
    
    catchmap5 <- ggplot(data_to_plot) +
      geom_point(aes(x = Lon, y = Lat, col=Covariate), alpha=0.6) +
      scale_colour_distiller(palette = "RdYlGn", direction = 1) +
      xlab("Longitude (°E)") + ylab("Latitude (°N)") +
      #ggtitle(paste0("Catchment map of ", cov)) + 
      labs(colour = "") +
      theme_bw(base_size = 14) +
      theme(axis.text = element_text(size = rel(1)),
            axis.title=element_text(size = rel(1.5),face="bold"),
            axis.text.x = element_text(angle = 90),
            legend.text=element_text(size = rel(1.5)))
    ggsave(file.path(covariate_plot_dir, paste0("Final_covariate_map - ", cov,".png")), catchmap5)
    
  }
}
