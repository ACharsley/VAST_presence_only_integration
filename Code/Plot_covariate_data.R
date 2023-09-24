

rm(list=ls())



##############
#  Packages  #
##############

library(tidyverse)



#################
#  Directories  #
#################

VAST_input_data_dir <- paste0(getwd(), "/Data_processed/VAST_input_data")
fig_dir <- paste0(getwd(), "/Data_processed/Figures/Covariate_plots")
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
  
  data_to_plot <- covariate_df %>%
    mutate("Covariate" = covariate_df[,cov])
  
  catchmap <- ggplot(data_to_plot) +
    #geom_point(data=network, aes(x = Lon, y = Lat), col="gray") +
    geom_point(aes(x = Lon, y = Lat, col=Covariate), alpha=0.6) +
    facet_wrap(~Year) +
    scale_colour_distiller(palette = "RdYlGn") +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle(paste0("Catchment map of ", cov)) + 
    #guides(col=guide_legend(title="")) +
    theme_bw(base_size = 14)
  if(network_type == "downstream"){ggsave(file.path(fig_dir, paste0("Covariate_map_yr_ds - ", cov,".png")), catchmap)}
  if(network_type == "full"){ggsave(file.path(fig_dir, paste0("Covariate_map_yr - ", cov,".png")), catchmap)}
  
  
  catchmap2 <- ggplot(data_to_plot) +
    #geom_point(data=network, aes(x = Lon, y = Lat), col="gray") +
    geom_point(aes(x = Lon, y = Lat, col=Covariate), alpha=0.6) +
    scale_colour_distiller(palette = "RdYlGn") +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle(paste0("Catchment map of ", cov)) + 
    #guides(col=guide_legend(title="")) +
    theme_bw(base_size = 14)
  if(network_type == "downstream"){ggsave(file.path(fig_dir, paste0("Covariate_map_ds - ", cov,".png")), catchmap2)}
  if(network_type == "full"){ggsave(file.path(fig_dir, paste0("Covariate_map - ", cov,".png")), catchmap2)}
  
}

#Raw covariate data
for(cov in REC_covs){# cov = "loc_elev"
  
  data_to_plot <- raw_covs %>%
    mutate("Covariate" = raw_covs[,cov])
  
  catchmap3 <- ggplot(data_to_plot) +
    geom_point(aes(x = Lon, y = Lat, col=Covariate), alpha=0.6) +
    scale_colour_distiller(palette = "RdYlGn") +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle(paste0("Catchment map of ", cov)) + 
    theme_bw(base_size = 14)
  if(network_type == "downstream"){ggsave(file.path(fig_dir, paste0("Raw_covariate_map_ds - ", cov,".png")), catchmap3)}
  if(network_type == "full"){ggsave(file.path(fig_dir, paste0("Raw_covariate_map - ", cov,".png")), catchmap3)}
  
}
