###################################################
#     Plotting spatially varying catchability     #
#                                                 #
#               Anthony Charsley                  #
#              14th February 2024                 #
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
  
  
  
  ############
  # Make map #
  ############
  
  
  # xct <- array(Fit$Report$Phi1_gk, dim = c(nrow(Fit$Report$Phi1_gk),ncol(Fit$Report$Phi1_gk),1))
  # 
  # plot_maps_network(Array_xct = xct, 
  #                   fit = Fit, 
  #                   Sdreport = Fit$parameter_estimates$SD, 
  #                   TmbData = Fit$data_list, 
  #                   spatial_list = Fit$spatial_list, 
  #                   DirName = path_figs, 
  #                   Panel = "category", 
  #                   category_names = c("Phi_NetTrap", "Phi_Presence_Pseudo-absence"),
  #                   year_labels = "",
  #                   PlotName = "Spatially_varying_catchability",
  #                   PlotTitle = "",
  #                   cex = 0.5, 
  #                   Zlim = c(-3,6), 
  #                   arrows=T, 
  #                   pch=15)
  
  
  
  
  #For river tracing in plot
  Network_sz_EN <- data.frame('parent_s'=Fit$data_list$parent_s, 'child_s'=Fit$data_list$child_s, Fit$spatial_list$latlon_g)
  l2 <- lapply(1:nrow(Network_sz_EN), function(x){
    parent <- Network_sz_EN$parent_s[x]
    find <- Network_sz_EN %>% filter(child_s == parent)
    if(nrow(find)>0) out <- cbind.data.frame(Network_sz_EN[x,], 'long2'=find$Lon, 'lat2'=find$Lat)
    if(nrow(find)==0) out <- cbind.data.frame(Network_sz_EN[x,], 'long2'=NA, 'lat2'=NA)
    return(out)
  })
  l2 <- do.call(rbind, l2)
  
  
  if(scenario == "Taranaki data"){
    
    xct <- data.frame("Lat" = Fit$spatial_list$latlon_g[,"Lat"], "Lon" = Fit$spatial_list$latlon_g[,"Lon"],
                      "Phi_Net_Trap" = Fit$Report$Phi1_gk[,1])
    
    
    #Set limits for plotting
    zlim_Net_Trap <- c(round_any(min(xct$Phi_Net_Trap), 1, f=floor), round_any(max(xct$Phi_Net_Trap), 1, f=ceiling)) #Phi_Net_Trap
    
    Xlim = c(min(xct$Lon),max(xct$Lon))
    Ylim = c(min(xct$Lat),max(xct$Lat))
    
    ## Phi_Net_Trap
    p <- ggplot(xct)
    p <- p + geom_segment(data=l2, aes(x = Lon,y = Lat, xend = long2, yend = lat2), col="gray92")
    
    p <- p +
      geom_point(aes(x = Lon, y = Lat, color = Phi_Net_Trap), cex = 1, pch = 19) +
      scale_color_distiller(palette = "RdYlBu", limits = zlim_Net_Trap, direction = -1) +
      coord_cartesian(xlim = Xlim, ylim = Ylim) +
      scale_x_continuous(breaks=quantile(xct$Lon, prob=c(0.1,0.5,0.9)), labels=round(quantile(xct$Lon, prob=c(0.1,0.5,0.9)),0)) +
      mytheme() +
      xlab("Longitude") + ylab("Latitude")
    
    #Save without title
    ggsave(file.path(path_figs, "SVC_map_Net_Trap.png"), p, width=8,height=8)
    
  }else{
    
    xct <- data.frame("Lat" = Fit$spatial_list$latlon_g[,"Lat"], "Lon" = Fit$spatial_list$latlon_g[,"Lon"],
                      "Phi_Net_Trap" = Fit$Report$Phi1_gk[,1], "Phi_P_PA" = Fit$Report$Phi1_gk[,2])
    
    
    #Set limits for plotting
    # zlim_Net_Trap <- c(round_any(min(xct$Phi_Net_Trap), 1, f=floor), round_any(max(xct$Phi_Net_Trap), 1, f=ceiling)) #Phi_Net_Trap
    # zlim_P_PA <- c(round_any(min(xct$Phi_P_PA), 1, f=floor), round_any(max(xct$Phi_P_PA), 1, f=ceiling)) #Phi_P_PA
    
    #Min/Max for both
    zlim_Net_Trap <- zlim_P_PA <- c(min(round_any(min(xct$Phi_Net_Trap), 1, f=floor), round_any(min(xct$Phi_P_PA), 1, f=floor)),
                                    max(round_any(max(xct$Phi_Net_Trap), 1, f=ceiling), round_any(max(xct$Phi_P_PA), 1, f=ceiling)))
    
    
    Xlim = c(min(xct$Lon),max(xct$Lon))
    Ylim = c(min(xct$Lat),max(xct$Lat))
    
    ## Phi_Net_Trap
    p <- ggplot(xct)
    p <- p + geom_segment(data=l2, aes(x = Lon,y = Lat, xend = long2, yend = lat2), col="gray92")
    
    p <- p +
      geom_point(aes(x = Lon, y = Lat, color = Phi_Net_Trap), cex = 1, pch = 19) +
      scale_color_distiller(palette = "RdYlBu", limits = zlim_Net_Trap, direction = -1) +
      coord_cartesian(xlim = Xlim, ylim = Ylim) +
      scale_x_continuous(breaks=quantile(xct$Lon, prob=c(0.1,0.5,0.9)), labels=round(quantile(xct$Lon, prob=c(0.1,0.5,0.9)),0)) +
      mytheme() +
      xlab("Longitude") + ylab("Latitude")
    
    #Save without title
    ggsave(file.path(path_figs, "SVC_map_Net_Trap.png"), p, width=8,height=8)
    
    
    ## Phi_P_PA
    p <- ggplot(xct)
    p <- p + geom_segment(data=l2, aes(x = Lon,y = Lat, xend = long2, yend = lat2), col="gray92")
    
    p <- p +
      geom_point(aes(x = Lon, y = Lat, color = Phi_P_PA), cex = 1, pch = 19) +
      scale_color_distiller(palette = "RdYlBu", limits = zlim_P_PA, direction = -1) +
      coord_cartesian(xlim = Xlim, ylim = Ylim) +
      scale_x_continuous(breaks=quantile(xct$Lon, prob=c(0.1,0.5,0.9)), labels=round(quantile(xct$Lon, prob=c(0.1,0.5,0.9)),0)) +
      mytheme() +
      xlab("Longitude") + ylab("Latitude")
    
    #Save without title
    ggsave(file.path(path_figs, "SVC_map_P_PA.png"), p, width=8,height=8)
    
  }
  
  
  
}
