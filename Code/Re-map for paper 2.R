###################################################
##  Species distribution maps for longfin eel    ##
##              in the Taranaki region           ##
##                                               ##
##               Anthony Charsley                ##
##                 August 2023                   ##
###################################################

rm(list=ls())



##############
#  Packages  #
##############

library(tidyverse)
library(plyr)
library(ggpubr)



#################
#  Directories  #
#################

VAST_input_data_dir <- paste0(getwd(), "/Data_processed/VAST_input_data")



####################
#  Call functions  #
####################

source(paste0(getwd(), "/Code/funcs.R"))



########################
#  Modelling scenario  #
########################

network_type <- "full"
SE_switch <- "SE_off"

#scenario <- "Taranaki data" #"1a" "1b" "2a" "2b" "3a" "3b" "4a" "4b"
scenario <- c("Taranaki data", "1a", "1b", "2a", "2b", "3a", "3b", "4a", "4b")

print(scenario)

POE_upper_lim <- vector()
yr <- which(c(1978:2022) == 2022)

for(sce in scenario){ #sce = "Taranaki data"
  ##############
  # Model path #
  ##############
  
  #Set model path
  model_path <- paste0(getwd(), "/Models")
  
  #Set path
  if(sce == "Taranaki data"){
    if(network_type == "downstream"){
      path <- file.path(model_path, "Taranaki_data_model_ds")
    }
    if(network_type == "full"){
      path <- file.path(model_path, "Taranaki_data_model")
    }
  }else{
    if(network_type == "downstream"){
      path <- file.path(model_path, paste0("Model_", sce, "_ds"))
    }
    if(network_type == "full"){
      path <- file.path(model_path, paste0("Model_", sce))
    }
  }
  
  #If building uncertainty maps then add to path
  if(SE_switch == "SE_on"){
    path <- paste0(path, "_SE")
  }
  
  
  
  ##################
  # Load model fit #
  ##################
  
  load(file.path(path, "Fit.RData"))
  
  plot_xct <- plot_maps_network(plot_set = c(1), 
                                make_plot=F,
                                fit = Fit, 
                                Sdreport = Fit$parameter_estimates$SD, 
                                TmbData = Fit$data_list, 
                                spatial_list = Fit$spatial_list, 
                                DirName = path, 
                                Panel = "category", 
                                PlotName = "POE_lf_yearly_v3",
                                PlotTitle = "",
                                cex = 0.5, 
                                Zlim = c(0,1), 
                                arrows=T, 
                                pch=15)
  
  if(sce == "Taranaki data"){
    #saveRDS(plot_xct, file.path(path, paste0("POE_array_xct_struc_only.rds")))
    
    assign("Fit_struc_only", Fit)
    assign("plot_xct_struc_only", plot_xct)
  }else{
    #saveRDS(plot_xct, file.path(path, paste0("POE_array_xct_",sce,".rds")))
    
    assign(paste0("Fit_", sce), Fit)
    assign(paste0("plot_xct_", sce), plot_xct)
  }
  
  POE_upper_lim <- c(POE_upper_lim, max(Fit$Report$R1_gct[,1,yr]))
  
  rm(Fit) ; rm(plot_xct)
  
  
}



#######################
# Make 2022 POE plots #
#######################


#For river tracing in plot, use structured data network, but all the same
Network_sz_EN <- data.frame('parent_s'=Fit_struc_only$data_list$parent_s, 'child_s'=Fit_struc_only$data_list$child_s, Fit_struc_only$spatial_list$latlon_g)
l2 <- lapply(1:nrow(Network_sz_EN), function(x){
  parent <- Network_sz_EN$parent_s[x]
  find <- Network_sz_EN %>% filter(child_s == parent)
  if(nrow(find)>0) out <- cbind.data.frame(Network_sz_EN[x,], 'long2'=find$Lon, 'lat2'=find$Lat)
  if(nrow(find)==0) out <- cbind.data.frame(Network_sz_EN[x,], 'long2'=NA, 'lat2'=NA)
  return(out)
})
l2 <- do.call(rbind, l2)


scenario_new <- c("struc_only", "1a", "1b", "2a", "2b", "3a", "3b", "4a", "4b")
zlim_inp <- c(0, round_any(max(POE_upper_lim), 0.1, f=ceiling))



for (sce in scenario_new) { #sce = "struc_only"
  
  Fit <- get(paste0("Fit_", sce))
  xct <- get(paste0("plot_xct_", sce))
  
  
  plot_xct <- data.frame('value'=xct[,1,yr], 'year'="2022", Fit$spatial_list$latlon_g)
  
  #Set limits for plotting
  Xlim = c(min(plot_xct$Lon),max(plot_xct$Lon))
  Ylim = c(min(plot_xct$Lat),max(plot_xct$Lat))
  
  ## Plot
  p <- ggplot(plot_xct)
  p <- p + geom_segment(data=l2, aes(x = Lon,y = Lat, xend = long2, yend = lat2), col="gray92")
  
  p <- p +
    geom_point(aes(x = Lon, y = Lat, color = value), cex = 1, pch = 19) +
    scale_color_distiller(palette = "RdYlGn", limits = zlim_inp, direction = 1) +
    coord_cartesian(xlim = Xlim, ylim = Ylim) +
    #scale_x_continuous(breaks=quantile(plot_xct$Lon, prob=c(0,0.25,0.5,0.75,1)), labels=round(quantile(plot_xct$Lon, prob=c(0,0.25,0.5,0.75,1)),1)) +
    # guides(color=guide_legend(title=plot_codes[plot_num])) +
    #mytheme() +
    theme_bw(base_size = 14) +
    theme(legend.title = element_blank(),
          axis.text = element_text(size = rel(1.5)),
          axis.title=element_text(size = rel(1.5),face="bold"),
          axis.text.x = element_text(angle = 90, size = rel(1)),
          axis.text.y = element_text(size = rel(1)),
          legend.text=element_text(size = rel(1.1))) +
    xlab("Longitude") + ylab("Latitude")
  
  if(sce != "struc_only"){ 
    
    labels = c("Unstructured data",
               "Random generation (Equal)", "Random generation (5x)",
               "At unsuitable habitat (Equal)", "At unsuitable habitat (5x)",
               "Near roads (Equal)", "Near roads (5x)",
               "At unsuitable habitat and near roads (Equal)", "At unsuitable habitat and near roads (5x)")
    
    p <- p + 
      theme(legend.key.size = unit(1.5, "cm"), 
            legend.text=element_text(size = rel(1.8)))
    
    # if(sce %in% c("1a","1b","2a","2b")){
    #   p <- p + 
    #     theme(legend.key.size = unit(1.5, "cm"), 
    #           legend.text=element_text(size = rel(1.8))) +
    #     geom_label(label=labels[which(scenario_new %in% sce)],
    #                x=174, y=-38.75, label.size=1.5)
    # }
    # if(sce %in% c("3a","3b")){
    #   p <- p + 
    #     theme(legend.key.size = unit(1.5, "cm"), 
    #           legend.text=element_text(size = rel(1.8))) +
    #     geom_label(label=labels[which(scenario_new %in% sce)],
    #                x=173.85, y=-38.75, label.size=1.5)
    # }
    # if(sce %in% c("4a","4b")){
    #   p <- p + 
    #     theme(legend.key.size = unit(1.5, "cm"), 
    #           legend.text=element_text(size = rel(1.8))) +
    #     geom_label(label=labels[which(scenario_new %in% sce)],
    #                x=174.2, y=-38.75, label.size=1.5)
    # }
    
    }
  
  assign(paste0("Catchment_plot_", sce), p)
  
  rm(p)
  
}


#Save maps to directory
res_dir <- file.path(getwd(), "Results")
dir.create(res_dir, showWarnings = F)

ggsave(file.path(res_dir, "POE_2022_struc_only.png"), Catchment_plot_struc_only, width=8,height=8)

catchmaps <- ggarrange(Catchment_plot_1a + rremove("xlab") + rremove("x.text") + rremove("x.ticks"),
                       Catchment_plot_1b + rremove("xylab") + rremove("xy.text") + rremove("y.ticks") + rremove("x.ticks"),
                       Catchment_plot_2a + rremove("xlab") + rremove("x.text") + rremove("x.ticks"),
                       Catchment_plot_2b + rremove("xylab") + rremove("xy.text") + rremove("y.ticks") + rremove("x.ticks"),
                       Catchment_plot_3a + rremove("xlab") + rremove("x.text") + rremove("x.ticks"),
                       Catchment_plot_3b + rremove("xylab") + rremove("xy.text") + rremove("y.ticks") + rremove("x.ticks"),
                       Catchment_plot_4a,
                       Catchment_plot_4b + rremove("ylab") + rremove("y.text") + rremove("y.ticks"),
                       labels = c("Random generation (Equal)", "Random generation (5x)",
                                  "At unsuitable habitat (Equal)", "At unsuitable habitat (5x)",
                                  "Near roads (Equal)", "Near roads (5x)",
                                  "At unsuitable habitat and\n    near roads (Equal)", "At unsuitable habitat and\n      near roads (5x)"),
                       label.x = c(0.02,-0.095,
                                   0.01,-0.12,
                                   0.09,-0.04,
                                   #-0.15,-0.3
                                   0.05,-0.11),
                       label.y = c(0.98,0.98,0.98,0.98,0.98,0.98,0.99,0.99),
                       font.label = list(size = 16),
                       #align = "hv",
                       ncol = 2, nrow = 4,
                       widths = c(1.2,1), heights = c(1,1,1,1.3), 
                       common.legend = TRUE, legend = "right")

ggsave(file.path(res_dir, "POE_2022.png"), catchmaps, height = 20, width = 15)



