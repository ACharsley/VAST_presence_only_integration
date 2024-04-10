###################################################
##        Build VAST model inputs for the        ##
##       		        case study  		             ##
##                                               ##
##               Anthony Charsley                ##
##                  March 2023                   ##
###################################################


rm(list=ls())


##############
#  Packages  #
##############

library(tidyverse)
library(VAST)
library(ggpubr)


#################
#  Directories  #
#################

raw_data_dir <- "./Data_raw"
data_taranaki_dir <- "./Data_processed"
fig_dir <- "./Data_processed/Figures"

VAST_input_data_dir <- "./Data_processed/VAST_input_data"
dir.create(VAST_input_data_dir, showWarnings = F)


#################################
# Scenarios to build inputs for #
#################################

scenarios <- c("Taranaki data", 
               "1a", "1b", "1c", "1d", #"OM_1a", "OM_1b",
               "2a", "2b", "2c", "2d", #"OM_2a", "OM_2b",
               "3a", "3b", "3c", "3d", #"OM_3a", "OM_3b",
               "4a", "4b", "4c", "4d") #"OM_4a", "OM_4b")
network_type <- "full"

VAST_input_data <- list()

# Loop over all scenarios
for(sce in scenarios){ #sce = "Taranaki data" #sce = "1a"
  
  print(sce)
  
  #############
  # Load data #
  #############
  
  if(sce == "Taranaki data"){
    data_list <- readRDS(file.path(data_taranaki_dir, "Taranaki_data.rds"))
  }else{
    data_list <- readRDS(file.path(data_taranaki_dir, paste0("Taranaki_data_", sce, ".rds")))
  }
  
  if(network_type == "downstream"){
  obs <- data_list$obs_ds
  network <- data_list$network_ds
  covariate_df <- data_list$covariate_df_ds
  }
  if(network_type == "full"){
  obs <- data_list$obs
  network <- data_list$network
  covariate_df <- data_list$covariate_df
  }
  
  
  ########################
  # Set data up for VAST #
  ########################
  
  ## 1. Examine data type variable
  fct_count(obs$Data_source)

  ## 2. Examine fishing method
  fct_count(obs$FishMethod)

  ## 3. Examine fish samplers
  fct_count(obs$org) #Will assume catchability is constant between organisations
  
  #Examine fishing method and fish samplers together
  addmargins(table(obs$FishMethod, obs$org, useNA = "ifany"))
  
  ## 4. Examine Years of data
  addmargins(table(obs$`Anguilla dieffenbachii`, obs$Year))
  #addmargins(table(obs$Year, obs$`Anguilla dieffenbachii`))
  
  addmargins(table(obs$Year, obs$Data_source)) #Some years, data type doesn't overlap. Is that an issue?
  
  ## 5. Create final dataset
  #Data for longfin eel catch
  Data_Geostat = data.frame(Lon=obs$Lon,
                            Lat=obs$Lat,
                            Child_i=obs$child_i,
                            Year=obs$Year,
                            Catch_KG=obs$`Anguilla dieffenbachii`, 
                            Data_source=obs$Data_source,
                            Sampler = obs$org,
                            FishMethod = obs$FishMethod,
                            Length = obs$dist_i) 
  
  set.seed(22)
  Data_Geostat[,'Catch_KG'] = Data_Geostat[,'Catch_KG'] * exp(1e-3*rnorm(nrow(Data_Geostat)))
  
  ##Final data set
  #pander::pandoc.table( Data_Geostat[1:6,], digits=6 ) #table of the first 6 observations
  
  
  ########################
  #  Habitat covariates  #
  ########################
  
  # All covariate names
  #covars_all <- colnames(covariate_df)[!(colnames(covariate_df) %in% c("Lon","Lat","Year"))]
  yrs <- min(Data_Geostat$Year):max(Data_Geostat$Year)
  
  covariate_df <- covariate_df %>% filter(Year %in% yrs) # This ensures that if any years are dropped then covariate_df has the right years still
  
  ###############
  ###############
  
  ## Save data ##
  VAST_input_data[[sce]]$Data_Geostat <- Data_Geostat
  VAST_input_data[[sce]]$network <- network
  VAST_input_data[[sce]]$covariate_data <- covariate_df
  
}

if(network_type == "downstream"){
  saveRDS(VAST_input_data, file.path(VAST_input_data_dir, "VAST_input_data_ds.rds"))
  }
if(network_type == "full"){
  saveRDS(VAST_input_data, file.path(VAST_input_data_dir, "VAST_input_data.rds"))
  }







###############################################
###############################################





#re-load data if needed
#VAST_input_data <- readRDS(file.path(VAST_input_data_dir, "VAST_input_data.rds"))
#network_type = "full"


#########
# Plots #
#########

Data_to_plot <- VAST_input_data[["1a"]]$Data_Geostat
Data_to_plot$present <- ifelse(round(Data_to_plot$Catch_KG)==1, "Encounter", "Non-encounter")


if(network_type == "downstream"){
  network <- VAST_input_data[["1a"]]$network_ds
}
if(network_type == "full"){
  network <- VAST_input_data[["1a"]]$network
}


## Load full network for plots
netfull <- readRDS(file.path(raw_data_dir, "NZ_network.rds"))

#New Zealand map
nzmap <- ggplot() +
  geom_point(data = netfull, aes(x = long, y = lat), pch = ".") +
  geom_point(data = network, aes(x = Lon, y = Lat), color = "red", pch = ".") +
  xlab("Longitude (°E)") + ylab("Latitude (°N)") +
  theme_bw(base_size = 14)

if(network_type == "downstream"){
  ggsave(file.path(fig_dir, "Taranaki_on_NZ_ds.png"), nzmap, height = 6, width = 5)
  }
if(network_type == "full"){
  ggsave(file.path(fig_dir, "Taranaki_on_NZ.png"), nzmap, height = 6, width = 5)
  }








###################
# Plot e/n-e data #
###################

Data_to_plot_no_pseudo_absences <- VAST_input_data[["1a"]]$Data_Geostat
Data_to_plot_no_pseudo_absences <- Data_to_plot_no_pseudo_absences %>% filter(!(Data_source == "Unstructured" & Catch_KG == 0))
Data_to_plot_no_pseudo_absences$present <- ifelse(round(Data_to_plot_no_pseudo_absences$Catch_KG)==1, "Present", "Absent")


network <- VAST_input_data$`Taranaki data`$network #any network will do

l2 <- lapply(1:nrow(network), function(x){
  parent <- network$parent_s[x]
  find <- network %>% filter(child_s == parent)
  if(nrow(find)>0) out <- cbind.data.frame(network[x,], 'Lon2'=find$Lon, 'Lat2'=find$Lat)
  if(nrow(find)==0) out <- cbind.data.frame(network[x,], 'Lon2'=NA, 'Lat2'=NA)
  
  return(out)
})
l2 <- do.call(rbind, l2)



#Taranaki network by year
tab <- table(Data_to_plot_no_pseudo_absences$present, Data_to_plot_no_pseudo_absences$Year)
years <- unique(Data_to_plot$Year)[order(unique(Data_to_plot$Year))]
data_text <- data.frame("Year"= years, label=paste0(tab[1,], "/", tab[2,]),
                        #x=174, y=-39.8
                        x=174, y=-38.8)


catchmap <- ggplot(Data_to_plot_no_pseudo_absences) +
  geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
  geom_segment(data=l2, aes(x = Lon2,y = Lat2, xend = Lon, yend = Lat), col="gray") +
  geom_point(aes(x = Lon, y = Lat, col = present), alpha = 0.6) +
  facet_wrap(.~Year) +
  xlab("Longitude (°E)") + ylab("Latitude (°N)") +
  #ggtitle("Longfin eel NZFFD encounter/non-encounter observations by year") +
  guides(color = guide_legend(title = "", override.aes = list(size=10))) + #increase size of point
  scale_colour_manual(values = c("#E41A1C", "chartreuse4")) +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = rel(1)),
        axis.title=element_text(size = rel(1.5),face="bold"),
        axis.text.x = element_text(angle = 90),
        legend.text=element_text(size = rel(1)))

catchmap <- catchmap +geom_text(
  data = data_text,
  mapping = aes(x = x, y = y, label = label),  
  size=5
)

ggsave(file.path(fig_dir, "Taranaki_lf_observations_byYear.png"), catchmap, height = 12, width = 15)


## Have commented out as it takes ages to run! #

## Now plot with pseudo-absences - loop over all scenarios ##
# for(sce in scenarios){
#   
#   print(sce)
#   
#   Data_to_plot <- VAST_input_data[[sce]]$Data_Geostat
#   Data_to_plot$present <- ifelse(round(Data_to_plot$Catch_KG)==1, "Present", "Absent")
#   
#   
#   #Taranaki network by year
#   tab <- table(Data_to_plot$present, Data_to_plot$Year)
#   years <- unique(Data_to_plot$Year)[order(unique(Data_to_plot$Year))]
#   data_text <- data.frame("Year"= years, label=paste0(tab[1,], "/", tab[2,]),
#                           #x=174, y=-39.8
#                           x=174, y=-38.8)
#   
#   
#   catchmap <- ggplot(Data_to_plot) +
#     geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
#     geom_segment(data=l2, aes(x = Lon2,y = Lat2, xend = Lon, yend = Lat), col="gray") +
#     geom_point(aes(x = Lon, y = Lat, col = present), alpha = 0.6) +
#     facet_wrap(.~Year) +
#     xlab("Longitude (°E)") + ylab("Latitude (°N)") +
#     #ggtitle("Longfin eel NZFFD encounter/non-encounter observations by year") +
#     guides(color = guide_legend(title = "", override.aes = list(size=10))) + #increase size of point
#     scale_colour_manual(values = c("#E41A1C", "chartreuse4")) +
#     theme_bw(base_size = 14) +
#     theme(axis.text = element_text(size = rel(1)),
#           axis.title=element_text(size = rel(1.5),face="bold"),
#           axis.text.x = element_text(angle = 90),
#           legend.text=element_text(size = rel(1)))
#   
#   catchmap <- catchmap +geom_text(
#     data = data_text,
#     mapping = aes(x = x, y = y, label = label),  
#     size=5
#   )
#   
#   ggsave(file.path(fig_dir, paste0("Taranaki_lf_observations_byYear_",sce,".png")), catchmap, height = 12, width = 15)
#   
#   
#   rm(Data_to_plot)
#   
# }



###################################################
###################################################
## Plot each data set (structured EF, structured 
## NetTrap and unstructured data)

# Set new dir's for data
catchment_map_dir <- "./Data_processed/Figures/Catchment_maps"
dir.create(catchment_map_dir, showWarnings = F)

# Set data needed
VAST_input_data <- readRDS(file.path(VAST_input_data_dir, "VAST_input_data.rds"))
network <- VAST_input_data$`Taranaki data`$network #any network will do

l2 <- lapply(1:nrow(network), function(x){
  parent <- network$parent_s[x]
  find <- network %>% filter(child_s == parent)
  if(nrow(find)>0) out <- cbind.data.frame(network[x,], 'Lon2'=find$Lon, 'Lat2'=find$Lat)
  if(nrow(find)==0) out <- cbind.data.frame(network[x,], 'Lon2'=NA, 'Lat2'=NA)
  
  return(out)
})
l2 <- do.call(rbind, l2)

## Load full network for plots
netfull <- readRDS(file.path(raw_data_dir, "NZ_network.rds"))

#New Zealand map
Data_to_plot <- VAST_input_data$`Taranaki data`$Data_Geostat

nzmap <- ggplot() +
  geom_point(data = netfull, aes(x = long, y = lat), pch = ".") +
  geom_point(data = network, aes(x = Lon, y = Lat), color = "gray", pch = ".") +
  xlab("Longitude (°E)") + ylab("Latitude (°N)") +
  theme_bw(base_size = 14)


## Network and data on NZ
nzmap_taranaki <- nzmap +
  #geom_point(data = Data_to_plot, aes(x = Lon, y = Lat, fill = "Sites"), pch = 21, alpha = 0.8) +
  #scale_fill_manual(values="white") +
  #guides(fill=guide_legend(title=""), color=guide_legend(title="Study area")) +
  theme(legend.position = c(0.3,0.8),
        axis.text = element_text(size = rel(1.5)),
        axis.title=element_text(size = rel(1.5),face="bold"),
        axis.text.x = element_text(angle = 90),
        legend.text=element_text(size = rel(1.5)))
ggsave(file.path(catchment_map_dir, "Taranaki_on_NZmap.png"), nzmap_taranaki)


## structured data only
Data_to_plot <- VAST_input_data$`Taranaki data`$Data_Geostat

catchmap <- ggplot(Data_to_plot) +
  geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
  geom_segment(data=l2, aes(x = Lon2,y = Lat2, xend = Lon, yend = Lat), col="gray") +
  geom_point(aes(x = Lon, y = Lat, col = Data_source), alpha = 0.8, size=3) +
  
  xlab("Longitude (°E)") + ylab("Latitude (°N)") +
  #ggtitle("") +
  guides(color = guide_legend(title = "", override.aes = list(size=10))) + #increase size of point
  scale_colour_manual(values = c("#0000a7", "#c1272d")) +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = rel(1.5)),
        axis.title=element_text(size = rel(1.5),face="bold"),
        axis.text.x = element_text(angle = 90),
        legend.text=element_text(size = rel(1.5)))

ggsave(file.path(catchment_map_dir, "Catchment_map_structured_data.png"), catchmap, height = 12, width = 15)

rm(Data_to_plot) ; rm(catchmap)


## structured data + unstructured data
Data_to_plot <- VAST_input_data$`1a`$Data_Geostat #can use any scenario to demonstrate, just need to remove pseudo-absence data
# Data_to_plot$Data_source <- ifelse(Data_to_plot$Data_source == "Structured_EF", "EF",
#                                    ifelse(Data_to_plot$Data_source == "Structured_NetTrap", "NetTrap", 
#                                           ifelse( c(Data_to_plot$Data_source == "Unstructured" & Data_to_plot$Catch_KG > 0), "PO",
#                                                   "Pseudo-absence")))

Data_to_plot$Data_source <- ifelse(Data_to_plot$Data_source == "Structured_EF", "Electric Fishing",
                                   ifelse(Data_to_plot$Data_source == "Structured_NetTrap", "Net and Trap", 
                                          ifelse( c(Data_to_plot$Data_source == "Unstructured" & Data_to_plot$Catch_KG > 0), "PO",
                                                  "Pseudo-absence")))

Data_to_plot <- Data_to_plot %>% filter(!(Data_source == "Pseudo-absence"))

#Arrange data for plotting
#Data_to_plot$Data_source <- factor(Data_to_plot$Data_source, levels = c("PO","EF","NetTrap"))
Data_to_plot$Data_source <- factor(Data_to_plot$Data_source, levels = c("PO","Electric Fishing","Net and Trap"))
Data_to_plot <- Data_to_plot %>% arrange(Data_source)


catchmap <- ggplot(Data_to_plot) +
  geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
  geom_segment(data=l2, aes(x = Lon2,y = Lat2, xend = Lon, yend = Lat), col="gray") +
  geom_point(aes(x = Lon, y = Lat, col = Data_source), alpha = 0.8, size=3) +
  
  xlab("Longitude (°E)") + ylab("Latitude (°N)") +
  #ggtitle("") +
  guides(color = guide_legend(title = "", override.aes = list(size=10))) + #increase size of point
  scale_colour_manual(values = c("#eecc16","#0000a7", "#c1272d")) +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = rel(1.5)),
        axis.title=element_text(size = rel(1.5),face="bold"),
        axis.text.x = element_text(angle = 90),
        legend.text=element_text(size = rel(1.5)))

ggsave(file.path(catchment_map_dir, "Catchment_map_all_data.png"), catchmap, height = 12, width = 15)




#NZmap with structured data + unstructured data catchment map
Data_to_plot <- VAST_input_data$`1a`$Data_Geostat #can use any scenario to demonstrate, just need to remove pseudo-absence data
Data_to_plot$Data_source <- ifelse(Data_to_plot$Data_source == "Structured_EF", "Electric fishing",
                                   ifelse(Data_to_plot$Data_source == "Structured_NetTrap", "Net and trap", 
                                          ifelse( c(Data_to_plot$Data_source == "Unstructured" & Data_to_plot$Catch_KG > 0), "Presence-only",
                                                  "Pseudo-absence")))
Data_to_plot <- Data_to_plot %>% filter(!(Data_source == "Pseudo-absence"))

#Arrange data for plotting
Data_to_plot$Data_source <- factor(Data_to_plot$Data_source, levels = c("Presence-only","Electric fishing","Net and trap"))
Data_to_plot <- Data_to_plot %>% arrange(Data_source)


catchmap2 <- ggplot(Data_to_plot) +
  geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
  geom_segment(data=l2, aes(x = Lon2,y = Lat2, xend = Lon, yend = Lat), col="gray") +
  geom_point(aes(x = Lon, y = Lat, col = Data_source), alpha = 0.8, size=3) +
  
  xlab("Longitude (°E)") + ylab("Latitude (°N)") +
  #ggtitle("") +
  guides(color = guide_legend(title = "", override.aes = list(size=10))) + #increase size of point
  scale_colour_manual(values = c("#eecc16","#0000a7", "#c1272d")) +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = rel(1.5)),
        axis.title=element_text(size = rel(1.5),face="bold"),
        axis.text.x = element_text(angle = 90),
        legend.text=element_text(size = rel(1.5)))

catchmap_new_legend <- catchmap2 + theme(legend.position = c(.2, .9),
                                        legend.text=element_text(size = rel(1.2)))

# NZmap_catchmap <- ggarrange(nzmap_taranaki,
#                             catchmap_new_legend,
#                             widths = c(1,1))
NZmap_catchmap <- ggarrange(catchmap_new_legend,
                            nzmap_taranaki,
                            widths = c(1,1))
ggsave(file.path(catchment_map_dir, "NZmap_catchmap.png"), NZmap_catchmap, height = 12, width = 15)


## structured data + unstructured data - yearly
catchmap <- ggplot(Data_to_plot) +
  geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
  geom_segment(data=l2, aes(x = Lon2,y = Lat2, xend = Lon, yend = Lat), col="gray") +
  geom_point(aes(x = Lon, y = Lat, col = Data_source), alpha = 0.8, size=3) +
  facet_wrap(.~Year) +
  
  xlab("Longitude (°E)") + ylab("Latitude (°N)") +
  #ggtitle("") +
  guides(color = guide_legend(title = "", override.aes = list(size=14))) + #increase size of point
  scale_colour_manual(values = c("#eecc16","#0000a7", "#c1272d")) +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = rel(1.25)),
        axis.title=element_text(size = rel(1.5),face="bold"),
        axis.text.x = element_text(angle = 90),
        legend.text=element_text(size = rel(1)))

ggsave(file.path(catchment_map_dir, "Catchment_map_all_data_yr.png"), catchmap, height = 12, width = 15)

rm(Data_to_plot) ; rm(catchmap)



############################################
############################################

##        1a        ##
Data_to_plot <- VAST_input_data$`1a`$Data_Geostat
Data_to_plot$Data_source <- ifelse(Data_to_plot$Data_source == "Structured_EF", "EF",
                                   ifelse(Data_to_plot$Data_source == "Structured_NetTrap", "NetTrap", 
                                          ifelse( c(Data_to_plot$Data_source == "Unstructured" & Data_to_plot$Catch_KG > 0), "PO",
                                                  "Pseudo-absence")))
#Arrange data for plotting
Data_to_plot$Data_source <- factor(Data_to_plot$Data_source, levels = c("Pseudo-absence","PO","EF","NetTrap"))
Data_to_plot <- Data_to_plot %>% arrange(Data_source)

# data_text <- data.frame(label="Random generation (n)",
#                         x=174, y=-39.85)

catchmap1a <- ggplot(Data_to_plot) +
  geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
  geom_segment(data=l2, aes(x = Lon2,y = Lat2, xend = Lon, yend = Lat), col="gray") +
  geom_point(aes(x = Lon, y = Lat, col = Data_source), alpha = 0.8, size=3) +
  
  xlab("Longitude (°E)") + ylab("Latitude (°N)") +
  #ggtitle("") +
  guides(color = guide_legend(title = "", override.aes = list(size=10))) + #increase size of point
  #scale_colour_manual(values = c("#008176","#0000a7", "#c1272d", "#eecc16")) +
  scale_colour_manual(values = c("#008176","#eecc16","#0000a7", "#c1272d")) +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = rel(1.5)),
        axis.title=element_text(size = rel(1.5),face="bold"),
        axis.text.x = element_text(angle = 90),
        legend.text=element_text(size = rel(1.5)))

# catchmap1a <- catchmap1a + geom_text(
#   data = data_text,  
#   size=8,
#   mapping = aes(x = x, y = y, label = label)
# )

ggsave(file.path(catchment_map_dir, "Catchment_map_1a_data.png"), catchmap1a, height = 12, width = 15)

rm(Data_to_plot)


##        1b        ##
Data_to_plot <- VAST_input_data$`1b`$Data_Geostat
Data_to_plot$Data_source <- ifelse(Data_to_plot$Data_source == "Structured_EF", "EF",
                                   ifelse(Data_to_plot$Data_source == "Structured_NetTrap", "NetTrap", 
                                          ifelse( c(Data_to_plot$Data_source == "Unstructured" & Data_to_plot$Catch_KG > 0), "PO",
                                                  "Pseudo-absence")))
#Arrange data for plotting
Data_to_plot$Data_source <- factor(Data_to_plot$Data_source, levels = c("Pseudo-absence","PO","EF","NetTrap"))
Data_to_plot <- Data_to_plot %>% arrange(Data_source)

catchmap1b <- ggplot(Data_to_plot) +
  geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
  geom_segment(data=l2, aes(x = Lon2,y = Lat2, xend = Lon, yend = Lat), col="gray") +
  geom_point(aes(x = Lon, y = Lat, col = Data_source), alpha = 0.8, size=3) +
  
  xlab("Longitude (°E)") + ylab("Latitude (°N)") +
  #ggtitle("") +
  guides(color = guide_legend(title = "", override.aes = list(size=10))) + #increase size of point
  #scale_colour_manual(values = c("#008176","#0000a7", "#c1272d", "#eecc16")) +
  scale_colour_manual(values = c("#008176","#eecc16","#0000a7", "#c1272d")) +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = rel(1.5)),
        axis.title=element_text(size = rel(1.5),face="bold"),
        axis.text.x = element_text(angle = 90),
        legend.text=element_text(size = rel(1.5)))

ggsave(file.path(catchment_map_dir, "Catchment_map_1b_data.png"), catchmap1b, height = 12, width = 15)

rm(Data_to_plot)


##        1c        ##
Data_to_plot <- VAST_input_data$`1c`$Data_Geostat
Data_to_plot$Data_source <- ifelse(Data_to_plot$Data_source == "Structured_EF", "EF",
                                   ifelse(Data_to_plot$Data_source == "Structured_NetTrap", "NetTrap", 
                                          ifelse( c(Data_to_plot$Data_source == "Unstructured" & Data_to_plot$Catch_KG > 0), "PO",
                                                  "Pseudo-absence")))
#Arrange data for plotting
Data_to_plot$Data_source <- factor(Data_to_plot$Data_source, levels = c("Pseudo-absence","PO","EF","NetTrap"))
Data_to_plot <- Data_to_plot %>% arrange(Data_source)

catchmap1c <- ggplot(Data_to_plot) +
  geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
  geom_segment(data=l2, aes(x = Lon2,y = Lat2, xend = Lon, yend = Lat), col="gray") +
  geom_point(aes(x = Lon, y = Lat, col = Data_source), alpha = 0.8, size=3) +
  
  xlab("Longitude (°E)") + ylab("Latitude (°N)") +
  #ggtitle("") +
  guides(color = guide_legend(title = "", override.aes = list(size=10))) + #increase size of point
  #scale_colour_manual(values = c("#008176","#0000a7", "#c1272d", "#eecc16")) +
  scale_colour_manual(values = c("#008176","#eecc16","#0000a7", "#c1272d")) +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = rel(1.5)),
        axis.title=element_text(size = rel(1.5),face="bold"),
        axis.text.x = element_text(angle = 90),
        legend.text=element_text(size = rel(1.5)))

ggsave(file.path(catchment_map_dir, "Catchment_map_1c_data.png"), catchmap1c, height = 12, width = 15)

rm(Data_to_plot)


##        1d        ##
Data_to_plot <- VAST_input_data$`1d`$Data_Geostat
Data_to_plot$Data_source <- ifelse(Data_to_plot$Data_source == "Structured_EF", "EF",
                                   ifelse(Data_to_plot$Data_source == "Structured_NetTrap", "NetTrap", 
                                          ifelse( c(Data_to_plot$Data_source == "Unstructured" & Data_to_plot$Catch_KG > 0), "PO",
                                                  "Pseudo-absence")))
#Arrange data for plotting
Data_to_plot$Data_source <- factor(Data_to_plot$Data_source, levels = c("Pseudo-absence","PO","EF","NetTrap"))
Data_to_plot <- Data_to_plot %>% arrange(Data_source)

catchmap1d <- ggplot(Data_to_plot) +
  geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
  geom_segment(data=l2, aes(x = Lon2,y = Lat2, xend = Lon, yend = Lat), col="gray") +
  geom_point(aes(x = Lon, y = Lat, col = Data_source), alpha = 0.8, size=3) +
  
  xlab("Longitude (°E)") + ylab("Latitude (°N)") +
  #ggtitle("") +
  guides(color = guide_legend(title = "", override.aes = list(size=10))) + #increase size of point
  #scale_colour_manual(values = c("#008176","#0000a7", "#c1272d", "#eecc16")) +
  scale_colour_manual(values = c("#008176","#eecc16","#0000a7", "#c1272d")) +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = rel(1.5)),
        axis.title=element_text(size = rel(1.5),face="bold"),
        axis.text.x = element_text(angle = 90),
        legend.text=element_text(size = rel(1.5)))

ggsave(file.path(catchment_map_dir, "Catchment_map_1d_data.png"), catchmap1d, height = 12, width = 15)

rm(Data_to_plot)


## Scenario 1 maps all together ##
# catchmap1a_new_legend <- catchmap1a + theme(legend.position = c(.15, .88),
#                                             legend.text=element_text(size = rel(0.6)))
# catchmaps_sce1 <- ggarrange(catchmap1a_new_legend + rremove("xlab") + rremove("x.text") + rremove("x.ticks"), 
#                             catchmap1b + rremove("legend") + rremove("xylab") + rremove("xy.text") + rremove("y.ticks") + rremove("x.ticks"),
#                             catchmap1c + rremove("legend"), 
#                             catchmap1d + rremove("legend") + rremove("ylab") + rremove("y.text") + rremove("y.ticks"),
#                             widths = c(1.2,1), heights = c(1,1.2))

catchmaps_sce1 <- ggarrange(catchmap1a + rremove("xlab") + rremove("x.text") + rremove("x.ticks"), 
                            catchmap1b + rremove("xylab") + rremove("xy.text") + rremove("y.ticks") + rremove("x.ticks"),
                            catchmap1c, 
                            catchmap1d + rremove("ylab") + rremove("y.text") + rremove("y.ticks"),
                            widths = c(1.2,1), heights = c(1,1.2), common.legend = TRUE, legend = "top")

ggsave(file.path(catchment_map_dir, "Catchment_maps_scenario_1.png"), catchmaps_sce1, height = 12, width = 15)





############################################
############################################

##        2a        ##
Data_to_plot <- VAST_input_data$`2a`$Data_Geostat
Data_to_plot$Data_source <- ifelse(Data_to_plot$Data_source == "Structured_EF", "EF",
                                   ifelse(Data_to_plot$Data_source == "Structured_NetTrap", "NetTrap", 
                                          ifelse( c(Data_to_plot$Data_source == "Unstructured" & Data_to_plot$Catch_KG > 0), "PO",
                                                  "Pseudo-absence")))
#Arrange data for plotting
Data_to_plot$Data_source <- factor(Data_to_plot$Data_source, levels = c("Pseudo-absence","PO","EF","NetTrap"))
Data_to_plot <- Data_to_plot %>% arrange(Data_source)

# data_text <- data.frame(label="Random generation (n)",
#                         x=174, y=-39.85)

catchmap2a <- ggplot(Data_to_plot) +
  geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
  geom_segment(data=l2, aes(x = Lon2,y = Lat2, xend = Lon, yend = Lat), col="gray") +
  geom_point(aes(x = Lon, y = Lat, col = Data_source), alpha = 0.8, size=3) +
  
  xlab("Longitude (°E)") + ylab("Latitude (°N)") +
  #ggtitle("") +
  guides(color = guide_legend(title = "", override.aes = list(size=10))) + #increase size of point
  #scale_colour_manual(values = c("#008176","#0000a7", "#c1272d", "#eecc16")) +
  scale_colour_manual(values = c("#008176","#eecc16","#0000a7", "#c1272d")) +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = rel(1.5)),
        axis.title=element_text(size = rel(1.5),face="bold"),
        axis.text.x = element_text(angle = 90),
        legend.text=element_text(size = rel(1.5)))

# catchmap2a <- catchmap2a + geom_text(
#   data = data_text,  
#   size=8,
#   mapping = aes(x = x, y = y, label = label)
# )

ggsave(file.path(catchment_map_dir, "Catchment_map_2a_data.png"), catchmap2a, height = 12, width = 15)

rm(Data_to_plot)


##        2b        ##
Data_to_plot <- VAST_input_data$`2b`$Data_Geostat
Data_to_plot$Data_source <- ifelse(Data_to_plot$Data_source == "Structured_EF", "EF",
                                   ifelse(Data_to_plot$Data_source == "Structured_NetTrap", "NetTrap", 
                                          ifelse( c(Data_to_plot$Data_source == "Unstructured" & Data_to_plot$Catch_KG > 0), "PO",
                                                  "Pseudo-absence")))
#Arrange data for plotting
Data_to_plot$Data_source <- factor(Data_to_plot$Data_source, levels = c("Pseudo-absence","PO","EF","NetTrap"))
Data_to_plot <- Data_to_plot %>% arrange(Data_source)

catchmap2b <- ggplot(Data_to_plot) +
  geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
  geom_segment(data=l2, aes(x = Lon2,y = Lat2, xend = Lon, yend = Lat), col="gray") +
  geom_point(aes(x = Lon, y = Lat, col = Data_source), alpha = 0.8, size=3) +
  
  xlab("Longitude (°E)") + ylab("Latitude (°N)") +
  #ggtitle("") +
  guides(color = guide_legend(title = "", override.aes = list(size=10))) + #increase size of point
  #scale_colour_manual(values = c("#008176","#0000a7", "#c1272d", "#eecc16")) +
  scale_colour_manual(values = c("#008176","#eecc16","#0000a7", "#c1272d")) +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = rel(1.5)),
        axis.title=element_text(size = rel(1.5),face="bold"),
        axis.text.x = element_text(angle = 90),
        legend.text=element_text(size = rel(1.5)))

ggsave(file.path(catchment_map_dir, "Catchment_map_2b_data.png"), catchmap2b, height = 12, width = 15)

rm(Data_to_plot)


##        2c        ##
Data_to_plot <- VAST_input_data$`2c`$Data_Geostat
Data_to_plot$Data_source <- ifelse(Data_to_plot$Data_source == "Structured_EF", "EF",
                                   ifelse(Data_to_plot$Data_source == "Structured_NetTrap", "NetTrap", 
                                          ifelse( c(Data_to_plot$Data_source == "Unstructured" & Data_to_plot$Catch_KG > 0), "PO",
                                                  "Pseudo-absence")))
#Arrange data for plotting
Data_to_plot$Data_source <- factor(Data_to_plot$Data_source, levels = c("Pseudo-absence","PO","EF","NetTrap"))
Data_to_plot <- Data_to_plot %>% arrange(Data_source)

catchmap2c <- ggplot(Data_to_plot) +
  geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
  geom_segment(data=l2, aes(x = Lon2,y = Lat2, xend = Lon, yend = Lat), col="gray") +
  geom_point(aes(x = Lon, y = Lat, col = Data_source), alpha = 0.8, size=3) +
  
  xlab("Longitude (°E)") + ylab("Latitude (°N)") +
  #ggtitle("") +
  guides(color = guide_legend(title = "", override.aes = list(size=10))) + #increase size of point
  #scale_colour_manual(values = c("#008176","#0000a7", "#c1272d", "#eecc16")) +
  scale_colour_manual(values = c("#008176","#eecc16","#0000a7", "#c1272d")) +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = rel(1.5)),
        axis.title=element_text(size = rel(1.5),face="bold"),
        axis.text.x = element_text(angle = 90),
        legend.text=element_text(size = rel(1.5)))

ggsave(file.path(catchment_map_dir, "Catchment_map_2c_data.png"), catchmap2c, height = 12, width = 15)

rm(Data_to_plot)


##        2d        ##
Data_to_plot <- VAST_input_data$`2d`$Data_Geostat
Data_to_plot$Data_source <- ifelse(Data_to_plot$Data_source == "Structured_EF", "EF",
                                   ifelse(Data_to_plot$Data_source == "Structured_NetTrap", "NetTrap", 
                                          ifelse( c(Data_to_plot$Data_source == "Unstructured" & Data_to_plot$Catch_KG > 0), "PO",
                                                  "Pseudo-absence")))
#Arrange data for plotting
Data_to_plot$Data_source <- factor(Data_to_plot$Data_source, levels = c("Pseudo-absence","PO","EF","NetTrap"))
Data_to_plot <- Data_to_plot %>% arrange(Data_source)

catchmap2d <- ggplot(Data_to_plot) +
  geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
  geom_segment(data=l2, aes(x = Lon2,y = Lat2, xend = Lon, yend = Lat), col="gray") +
  geom_point(aes(x = Lon, y = Lat, col = Data_source), alpha = 0.8, size=3) +
  
  xlab("Longitude (°E)") + ylab("Latitude (°N)") +
  #ggtitle("") +
  guides(color = guide_legend(title = "", override.aes = list(size=10))) + #increase size of point
  #scale_colour_manual(values = c("#008176","#0000a7", "#c1272d", "#eecc16")) +
  scale_colour_manual(values = c("#008176","#eecc16","#0000a7", "#c1272d")) +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = rel(1.5)),
        axis.title=element_text(size = rel(1.5),face="bold"),
        axis.text.x = element_text(angle = 90),
        legend.text=element_text(size = rel(1.5)))

ggsave(file.path(catchment_map_dir, "Catchment_map_2d_data.png"), catchmap2d, height = 12, width = 15)

rm(Data_to_plot)


## Scenario 2 maps all together ##
# catchmap2a_new_legend <- catchmap2a + theme(legend.position = c(.15, .88),
#                                             legend.text=element_text(size = rel(0.6)))
# 
# catchmaps_sce2 <- ggarrange(catchmap2a_new_legend + rremove("xlab") + rremove("x.text") + rremove("x.ticks"), 
#                             catchmap2b + rremove("legend") + rremove("xylab") + rremove("xy.text") + rremove("y.ticks") + rremove("x.ticks"),
#                             catchmap2c + rremove("legend"), 
#                             catchmap2d + rremove("legend") + rremove("ylab") + rremove("y.text") + rremove("y.ticks"),
#                             widths = c(1.2,1), heights = c(1,1.2))
catchmaps_sce2 <- ggarrange(catchmap2a + rremove("xlab") + rremove("x.text") + rremove("x.ticks"), 
                            catchmap2b + rremove("xylab") + rremove("xy.text") + rremove("y.ticks") + rremove("x.ticks"),
                            catchmap2c, 
                            catchmap2d + rremove("ylab") + rremove("y.text") + rremove("y.ticks"),
                            widths = c(1.2,1), heights = c(1,1.2), common.legend = TRUE, legend = "top")

ggsave(file.path(catchment_map_dir, "Catchment_maps_scenario_2.png"), catchmaps_sce2, height = 12, width = 15)






############################################
############################################

##        3a        ##
Data_to_plot <- VAST_input_data$`3a`$Data_Geostat
Data_to_plot$Data_source <- ifelse(Data_to_plot$Data_source == "Structured_EF", "EF",
                                   ifelse(Data_to_plot$Data_source == "Structured_NetTrap", "NetTrap", 
                                          ifelse( c(Data_to_plot$Data_source == "Unstructured" & Data_to_plot$Catch_KG > 0), "PO",
                                                  "Pseudo-absence")))
#Arrange data for plotting
Data_to_plot$Data_source <- factor(Data_to_plot$Data_source, levels = c("Pseudo-absence","PO","EF","NetTrap"))
Data_to_plot <- Data_to_plot %>% arrange(Data_source)

# data_text <- data.frame(label="Random generation (n)",
#                         x=174, y=-39.85)

catchmap3a <- ggplot(Data_to_plot) +
  geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
  geom_segment(data=l2, aes(x = Lon2,y = Lat2, xend = Lon, yend = Lat), col="gray") +
  geom_point(aes(x = Lon, y = Lat, col = Data_source), alpha = 0.8, size=3) +
  
  xlab("Longitude (°E)") + ylab("Latitude (°N)") +
  #ggtitle("") +
  guides(color = guide_legend(title = "", override.aes = list(size=10))) + #increase size of point
  #scale_colour_manual(values = c("#008176","#0000a7", "#c1272d", "#eecc16")) +
  scale_colour_manual(values = c("#008176","#eecc16","#0000a7", "#c1272d")) +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = rel(1.5)),
        axis.title=element_text(size = rel(1.5),face="bold"),
        axis.text.x = element_text(angle = 90),
        legend.text=element_text(size = rel(1.5)))

# catchmap3a <- catchmap3a + geom_text(
#   data = data_text,  
#   size=8,
#   mapping = aes(x = x, y = y, label = label)
# )

ggsave(file.path(catchment_map_dir, "Catchment_map_3a_data.png"), catchmap3a, height = 12, width = 15)

rm(Data_to_plot)


##        3b        ##
Data_to_plot <- VAST_input_data$`3b`$Data_Geostat
Data_to_plot$Data_source <- ifelse(Data_to_plot$Data_source == "Structured_EF", "EF",
                                   ifelse(Data_to_plot$Data_source == "Structured_NetTrap", "NetTrap", 
                                          ifelse( c(Data_to_plot$Data_source == "Unstructured" & Data_to_plot$Catch_KG > 0), "PO",
                                                  "Pseudo-absence")))
#Arrange data for plotting
Data_to_plot$Data_source <- factor(Data_to_plot$Data_source, levels = c("Pseudo-absence","PO","EF","NetTrap"))
Data_to_plot <- Data_to_plot %>% arrange(Data_source)

catchmap3b <- ggplot(Data_to_plot) +
  geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
  geom_segment(data=l2, aes(x = Lon2,y = Lat2, xend = Lon, yend = Lat), col="gray") +
  geom_point(aes(x = Lon, y = Lat, col = Data_source), alpha = 0.8, size=3) +
  
  xlab("Longitude (°E)") + ylab("Latitude (°N)") +
  #ggtitle("") +
  guides(color = guide_legend(title = "", override.aes = list(size=10))) + #increase size of point
  #scale_colour_manual(values = c("#008176","#0000a7", "#c1272d", "#eecc16")) +
  scale_colour_manual(values = c("#008176","#eecc16","#0000a7", "#c1272d")) +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = rel(1.5)),
        axis.title=element_text(size = rel(1.5),face="bold"),
        axis.text.x = element_text(angle = 90),
        legend.text=element_text(size = rel(1.5)))

ggsave(file.path(catchment_map_dir, "Catchment_map_3b_data.png"), catchmap3b, height = 12, width = 15)

rm(Data_to_plot)


##        3c        ##
Data_to_plot <- VAST_input_data$`3c`$Data_Geostat
Data_to_plot$Data_source <- ifelse(Data_to_plot$Data_source == "Structured_EF", "EF",
                                   ifelse(Data_to_plot$Data_source == "Structured_NetTrap", "NetTrap", 
                                          ifelse( c(Data_to_plot$Data_source == "Unstructured" & Data_to_plot$Catch_KG > 0), "PO",
                                                  "Pseudo-absence")))
#Arrange data for plotting
Data_to_plot$Data_source <- factor(Data_to_plot$Data_source, levels = c("Pseudo-absence","PO","EF","NetTrap"))
Data_to_plot <- Data_to_plot %>% arrange(Data_source)

catchmap3c <- ggplot(Data_to_plot) +
  geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
  geom_segment(data=l2, aes(x = Lon2,y = Lat2, xend = Lon, yend = Lat), col="gray") +
  geom_point(aes(x = Lon, y = Lat, col = Data_source), alpha = 0.8, size=3) +
  
  xlab("Longitude (°E)") + ylab("Latitude (°N)") +
  #ggtitle("") +
  guides(color = guide_legend(title = "", override.aes = list(size=10))) + #increase size of point
  #scale_colour_manual(values = c("#008176","#0000a7", "#c1272d", "#eecc16")) +
  scale_colour_manual(values = c("#008176","#eecc16","#0000a7", "#c1272d")) +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = rel(1.5)),
        axis.title=element_text(size = rel(1.5),face="bold"),
        axis.text.x = element_text(angle = 90),
        legend.text=element_text(size = rel(1.5)))

ggsave(file.path(catchment_map_dir, "Catchment_map_3c_data.png"), catchmap3c, height = 12, width = 15)

rm(Data_to_plot)


##        3d        ##
Data_to_plot <- VAST_input_data$`3d`$Data_Geostat
Data_to_plot$Data_source <- ifelse(Data_to_plot$Data_source == "Structured_EF", "EF",
                                   ifelse(Data_to_plot$Data_source == "Structured_NetTrap", "NetTrap", 
                                          ifelse( c(Data_to_plot$Data_source == "Unstructured" & Data_to_plot$Catch_KG > 0), "PO",
                                                  "Pseudo-absence")))
#Arrange data for plotting
Data_to_plot$Data_source <- factor(Data_to_plot$Data_source, levels = c("Pseudo-absence","PO","EF","NetTrap"))
Data_to_plot <- Data_to_plot %>% arrange(Data_source)

catchmap3d <- ggplot(Data_to_plot) +
  geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
  geom_segment(data=l2, aes(x = Lon2,y = Lat2, xend = Lon, yend = Lat), col="gray") +
  geom_point(aes(x = Lon, y = Lat, col = Data_source), alpha = 0.8, size=3) +
  
  xlab("Longitude (°E)") + ylab("Latitude (°N)") +
  #ggtitle("") +
  guides(color = guide_legend(title = "", override.aes = list(size=10))) + #increase size of point
  #scale_colour_manual(values = c("#008176","#0000a7", "#c1272d", "#eecc16")) +
  scale_colour_manual(values = c("#008176","#eecc16","#0000a7", "#c1272d")) +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = rel(1.5)),
        axis.title=element_text(size = rel(1.5),face="bold"),
        axis.text.x = element_text(angle = 90),
        legend.text=element_text(size = rel(1.5)))

ggsave(file.path(catchment_map_dir, "Catchment_map_3d_data.png"), catchmap3d, height = 12, width = 15)

rm(Data_to_plot)


## Scenario 3 maps all together ##
catchmap3a_new_legend <- catchmap3a + theme(legend.position = c(.15, .88),
                                            legend.text=element_text(size = rel(0.6)))

# catchmaps_sce3 <- ggarrange(catchmap3a_new_legend + rremove("xlab") + rremove("x.text") + rremove("x.ticks"), 
#                             catchmap3b + rremove("legend") + rremove("xylab") + rremove("xy.text") + rremove("y.ticks") + rremove("x.ticks"),
#                             catchmap3c + rremove("legend"), 
#                             catchmap3d + rremove("legend") + rremove("ylab") + rremove("y.text") + rremove("y.ticks"),
#                             widths = c(1.2,1), heights = c(1,1.2))
catchmaps_sce3 <- ggarrange(catchmap3a + rremove("xlab") + rremove("x.text") + rremove("x.ticks"), 
                            catchmap3b + rremove("xylab") + rremove("xy.text") + rremove("y.ticks") + rremove("x.ticks"),
                            catchmap3c, 
                            catchmap3d + rremove("ylab") + rremove("y.text") + rremove("y.ticks"),
                            widths = c(1.2,1), heights = c(1,1.2), common.legend = TRUE, legend = "top")
ggsave(file.path(catchment_map_dir, "Catchment_maps_scenario_3.png"), catchmaps_sce3, height = 12, width = 15)




############################################
############################################

##        4a        ##
Data_to_plot <- VAST_input_data$`4a`$Data_Geostat
Data_to_plot$Data_source <- ifelse(Data_to_plot$Data_source == "Structured_EF", "EF",
                                   ifelse(Data_to_plot$Data_source == "Structured_NetTrap", "NetTrap", 
                                          ifelse( c(Data_to_plot$Data_source == "Unstructured" & Data_to_plot$Catch_KG > 0), "PO",
                                                  "Pseudo-absence")))
#Arrange data for plotting
Data_to_plot$Data_source <- factor(Data_to_plot$Data_source, levels = c("Pseudo-absence","PO","EF","NetTrap"))
Data_to_plot <- Data_to_plot %>% arrange(Data_source)

# data_text <- data.frame(label="Random generation (n)",
#                         x=174, y=-39.85)

catchmap4a <- ggplot(Data_to_plot) +
  geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
  geom_segment(data=l2, aes(x = Lon2,y = Lat2, xend = Lon, yend = Lat), col="gray") +
  geom_point(aes(x = Lon, y = Lat, col = Data_source), alpha = 0.8, size=3) +
  
  xlab("Longitude (°E)") + ylab("Latitude (°N)") +
  #ggtitle("") +
  guides(color = guide_legend(title = "", override.aes = list(size=10))) + #increase size of point
  #scale_colour_manual(values = c("#008176","#0000a7", "#c1272d", "#eecc16")) +
  scale_colour_manual(values = c("#008176","#eecc16","#0000a7", "#c1272d")) +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = rel(1.5)),
        axis.title=element_text(size = rel(1.5),face="bold"),
        axis.text.x = element_text(angle = 90),
        legend.text=element_text(size = rel(1.5)))

# catchmap4a <- catchmap4a + geom_text(
#   data = data_text,  
#   size=8,
#   mapping = aes(x = x, y = y, label = label)
# )

ggsave(file.path(catchment_map_dir, "Catchment_map_4a_data.png"), catchmap4a, height = 12, width = 15)

rm(Data_to_plot)


##        4b        ##
Data_to_plot <- VAST_input_data$`4b`$Data_Geostat
Data_to_plot$Data_source <- ifelse(Data_to_plot$Data_source == "Structured_EF", "EF",
                                   ifelse(Data_to_plot$Data_source == "Structured_NetTrap", "NetTrap", 
                                          ifelse( c(Data_to_plot$Data_source == "Unstructured" & Data_to_plot$Catch_KG > 0), "PO",
                                                  "Pseudo-absence")))
#Arrange data for plotting
Data_to_plot$Data_source <- factor(Data_to_plot$Data_source, levels = c("Pseudo-absence","PO","EF","NetTrap"))
Data_to_plot <- Data_to_plot %>% arrange(Data_source)

catchmap4b <- ggplot(Data_to_plot) +
  geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
  geom_segment(data=l2, aes(x = Lon2,y = Lat2, xend = Lon, yend = Lat), col="gray") +
  geom_point(aes(x = Lon, y = Lat, col = Data_source), alpha = 0.8, size=3) +
  
  xlab("Longitude (°E)") + ylab("Latitude (°N)") +
  #ggtitle("") +
  guides(color = guide_legend(title = "", override.aes = list(size=10))) + #increase size of point
  #scale_colour_manual(values = c("#008176","#0000a7", "#c1272d", "#eecc16")) +
  scale_colour_manual(values = c("#008176","#eecc16","#0000a7", "#c1272d")) +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = rel(1.5)),
        axis.title=element_text(size = rel(1.5),face="bold"),
        axis.text.x = element_text(angle = 90),
        legend.text=element_text(size = rel(1.5)))

ggsave(file.path(catchment_map_dir, "Catchment_map_4b_data.png"), catchmap4b, height = 12, width = 15)

rm(Data_to_plot)


##        4c        ##
Data_to_plot <- VAST_input_data$`4c`$Data_Geostat
Data_to_plot$Data_source <- ifelse(Data_to_plot$Data_source == "Structured_EF", "EF",
                                   ifelse(Data_to_plot$Data_source == "Structured_NetTrap", "NetTrap", 
                                          ifelse( c(Data_to_plot$Data_source == "Unstructured" & Data_to_plot$Catch_KG > 0), "PO",
                                                  "Pseudo-absence")))
#Arrange data for plotting
Data_to_plot$Data_source <- factor(Data_to_plot$Data_source, levels = c("Pseudo-absence","PO","EF","NetTrap"))
Data_to_plot <- Data_to_plot %>% arrange(Data_source)

catchmap4c <- ggplot(Data_to_plot) +
  geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
  geom_segment(data=l2, aes(x = Lon2,y = Lat2, xend = Lon, yend = Lat), col="gray") +
  geom_point(aes(x = Lon, y = Lat, col = Data_source), alpha = 0.8, size=3) +
  
  xlab("Longitude (°E)") + ylab("Latitude (°N)") +
  #ggtitle("") +
  guides(color = guide_legend(title = "", override.aes = list(size=10))) + #increase size of point
  #scale_colour_manual(values = c("#008176","#0000a7", "#c1272d", "#eecc16")) +
  scale_colour_manual(values = c("#008176","#eecc16","#0000a7", "#c1272d")) +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = rel(1.5)),
        axis.title=element_text(size = rel(1.5),face="bold"),
        axis.text.x = element_text(angle = 90),
        legend.text=element_text(size = rel(1.5)))

ggsave(file.path(catchment_map_dir, "Catchment_map_4c_data.png"), catchmap4c, height = 12, width = 15)

rm(Data_to_plot)


##        4d        ##
Data_to_plot <- VAST_input_data$`4d`$Data_Geostat
Data_to_plot$Data_source <- ifelse(Data_to_plot$Data_source == "Structured_EF", "EF",
                                   ifelse(Data_to_plot$Data_source == "Structured_NetTrap", "NetTrap", 
                                          ifelse( c(Data_to_plot$Data_source == "Unstructured" & Data_to_plot$Catch_KG > 0), "PO",
                                                  "Pseudo-absence")))
#Arrange data for plotting
Data_to_plot$Data_source <- factor(Data_to_plot$Data_source, levels = c("Pseudo-absence","PO","EF","NetTrap"))
Data_to_plot <- Data_to_plot %>% arrange(Data_source)

catchmap4d <- ggplot(Data_to_plot) +
  geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
  geom_segment(data=l2, aes(x = Lon2,y = Lat2, xend = Lon, yend = Lat), col="gray") +
  geom_point(aes(x = Lon, y = Lat, col = Data_source), alpha = 0.8, size=3) +
  
  xlab("Longitude (°E)") + ylab("Latitude (°N)") +
  #ggtitle("") +
  guides(color = guide_legend(title = "", override.aes = list(size=10))) + #increase size of point
  #scale_colour_manual(values = c("#008176","#0000a7", "#c1272d", "#eecc16")) +
  scale_colour_manual(values = c("#008176","#eecc16","#0000a7", "#c1272d")) +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = rel(1.5)),
        axis.title=element_text(size = rel(1.5),face="bold"),
        axis.text.x = element_text(angle = 90),
        legend.text=element_text(size = rel(1.5)))

ggsave(file.path(catchment_map_dir, "Catchment_map_4d_data.png"), catchmap4d, height = 12, width = 15)

rm(Data_to_plot)


## Scenario 4 maps all together ##
# catchmap4a_new_legend <- catchmap4a + theme(legend.position = c(.15, .88),
#                                             legend.text=element_text(size = rel(0.6)))
# 
# catchmaps_sce4 <- ggarrange(catchmap4a_new_legend + rremove("xlab") + rremove("x.text") + rremove("x.ticks"), 
#                             catchmap4b + rremove("legend") + rremove("xylab") + rremove("xy.text") + rremove("y.ticks") + rremove("x.ticks"),
#                             catchmap4c + rremove("legend"), 
#                             catchmap4d + rremove("legend") + rremove("ylab") + rremove("y.text") + rremove("y.ticks"),
#                             widths = c(1.2,1), heights = c(1,1.2))

catchmaps_sce4 <- ggarrange(catchmap4a + rremove("xlab") + rremove("x.text") + rremove("x.ticks"), 
                            catchmap4b + rremove("xylab") + rremove("xy.text") + rremove("y.ticks") + rremove("x.ticks"),
                            catchmap4c, 
                            catchmap4d + rremove("ylab") + rremove("y.text") + rremove("y.ticks"),
                            widths = c(1.2,1), heights = c(1,1.2), common.legend = TRUE, legend = "top")

ggsave(file.path(catchment_map_dir, "Catchment_maps_scenario_4.png"), catchmaps_sce4, height = 12, width = 15)


