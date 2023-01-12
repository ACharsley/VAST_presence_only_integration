
rm(list=ls())

library(tidyverse)
library(proj4)
library(readxl)


#####################
##   Directories   ##
#####################

data_dir <- "./Data_processed"
raw_data_dir <- "./Data_raw"
data_taranaki_dir <- "./Data_processed/Taranaki"
fig_dir <- file.path(data_taranaki_dir, "Figures")


######################
##   Network data   ##
######################

## Load network to extract habitat covariates
network <- readRDS(file.path(data_taranaki_dir, "Taranaki_network_aa.rds"))

network$DSDamAffected <- factor(network$DSDamAffected)


#######################
##   Dam meta data   ##
#######################

dam_data <- read_excel(file.path(raw_data_dir, "latest_dams2004.xls"))

#Convert northing/easting data to lat/long

## function to calculate latitude and longitude from eastings and northings
calc_NZ_latlon <- function(northing, easting){
  #proj4string <- "+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +datum=WGS84 +units=m"
  proj4string <- "+proj=nzmg +lat_0=-41 +lon_0=173 +x_0=2510000 +y_0=6023150 +ellps=intl +datum=nzgd49 +units=m +towgs84=59.47,-5.04,187.44,0.47,-0.1,1.024,-4.5993 +nadgrids=nzgd2kgrid0005.gsb +no_defs"
  p <- project(matrix(c(easting, northing),nrow=1, ncol=2), proj=proj4string, inv=T)
  colnames(p) <- c('long', 'lat')
  return(p)
}

## latitude and longitude for child nodes in network
dam_ll <- lapply(1:nrow(dam_data), function(x){
  p <- calc_NZ_latlon(northing = dam_data$NORTHING[x], easting = dam_data$EASTING[x])
  return(p)
})
dam_ll <- do.call(rbind, dam_ll)

dam_data_full <- cbind.data.frame(dam_data, dam_ll)

dam_data_full <- dam_data_full %>% select("NZDAM_ID", "NAME OF DAM", "OWNER", "DATE", "REF", "long", "lat")

write_csv(dam_data_full, file = file.path(data_dir, "dam_meta_data.csv"))

#################
##   Dam map   ##
#################

## Plot of entire dam variable ##
aa <- ggplot(network) +
  geom_point(data = network, aes(x = Lon, y = Lat, colour = DSDamAffected), alpha=0.6) +
  xlab("Longitude") + ylab("Latitude")  

ggsave(file.path(fig_dir, "Dam_plot.png"), aa, width=10, height = 10)
####


## Plot of dam variable for stream order >=3 ##
network_2 <- network %>% filter(StreamOrder>=3)

bb <- ggplot(network_2) +
  geom_point(data = network_2, aes(x = Lon, y = Lat, colour = DSDamAffected), alpha=0.6) +
  xlab("Longitude") + ylab("Latitude")  

ggsave(file.path(fig_dir, "Dam_plot_SO3.png"), bb, width=10, height = 10)
####


###########

parent <- network %>% filter(parent_s==0)
damhere <- parent %>% filter(DSDamAffected==1)
damnothere <- parent %>% filter(DSDamAffected==0)

GO <- TRUE
counter <- 0

while(GO){ #Go until convergence
  
  counter <- counter + 1
  print(counter)
  
  
  nextseg <- network %>% filter(parent_s %in% damnothere$child_s) 
  damnothere <- nextseg %>% filter(DSDamAffected==0)
  
  if(nrow(nextseg)==0){GO <- FALSE}
  
  damhere_next <- nextseg %>% filter(DSDamAffected==1)
  damhere <- rbind(damhere, damhere_next)
  
  if(counter>10000) {stop()}
  
}


write_csv(damhere, file = file.path(data_taranaki_dir, "Taranaki_dam_locations.csv"))


# network_3 = network %>% 
#   select(c('parent_s', 'child_s', 'dist_s', 'lat', 'long', 'DSDamAffected', 'StreamOrder', 'isTerminal')) %>%
#   rename("Lon"=long, "Lat"=lat)
# 
# 
# dam_parent_s <- damhere$parent_s
# 
# network_3$dampoint <- ifelse((network_3$parent_s %in% dam_parent_s) & network_3$DSDamAffected==1, 1, 
#                                    ifelse(network_3$DSDamAffected==1, 2, 0))
# network_3$dampoint <- factor(network_3$dampoint)
# 
# network_3 <- network_3 %>% filter(isTerminal==FALSE)
# 
# 
# network_3$damsize <- ifelse(network_3$dampoint==1, 2, 1)
# 
# 
# 
# cc <- ggplot(network_3) +
#   mytheme() +
#   geom_point(data = network_3, aes(x = Lon, y = Lat, colour = dampoint, size=damsize), alpha=0.6) +
#   geom_point(aes(x = x, y = y, color = label, size = size), subset = .(label == 'point')) +
#   scale_size_continuous(range = c(1, 3)) +
#   xlab("Longitude") + ylab("Latitude") +
#   ggtitle("Canterbury dam plot") +
#   scale_fill_discrete(labels=c("River segment", "Dam location", "River segment upstream of dam", "NA"))
# 
# ggsave(file.path(fig_dir, "Dam_locations_plot.png"), cc, width=10, height = 10)

net_data <- network %>% filter(!is.na(DSDamAffected))

dd <- ggplot(net_data) +
  geom_point(data = net_data, aes(x = Lon, y = Lat, colour = DSDamAffected), alpha=0.6) +
  geom_point(data = damhere, aes(x = Lon, y = Lat, col="Barrier")) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Barriers to upstream movement - Taranaki, NZ") +
  scale_colour_manual(name="", values = c("#F8766D","#00BFC4","black"), labels=c("DS of barrier", "US of barrier", "Barrier"))

ggsave(file.path(fig_dir, "Dam_locations_plot.png"), dd, width=10, height = 10)


