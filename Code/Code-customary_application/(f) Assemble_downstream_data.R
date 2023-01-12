###################################################
##    Assemble 'downstream' Waitaki catchment    ##
##                data for testing               ##
##                                               ##
##                Anthony Charsley               ##
##                  October 2022                 ##
###################################################

# This code assembles presence/absence 
# data from the NZFFD.

###################################################

rm(list=ls())


##############
#  Packages  #
##############

library(tidyverse)
library(readxl)

#################
#  Directories  #
#################

data_dir <- "./Data"


raw_data <- "./Data/raw_data"
fig_dir <- file.path(data_dir, "Figures")


###################
#  Load datasets  #
###################

##Network
network <- readRDS(file.path(data_dir, "Waitaki_network.rds"))

##NZFFD observations
obs <- readRDS(file.path(data_dir, "Waitaki_NZFFD_cust_obs.rds"))

##Habitat data
hab <- readRDS(file.path(data_dir, "Waitaki_REC_covs.rds"))


###############################
#  Create downstream network  #
###############################

obs_child <- unique(obs$child_i)

net_obs <- network %>% filter(child_s %in% obs_child) #extract unique observation-nodes from network
nextdown <- network %>% filter(child_s %in% net_obs$parent_s) #Find the unique node directly downstream
save <- rbind.data.frame(net_obs,nextdown) #save these together
for(i in 1:100){ #Continue until number of rows converges - easily converges with 100, 3381 rows total
  nextdown <- network %>% filter(child_s %in% nextdown$parent_s)
  save <- unique(rbind.data.frame(save, nextdown))
  print(nrow(save))
}
network_sub <- save


####################################
#  Create downstream habitat data  #
####################################

#Select nodes to keep for downstream data
hab_sub <- hab %>% filter(child_s %in% network_sub$child_s)


##################
#  Rename nodes  #
##################

nodes <- unique(c(network_sub$child_s, network_sub$parent_s))
inodes <- seq_along(nodes)

## Rename nodes in network
# 1. Parent nodes
net_parents <- sapply(1:nrow(network_sub), function(x){
  if(network_sub$parent_s[x] != 0) new_node <- inodes[which(nodes == network_sub$parent_s[x])]
  if(network_sub$parent_s[x] == 0) new_node <- 0
  return(new_node)
})
# 2. Child nodes
net_children <- sapply(1:nrow(network_sub), function(x) inodes[which(nodes == network_sub$child_s[x])])
# 3. Apply renaming
network_sub$parent_s <- net_parents
network_sub$child_s <- net_children

## Rename nodes in observations
# 1. Parent nodes
obs_parents <- sapply(1:nrow(obs), function(x){
  if(obs$parent_i[x] != 0) new_node <- inodes[which(nodes == obs$parent_i[x])]
  if(obs$parent_i[x] == 0) new_node <- 0
  return(new_node)  
})
# 2. Child nodes
obs_children <- sapply(1:nrow(obs), function(x) inodes[which(nodes == obs$child_i[x])])
# 3. Apply renaming
obs_sub <- obs
obs_sub$parent_i <- obs_parents
obs_sub$child_i <- obs_children

## Rename nodes in habitat covariates
# 1. Child nodes (no parent nodes)
hab_children <- sapply(1:nrow(hab_sub), function(x) inodes[which(nodes == hab_sub$child_s[x])])
# 2. Apply renaming
hab_sub$child_s <- hab_children


################################
#  Check all nodes in network  #
################################

all(hab_children %in% net_children)
all(obs_children %in% net_children)


########################
#  Rename for mapping  #
########################

network_sub2 <- network_sub
obs_sub2 <- obs_sub


##########
#  Save  #
##########

saveRDS(obs_sub, file.path(data_dir, "Waitaki_NZFFD_cust_obs_ds.rds"))
saveRDS(network_sub, file.path(data_dir, "Waitaki_network_ds.rds"))
saveRDS(hab_sub, file=file.path(data_dir, "Waitaki_REC_covs_ds.rds"))


################################################
################################################


##########################
#  Build catchment maps  #
##########################

# To set connecting river segments
l2 <- lapply(1:nrow(network_sub2), function(x){
  parent <- network_sub2$parent_s[x]
  find <- network_sub2 %>% filter(child_s == parent)
  if(nrow(find)>0) out <- cbind.data.frame(network_sub2[x,], 'Lat2'=find$Lat, 'Lon2'=find$Lon)
  if(nrow(find)==0) out <- cbind.data.frame(network_sub2[x,], 'Lat2'=NA, 'Lon2'=NA)
  return(out)
})
l2 <- do.call(rbind, l2)

# Waitaki (downstream) catchment map
catchmap <- ggplot() +
  geom_point(data=network_sub2, aes(x = Lon, y = Lat), col="gray") +
  geom_segment(data=l2, aes(x = Lon2,y = Lat2, xend = Lon, yend = Lat), col="gray") +
  geom_point(data=obs_sub2, aes(x = Lon, y = Lat), col="red", alpha=0.6) +
  scale_fill_brewer(palette = "Set1") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Waitaki river network map") +
  theme(plot.margin = unit(c(0.2,0.5,0.2,0.25), "cm"))
ggsave(file.path(fig_dir, "Waitaki_map_ds.png"), catchmap)

# Waitaki (downstream) catchment map across time
catchmap_yr <- ggplot() +
  geom_point(data=network_sub2, aes(x = Lon, y = Lat), col="gray") +
  geom_segment(data=l2, aes(x = Lon2,y = Lat2, xend = Lon, yend = Lat), col="gray") +
  geom_point(data=obs_sub2, aes(x = Lon, y = Lat), col="red", alpha=0.6) +
  facet_wrap(.~Year) +
  scale_fill_brewer(palette = "Set1") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Waitaki river network map") +
  theme(axis.text.x = element_text(angle = 90), plot.margin = unit(c(0.2,0.5,0.2,0.25), "cm"))
ggsave(file.path(fig_dir, "Waitaki_map_yr_ds.png"), catchmap_yr)


