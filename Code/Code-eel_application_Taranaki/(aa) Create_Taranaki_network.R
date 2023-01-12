###################################################
##          Create Taranaki network data         ## 
##                                               ##
##                Anthony Charsley               ##
##                 November 2022                 ##
###################################################

# This code creates a stream network of the Taranki region  
# in New Zealand from REC2 data for stream network modelling.

###################################################

rm(list=ls())


#################
#  Directories  #
#################

data_dir <- "./Data_processed"
raw_data_dir <- "./Data_raw"

data_taranaki_dir <- "./Data_processed/Taranaki"
dir.create(data_taranaki_dir, showWarnings=FALSE)

fig_dir <- file.path(data_taranaki_dir, "Figures")
dir.create(fig_dir, showWarnings=FALSE)


##############
#  Packages  #
##############

library(tidyverse)
library(proj4)


##########################
#  Load and filter data  #
##########################

## Raw REC network data
load(file.path(raw_data_dir, "REC2.4_variables.RData"))
network_raw <- REC2.4 ; rm(REC2.4)

#Covariates used in the Ngai Tahu study, perhaps reassess later ??
REC_covs <- c("FWENZ_SegRipShade", "FWENZ_segSubstrate", "loc_slope", "FWENZ_segAveTWarm",
              "Dist2Coast_FromMid", "DSDIST2LAK", "FWENZ_DSDamAffected")

# Create network for the taranaki with variables of interest
network <- network_raw %>%
  select(CatName, nzsegment, upcoordX, upcoordY, fnode, tnode, Shape_Leng, WidthMeanFlow, StreamOrder, FWENZ_isLake,
         isTerminal, rcID, all_of(REC_covs)) %>%
  rename('northing'=upcoordY, 'easting'=upcoordX, 'parent_s' = tnode, 'child_s' = fnode, #Coordinates and nodes
         'length'=Shape_Leng, 'width'=WidthMeanFlow,  #River dimensions
         'Shade' = FWENZ_SegRipShade, 'Substrate' = FWENZ_segSubstrate, 'Slope' = loc_slope, #covariates
         'AveTWarm' = FWENZ_segAveTWarm, 'Dist2Coast' = Dist2Coast_FromMid, 'DSDist2Lake'= DSDIST2LAK, #covariates
         'DSDamAffected'=FWENZ_DSDamAffected) %>%
  mutate('length' = length / 1000, 'width' = width/1000) %>% #in km
  mutate(rcID = factor(rcID)) %>%
  filter(rcID == 6) #rcID 6 is the taranaki region, better to identify specific catchments as some rivers may be missed (talk to Shan)

#Create variable 'dist_s"
network <- network %>% 
  mutate('dist_s' = length) #Add dist_s.

#Change names of REC_covs
REC_covs <- c('Shade', 'Substrate', 'Slope', 'AveTWarm', 'Dist2Coast', 'DSDist2Lake', 'DSDamAffected')

#child nodes should be unique
length(unique(network$child_s)) == nrow(network)

#Network variables
colnames(network)

#Check missing
sapply(1:ncol(network), function(x) length(which(is.na(network[,x])))/nrow(network))

# #Remove NAs from network
# anyNA(network)
# sapply(1:ncol(network), function(x) length(which(is.na(network[,x])))/nrow(network)) #0 missing all columns


#############################################
#  Construct nodes and root nodes for VAST  #
#############################################

## identify root nodes
root_nodes <- which((network$parent_s %in% network$child_s) == FALSE)

## Create end nodes (to sea)
root_list <- lapply(1:length(root_nodes), function(x){
  sub <- network[root_nodes[x],]
  df <- sub %>% mutate('child_s'=parent_s) %>% 
    mutate('parent_s'=0) %>% 
    mutate('dist_s'=Inf) 
  return(df)
})
roots <- do.call(rbind, root_list)


#############################
#If roots are non-unique then either one is selected or they are both eliminated
if(length(unique(roots$child_s)) != nrow(roots)){
  
  child_roots <- unique(roots$child_s)
  root_byChild <- lapply(1:length(child_roots), function(x){
    sub <- roots %>% filter(child_s == child_roots[x])
    return(sub)
  })
  ii <- sapply(1:length(root_byChild), function(x) nrow(root_byChild[[x]]))
  root_single <- root_byChild[which(ii==1)]
  
  root_multi <- root_byChild[which(ii > 1)]
  multi_to_single <- lapply(1:length(root_multi), function(x){
    sub <- root_multi[[x]]
    if(all(sub$parent_s == sub$parent_s[1]) & all(sub$child_s == sub$child_s[1])){
      out <- sub[1,]
    } else {
      out <- NULL
    }
    return(out)
  })
  any(is.null(multi_to_single))
  root_single2 <- do.call(rbind, root_single)
  root_single3 <- do.call(rbind, multi_to_single)
  root_toUse <- unique(rbind.data.frame(root_single2, root_single3))
  
}else{
  
  root_toUse <- roots
  
}
#############################

#Ensure roots have slightly different coordinates than the joining segment
root_toUse <- root_toUse %>% mutate(easting = easting + runif(length(easting),-0.01,0.01))

#Connect roots to network
network_all <- rbind.data.frame(network, unique(root_toUse))

#Check all child_s are unique
nrow(network_all) == length(unique(network_all$child_s))

## Check all eastings / northings to be unique
nrow(unique(network_all %>% select(easting,northing))) == nrow(network_all)

#The following runs code if eastings / northings are non-unique

# e_mult <- names(table(network_all$easting))[which(table(network_all$easting)>1)]
# e_uni <- network_all %>% filter(easting %in% e_mult == FALSE)
# set.seed(123)
# e_rep <- network_all %>% filter(easting %in% e_mult) %>% mutate(easting = easting + runif(length(easting),-1,1)) 
# 
# network_all2 <- rbind.data.frame(e_uni, e_rep)
# max(table(network_all2$easting))
# 
# n_mult <- names(table(network_all2$northing))[which(table(network_all2$northing)>1)]
# n_uni <- network_all2 %>% filter(northing %in% n_mult == FALSE)
# set.seed(456)
# n_rep <- network_all2 %>% filter(northing %in% n_mult) %>% mutate(northing = northing + runif(length(northing),-1,1))
# 
# network_all3 <- rbind.data.frame(n_uni, n_rep)
# max(table(network_all3$northing))

############################
#  Latitude and longitude  #
############################

## function to calculate latitude and longitude from eastings and northings
calc_NZ_latlon <- function(northing, easting){
  proj4string <- "+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +datum=WGS84 +units=m"
  p <- project(matrix(c(easting, northing),nrow=1), proj=proj4string, inv=T)
  colnames(p) <- c('Lon', 'Lat')
  return(p)
}

## latitude and longitude for child nodes in network
network_ll_child <- lapply(1:nrow(network_all), function(x){
  p <- calc_NZ_latlon(northing = network_all$northing[x], easting = network_all$easting[x])
  return(p)
})
network_ll_child <- do.call(rbind, network_ll_child)

## attach latitude and longtiude to network
network_full <- cbind.data.frame(network_all, network_ll_child)
nrow(network_full)
nrow(unique(network_full))
nrow(network_full %>% select('Lat','Lon'))
nrow(network_full %>% select('easting','northing'))


################
# Rename nodes #
################

####
nodes <- unique(c(network_full$child_s, network_full$parent_s))
inodes <- seq_along(nodes)

net_parents <- sapply(1:nrow(network_full), function(x){
  if(network_full$parent_s[x] != 0) new_node <- inodes[which(nodes == network_full$parent_s[x])]
  if(network_full$parent_s[x] == 0) new_node <- 0
  return(new_node)
})
net_children <- sapply(1:nrow(network_full), function(x) inodes[which(nodes == network_full$child_s[x])])

network_full$parent_s <- net_parents
network_full$child_s <- net_children
####


########
# Save #
########

saveRDS(network_full, file.path(data_taranaki_dir, "Taranaki_network_aa.rds"))


