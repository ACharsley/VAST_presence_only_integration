



rm(list=ls())


#################
#  Directories  #
#################

raw_data <- "./Data/raw_data"

data_waitaki_dir <- "./Data/Taranaki"

fig_dir <- file.path(data_waitaki_dir, "Figures")


##############
#  Packages  #
##############

library(tidyverse)
library(proj4)


##########################
#  Load and filter data  #
##########################

## Raw REC network data
load(file.path(raw_data, "REC2.4_variables.RData"))
network_raw <- REC2.4 ; rm(REC2.4)


net_to_plot <- network_raw %>%
  select(CatName, nzsegment, upcoordX, upcoordY, fnode, tnode, Shape_Leng, WidthMeanFlow, StreamOrder, FWENZ_isLake,
         isTerminal, rcID) %>% 
  filter(upcoordY > 4000000) %>%
  mutate(rcID = factor(rcID))

# NZ plot
NZ_net <- ggplot(net_to_plot) +
  geom_point(aes(upcoordX, upcoordY, col=rcID))
ggsave(file.path(fig_dir, "NZ_network_rcID.png"), NZ_net)


net_taranaki <- net_to_plot %>% mutate(Taranaki = factor(ifelse(rcID==6, 2, 1)))

# Taranaki plot
taranaki_on_NZ <- ggplot(net_taranaki) +
  geom_point(aes(upcoordX, upcoordY, col=Taranaki))
ggsave(file.path(fig_dir, "Taranaki_on_NZ.png"), taranaki_on_NZ)


#Taranaki, Manawatu/Ruapehu, Waikato
net_TMRW <- net_to_plot %>% filter(rcID %in% c("3", "6", "7"))

NZplot <- ggplot(net_to_plot) +
  geom_point(aes(upcoordX, upcoordY))

regions_on_NZ <- NZplot +
  geom_point(data=net_TMRW, aes(upcoordX, upcoordY, col=rcID))
ggsave(file.path(fig_dir, "NZ_network_filtered_rcID.png"), regions_on_NZ)
#This plot confirms we are looking at the regions around Taranaki - now look at the individual catchments!



###################
# Catchment plots #
###################

length(unique(net_TMRW$CatName))

#Lots to consider and many only have 1 so may be impossible to separate out by CatName. Could use 'catch' in NZFFD
#but this would require joining the datasets together and may have the same issue.



