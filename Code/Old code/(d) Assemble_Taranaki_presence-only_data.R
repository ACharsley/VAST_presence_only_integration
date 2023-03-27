###################################################
##      Assemble presence-only data for the      ##
##                 Taranaki region               ##
##                                               ##
##               Anthony Charsley                ##
##                 January 2023                  ##
###################################################

# This code assembles presence-only data, currently using
# abundance data from the NZFFD collected by Taranaki
# Regional Council.

###################################################



rm(list=ls())


#################
#  Directories  #
#################

data_taranaki_dir <- "./Data_processed/Taranaki"
fig_dir <- "./Data_processed/Taranaki/Figures"


##############
#  Packages  #
##############

library(tidyverse)


###############
#  Load data  #
###############

#NZFFD abundance data
NZFFD_abund_TRC_lf <- readRDS(file.path(data_taranaki_dir, "Taranaki_NZFFD_abund_data.rds"))


########################
# Set data to presence #
########################

## Ensure data is formatted equivalently to NZFFD data

presence_only_lf_data <- NZFFD_abund_TRC_lf %>%
  filter(totalCount > 0, Year <= 2020) %>% #data goes until 2021 but remove as presence/absence data not available in 2021
  mutate(org="presence_only", institution="Presence only",
         "Anguilla dieffenbachii"=1) %>%
  select(-c(taxonName, totalCount)) #remove these unnecessary variables


#############
# Save data #
#############

saveRDS(presence_only_lf_data, file.path(data_taranaki_dir, "Taranaki_presence_only_lf_data.rds"))


#############
# Plot data #
#############

Data_to_plot <- presence_only_lf_data
Data_to_plot$present <- ifelse(round(Data_to_plot$`Anguilla dieffenbachii`)==1, "Present", "Absent")

#Load network data
network <- readRDS(file.path(data_taranaki_dir, "Taranaki_network.rds"))

#Taranaki network by year
tab <- table(Data_to_plot$present, Data_to_plot$Year)
years <- unique(Data_to_plot$Year)[order(unique(Data_to_plot$Year))]
data_text <- data.frame("Year"= years, label=tab[1,],
                        x=174, y=-39.8)


catchmap <- ggplot(Data_to_plot) +
  geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
  geom_point(aes(x = Lon, y = Lat, col = present), alpha = 0.6) +
  facet_wrap(.~Year) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Longfin eel TRC - NZFFD presence-only observations by year") +
  guides(color = guide_legend(title = "")) +
  scale_colour_manual(values = "#E41A1C") +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = rel(0.5)),
        axis.text.x = element_text(angle = 90))

catchmap <- catchmap +geom_text(
  data = data_text,
  mapping = aes(x = x, y = y, label = label)
)

ggsave(file.path(fig_dir, "Taranaki_presence_only_observations_byYear.png"), catchmap, height = 12, width = 15)


###################################################

# Taranaki catchment

l2 <- lapply(1:nrow(network), function(x){
  parent <- network$parent_s[x]
  find <- network %>% filter(child_s == parent)
  if(nrow(find)>0) out <- cbind.data.frame(network[x,], 'Lon2'=find$Lon, 'Lat2'=find$Lat)
  if(nrow(find)==0) out <- cbind.data.frame(network[x,], 'Lon2'=NA, 'Lat2'=NA)
  
  return(out)
})
l2 <- do.call(rbind, l2)


catchmap2 <- ggplot(Data_to_plot) +
  geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
  geom_segment(data=l2, aes(x = Lon2,y = Lat2, xend = Lon, yend = Lat), col="gray") +
  geom_point(aes(x = Lon, y = Lat, col = present), size=3, alpha = 0.6) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Longfin eel NZFFD presence/absence observations") +
  guides(color = guide_legend(title = "")) +
  scale_colour_manual(values = "#E41A1C") +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = rel(0.8)))

ggsave(file.path(fig_dir, "Taranaki_presence_only_observations.png"), catchmap2, height = 12, width = 15)
