###################################################
##           Habitat unsuitability model         ##
##             for the Taranaki region           ##
##                                               ##
##               Anthony Charsley                ##
##                December 2022                  ##
###################################################

# This code sets up a full model for the HPC with 
# presence/absence data to determine habitat locations 
# that are unsuitable for eels.

###########################################

rm(list=ls())

setwd("/nesi/nobackup/niwa03347/ACharsley/PhD/Eel_presence_only_integration")


##############
#  Packages  #
##############

library(tidyverse)
library(VAST)


#################
#  Directories  #
#################

data_dir <- file.path(getwd(), "Data")
raw_data <- file.path(data_dir, "raw_data")
data_taranaki_dir <- file.path(data_dir, "Taranaki")
fig_dir <- file.path(data_taranaki_dir, "Figures")
pseudoabsence_data_dir <- file.path(data_taranaki_dir, "Pseudo_absence_data")

####################
#  Call functions  #
####################

source(file.path(getwd(),"Code/funcs.R"))


###################
#  Load datasets  #
###################

#Data for habitat unsuitability modelling
load(file.path(data_taranaki_dir, "Taranaki_data.RData"))

##Network
network <- Taranaki_data_with_ds$network

#Format network data
Network_sz = network %>% select(parent_s,child_s,dist_s)
Network_sz_LL = network %>% select(parent_s, child_s, dist_s, Lat, Lon)

##NZFFD observations
NZFFD_data <- Taranaki_data_with_ds$obs


########################
# Set data up for VAST #
########################

##Set up 'gear'
## Fishing Method:
fct_count(NZFFD_data$fishmeth)

#Reduce to two groups:
#     - EF
#     - Fyke net, trap and visual
NZFFD_data$fishmeth <- fct_collapse(NZFFD_data$fishmeth,
                                    "EF" = c("Electric fishing"),
                                    "NetTrapVisual" = c("Net","Trap","Visual"))
NZFFD_data <- NZFFD_data %>% droplevels()

fct_count(NZFFD_data$fishmeth)


## Organisations
fct_count(NZFFD_data$org)

#Reduce to two groups: 
#     - council_doc_niwa (Council, doc and NIWA - similar target species, sampling protocols likely to be followed) 
#     - others (consultants, fish&game and university - mixed catchability)
NZFFD_data$org <- fct_collapse(NZFFD_data$org,
                               CouncilDocNiwa = c("council","niwa","doc"),
                               Other = c("consultants","fish&game","university"))
NZFFD_data <- NZFFD_data %>% droplevels()

fct_count(NZFFD_data$org)


## Add method_organisation variable
table(NZFFD_data$fishmeth, NZFFD_data$org)

NZFFD_data <- NZFFD_data %>%
  mutate(method_organisation = paste0(fishmeth, "_", org))

fct_count(NZFFD_data$method_organisation)



#Data for longfin eel catch
Data_Geostat = data.frame(Lon=NZFFD_data$Lon,
                          Lat=NZFFD_data$Lat,
                          Child_i=NZFFD_data$child_i,
                          Year=NZFFD_data$Year,
                          Catch_KG=NZFFD_data$angdie, 
                          Meth_Org=NZFFD_data$method_organisation,
                          Length_sampled = 150/1000, #Assuming each sample, sampled a 150m stretch of river
                          Length = NZFFD_data$dist_i) 

set.seed(22)
Data_Geostat[,'Catch_KG'] = Data_Geostat[,'Catch_KG'] * exp(1e-3*rnorm(nrow(Data_Geostat)))

#Check when length sampled is greater than length of river - i.e. doesn't make sense
any(Data_Geostat$Length_sampled > Data_Geostat$Length)

#When the length sampled is greater than the segment length, set length sampled as the segment length
Data_Geostat[Data_Geostat$Length_sampled > Data_Geostat$Length,"Length_sampled"] <- Data_Geostat[Data_Geostat$Length_sampled > Data_Geostat$Length,"Length"]
any(Data_Geostat$Length_sampled > Data_Geostat$Length)


#Table of data
table(NZFFD_data$Year, NZFFD_data$angdie)

##Final data set
pander::pandoc.table( Data_Geostat[1:6,], digits=6 ) #table of the first 6 observations

###############
###############


########################
#  Habitat covariates  #
########################

X_gctp <- Taranaki_data_with_ds$X_gctp

REC_covs <- c("std_Shade", "std_Substrate", "std_Slope", "std_AveTWarm", "std_log_Dist2Coast",
              "std_log_DSDist2Lake")
Barrier_covs <- c("std_Years_since_barrier", "Barrier_present") #NOTE: will only use one or the other, not both


# All covariate names
covars_all <- c(REC_covs, Barrier_covs)

n_p <- length(covars_all)

#Create habitat data matrix at observation locations
n_i <- nrow(Data_Geostat)
n_t <- length(min(Data_Geostat$Year):max(Data_Geostat$Year))
hab_children <- as.numeric(rownames(X_gctp))

X_itp <- array(0, dim=c(n_i,n_t,n_p))
for(i in 1:n_i){
  for(p in 1:n_p){
    child_i <- Data_Geostat$Child_i[i]
    index <- which(hab_children == child_i)
    X_itp[i,,p] <- X_gctp[index,,,p] #All categories are the same so dont need to loop by c
  }
}

#check habitat covariates are right
all(rownames(X_gctp) == network$child_s)
all(rownames(X_itp) == Data_Geostat$Child_i)



#############################
#  Catchability covariates  #
#############################

## Fishing method and organisation catchability covariate ##
a = table(Data_Geostat$Meth_Org)
Survey_max <- names(a[a==max(a)])
Q_ik = ThorsonUtilities::vector_to_design_matrix(Data_Geostat[,'Meth_Org'])
Q_ik = Q_ik[, !(colnames(Q_ik) %in% Survey_max)]

head(Q_ik)

## SAVE ##
save.image(file.path(data_taranaki_dir, paste0("general_inputs_HUM.Rdata")))



#########
# Plots #
#########

Data_to_plot <- NZFFD_data
Data_to_plot$present <- ifelse(round(Data_to_plot$angdie)==1, "Present", "Absent")

## Load full network for plots
netfull <- readRDS(file.path(raw_data, "NZ_network.rds"))

#New Zealand map
nzmap <- ggplot() +
  geom_point(data = netfull, aes(x = long, y = lat), pch = ".") + 
  geom_point(data = network, aes(x = Lon, y = Lat), color = "red", pch = ".") +
  xlab("Longitude") + ylab("Latitude") +
  theme_bw(base_size = 14)

#ggsave(file.path(fig_dir, "Taranaki_on_NZ.png"), nzmap, height = 6, width = 5)
ggsave(file.path(fig_dir, "Taranaki_on_NZ.png"), nzmap, height = 6, width = 5)

###################################################

#Taranaki network by year

tab <- table(Data_to_plot$present, Data_to_plot$Year)
years <- unique(Data_to_plot$Year)[order(unique(Data_to_plot$Year))]

# data_text <- data.frame("Year"= years, label=paste0(tab[2,], "/", tab[1,]), 
#                         x=1600000, y=5050000)

data_text <- data.frame("Year"= years, label=paste0(tab[2,], "/", tab[1,]), 
                        x=174, y=-39.8)


catchmap <- ggplot(Data_to_plot) +
  #geom_point(data = network, aes(x = easting, y = northing), col = "gray") +
  #geom_point(aes(x = easting, y = northing, col = present), alpha = 0.6) +
  geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
  geom_point(aes(x = Lon, y = Lat, col = present), alpha = 0.6) +
  facet_wrap(.~Year) +
  #xlab("Easting") + ylab("Northing") +
  
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Longfin eel NZFFD presence/absence observations by year") +
  #scale_color_brewer(palette = "Set1") +
  guides(color = guide_legend(title = "")) +
  scale_colour_manual(values = c("#377EB8", "#E41A1C")) +
  #scale_colour_manual(values = c("#E41A1C", "green")) +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = rel(0.5)),
        axis.text.x = element_text(angle = 90))

catchmap <- catchmap +geom_text(
  data = data_text,
  mapping = aes(x = x, y = y, label = label)
)

#ggsave(file.path(fig_dir, "Taranaki_lf_observations_byYear.png"), catchmap, height = 12, width = 15)
ggsave(file.path(fig_dir, "Taranaki_lf_observations_byYear.png"), catchmap, height = 12, width = 15)


###################################################

# Taranaki catchment 

l2 <- lapply(1:nrow(network), function(x){
  parent <- network$parent_s[x]
  find <- network %>% filter(child_s == parent)
  # if(nrow(find)>0) out <- cbind.data.frame(network[x,], 'E2'=find$easting, 'N2'=find$northing)
  # if(nrow(find)==0) out <- cbind.data.frame(network[x,], 'E2'=NA, 'N2'=NA)
  
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
  scale_colour_manual(values = c("#377EB8", "#E41A1C")) +
  #scale_colour_manual(values = c("#E41A1C", "#39B600")) +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = rel(0.8)))

#ggsave(file.path(fig_dir, "Taranaki_lf_observations.png"), catchmap2, height = 12, width = 15)
ggsave(file.path(fig_dir, "Taranaki_lf_observations_ds.png"), catchmap2, height = 12, width = 15)

###################################################
###################################################
