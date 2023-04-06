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
               "1a", "1b", "1c", "1d", 
               "2a", "2b", "2c", "2d",
               "3a", "3b", "3c", "3d",
               "4a", "4b", "4c", "4d")
network_type <- "downstream" #full

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
  X_gctp <- data_list$X_gctp_ds
  }
  if(network_type == "full"){
  obs <- data_list$obs
  network <- data_list$network
  X_gctp <- data_list$X_gctp
  }
  
  
  ########################
  # Set data up for VAST #
  ########################
  
  ## 1. Build data type variable and build catchability covariate
  obs$Data_type <- ifelse(obs$FishMethod == "Electric fishing", "Structured_EF", 
                          ifelse(obs$FishMethod %in% c("Net", "Trap"), "Structured_NetTrap", 
                                 ifelse(obs$FishMethod == "Other", "Unstructured", NA)))
  fct_count(obs$Data_type)

  ## 2. Examine fishing method
  fct_count(obs$FishMethod)

  ## 3. Examine fish samplers
  fct_count(obs$org) #Will assume catchability is constant between organisations
  
  #Examine fishing method and fish samplers together
  addmargins(table(obs$FishMethod, obs$org, useNA = "ifany"))
  
  ## 4. Examine Years of data
  addmargins(table(obs$`Anguilla dieffenbachii`, obs$Year))
  #addmargins(table(obs$Year, obs$`Anguilla dieffenbachii`))
  
  addmargins(table(obs$Year, obs$Data_type)) #Some years, data type doesn't overlap. Is that an issue?
  
  #Remove pre-1970s data
  obs <- obs %>% filter(Year >= 1970)
  
  ## 5. Create final dataset
  #Data for longfin eel catch
  Data_Geostat = data.frame(Lon=obs$Lon,
                            Lat=obs$Lat,
                            Child_i=obs$child_i,
                            Year=obs$Year,
                            Catch_KG=obs$`Anguilla dieffenbachii`, 
                            Data_type=obs$Data_type,
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
  covars_all <- dimnames(X_gctp)[[4]]
  n_p <- length(covars_all)
  
  #Create habitat data matrix at observation locations
  n_i <- nrow(Data_Geostat)
  yrs <- min(Data_Geostat$Year):max(Data_Geostat$Year)
  n_t <- length(yrs)
  hab_children <- as.numeric(rownames(X_gctp))
  
  X_gctp <- X_gctp[,,as.character(yrs),,drop=F] # This ensures that if any years are dropped then X_gctp has the right years still
  
  X_itp <- array(0, dim=c(n_i,n_t,n_p))
  for(i in 1:n_i){
    for(p in 1:n_p){
      child_i <- Data_Geostat$Child_i[i]
      index <- which(hab_children == child_i)
      X_itp[i,,p] <- X_gctp[index,,as.character(yrs),p] #All categories are the same so dont need to loop by c
    }
  }
  
  #check habitat covariates are right
  all(rownames(X_gctp) == network$child_s)
  all(rownames(X_itp) == Data_Geostat$Child_i)
  
  ###############
  ###############
  
  ## Save data ##
  VAST_input_data[[sce]]$Data_Geostat <- Data_Geostat
  VAST_input_data[[sce]]$network <- network
  VAST_input_data[[sce]]$X_gctp <- X_gctp
  VAST_input_data[[sce]]$X_itp <- X_itp
  
}

if(network_type == "downstream"){
  saveRDS(VAST_input_data, file.path(VAST_input_data_dir, "VAST_input_data_ds.rds"))
  }
if(network_type == "full"){
  saveRDS(VAST_input_data, file.path(VAST_input_data_dir, "VAST_input_data.rds"))
  }


#VAST_input_data <- readRDS(file.path(VAST_input_data_dir, "VAST_input_data_ds.rds"))


#########
# Plots #
#########
Data_to_plot <- VAST_input_data[["1a"]]$Data_Geostat
Data_to_plot$present <- ifelse(round(Data_to_plot$Catch_KG)==1, "Encounter", "Non-encounter")

## Load full network for plots
netfull <- readRDS(file.path(raw_data_dir, "NZ_network.rds"))

#New Zealand map
nzmap <- ggplot() +
  geom_point(data = netfull, aes(x = long, y = lat), pch = ".") +
  geom_point(data = network, aes(x = Lon, y = Lat), color = "red", pch = ".") +
  xlab("Longitude") + ylab("Latitude") +
  theme_bw(base_size = 14)

if(network_type == "downstream"){
  ggsave(file.path(fig_dir, "Taranaki_on_NZ_ds.png"), nzmap, height = 6, width = 5)
  }
if(network_type == "full"){
  ggsave(file.path(fig_dir, "Taranaki_on_NZ.png"), nzmap, height = 6, width = 5)
  }


###################################################
#Taranaki network by year
tab <- table(Data_to_plot$present, Data_to_plot$Year)
years <- unique(Data_to_plot$Year)[order(unique(Data_to_plot$Year))]

data_text <- data.frame("Year"= years, label=paste0(tab[2,], "/", tab[1,]),
                        x=174, y=-39.8)


## Structured electric fishing data 
Data_to_plot_struc_EF <- Data_to_plot %>% filter(Data_type == "Structured_EF")

catchmap_struc_EF <- ggplot(Data_to_plot_struc_EF) +
  geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
  geom_point(aes(x = Lon, y = Lat, col = present), alpha = 0.6) +
  facet_wrap(.~Year) +
  
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Longfin eel structured electric fishing data from the NZFFD by year") +
  guides(color = guide_legend(title = "")) +
  #scale_colour_manual(values = c("#377EB8", "#E41A1C")) +
  scale_colour_manual(values = c("green", "#E41A1C")) +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = rel(0.5)),
        axis.text.x = element_text(angle = 90))

catchmap_struc_EF <- catchmap_struc_EF +geom_text(
  data = data_text,
  mapping = aes(x = x, y = y, label = label)
)

if(network_type == "downstream"){
  ggsave(file.path(fig_dir, "Taranaki_struc_EF_byYear_ds.png"), catchmap_struc_EF, height = 12, width = 15)
  }
if(network_type == "full"){
  ggsave(file.path(fig_dir, "Taranaki_struc_EF_byYear.png"), catchmap_struc_EF, height = 12, width = 15)
  }


## Structured Net and Trap data
Data_to_plot_struc_NetTrap <- Data_to_plot %>% filter(Data_type == "Structured_NetTrap")

catchmap_struc_NetTrap <- ggplot(Data_to_plot_struc_NetTrap) +
  geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
  geom_point(aes(x = Lon, y = Lat, col = present), alpha = 0.6) +
  facet_wrap(.~Year) +
  
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Longfin eel structured Net and Trap data from the NZFFD by year") +
  guides(color = guide_legend(title = "")) +
  #scale_colour_manual(values = c("#377EB8", "#E41A1C")) +
  scale_colour_manual(values = c("green", "#E41A1C")) +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = rel(0.5)),
        axis.text.x = element_text(angle = 90))

catchmap_struc_NetTrap <- catchmap_struc_NetTrap +geom_text(
  data = data_text,
  mapping = aes(x = x, y = y, label = label)
)

if(network_type == "downstream"){
  ggsave(file.path(fig_dir, "Taranaki_struc_NetTrap_byYear_ds.png"), catchmap_struc_NetTrap, height = 12, width = 15)
}
if(network_type == "full"){
  ggsave(file.path(fig_dir, "Taranaki_struc_NetTrap_byYear.png"), catchmap_struc_NetTrap, height = 12, width = 15)
}


## Unstructured data
Data_to_plot_unstruc <- Data_to_plot %>% filter(Data_type == "Unstructured")

catchmap_unstruc <- ggplot(Data_to_plot_unstruc) +
  geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
  geom_point(aes(x = Lon, y = Lat, col = present), alpha = 0.6) +
  facet_wrap(.~Year) +
  
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Longfin eel unstructured data from the NZFFD by year") +
  guides(color = guide_legend(title = "")) +
  #scale_colour_manual(values = c("#377EB8", "#E41A1C")) +
  scale_colour_manual(values = c("green", "#E41A1C")) +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = rel(0.5)),
        axis.text.x = element_text(angle = 90))

catchmap_unstruc <- catchmap_unstruc +geom_text(
  data = data_text,
  mapping = aes(x = x, y = y, label = label)
)

if(network_type == "downstream"){
  ggsave(file.path(fig_dir, "Taranaki_unstruc_byYear_ds.png"), catchmap_unstruc, height = 12, width = 15)
}
if(network_type == "full"){
  ggsave(file.path(fig_dir, "Taranaki_unstruc_byYear.png"), catchmap_unstruc, height = 12, width = 15)
}

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

## Structured electric fishing data 
catchmap_struc_EF_2 <- ggplot(Data_to_plot_struc_EF) +
  geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
  geom_segment(data=l2, aes(x = Lon2,y = Lat2, xend = Lon, yend = Lat), col="gray") +
  geom_point(aes(x = Lon, y = Lat, col = present), alpha = 0.6) +
  
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Longfin eel structured electric fishing data from the NZFFD") +
  guides(color = guide_legend(title = "")) +
  #scale_colour_manual(values = c("#377EB8", "#E41A1C")) +
  scale_colour_manual(values = c("green", "#E41A1C")) +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = rel(0.5)),
        axis.text.x = element_text(angle = 90))

if(network_type == "downstream"){
  ggsave(file.path(fig_dir, "Taranaki_struc_EF_ds.png"), catchmap_struc_EF_2, height = 12, width = 15)
}
if(network_type == "full"){
  ggsave(file.path(fig_dir, "Taranaki_struc_EF.png"), catchmap_struc_EF_2, height = 12, width = 15)
}


## Structured Net and Trap data
catchmap_struc_NetTrap_2 <- ggplot(Data_to_plot_struc_NetTrap) +
  geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
  geom_segment(data=l2, aes(x = Lon2,y = Lat2, xend = Lon, yend = Lat), col="gray") +
  geom_point(aes(x = Lon, y = Lat, col = present), alpha = 0.6) +
  
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Longfin eel structured Net and Trap data from the NZFFD") +
  guides(color = guide_legend(title = "")) +
  #scale_colour_manual(values = c("#377EB8", "#E41A1C")) +
  scale_colour_manual(values = c("green", "#E41A1C")) +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = rel(0.5)),
        axis.text.x = element_text(angle = 90))

if(network_type == "downstream"){
  ggsave(file.path(fig_dir, "Taranaki_struc_NetTrap_ds.png"), catchmap_struc_NetTrap_2, height = 12, width = 15)
}
if(network_type == "full"){
  ggsave(file.path(fig_dir, "Taranaki_struc_NetTrap.png"), catchmap_struc_NetTrap_2, height = 12, width = 15)
}


## Unstructured data
catchmap_unstruc_2 <- ggplot(Data_to_plot_unstruc) +
  geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
  geom_segment(data=l2, aes(x = Lon2,y = Lat2, xend = Lon, yend = Lat), col="gray") +
  geom_point(aes(x = Lon, y = Lat, col = present), alpha = 0.6) +
  
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Longfin eel unstructured data from the NZFFD") +
  guides(color = guide_legend(title = "")) +
  #scale_colour_manual(values = c("#377EB8", "#E41A1C")) +
  scale_colour_manual(values = c("green", "#E41A1C")) +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = rel(0.5)),
        axis.text.x = element_text(angle = 90))

if(network_type == "downstream"){
  ggsave(file.path(fig_dir, "Taranaki_unstruc_ds.png"), catchmap_unstruc_2, height = 12, width = 15)
}
if(network_type == "full"){
  ggsave(file.path(fig_dir, "Taranaki_unstruc.png"), catchmap_unstruc_2, height = 12, width = 15)
}

###################################################
###################################################


