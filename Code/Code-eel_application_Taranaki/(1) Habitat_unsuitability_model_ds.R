###################################################
##           Habitat unsuitability model         ##
##             for the Taranaki region           ##
##                                               ##
##                Downstream model               ##
##                                               ##
##               Anthony Charsley                ##
##                December 2022                  ##
###################################################

# This code builds a 'downstream' model with presence/absence 
# data to determine habitat locations that are unsuitable for 
# eels.
#
# I will use VAST to build this model.

###########################################

rm(list=ls())


##############
#  Packages  #
##############

library(tidyverse)
library(VAST)


#################
#  Directories  #
#################

data_dir <- "./Data"
raw_data <- "./Data/raw_data"
data_taranaki_dir <- "./Data/Taranaki"
fig_dir <- "./Data/Taranaki/Figures"
pseudoabsence_data_dir <- file.path(data_taranaki_dir, "Pseudo_absence_data")

####################
#  Call functions  #
####################

source("./Code/funcs.R")


###################
#  Load datasets  #
###################

#Data for habitat unsuitability modelling
load(file.path(data_taranaki_dir, "Taranaki_data.RData"))

##Network
network <- Taranaki_data_with_ds$network_ds

#Format network data
Network_sz = network %>% select(parent_s,child_s,dist_s)
Network_sz_LL = network %>% select(parent_s, child_s, dist_s, Lat, Lon)

##Habitat data
hab_REC_full <- Taranaki_data_with_ds$habitat_ds

##NZFFD observations
NZFFD_data <- Taranaki_data_with_ds$obs_ds


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

X_gctp <- Taranaki_data_with_ds$X_gctp_ds

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
save.image(file.path(data_taranaki_dir, paste0("general_inputs_HUM_ds.Rdata")))



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
ggsave(file.path(fig_dir, "Taranaki_on_NZ_ds.png"), nzmap, height = 6, width = 5)

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
ggsave(file.path(fig_dir, "Taranaki_lf_observations_byYear_ds.png"), catchmap, height = 12, width = 15)


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



###############
# Build model #
###############

library(tidyverse)
library(VAST)


#########################
# Model - no covariates #
#########################

#Removing all covariates

rm(list=ls())

data_taranaki_dir <- "./Data/Taranaki"

# Load inputs
load(file.path(data_taranaki_dir, paste0("general_inputs_HUM_ds.Rdata")))

# Set paths
path <- "./Models/HUM_Taranaki_nocovariates"
fig <- file.path(path, "Figures")
dir.create(path, showWarnings = FALSE)
dir.create(fig, showWarnings=FALSE)

# TURN OFF all covariates 
X1config_inp <- array(0, dim = c(1,n_p))

# TURN OFF covariates in 2nd predictor
X2config_inp <- array(0, dim = c(1,n_p))

#Set up covariate formula
covars_to_use <- covars_all[which(X1config_inp == 1)]
X1_formula_inp = X2_formula_inp = paste0("~",(paste0(covars_to_use, collapse = "+")))

# Number of factors in each linear predictor (linear predictor 2 turned off)
FieldConfig <- c("Omega1" = 1, "Epsilon1" = 1, "Omega2" = 0, "Epsilon2" = 0) #1 category

# Controls structure on temporal and spatio-temporal effects - both set to a random walk (linear predictor 2 turned off)
RhoConfig <- c("Beta1" = 1, "Beta2" = 3, "Epsilon1" = 0, "Epsilon2" = 0)

# Model settings for a binomial model
ObsModel <- c(2,0)
OverdispersionConfig <- c("Eta1" = 0, "Eta2" = 0)
Options <- c("Calculate_range" = 1, "Calculate_effective_area" = 1) #I want to calculate these metrics


# Put all together into model settings
settings <- make_settings(n_x = nrow(Network_sz),
                          purpose = "index2",
                          Region = "Stream_network",
                          fine_scale = FALSE,
                          FieldConfig = FieldConfig,
                          RhoConfig = RhoConfig,
                          OverdispersionConfig = OverdispersionConfig,
                          ObsModel = ObsModel,
                          bias.correct = FALSE,
                          Options = Options,
                          Version = "VAST_v14_0_1") #latest version downloaded on computer, a later version is on the HPC
settings$Method <- "Stream_network"
settings$grid_size_km <- 1


start1=Sys.time() #measure how long it takes

## fit0 - Compile the model and set up the model structure specified in settings.
fit0 <- fit_model(settings = settings,
                  Lat_i = Data_Geostat$Lat,
                  Lon_i = Data_Geostat$Lon,
                  t_i = Data_Geostat$Year,
                  b_i = Data_Geostat$Catch_KG,
                  #a_i = Data_Geostat$Length_sampled,
                  a_i = as_units(rep(1, nrow(Data_Geostat)), unitless),
                  Q_ik = Q_ik,
                  working_dir = path,
                  X_gctp = X_gctp,
                  X_itp = X_itp,
                  X1_formula = X1_formula_inp,
                  X1config_cp = X1config_inp,
                  X2_formula = X2_formula_inp,
                  X2config_cp = X2config_inp,
                  run_model = FALSE,
                  
                  extrapolation_args = list(input_grid = cbind("Lat" = Data_Geostat$Lat, "Lon" = Data_Geostat$Lon, 
                                                               "child_i" = Data_Geostat$Child_i, 
                                                               "Area_km2" = Data_Geostat$Length)),
                  spatial_args = list(Network_sz_LL = Network_sz_LL),
                  Network_sz = Network_sz)



#Try changing starting values
Par <- fit0$tmb_list$Parameters
Map <- fit0$tmb_list$Map


#Map off lambda2_k as 2nd component is ignored
Map[["lambda2_k"]] <- factor(rep(NA, length(Par[["lambda2_k"]]))) 


## fit1 - check if the model parameters are identifiable.
fit1 <- fit_model(settings = settings,
                  Lat_i = Data_Geostat$Lat,
                  Lon_i = Data_Geostat$Lon,
                  t_i = Data_Geostat$Year,
                  b_i = Data_Geostat$Catch_KG,
                  #a_i = Data_Geostat$Length_sampled,
                  a_i = as_units(rep(1, nrow(Data_Geostat)), unitless),
                  Q_ik = Q_ik,
                  working_dir = path,
                  X_gctp = X_gctp,
                  X_itp = X_itp,
                  X1_formula = X1_formula_inp,
                  X1config_cp = X1config_inp,
                  X2_formula = X2_formula_inp,
                  X2config_cp = X2config_inp,
                  extrapolation_args = list(input_grid = cbind("Lat" = Data_Geostat$Lat, "Lon" = Data_Geostat$Lon, 
                                                               "child_i" = Data_Geostat$Child_i, 
                                                               "Area_km2" = Data_Geostat$Length)),
                  spatial_args = list(Network_sz_LL = Network_sz_LL),
                  Network_sz = Network_sz,
                  
                  Parameters = Par,
                  Map = Map,
                  
                  newtonsteps = 3,
                  test_fit = FALSE,
                  optimize_args = list(getsd = FALSE))
TMBhelper::check_estimability(fit1$tmb_list$Obj) #if(check$WhichBad>0) {stop()}
#saveRDS(check, file.path(path, "check.rds"))

saveRDS(fit1, file.path(path, "fit1.rds"))

## fit - Run the model, estimating standard errors (should converge if model is checked properly)
fit <- fit_model(settings = settings,
                 Lat_i = Data_Geostat$Lat,
                 Lon_i = Data_Geostat$Lon,
                 t_i = Data_Geostat$Year,
                 b_i = Data_Geostat$Catch_KG,
                 #a_i = Data_Geostat$Length_sampled,
                 a_i = as_units(rep(1, nrow(Data_Geostat)), unitless),
                 Q_ik = Q_ik,
                 working_dir = path,
                 X_gctp = X_gctp,
                 X_itp = X_itp,
                 X1_formula = X1_formula_inp,
                 X1config_cp = X1config_inp,
                 X2_formula = X2_formula_inp,
                 X2config_cp = X2config_inp,
                 extrapolation_args = list(input_grid = cbind("Lat" = Data_Geostat$Lat, "Lon" = Data_Geostat$Lon, 
                                                              "child_i" = Data_Geostat$Child_i, 
                                                              "Area_km2" = Data_Geostat$Length)),
                 spatial_args = list(Network_sz_LL = Network_sz_LL),
                 Network_sz = Network_sz,
                 
                 Parameters = Par,
                 Map = Map,
                 
                 newtonsteps = 3,
                 test_fit = FALSE,
                 optimize_args = list(startpar = fit1$parameter_estimates$par))
saveRDS(fit, file.path(path, "Fit.rds"))

end1 <- Sys.time()
time1 <- end1-start1 ; time1

#fit <- readRDS(file.path(path, "Fit.rds"))


#####
## Check model ##
dharmaRes = summary(fit, what="residuals", working_dir=paste0(fig,"/"), type=1)
#dharmaRes <- readRDS(file.path(path, "dharmaRes.rds"))
plot_residuals(residuals=dharmaRes$scaledResiduals, fit=fit, save_dir=fig,
               Data_inp = Data_Geostat, network=network, coords="lat_long")

saveRDS(dharmaRes, file.path(path, "dharmaRes.rds"))
#dharmaRes <- readRDS(file.path(path, "dharmaRes.rds"))
####

## dharma plots ##
# Histogram of residuals #
jpeg(file.path(fig, "Resid_hist.jpg"), width = 600, height = 600)
hist(dharmaRes$scaledResiduals, 
     breaks = seq(-0.02, 1.02, len = 53),
     col = c("red",rep("lightgrey",50), "red"),
     main = "Hist of DHARMa residuals",
     xlab = "Residuals (outliers are marked red)",
     cex.main = 1)
dev.off()
##
## QQ plot ##
jpeg(file.path(fig, "QQplot.jpg"), width = 600, height = 600)
gap::qqunif(dharmaRes$scaledResiduals,
            pch=2,
            bty="n", 
            logscale = F, 
            col = "black", 
            cex = 0.6, 
            main = "QQ plot residuals", 
            cex.main = 1)
dev.off()
##
####



## Model comparisons ##
fit$parameter_estimates$AIC #188.0617


full_name = "Longfin eel"
sp = "lf"

##################################
# Probability of encounter plots #
##################################

# Map of sp probability of encounter across time
POE_array <- plot_maps_network(plot_set = 1, 
                               fit = fit, 
                               Sdreport = fit$parameter_estimates$SD, 
                               TmbData = fit$data_list, 
                               spatial_list = fit$spatial_list, 
                               DirName = fig, 
                               Panel = "category", 
                               PlotName = paste0("POE_",sp,"_yearly"),
                               PlotTitle = paste0(full_name," yearly probability of encounter in Taranaki, NZ"),
                               cex = 0.5, 
                               Zlim = c(0,1), 
                               arrows=F, 
                               pch=15)

saveRDS(POE_array, file.path(fig, "POE_array.rds"))


#############################################################
##            Difference between 2000 and 1978             ##
#############################################################


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

POE_1978 <- POE_array[,1,which(c(1978:2021)==1978)]
POE_2000 <- POE_array[,1,which(c(1978:2021)==2000)]
POE_2021 <- POE_array[,1,which(c(1978:2021)==2021)]


#Create data frame for plotting
xct <- data.frame('value'=POE_2000 - POE_1978, 'year'=2000, fit$spatial_list$latlon_g, "category"="Longfin")

Xlim = c(min(xct$Lon),max(xct$Lon))
Ylim = c(min(xct$Lat),max(xct$Lat))

#Z limits should be set for consistency between years
inp_Zlim = c(-1,1)

p <- ggplot(xct)

p <- p + geom_segment(data=l2, aes(x = Lon,y = Lat, xend = Lon2, yend = Lat2), col="gray92")

p <- p +
  geom_point(aes(x = Lon, y = Lat, color = value), cex = 0.5, pch = 19) +
  scale_color_distiller(palette = "RdYlGn", limits = inp_Zlim, direction = 1) +
  coord_cartesian(xlim = Xlim, ylim = Ylim) +
  scale_x_continuous(breaks=quantile(xct$Lon, prob=c(0.1,0.5,0.9)), labels=round(quantile(xct$Lon, prob=c(0.1,0.5,0.9)),0)) +
  # guides(color=guide_legend(title=plot_codes[plot_num])) +
  mytheme() +
  xlab("Longitude") + ylab("Latitude")

p <- p + ggtitle(wrapper("Difference in probability of encounter in 2000 compared to 1978", width = 50))
ggsave(file.path(fig, paste0("POE_diff2000_1978.png")), p, width=8,height=8)
####

## Difference between 2021 and 1978 ##
#Create data frame for plotting
xct <- data.frame('value'=POE_2021 - POE_1978, 'year'=2021, fit$spatial_list$latlon_g, "category"="Longfin")

Xlim = c(min(xct$Lon),max(xct$Lon))
Ylim = c(min(xct$Lat),max(xct$Lat))

#Z limits should be set for consistency between years
inp_Zlim = c(-1,1)

p <- ggplot(xct)

p <- p + geom_segment(data=l2, aes(x = Lon,y = Lat, xend = Lon2, yend = Lat2), col="gray92")

p <- p +
  geom_point(aes(x = Lon, y = Lat, color = value), cex = 0.5, pch = 19) +
  scale_color_distiller(palette = "RdYlGn", limits = inp_Zlim, direction = 1) +
  coord_cartesian(xlim = Xlim, ylim = Ylim) +
  scale_x_continuous(breaks=quantile(xct$Lon, prob=c(0.1,0.5,0.9)), labels=round(quantile(xct$Lon, prob=c(0.1,0.5,0.9)),0)) +
  # guides(color=guide_legend(title=plot_codes[plot_num])) +
  mytheme() +
  xlab("Longitude") + ylab("Latitude")

p <- p + ggtitle(wrapper("Difference in probability of encounter in 2021 compared to 1978", width = 50))
ggsave(file.path(fig, paste0("POE_diff2021_1978.png")), p, width=8,height=8)


## Difference between 2021 and 2000 ##
#Create data frame for plotting
xct <- data.frame('value'=POE_2021 - POE_2000, 'year'=2021, fit$spatial_list$latlon_g, "category"="Longfin")

Xlim = c(min(xct$Lon),max(xct$Lon))
Ylim = c(min(xct$Lat),max(xct$Lat))

#Z limits should be set for consistency between years
inp_Zlim = c(-1,1)

p <- ggplot(xct)

p <- p + geom_segment(data=l2, aes(x = Lon,y = Lat, xend = Lon2, yend = Lat2), col="gray92")

p <- p +
  geom_point(aes(x = Lon, y = Lat, color = value), cex = 0.5, pch = 19) +
  scale_color_distiller(palette = "RdYlGn", limits = inp_Zlim, direction = 1) +
  coord_cartesian(xlim = Xlim, ylim = Ylim) +
  scale_x_continuous(breaks=quantile(xct$Lon, prob=c(0.1,0.5,0.9)), labels=round(quantile(xct$Lon, prob=c(0.1,0.5,0.9)),0)) +
  # guides(color=guide_legend(title=plot_codes[plot_num])) +
  mytheme() +
  xlab("Longitude") + ylab("Latitude")

p <- p + ggtitle(wrapper("Difference in probability of encounter in 2021 compared to 2000", width = 50))
ggsave(file.path(fig, paste0("POE_diff2021_2000.png")), p, width=8,height=8)


#############################################################
#############################################################



# Map of sp probability of encounter for each year
plot_maps_network(plot_set = 1, 
                  fit = fit, 
                  Sdreport = fit$parameter_estimates$SD, 
                  TmbData = fit$data_list, 
                  spatial_list = fit$spatial_list, 
                  DirName = fig,
                  Panel = "Year",
                  PlotName = paste0("POE_",sp),
                  PlotTitle = paste0(full_name," P.O.E in Taranaki, NZ"),
                  cex = 0.75,  
                  Zlim = c(0,1), 
                  arrows=F, 
                  pch=15)


###############################
# Spatio-temporal variability #
###############################

# Map of spatio-temporal variation of sp probability of encounter
stvar_array <- plot_maps_network(plot_set = 5, 
                                 fit = fit, 
                                 Sdreport = fit$parameter_estimates$SD, 
                                 TmbData = fit$data_list, 
                                 spatial_list = fit$spatial_list, 
                                 DirName = fig, 
                                 Panel = "category",
                                 PlotName = paste0("Epsilon_",sp),
                                 PlotTitle = paste0("Spatio-temporal variation of " ,full_name, " probability of encounter"),
                                 cex = 0.5, 
                                 arrows=F)

saveRDS(stvar_array, file.path(fig, "stvar_array.rds"))

####################################################
####################################################


##########################
# Model - all covariates #
##########################

#Testing with all covariates

rm(list=ls())

data_taranaki_dir <- "./Data/Taranaki"

# Load inputs
load(file.path(data_taranaki_dir, paste0("general_inputs_HUM_ds.Rdata")))

# Set paths
path <- "./Models/HUM_Taranaki_allcovariates"
fig <- file.path(path, "Figures")
dir.create(path, showWarnings = FALSE)
dir.create(fig, showWarnings=FALSE)

# TURN ON all covariates except a barrier covariate in 1st linear predictor
X1config_inp <- array(1, dim = c(1,n_p))
X1config_inp[,which(covars_all == "std_Years_since_barrier")] <- 0 #either switch off std_Years_since_barrier or Barrier_present 

# TURN OFF covariates in 2nd predictor
X2config_inp <- array(0, dim = c(1,n_p))

#Set up covariate formula
covars_to_use <- covars_all[which(X1config_inp == 1)]
X1_formula_inp = X2_formula_inp = paste0("~",(paste0(covars_to_use, collapse = "+")))

# Number of factors in each linear predictor (linear predictor 2 turned off)
FieldConfig <- c("Omega1" = 1, "Epsilon1" = 1, "Omega2" = 0, "Epsilon2" = 0) #1 category

# Controls structure on temporal and spatio-temporal effects - both set to a random walk (linear predictor 2 turned off)
RhoConfig <- c("Beta1" = 2, "Beta2" = 3, "Epsilon1" = 2, "Epsilon2" = 0)

# Model settings for a binomial model
ObsModel <- c(2,0)
OverdispersionConfig <- c("Eta1" = 0, "Eta2" = 0)
Options <- c("Calculate_range" = 1, "Calculate_effective_area" = 1) #I want to calculate these metrics


# Put all together into model settings
settings <- make_settings(n_x = nrow(Network_sz),
                          purpose = "index2",
                          Region = "Stream_network",
                          fine_scale = FALSE,
                          FieldConfig = FieldConfig,
                          RhoConfig = RhoConfig,
                          OverdispersionConfig = OverdispersionConfig,
                          ObsModel = ObsModel,
                          bias.correct = FALSE,
                          Options = Options,
                          Version = "VAST_v14_0_1") #latest version downloaded on computer, a later version is on the HPC
settings$Method <- "Stream_network"
settings$grid_size_km <- 1

# to_remove <- names(table(Data_Geostat$Year)[table(Data_Geostat$Year) < 30])
# Data_Geostat <- Data_Geostat %>% filter(!(Year %in% c(to_remove)))
# 
# addmargins(table(Data_Geostat$Year, round(Data_Geostat$Catch_KG)))
# 
# 
## Define the extrapolation grid
extrap_info <- make_extrapolation_info(Region = settings$Region,
                                       input_grid = cbind("Lat" = Data_Geostat$Lat, "Lon" = Data_Geostat$Lon,
                                                          "child_i" = Data_Geostat$Child_i,
                                                          "Area_km2" = Data_Geostat$Length))

## Generate the spatial information
spatial_info <- make_spatial_info_AC(n_x = settings$n_x,
                                     Lon_i = Data_Geostat$Lon,
                                     Lat_i = Data_Geostat$Lat,
                                     Extrapolation_List = extrap_info,
                                     knot_method = settings$knot_method,
                                     Method = settings$Method,
                                     Network_sz_LL = Network_sz_LL,
                                     DirPath = paste0(path, "/"))

## Build the TMB object
TmbData = make_data( Version = settings$Version,
                     FieldConfig = settings$FieldConfig,
                     OverdispersionConfig = settings$OverdispersionConfig,
                     RhoConfig = settings$RhoConfig,
                     ObsModel = settings$ObsModel,
                     c_i = rep(0, nrow(Data_Geostat)),
                     b_i = Data_Geostat$Catch_KG,
                     a_i = Data_Geostat$Length_sampled,
                     t_i = Data_Geostat$Year,
                     Network_sz = Network_sz,
                     # covariate_data = hab_std,
                     # X1_formula = X1_formula_inp,
                     # X1config_cp = X1config_inp,
                     # X2_formula = X2_formula_inp,
                     # X2config_cp = X2config_inp,
                     spatial_list = spatial_info,
                     # Q_ik = Q_ik,
                     Options = settings$Options,
                     CheckForErrors = TRUE)


## Build the VAST model
TmbList = make_model(build_model = T,
                     TmbData = TmbData,
                     RunDir = path,
                     Version = settings$Version,
                     RhoConfig = settings$RhoConfig,
                     loc_x = spatial_info$loc_x,
                     Method = settings$Method)

start1=Sys.time() #measure how long it takes

## fit0 - Compile the model and set up the model structure specified in settings.
fit0 <- fit_model(settings = settings,
                  Lat_i = Data_Geostat$Lat,
                  Lon_i = Data_Geostat$Lon,
                  t_i = Data_Geostat$Year,
                  b_i = Data_Geostat$Catch_KG,
                  #a_i = Data_Geostat$Length_sampled,
                  a_i = as_units(rep(1, nrow(Data_Geostat)), unitless),
                  Q_ik = Q_ik,
                  working_dir = path,
                  X_gctp = X_gctp,
                  X_itp = X_itp,
                  X1_formula = X1_formula_inp,
                  X1config_cp = X1config_inp,
                  X2_formula = X2_formula_inp,
                  X2config_cp = X2config_inp,
                  run_model = FALSE,
                  
                  extrapolation_args = list(input_grid = cbind("Lat" = Data_Geostat$Lat, "Lon" = Data_Geostat$Lon, 
                                                               "child_i" = Data_Geostat$Child_i, 
                                                               "Area_km2" = Data_Geostat$Length)),
                  spatial_args = list(Network_sz_LL = Network_sz_LL),
                  Network_sz = Network_sz)



#Try changing starting values
Par <- fit0$tmb_list$Parameters
Map <- fit0$tmb_list$Map


#Map off lambda2_k as 2nd component is ignored
Map[["lambda2_k"]] <- factor(rep(NA, length(Par[["lambda2_k"]]))) 


## fit1 - check if the model parameters are identifiable.
fit1 <- fit_model(settings = settings,
                  Lat_i = Data_Geostat$Lat,
                  Lon_i = Data_Geostat$Lon,
                  t_i = Data_Geostat$Year,
                  b_i = Data_Geostat$Catch_KG,
                  #a_i = Data_Geostat$Length_sampled,
                  a_i = as_units(rep(1, nrow(Data_Geostat)), unitless),
                  Q_ik = Q_ik,
                  working_dir = path,
                  X_gctp = X_gctp,
                  X_itp = X_itp,
                  X1_formula = X1_formula_inp,
                  X1config_cp = X1config_inp,
                  X2_formula = X2_formula_inp,
                  X2config_cp = X2config_inp,
                  extrapolation_args = list(input_grid = cbind("Lat" = Data_Geostat$Lat, "Lon" = Data_Geostat$Lon, 
                                                               "child_i" = Data_Geostat$Child_i, 
                                                               "Area_km2" = Data_Geostat$Length)),
                  spatial_args = list(Network_sz_LL = Network_sz_LL),
                  Network_sz = Network_sz,
                  
                  Parameters = Par,
                  Map = Map,
                  
                  newtonsteps = 3,
                  test_fit = FALSE,
                  optimize_args = list(getsd = FALSE))
TMBhelper::check_estimability(fit1$tmb_list$Obj) #if(check$WhichBad>0) {stop()}
#saveRDS(check, file.path(path, "check.rds"))

saveRDS(fit1, file.path(path, "fit1.rds"))

## fit - Run the model, estimating standard errors (should converge if model is checked properly)
fit <- fit_model(settings = settings,
                 Lat_i = Data_Geostat$Lat,
                 Lon_i = Data_Geostat$Lon,
                 t_i = Data_Geostat$Year,
                 b_i = Data_Geostat$Catch_KG,
                 #a_i = Data_Geostat$Length_sampled,
                 a_i = as_units(rep(1, nrow(Data_Geostat)), unitless),
                 Q_ik = Q_ik,
                 working_dir = path,
                 X_gctp = X_gctp,
                 X_itp = X_itp,
                 X1_formula = X1_formula_inp,
                 X1config_cp = X1config_inp,
                 X2_formula = X2_formula_inp,
                 X2config_cp = X2config_inp,
                 extrapolation_args = list(input_grid = cbind("Lat" = Data_Geostat$Lat, "Lon" = Data_Geostat$Lon, 
                                                              "child_i" = Data_Geostat$Child_i, 
                                                              "Area_km2" = Data_Geostat$Length)),
                 spatial_args = list(Network_sz_LL = Network_sz_LL),
                 Network_sz = Network_sz,
                 
                 Parameters = Par,
                 Map = Map,
                 
                 newtonsteps = 3,
                 test_fit = FALSE,
                 optimize_args = list(startpar = fit1$parameter_estimates$par))
saveRDS(fit, file.path(path, "Fit.rds"))

end1 <- Sys.time()
time1 <- end1-start1 ; time1

#fit <- readRDS(file.path(path, "Fit.rds"))


#####
## Check model ##
dharmaRes = summary(fit, what="residuals", working_dir=paste0(fig,"/"), type=1)
#dharmaRes <- readRDS(file.path(path, "dharmaRes.rds"))
plot_residuals(residuals=dharmaRes$scaledResiduals, fit=fit, save_dir=fig,
               Data_inp = Data_Geostat, network=network, coords="lat_long")

saveRDS(dharmaRes, file.path(path, "dharmaRes.rds"))
#dharmaRes <- readRDS(file.path(path, "dharmaRes.rds"))
####

## dharma plots ##
# Histogram of residuals #
jpeg(file.path(fig, "Resid_hist.jpg"), width = 600, height = 600)
hist(dharmaRes$scaledResiduals, 
     breaks = seq(-0.02, 1.02, len = 53),
     col = c("red",rep("lightgrey",50), "red"),
     main = "Hist of DHARMa residuals",
     xlab = "Residuals (outliers are marked red)",
     cex.main = 1)
dev.off()
##
## QQ plot ##
jpeg(file.path(fig, "QQplot.jpg"), width = 600, height = 600)
gap::qqunif(dharmaRes$scaledResiduals,
            pch=2,
            bty="n", 
            logscale = F, 
            col = "black", 
            cex = 0.6, 
            main = "QQ plot residuals", 
            cex.main = 1)
dev.off()
##
####


# Covariate names in plotting
covar_full_names_all <- c("Segment shade", "Segment substrate", "Segment slope", "Mean January air temperature", "Log distance to coast", 
                          "Log distance to nearest downstream lake", "Years since barrier was constructed", "Barrier present") 

Raw_cov_array = plot_maps_network(plot_set = 9,
                                  fit = fit,
                                  Sdreport = fit$parameter_estimates$SD,
                                  TmbData = fit$data_list,
                                  spatial_list = fit$spatial_list,
                                  DirName = fig,
                                  Panel = "category",
                                  PlotName = "Raw_covariate_values",
                                  PlotTitle = "Standardised covariate values",
                                  covar_names = covar_full_names_all,
                                  cex = 0.5,
                                  arrows=F,
                                  pch=15)

saveRDS(Raw_cov_array, file.path(fig, "Raw_cov_array.rds"))



# Plot raw (standardised) covariate values in 1978, 2000 and 2021
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

##Loop through years of interest
for(yr in c(1978, 2000, 2021)){
  
  which_year_1978 <- fit$years_to_plot[fit$years_to_plot %in% which(fit$year_labels == 1978)]
  which_year_2000 <- fit$years_to_plot[fit$years_to_plot %in% which(fit$year_labels == 2000)]
  which_year_2021 <- fit$years_to_plot[fit$years_to_plot %in% which(fit$year_labels == 2021)]
  
  if(yr==1978){which_year = which_year_1978}
  if(yr==2000){which_year = which_year_2000}
  if(yr==2021){which_year = which_year_2021}
  
  ###Loop through covariates
  for(covar in covars_all){
    
    
    #Which covariate to plot
    which = which(covars_all == covar)
    print(paste0("Plotting: ", covar, " raw covariate values in ", yr))
    
    #Create data frame for plotting
    xct <- data.frame('value'=Raw_cov_array[,which,which_year], 'year'=yr, fit$spatial_list$latlon_g)
    
    Xlim = c(min(xct$Lon),max(xct$Lon))
    Ylim = c(min(xct$Lat),max(xct$Lat))
    
    #Z limits should be set for consistency between years
    inp_Zlim = quantile(c(Raw_cov_array[,which,which_year_1978], Raw_cov_array[,which,which_year_2000], Raw_cov_array[,which,which_year_2021]), 
                        prob = c(0,1), na.rm=TRUE)
    
    p <- ggplot(xct)
    
    p <- p + geom_segment(data=l2, aes(x = Lon,y = Lat, xend = Lon2, yend = Lat2), col="gray92")
    
    p <- p +
      geom_point(aes(x = Lon, y = Lat, color = value), cex = 0.5, pch = 19) +
      #scale_color_distiller(palette = "Spectral", limits = inp_Zlim) +
      scale_color_distiller(palette = "RdYlGn", limits = inp_Zlim, direction = 1) +
      coord_cartesian(xlim = Xlim, ylim = Ylim) +
      scale_x_continuous(breaks=quantile(xct$Lon, prob=c(0.1,0.5,0.9)), labels=round(quantile(xct$Lon, prob=c(0.1,0.5,0.9)),0)) +
      # guides(color=guide_legend(title=plot_codes[plot_num])) +
      mytheme() +
      xlab("Longitude") + ylab("Latitude")
    
    p <- p + ggtitle(wrapper(paste0("Standardised covariate values in ", yr, " for ", covar_full_names_all[which]), width = 50))
    ggsave(file.path(fig, paste0("Raw_covariate_values_",yr,"_", covar, ".png")), p, width=8,height=8)
    
    
  }
}


## Model comparisons ##
fit$parameter_estimates$AIC #-10647.61


full_name = "Longfin eel"
sp = "lf"

##################################
# Probability of encounter plots #
##################################

# Map of sp probability of encounter across time
POE_array <- plot_maps_network(plot_set = 1, 
                               fit = fit, 
                               Sdreport = fit$parameter_estimates$SD, 
                               TmbData = fit$data_list, 
                               spatial_list = fit$spatial_list, 
                               DirName = fig, 
                               Panel = "category", 
                               PlotName = paste0("POE_",sp,"_yearly"),
                               PlotTitle = paste0(full_name," yearly probability of encounter in Taranaki, NZ"),
                               cex = 0.5, 
                               Zlim = c(0,1), 
                               arrows=F, 
                               pch=15)

saveRDS(POE_array, file.path(fig, "POE_array.rds"))


#############################################################
##            Difference between 2000 and 1978             ##
#############################################################

POE_1978 <- POE_array[,1,which(c(1978:2021)==1978)]
POE_2000 <- POE_array[,1,which(c(1978:2021)==2000)]
POE_2021 <- POE_array[,1,which(c(1978:2021)==2021)]


#Create data frame for plotting
xct <- data.frame('value'=POE_2000 - POE_1978, 'year'=2000, fit$spatial_list$latlon_g, "category"="Longfin")

Xlim = c(min(xct$Lon),max(xct$Lon))
Ylim = c(min(xct$Lat),max(xct$Lat))

#Z limits should be set for consistency between years
inp_Zlim = c(-1,1)

p <- ggplot(xct)

p <- p + geom_segment(data=l2, aes(x = Lon,y = Lat, xend = Lon2, yend = Lat2), col="gray92")

p <- p +
  geom_point(aes(x = Lon, y = Lat, color = value), cex = 0.5, pch = 19) +
  scale_color_distiller(palette = "RdYlGn", limits = inp_Zlim, direction = 1) +
  coord_cartesian(xlim = Xlim, ylim = Ylim) +
  scale_x_continuous(breaks=quantile(xct$Lon, prob=c(0.1,0.5,0.9)), labels=round(quantile(xct$Lon, prob=c(0.1,0.5,0.9)),0)) +
  # guides(color=guide_legend(title=plot_codes[plot_num])) +
  mytheme() +
  xlab("Longitude") + ylab("Latitude")

p <- p + ggtitle(wrapper("Difference in probability of encounter in 2000 compared to 1978", width = 50))
ggsave(file.path(fig, paste0("POE_diff2000_1978.png")), p, width=8,height=8)
####

## Difference between 2021 and 1978 ##
#Create data frame for plotting
xct <- data.frame('value'=POE_2021 - POE_1978, 'year'=2021, fit$spatial_list$latlon_g, "category"="Longfin")

Xlim = c(min(xct$Lon),max(xct$Lon))
Ylim = c(min(xct$Lat),max(xct$Lat))

#Z limits should be set for consistency between years
inp_Zlim = c(-1,1)

p <- ggplot(xct)

p <- p + geom_segment(data=l2, aes(x = Lon,y = Lat, xend = Lon2, yend = Lat2), col="gray92")

p <- p +
  geom_point(aes(x = Lon, y = Lat, color = value), cex = 0.5, pch = 19) +
  scale_color_distiller(palette = "RdYlGn", limits = inp_Zlim, direction = 1) +
  coord_cartesian(xlim = Xlim, ylim = Ylim) +
  scale_x_continuous(breaks=quantile(xct$Lon, prob=c(0.1,0.5,0.9)), labels=round(quantile(xct$Lon, prob=c(0.1,0.5,0.9)),0)) +
  # guides(color=guide_legend(title=plot_codes[plot_num])) +
  mytheme() +
  xlab("Longitude") + ylab("Latitude")

p <- p + ggtitle(wrapper("Difference in probability of encounter in 2021 compared to 1978", width = 50))
ggsave(file.path(fig, paste0("POE_diff2021_1978.png")), p, width=8,height=8)


## Difference between 2021 and 2000 ##
#Create data frame for plotting
xct <- data.frame('value'=POE_2021 - POE_2000, 'year'=2021, fit$spatial_list$latlon_g, "category"="Longfin")

Xlim = c(min(xct$Lon),max(xct$Lon))
Ylim = c(min(xct$Lat),max(xct$Lat))

#Z limits should be set for consistency between years
inp_Zlim = c(-1,1)

p <- ggplot(xct)

p <- p + geom_segment(data=l2, aes(x = Lon,y = Lat, xend = Lon2, yend = Lat2), col="gray92")

p <- p +
  geom_point(aes(x = Lon, y = Lat, color = value), cex = 0.5, pch = 19) +
  scale_color_distiller(palette = "RdYlGn", limits = inp_Zlim, direction = 1) +
  coord_cartesian(xlim = Xlim, ylim = Ylim) +
  scale_x_continuous(breaks=quantile(xct$Lon, prob=c(0.1,0.5,0.9)), labels=round(quantile(xct$Lon, prob=c(0.1,0.5,0.9)),0)) +
  # guides(color=guide_legend(title=plot_codes[plot_num])) +
  mytheme() +
  xlab("Longitude") + ylab("Latitude")

p <- p + ggtitle(wrapper("Difference in probability of encounter in 2021 compared to 2000", width = 50))
ggsave(file.path(fig, paste0("POE_diff2021_2000.png")), p, width=8,height=8)


#############################################################
#############################################################



# Map of sp probability of encounter for each year
plot_maps_network(plot_set = 1, 
                  fit = fit, 
                  Sdreport = fit$parameter_estimates$SD, 
                  TmbData = fit$data_list, 
                  spatial_list = fit$spatial_list, 
                  DirName = fig,
                  Panel = "Year",
                  PlotName = paste0("POE_",sp),
                  PlotTitle = paste0(full_name," P.O.E in Taranaki, NZ"),
                  cex = 0.75,  
                  Zlim = c(0,1), 
                  arrows=F, 
                  pch=15)


###############################
# Spatio-temporal variability #
###############################

# Map of spatio-temporal variation of sp probability of encounter
stvar_array <- plot_maps_network(plot_set = 5, 
                                 fit = fit, 
                                 Sdreport = fit$parameter_estimates$SD, 
                                 TmbData = fit$data_list, 
                                 spatial_list = fit$spatial_list, 
                                 DirName = fig, 
                                 Panel = "category",
                                 PlotName = paste0("Epsilon_",sp),
                                 PlotTitle = paste0("Spatio-temporal variation of " ,full_name, " probability of encounter"),
                                 cex = 0.5, 
                                 arrows=F)

saveRDS(stvar_array, file.path(fig, "stvar_array.rds"))


# ###############
# # Range index #
# ###############
# 
# plot_range_index_riverlength(Sdreport = fit$parameter_estimates$SD,
#                              Report = fit$Report,
#                              TmbData = fit$data_list,
#                              year_labels = as.numeric(fit$year_labels),
#                              Znames = colnames(fit$data_list$Z_gm),
#                              PlotDir = fig,
#                              use_biascorr = TRUE,
#                              category_names = full_name,
#                              total_river_length = (sum(network$length)))





############################################
############################################



