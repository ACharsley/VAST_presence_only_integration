###################################################
##           Habitat unsuitability model         ##
##             for the Taranaki region           ##
##                                               ##
##               Anthony Charsley                ##
##                December 2022                  ##
###################################################

# This code builds a full model using the HPC with 
# presence/absence data to determine habitat locations 
# that are unsuitable for eels.
#
# I will use VAST to build this model.

###########################################

rm(list=ls())

setwd("/nesi/nobackup/niwa03347/ACharsley/PhD/Eel_presence_only_integration")


###############
# Build model #
###############

library(tidyverse)
library(VAST)


#########################
# Model - no covariates #
#########################

#Removing all covariates

data_taranaki_dir <- file.path(getwd(), "Data/Taranaki")

# Load inputs
load(file.path(data_taranaki_dir, paste0("general_inputs_HUM.Rdata")))

# Set paths
path <- file.path(getwd(),"Models/HUM_Taranaki_full_model_nocovariates")
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
print(fit$parameter_estimates$AIC)


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

