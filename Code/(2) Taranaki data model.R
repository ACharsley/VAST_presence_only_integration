###################################################
##        Taranaki Presence/absence model        ##
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
library(DHARMa)


#################
#  Directories  #
#################

VAST_input_data_dir <- paste0(getwd(), "/Data_processed/VAST_input_data")

model_path <- paste0(getwd(), "/Models")


####################
#  Call functions  #
####################

source("./Code/funcs.R")


########################
#  Modelling scenario  #
########################

# "Taranaki data", 
# "1a", "1b", "1c", "1d", 
# "2a", "2b", "2c", "2d",
# "3a", "3b", "3c", "3d",
# "4a", "4b", "4c", "4d"

scenario <- "Taranaki data"
network_type <- "downstream" #full


##########################
#  Load VAST input data  #
##########################

if(network_type == "downstream"){
  VAST_input_data <- readRDS(file.path(VAST_input_data_dir, "VAST_input_data_ds.rds"))
}
if(network_type == "full"){
  VAST_input_data <- readRDS(file.path(VAST_input_data_dir, "VAST_input_data.rds"))
}


######################################################
######################################################




########################################
# Taranaki presence/absence data model #
########################################

#Set path
if(scenario == "Taranaki data"){
  if(network_type == "downstream"){
    path <- file.path(model_path, "Taranaki_data_model_ds")
  }
  if(network_type == "full"){
    path <- file.path(model_path, "Taranaki_data_model")
  }
}else{
  if(network_type == "downstream"){
    path <- file.path(model_path, paste0("Model_", scenario, "_ds"))
  }
  if(network_type == "full"){
    path <- file.path(model_path, paste0("Model_", scenario))
  }
}

#Create model path
dir.create(path, showWarnings = FALSE)

path_figs <- file.path(path, "Figures")
dir.create(path_figs, showWarnings=FALSE)


##############
# Set inputs #
##############

network = VAST_input_data[[scenario]]$network
Network_sz = network %>% select(parent_s,child_s,dist_s)
Network_sz_LL = network %>% select(parent_s, child_s, dist_s, Lat, Lon)

# 1. Set data input
Data_inp <- VAST_input_data[[scenario]]$Data_Geostat

# 2. Set covariate input
X_gctp <- VAST_input_data[[scenario]]$X_gctp
X_itp <- VAST_input_data[[scenario]]$X_itp

covars_all <- dimnames(X_gctp)[[4]] ; n_p <- length(covars_all)

X1config_inp <- array(1, dim = c(1,n_p)) 
X1config_inp[covars_all == "Barrier_present"] <- 0 #don't use this variable
X1_formula_inp = X2_formula_inp = paste0("~",(paste0(covars_all, collapse = "+")))

#TURN OFF covariates in 2nd predictor
X2config_inp <- array(0, dim = c(1,n_p))

# 3. Set up catchability input
Q1_formula <- ~ FishMethod
Q1config_k <- 1
Q2_formula <- ~ 0
Q2config_k <- NULL
catchability_data <- Data_inp[,c("Lat", "Lon", "FishMethod")]


# 4. Set model settings
Version = "VAST_v14_0_1"

FieldConfig <- c("Omega1" = 1, "Epsilon1" = 1, "Omega2" = 0, "Epsilon2" = 0) #1 category
RhoConfig <- c("Beta1" = 2, "Beta2" = 3, "Epsilon1" = 1, "Epsilon2" = 0) #Remove RhoConfig[3] if necessary

ObsModel <- c(2,0)
OverdispersionConfig <- c("Eta1" = 0, "Eta2" = 0)
Options <- c("Calculate_range" = 1, "Calculate_effective_area" = 1)

bias_correct = F

# 5. Make settings - this isn't required but will do for saving later
settings <- make_settings(n_x = nrow(Network_sz),
                          purpose = "index2",
                          Region = "Stream_network",
                          fine_scale = FALSE,
                          FieldConfig = FieldConfig,
                          RhoConfig = RhoConfig,
                          OverdispersionConfig = OverdispersionConfig,
                          ObsModel = ObsModel,
                          bias.correct = bias_correct,
                          Options = Options,
                          Version = Version)
settings$Method <- "Stream_network"
settings$grid_size_km <- 1


############################
# Build extrapolation grid #
############################
input_grid <- data.frame("Lat" = Data_inp$Lat, 
                         "Lon" = Data_inp$Lon, 
                         "child_i" = Data_inp$Child_i, 
                         "Area_km2" = Data_inp$Length)

Extrapolation_List = make_extrapolation_info(Region = "stream_network", 
                                             input_grid = input_grid)


#############################
# Build spatial information #
#############################
Spatial_List = make_spatial_info(n_x = nrow(Network_sz),
                                 Lon_i = Data_inp$Lon,
                                 Lat_i = Data_inp$Lat,
                                 Extrapolation_List = Extrapolation_List,
                                 Method = "Stream_network",
                                 grid_size_km = 1,
                                 fine_scale = FALSE,
                                 Network_sz_LL = Network_sz_LL,
                                 DirPath = paste0(path, "/"),
                                 Save_Results = TRUE)


# ## Plot data and knots 
# plot_data(Extrapolation_List = Extrapolation_List, 
#           Spatial_List = Spatial_List, 
#           Data_Geostat = Data_inp,
#           PlotDir = paste0(path_figs, "/")) 


########################
# Build the TMB object #
########################
TmbData = make_data(b_i = as_units(Data_inp$Catch_KG, unitless), 
                    a_i = as_units(rep(1,nrow(Data_inp)), 'km'), 
                    t_i = Data_inp$Year,
                    
                    Version = Version,
                    FieldConfig = FieldConfig, 
                    OverdispersionConfig = OverdispersionConfig,
                    RhoConfig = RhoConfig, 
                    ObsModel_ez = ObsModel, 
                    Options = Options, 
                    
                    X_gctp = X_gctp,
                    X_itp = X_itp,
                    X1_formula = X1_formula_inp,
                    X1config_cp = X1config_inp,
                    X2_formula = X2_formula_inp,
                    X2config_cp = X2config_inp,
                    
                    catchability_data = catchability_data,
                    Q1_formula = Q1_formula,
                    Q2_formula = Q2_formula,
                    Q1config_k = Q1config_k,
                    Q2config_k = Q2config_k,
                    
                    spatial_list = Spatial_List,
                    Network_sz = Network_sz,
                    CheckForErrors = TRUE)


####################
# Build VAST model #
####################

TmbList = make_model(build_model = TRUE, 
                     TmbData = TmbData, 
                     RunDir = path,
                     Version = Version,
                     RhoConfig = RhoConfig,
                     Method = "Stream_network")

####################
# check parameters #
####################

Obj = TmbList[["Obj"]]
Obj$fn( Obj$par )
Obj$gr( Obj$par )

#####################################################
# Estimate fixed effects and predict random effects #
#####################################################
start_time <- Sys.time()
Opt = TMBhelper::fit_tmb(obj = Obj,
                         lower = TmbList[["Lower"]],
                         upper = TmbList[["Upper"]],
                         getsd = TRUE, 
                         savedir = path, 
                         bias.correct = bias_correct, 
                         newtonsteps = 3, 
                         bias.correct.control = list( sd = FALSE, split = NULL, nsplit = 1, vars_to_correct = c( "Index_cyl", "Index_ctl" ) ), 
                         getJointPrecision = TRUE) 
Report = Obj$report()
time = Sys.time() - start_time


####################################
# Save important model information #
####################################

Save = list( "Opt" = Opt, "Report" = Report, "ParHat" = Obj$env$parList( Opt$par ), "TmbData" = TmbData )
save(Save, file = file.path(path, "Save.RData"))

model_data <- Data_inp %>%
  select(Lat, Lon, Catch_KG, Year) %>%
  rename("Lat_i" = "Lat", "Lon_i" = "Lon", "b_i" = "Catch_KG", "t_i" = "Year") %>%
  mutate("a_i" = rep(1,nrow(Data_inp)), "v_i" = rep(0,nrow(Data_inp)),
         "c_iz" = rep(0,nrow(Data_inp))) %>%
  relocate("Lat_i", "Lon_i", "a_i", "v_i", "b_i", "t_i", "c_iz")


## Always double check this before running
Fit <- list( "data_frame" = model_data,
             "extrapolation_list" = Extrapolation_List,
             "spatial_list" = Spatial_List,
             "data_list" = TmbData,
             "tmb_list" = TmbList,
             "parameter_estimates" = Opt,
             "Report" = Report,
             "ParHat" = Obj$env$parList( Opt$par ),
             "year_labels" = c(min(model_data$t_i):max(model_data$t_i)),
             "years_to_plot" = which(c(min(model_data$t_i):max(model_data$t_i)) %in% unique(model_data$t_i)),
             "category_names" = NA,
             "settings" = settings,
             #"input_args" = input_args,
             "X1config_cp" = X1config_inp,
             "X2config_cp" = X2config_inp,
             "X_gctp" = X_gctp,
             "X_itp" = X_itp,
             #"covariate_data" = covariate_data,
             "X1_formula" = X1_formula_inp,
             "X2_formula" = X2_formula_inp,
             "Q1config_k" = Q1config_k,
             "Q2config_k" = Q2config_k,
             "catchability_data" = catchability_data,
             "Q1_formula" = Q1_formula,
             "Q2_formula" = Q2_formula,
             "total_time" = time)

save(Fit, file = file.path(path, "Fit.RData"))
#load(file.path(path, "Fit.RData"))


# Extract probability of encounter data
Probability_of_encounter<- matrix(Report$R1_gct, nrow = dim(Report$R1_gct)[1], ncol = dim(Report$R1_gct)[3],
                                  dimnames = list(Network_sz_LL$child_s, min(Fit$year_labels):max(Fit$year_labels)))
save(Probability_of_encounter, file = file.path(path, "Probability_of_encounter.RData"))
#load(file.path(path, "Probability_of_encounter.RData"))


###############
# Check model #
###############

######## Print the diagnostics generated during parameter estimation, and confirm that:
######## (1) no parameter is hitting an upper or lower bound and (2) the final gradient for each fixed-effect 
######## is close to zero (less than 0.0001). Also check model convergence via the Hessian (should be TRUE)
pander::pandoc.table( Opt$diagnostics[,c( 'Param', 'Lower', 'MLE', 'Upper', 'final_gradient' )] ) 
all( abs( Opt$diagnostics[,'final_gradient'] ) <1e-4 ) #### TRUE
all( eigen( Opt$SD$cov.fixed )$values >0 ) #### TRUE



######## Evaluate the model using DHARMa residuals
n_samples <- 1000
#Obj = TmbList$Obj
n_g_orig = Obj$env$data$n_g
Obj$env$data$n_g = 0
b_iz = matrix( NA, nrow = length( TmbData$b_i ), ncol = n_samples )
for ( zI in 1 : n_samples ) {
  if ( zI %% max( 1, floor( n_samples / 10 ) ) == 0 ) {
    message( "  Finished sample ", zI, " of ", n_samples )
  }
  b_iz[,zI] = simulate_data( fit = list( tmb_list = list( Obj = Obj ) ), type = 1 )$b_i
}
if ( any( is.na( b_iz ) ) ) {
  stop( "Check simulated residuals for NA values" )
}
b_iz <- as_units( b_iz, unitless )
dharmaRes = createDHARMa(simulatedResponse = b_iz, observedResponse = TmbData$b_i,
                         fittedPredictedResponse = Probability_of_encounter, integer = FALSE )
prop_lessthan_i = apply( as.numeric( b_iz ) < outer( as.numeric( TmbData$b_i ), 
                                                     rep( 1, n_samples ) ), MARGIN = 1, FUN = mean )
prop_lessthanorequalto_i = apply( as.numeric( b_iz ) <= outer( as.numeric( TmbData$b_i ), 
                                                               rep( 1, n_samples ) ), MARGIN = 1, FUN = mean )
PIT_i = runif( min = prop_lessthan_i, max = prop_lessthanorequalto_i, n = length( prop_lessthan_i ) )
dharmaRes$scaledResiduals = PIT_i

#Save residuals
save(dharmaRes, file = file.path(path, "dharmaRes.RData"))
#load(file.path(path, "dharmaRes.RData"))

## dharma plots ##
# Plot residuals on map
plot_residuals(residuals = dharmaRes$scaledResiduals, fit = fit, save_dir=path_figs,
               Data_inp = Data_inp, network=Network_sz_LL, coords="lat_long")

# Histogram of residuals #
val = dharmaRes$scaledResiduals
val[val == 0] = -0.01
val[val == 1] = 1.01

jpeg(file.path(path_figs, "Resid_hist.jpg"), width = 600, height = 600)
hist(val, 
     breaks = seq(-0.02, 1.02, len = 53),
     col = c("red",rep("lightgrey",50), "red"),
     #main = "Hist of DHARMa residuals",
     main = "",
     xlab = "Residuals (outliers are marked red)",
     cex.axis=1.5, cex.lab=1.5)
dev.off()
##
## QQ plot ##
jpeg(file.path(path_figs, "QQplot.jpg"), width = 600, height = 600)
gap::qqunif(dharmaRes$scaledResiduals,
            pch=2,
            bty="n", 
            logscale = F, 
            col = "black", 
            cex.axis=1.5, cex.lab=1.5 
            #main = "QQ plot residuals", 
            #cex.main = 1
)
dev.off()
##
####

#################################
# Plot probability of encounter #
#################################

## Yearly
plot_maps_network(plot_set = c(1), 
                  fit = Fit, 
                  Sdreport = Fit$parameter_estimates$SD, 
                  TmbData = Fit$data_list, 
                  spatial_list = Fit$spatial_list, 
                  DirName = path_figs, 
                  Panel = "category", 
                  PlotName = "POE_lf_yearly",
                  PlotTitle = "Longfin eel yearly probability of encounter in Taranaki, NZ",
                  cex = 0.5, 
                  Zlim = c(0,1), 
                  arrows=T, 
                  pch=15)


## Across time
plot_maps_network(plot_set = c(1), 
                  fit = Fit, 
                  Sdreport = Fit$parameter_estimates$SD, 
                  TmbData = Fit$data_list, 
                  spatial_list = Fit$spatial_list, 
                  DirName = path_figs, 
                  Panel = "Year", 
                  PlotName = "POE_lf",
                  PlotTitle = "Longfin eel yearly P.O.E in Taranaki, NZ",
                  cex = 0.75, 
                  Zlim = c(0,1), 
                  arrows=T, 
                  pch=15)




##############################
# Plot river length occupied #
##############################













# ##################################################################
# ##################################################################
# 
# 
# 
# 
# ########################################
# # Taranaki presence/absence data model #
# ########################################
# # Build initally with:
# #   - No habitat covariates
# #   - FishMethod as a cacthability covariate
# 
# 
# ##Set paths
# path <- file.path(model_path, "Taranaki_data_model_2")
# dir.create(path, showWarnings = FALSE)
# 
# path_figs <- file.path(path, "Figures")
# dir.create(path_figs, showWarnings=FALSE)
# 
# 
# ##############
# # Set inputs #
# ##############
# 
# network = VAST_input_data[[scenario]]$network
# Network_sz = network %>% select(parent_s,child_s,dist_s)
# Network_sz_LL = network %>% select(parent_s, child_s, dist_s, Lat, Lon)
# 
# # 1. Set data input
# Data_inp <- VAST_input_data[[scenario]]$Data_Geostat
# 
# # 2. Set covariate input
# X_gctp <- VAST_input_data[[scenario]]$X_gctp
# X_itp <- VAST_input_data[[scenario]]$X_itp
# 
# covars_all <- dimnames(X_gctp)[[4]] ; n_p <- length(covars_all)
# 
# X1config_inp <- array(1, dim = c(1,n_p)) 
# X1_formula_inp = X2_formula_inp = paste0("~",(paste0(covars_all, collapse = "+")))
# 
# #TURN OFF covariates in 2nd predictor
# X2config_inp <- array(0, dim = c(1,n_p))
# 
# # 3. Set up catchability input
# Q1_formula <- ~ FishMethod
# Q1config_k <- 1
# Q2_formula <- ~ 0
# Q2config_k <- NULL
# catchability_data <- Data_inp[,c("Lat", "Lon", "FishMethod")]
# 
# 
# # 4. Set model settings
# Version = "VAST_v14_0_1"
# 
# FieldConfig <- c("Omega1" = 1, "Epsilon1" = 1, "Omega2" = 0, "Epsilon2" = 0) #1 category
# RhoConfig <- c("Beta1" = 2, "Beta2" = 3, "Epsilon1" = 1, "Epsilon2" = 0) #Remove RhoConfig[3] if necessary
# 
# ObsModel <- c(2,0)
# OverdispersionConfig <- c("Eta1" = 0, "Eta2" = 0)
# Options <- c("Calculate_range" = 1, "Calculate_effective_area" = 1)
# 
# bias_correct = F
# 
# # 5. Make settings - this isn't required but will do for saving later
# settings <- make_settings(n_x = nrow(Network_sz),
#                           purpose = "index2",
#                           Region = "Stream_network",
#                           fine_scale = FALSE,
#                           FieldConfig = FieldConfig,
#                           RhoConfig = RhoConfig,
#                           OverdispersionConfig = OverdispersionConfig,
#                           ObsModel = ObsModel,
#                           bias.correct = bias_correct,
#                           Options = Options,
#                           Version = Version)
# settings$Method <- "Stream_network"
# settings$grid_size_km <- 1
# 
# 
# ############################
# # Build extrapolation grid #
# ############################
# input_grid <- data.frame("Lat" = Data_inp$Lat, 
#                          "Lon" = Data_inp$Lon, 
#                          "child_i" = Data_inp$Child_i, 
#                          "Area_km2" = Data_inp$Length)
# 
# Extrapolation_List = make_extrapolation_info(Region = "stream_network", 
#                                              input_grid = input_grid)
# 
# 
# #############################
# # Build spatial information #
# #############################
# Spatial_List = make_spatial_info(n_x = nrow(Network_sz),
#                                  Lon_i = Data_inp$Lon,
#                                  Lat_i = Data_inp$Lat,
#                                  Extrapolation_List = Extrapolation_List,
#                                  Method = "Stream_network",
#                                  grid_size_km = 1,
#                                  fine_scale = FALSE,
#                                  Network_sz_LL = Network_sz_LL,
#                                  DirPath = paste0(path, "/"),
#                                  Save_Results = TRUE)
# 
# 
# # ## Plot data and knots 
# # plot_data(Extrapolation_List = Extrapolation_List, 
# #           Spatial_List = Spatial_List, 
# #           Data_Geostat = Data_inp,
# #           PlotDir = paste0(path_figs, "/")) 
# 
# 
# ########################
# # Build the TMB object #
# ########################
# TmbData = make_data(b_i = as_units(Data_inp$Catch_KG, unitless), 
#                     a_i = as_units(rep(1,nrow(Data_inp)), 'km'), 
#                     t_i = Data_inp$Year,
#                     
#                     Version = Version,
#                     FieldConfig = FieldConfig, 
#                     OverdispersionConfig = OverdispersionConfig,
#                     RhoConfig = RhoConfig, 
#                     ObsModel_ez = ObsModel, 
#                     Options = Options, 
#                     
#                     X_gctp = X_gctp,
#                     X_itp = X_itp,
#                     X1_formula = X1_formula_inp,
#                     X1config_cp = X1config_inp,
#                     X2_formula = X2_formula_inp,
#                     X2config_cp = X2config_inp,
#                     
#                     catchability_data = catchability_data,
#                     Q1_formula = Q1_formula,
#                     Q2_formula = Q2_formula,
#                     Q1config_k = Q1config_k,
#                     Q2config_k = Q2config_k,
#                     
#                     spatial_list = Spatial_List,
#                     Network_sz = Network_sz,
#                     CheckForErrors = TRUE)
# 
# 
# ####################
# # Build VAST model #
# ####################
# 
# TmbList = make_model(build_model = TRUE, 
#                      TmbData = TmbData, 
#                      RunDir = path,
#                      Version = Version,
#                      RhoConfig = RhoConfig,
#                      Method = "Stream_network")
# 
# ####################
# # check parameters #
# ####################
# 
# Obj = TmbList[["Obj"]]
# Obj$fn( Obj$par )
# Obj$gr( Obj$par )
# 
# #####################################################
# # Estimate fixed effects and predict random effects #
# #####################################################
# start_time <- Sys.time()
# Opt = TMBhelper::fit_tmb(obj = Obj,
#                          lower = TmbList[["Lower"]],
#                          upper = TmbList[["Upper"]],
#                          getsd = TRUE, 
#                          savedir = path, 
#                          bias.correct = bias_correct, 
#                          newtonsteps = 3, 
#                          bias.correct.control = list( sd = FALSE, split = NULL, nsplit = 1, vars_to_correct = c( "Index_cyl", "Index_ctl" ) ), 
#                          getJointPrecision = TRUE) 
# Report = Obj$report()
# time = Sys.time() - start_time
# 
# 
# ####################################
# # Save important model information #
# ####################################
# 
# Save = list( "Opt" = Opt, "Report" = Report, "ParHat" = Obj$env$parList( Opt$par ), "TmbData" = TmbData )
# save(Save, file = file.path(path, "Save.RData"))
# 
# model_data <- Data_inp %>%
#   select(Lat, Lon, Catch_KG, Year) %>%
#   rename("Lat_i" = "Lat", "Lon_i" = "Lon", "b_i" = "Catch_KG", "t_i" = "Year") %>%
#   mutate("a_i" = rep(1,nrow(Data_inp)), "v_i" = rep(0,nrow(Data_inp)),
#          "c_iz" = rep(0,nrow(Data_inp))) %>%
#   relocate("Lat_i", "Lon_i", "a_i", "v_i", "b_i", "t_i", "c_iz")
# 
# 
# ## Always double check this before running
# Fit <- list( "data_frame" = model_data,
#              "extrapolation_list" = Extrapolation_List,
#              "spatial_list" = Spatial_List,
#              "data_list" = TmbData,
#              "tmb_list" = TmbList,
#              "parameter_estimates" = Opt,
#              "Report" = Report,
#              "ParHat" = Obj$env$parList( Opt$par ),
#              "year_labels" = c(min(model_data$t_i):max(model_data$t_i)),
#              "years_to_plot" = which(c(min(model_data$t_i):max(model_data$t_i)) %in% unique(model_data$t_i)),
#              "category_names" = NA,
#              "settings" = settings,
#              #"input_args" = input_args,
#              "X1config_cp" = X1config_inp,
#              "X2config_cp" = X2config_inp,
#              "X_gctp" = X_gctp,
#              "X_itp" = X_itp,
#              #"covariate_data" = covariate_data,
#              "X1_formula" = X1_formula_inp,
#              "X2_formula" = X2_formula_inp,
#              "Q1config_k" = Q1config_k,
#              "Q2config_k" = Q2config_k,
#              "catchability_data" = catchability_data,
#              "Q1_formula" = Q1_formula,
#              "Q2_formula" = Q2_formula,
#              "total_time" = time)
# 
# save(Fit, file = file.path(path, "Fit.RData"))
# #load(file.path(path, "Fit.RData"))
# 
# 
# # Extract probability of encounter data
# Probability_of_encounter<- matrix(Report$R1_gct, nrow = dim(Report$R1_gct)[1], ncol = dim(Report$R1_gct)[3],
#                                   dimnames = list(Network_sz_LL$child_s, min(Fit$year_labels):max(Fit$year_labels)))
# save(Probability_of_encounter, file = file.path(path, "Probability_of_encounter.RData"))
# #load(file.path(path, "Probability_of_encounter.RData"))
# 
# 
# ###############
# # Check model #
# ###############
# 
# ######## Print the diagnostics generated during parameter estimation, and confirm that:
# ######## (1) no parameter is hitting an upper or lower bound and (2) the final gradient for each fixed-effect 
# ######## is close to zero (less than 0.0001). Also check model convergence via the Hessian (should be TRUE)
# pander::pandoc.table( Opt$diagnostics[,c( 'Param', 'Lower', 'MLE', 'Upper', 'final_gradient' )] ) 
# all( abs( Opt$diagnostics[,'final_gradient'] ) <1e-4 ) #### TRUE
# all( eigen( Opt$SD$cov.fixed )$values >0 ) #### TRUE
# 
# 
# 
# ######## Evaluate the model using DHARMa residuals
# n_samples <- 1000
# #Obj = TmbList$Obj
# n_g_orig = Obj$env$data$n_g
# Obj$env$data$n_g = 0
# b_iz = matrix( NA, nrow = length( TmbData$b_i ), ncol = n_samples )
# for ( zI in 1 : n_samples ) {
#   if ( zI %% max( 1, floor( n_samples / 10 ) ) == 0 ) {
#     message( "  Finished sample ", zI, " of ", n_samples )
#   }
#   b_iz[,zI] = simulate_data( fit = list( tmb_list = list( Obj = Obj ) ), type = 1 )$b_i
# }
# if ( any( is.na( b_iz ) ) ) {
#   stop( "Check simulated residuals for NA values" )
# }
# b_iz <- as_units( b_iz, unitless )
# dharmaRes = createDHARMa(simulatedResponse = b_iz, observedResponse = TmbData$b_i,
#                          fittedPredictedResponse = Probability_of_encounter, integer = FALSE )
# prop_lessthan_i = apply( as.numeric( b_iz ) < outer( as.numeric( TmbData$b_i ), 
#                                                      rep( 1, n_samples ) ), MARGIN = 1, FUN = mean )
# prop_lessthanorequalto_i = apply( as.numeric( b_iz ) <= outer( as.numeric( TmbData$b_i ), 
#                                                                rep( 1, n_samples ) ), MARGIN = 1, FUN = mean )
# PIT_i = runif( min = prop_lessthan_i, max = prop_lessthanorequalto_i, n = length( prop_lessthan_i ) )
# dharmaRes$scaledResiduals = PIT_i
# 
# #Save residuals
# save(dharmaRes, file = file.path(path, "dharmaRes.RData"))
# #load(file.path(path, "dharmaRes.RData"))
# 
# ## dharma plots ##
# # Plot residuals on map
# plot_residuals(residuals = dharmaRes$scaledResiduals, fit = fit, save_dir=path_figs,
#                Data_inp = Data_inp, network=Network_sz_LL, coords="lat_long")
# 
# # Histogram of residuals #
# val = dharmaRes$scaledResiduals
# val[val == 0] = -0.01
# val[val == 1] = 1.01
# 
# jpeg(file.path(path_figs, "Resid_hist.jpg"), width = 600, height = 600)
# hist(val, 
#      breaks = seq(-0.02, 1.02, len = 53),
#      col = c("red",rep("lightgrey",50), "red"),
#      #main = "Hist of DHARMa residuals",
#      main = "",
#      xlab = "Residuals (outliers are marked red)",
#      cex.axis=1.5, cex.lab=1.5)
# dev.off()
# ##
# ## QQ plot ##
# jpeg(file.path(path_figs, "QQplot.jpg"), width = 600, height = 600)
# gap::qqunif(dharmaRes$scaledResiduals,
#             pch=2,
#             bty="n", 
#             logscale = F, 
#             col = "black", 
#             cex.axis=1.5, cex.lab=1.5 
#             #main = "QQ plot residuals", 
#             #cex.main = 1
# )
# dev.off()
# ##
# ####
# 
# #################################
# # Plot probability of encounter #
# #################################
# 
# ## Yearly
# plot_maps_network(plot_set = c(1), 
#                   fit = Fit, 
#                   Sdreport = Fit$parameter_estimates$SD, 
#                   TmbData = Fit$data_list, 
#                   spatial_list = Fit$spatial_list, 
#                   DirName = path_figs, 
#                   Panel = "category", 
#                   PlotName = "POE_lf_yearly",
#                   PlotTitle = "Longfin eel yearly probability of encounter in Taranaki, NZ",
#                   cex = 0.5, 
#                   Zlim = c(0,1), 
#                   arrows=T, 
#                   pch=15)
# 
# 
# ## Across time
# plot_maps_network(plot_set = c(1), 
#                   fit = Fit, 
#                   Sdreport = Fit$parameter_estimates$SD, 
#                   TmbData = Fit$data_list, 
#                   spatial_list = Fit$spatial_list, 
#                   DirName = path_figs, 
#                   Panel = "Year", 
#                   PlotName = "POE_lf",
#                   PlotTitle = "Longfin eel yearly P.O.E in Taranaki, NZ",
#                   cex = 0.75, 
#                   Zlim = c(0,1), 
#                   arrows=T, 
#                   pch=15)
# 
# 
# 
# 
# ##############################
# # Plot river length occupied #
# ##############################
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ##################################################################
# ##################################################################
