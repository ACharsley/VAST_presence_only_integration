###################################################


rm(list=ls())



##############
#  Packages  #
##############

library(tidyverse)
library(VAST)
library(splines)  # Used to include basis-splines
#library(effects)  # Used to visualize covariate effects
library(DHARMa)



#################
#  Directories  #
#################

VAST_input_data_dir <- paste0(getwd(), "/Data_processed/VAST_input_data")



####################
#  Call functions  #
####################

source(paste0(getwd(), "/Code/funcs.R"))



########################
#  Modelling scenario  #
########################

inputArgs <- commandArgs(trailingOnly=TRUE) #inputArgs <- c("HPC", "NA", "SE_off")
run_command <- inputArgs[1] ; print(run_command)
rerun <- inputArgs[2] ; print(rerun)
SE_switch <- inputArgs[3] ; print(SE_switch) #SE_off, SE_on


if(!is.na(run_command)){
  
  if(run_command == "HPC"){
    task_id = as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")) #task_id = 1
    
    if(task_id==1){scenario <- "Taranaki data"}
    if(task_id==2){scenario <- "1a"}
    if(task_id==3){scenario <- "1b"}
    if(task_id==4){scenario <- "1c"}
    if(task_id==5){scenario <- "1d"}
    if(task_id==6){scenario <- "2a"}
    if(task_id==7){scenario <- "2b"}
    if(task_id==8){scenario <- "2c"}
    if(task_id==9){scenario <- "2d"}
    if(task_id==10){scenario <- "3a"}
    if(task_id==11){scenario <- "3b"}
    if(task_id==12){scenario <- "3c"}
    if(task_id==13){scenario <- "3d"}
    if(task_id==14){scenario <- "4a"}
    if(task_id==15){scenario <- "4b"}
    if(task_id==16){scenario <- "4c"}
    if(task_id==17){scenario <- "4d"}
    
    if(task_id==18){scenario <- "OM_1a"}
    if(task_id==19){scenario <- "OM_1b"}
    if(task_id==20){scenario <- "OM_2a"}
    if(task_id==21){scenario <- "OM_2b"}
    if(task_id==22){scenario <- "OM_3a"}
    if(task_id==23){scenario <- "OM_3b"}
    if(task_id==24){scenario <- "OM_4a"}
    if(task_id==25){scenario <- "OM_4b"}
    
    network_type <- "full"
  }
  
}else{
  # "Taranaki data", 
  # "1a", "1b", "1c", "1d", 
  # "2a", "2b", "2c", "2d",
  # "3a", "3b", "3c", "3d",
  # "4a", "4b", "4c", "4d"
  #scenario <-   "1a"
  
  # "OM_1a", "OM_1b"
  # "OM_2a", "OM_2b"
  # "OM_3a", "OM_3b"
  # "OM_4a", "OM_4b"
  scenario <-   "1a"
  network_type <- "downstream"
}

print(scenario) ; print(network_type)



##############
# Model path #
##############

#Set model path
model_path <- paste0(getwd(), "/Models")

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

#If standard errors are being estimated then add to path
if(SE_switch == "SE_on"){
  path <- paste0(path, "_SE")
}

#Figures path
path_figs <- file.path(path, "Figures")



##########################
#  Load VAST input data  #
##########################

if(network_type == "downstream"){
  VAST_input_data <- readRDS(file.path(VAST_input_data_dir, "VAST_input_data_ds.rds"))
}
if(network_type == "full"){
  VAST_input_data <- readRDS(file.path(VAST_input_data_dir, "VAST_input_data.rds"))
}



##############
# Set inputs #
##############

network = VAST_input_data[[scenario]]$network
Network_sz = network %>% select(parent_s,child_s,dist_s)
Network_sz_LL = network %>% select(parent_s, child_s, dist_s, Lat, Lon)


# 1. Set data input
Data_inp <- VAST_input_data[[scenario]]$Data_Geostat


# 2. Set covariate input
covariate_df <- VAST_input_data[[scenario]]$covariate_data
#covars_all <- colnames(covariate_df)[!(colnames(covariate_df) %in% c("Lon","Lat","Year"))]

#Set covariate input
# X1_formula_inp = ~ bs(std_log_loc_elev, degree = 3, intercept = FALSE ) +
#   bs(std_FWENZ_SegRipShade, degree = 3, intercept = FALSE ) + bs(std_log_MeanFlowCumecs, degree = 3, intercept = FALSE ) +
#   bs(std_FWENZ_segSubstrate, degree = 3, intercept = FALSE ) + bs(std_local_twarm, degree = 3, intercept = FALSE ) +
#   factor(Barrier_present)
# X1_formula_inp = ~ bs(std_log_loc_elev, degree = 2, intercept = FALSE ) +
#   bs(std_FWENZ_SegRipShade, degree = 2, intercept = FALSE ) + bs(std_log_MeanFlowCumecs, degree = 2, intercept = FALSE ) +
#   bs(std_FWENZ_segSubstrate, degree = 2, intercept = FALSE ) + bs(std_local_twarm, degree = 2, intercept = FALSE ) +
#   factor(Barrier_present)
#
# if(scenario == "Taranaki data" & rerun == "rerun"){
#   
#   #Not using bsplines because I'm struggling to get this model to fit
#   X1_formula_inp = "~ std_log_loc_elev+std_FWENZ_SegRipShade+std_log_MeanFlowCumecs+std_FWENZ_segSubstrate+std_local_twarm+factor(Barrier_present)"
#   
# }

# X1_formula_inp = ~ bs(std_Dist2Coast, degree = 4, intercept = FALSE )
X1_formula_inp = ~ bs(std_log_loc_elev, degree = 3, intercept = FALSE ) + 
  bs(std_FWENZ_SegRipShade, degree = 3, intercept = FALSE ) + bs(std_FWENZ_segSubstrate, degree = 3, intercept = FALSE )
X2_formula_inp = ~0


# 3. Set up catchability input
table(Data_inp$Data_source) 

if(scenario == "Taranaki data"){
  Data_inp$Data_source_inp <- as.factor(ifelse(Data_inp$Data_source == "Structured_EF", 0, 1)) #Structured_EF = 0, Structured_NetTrap = 1
  
  Q1_formula <- ~ Data_source_inp
  Q1config_k <- 3
  Q2_formula <- ~ 0
  Q2config_k <- NULL
  catchability_data <- Data_inp[,c("Lat", "Lon", "Data_source_inp")]
}else{
  Data_inp$Data_source_inp <- as.factor(ifelse(Data_inp$Data_source == "Structured_EF", 0, 
                                               ifelse(Data_inp$Data_source == "Structured_NetTrap", 1, 2)) )
  #Structured_EF = 0, Structured_NetTrap = 1, Unstructured = 2
  
  Q1_formula <- ~ Data_source_inp
  Q1config_k <- c(3,3)
  Q2_formula <- ~ 0
  Q2config_k <- NULL
  catchability_data <- Data_inp[,c("Lat", "Lon", "Data_source_inp")]
}


# 4. Set model settings
Version = "VAST_v14_0_1"

FieldConfig <- c("Omega1" = 1, "Epsilon1" = 1, "Omega2" = 0, "Epsilon2" = 0) #1 category
RhoConfig <- c("Beta1" = 2, "Beta2" = 3, "Epsilon1" = 1, "Epsilon2" = 0) #Remove RhoConfig[3] if necessary

if(!is.na(rerun)){
  if(rerun == "rerun"){ #Remove spatio-temporal variation
    RhoConfig <- c("Beta1" = 2, "Beta2" = 3, "Epsilon1" = 0, "Epsilon2" = 0) 
  }
}


ObsModel <- c(2,0)
OverdispersionConfig <- c("Eta1" = 0, "Eta2" = 0)

if(SE_switch == "SE_off"){
  Options <- c("Calculate_range" = 1, "Calculate_effective_area" = 1) #Initially build without SEs for testing
}else{
  if(SE_switch == "SE_on"){
    Options <- c("SD_site_density" = 1, "Calculate_range" = 1, "Calculate_effective_area" = 1)
  }else{
    stop("SE_switch needs to be specified")
  }
}


if(network_type == "downstream"){
  bias_correct = F
  bias_correct_control = list( sd = FALSE, split = NULL, nsplit = NULL, vars_to_correct = NULL)
}
if(network_type == "full"){
  bias_correct = T
  bias_correct_control = list( sd = FALSE, split = NULL, nsplit = 1, vars_to_correct = c( "Index_cyl", "Index_ctl" ) )
}


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


###################
# Load model save #
###################

load(file.path(path, "Save.RData"))
## Here's where code gets to on 2_Models.R

#Set important stuff
Report <- Save$Report
Opt <- Save$Opt
TmbData <- Save$TmbData
ParHat <- Save$ParHat

####################
# Build VAST model #
####################

TmbList = make_model(build_model = TRUE, 
                     TmbData = TmbData, 
                     RunDir = path,
                     Version = Version,
                     RhoConfig = RhoConfig,
                     Method = "Stream_network")

# ## Modify the Map and Random lists 
Map = TmbList$Map
Random = TmbList$Random
ParHat = TmbList$Obj$env$parList()
Map[["beta2_ft"]] = factor( rep( NA, length( ParHat$beta2_ft ) ) )

## Rebuild the VAST model
TmbList = make_model(build_model = TRUE,
                     TmbData = TmbData,
                     RunDir = path,
                     Version = Version,
                     RhoConfig = RhoConfig,
                     Method = "Stream_network",
                     Map = Map,
                     Random = Random)



####################
# check parameters #
####################

Obj = TmbList[["Obj"]]
Obj$fn( Obj$par )
Obj$gr( Obj$par )

####################
# Save other stuff #
####################

model_data <- Data_inp %>%
  select(Lat, Lon, Catch_KG, Year) %>%
  dplyr::rename("Lat_i" = "Lat", "Lon_i" = "Lon", "b_i" = "Catch_KG", "t_i" = "Year") %>%
  mutate("a_i" = rep(1,nrow(Data_inp)), "v_i" = rep(0,nrow(Data_inp)),
         "c_iz" = rep(0,nrow(Data_inp))) %>%
  relocate("Lat_i", "Lon_i", "a_i", "v_i", "b_i", "t_i", "c_iz")


## Always double check this before running
Fit <- list( "data_frame" = model_data,
             "extrapolation_list" = Extrapolation_List,
             "spatial_list" = Spatial_List,
             "data_list" = Save$TmbData,
             "tmb_list" = TmbList,
             "parameter_estimates" = Save$Opt,
             "Report" = Save$Report,
             "ParHat" = Save$ParHat,
             "year_labels" = c(min(model_data$t_i):max(model_data$t_i)),
             "years_to_plot" = which(c(min(model_data$t_i):max(model_data$t_i)) %in% unique(model_data$t_i)),
             "category_names" = NA,
             "settings" = settings,
             #"input_args" = input_args,
             #"X1config_cp" = X1config_inp,
             #"X2config_cp" = X2config_inp,
             #"X_gctp" = X_gctp,
             #"X_itp" = X_itp,
             "covariate_data" = covariate_df,
             "X1_gctp" = Save$TmbData$X1_gctp,
             #"X2_gctp" = TmbData$X2_gctp, #No affect on 2nd linear predictor
             #"X1_formula" = X1_formula_inp,
             #"X2_formula" = X2_formula_inp,
             "Q1config_k" = Q1config_k,
             "Q2config_k" = Q2config_k,
             "catchability_data" = catchability_data,
             "Q1_formula" = Q1_formula,
             "Q2_formula" = Q2_formula )#, "total_time" = time)

save(Fit, file = file.path(path, "Fit.RData"))
#load(file.path(path, "Fit.RData"))


# Extract probability of encounter data
Probability_of_encounter<- matrix(Report$R1_gct, nrow = dim(Report$R1_gct)[1], ncol = dim(Report$R1_gct)[3],
                                  dimnames = list(Network_sz_LL$child_s, min(Fit$year_labels):max(Fit$year_labels)))
save(Probability_of_encounter, file = file.path(path, "Probability_of_encounter.RData"))
#load(file.path(path, "Probability_of_encounter.RData"))



###############


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
plot_residuals(residuals = dharmaRes$scaledResiduals, save_dir=path_figs,
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


# Without titles and with zlimit changed
print(summary(Fit$Report$R1_gct))

zlim_inp <- c(0, round_any(max(Fit$Report$R1_gct), 0.1, f=ceiling))

## Yearly
plot_maps_network(plot_set = c(1), 
                  fit = Fit, 
                  Sdreport = Fit$parameter_estimates$SD, 
                  TmbData = Fit$data_list, 
                  spatial_list = Fit$spatial_list, 
                  DirName = path_figs, 
                  Panel = "category", 
                  PlotName = "POE_lf_yearly_v2",
                  PlotTitle = "",
                  cex = 0.5, 
                  Zlim = zlim_inp, 
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
                  PlotName = "POE_lf_v2",
                  PlotTitle = "",
                  cex = 0.75, 
                  Zlim = zlim_inp, 
                  arrows=T, 
                  pch=15)


#####################
# Uncertainty plots #
#####################
if(SE_switch == "SE_on"){
  
  #For river tracing in plot
  Network_sz_EN <- data.frame('parent_s'=Fit$data_list$parent_s, 'child_s'=Fit$data_list$child_s, Fit$spatial_list$latlon_g)
  l2 <- lapply(1:nrow(Network_sz_EN), function(x){
    parent <- Network_sz_EN$parent_s[x]
    find <- Network_sz_EN %>% filter(child_s == parent)
    if(nrow(find)>0) out <- cbind.data.frame(Network_sz_EN[x,], 'long2'=find$Lon, 'lat2'=find$Lat)
    if(nrow(find)==0) out <- cbind.data.frame(Network_sz_EN[x,], 'long2'=NA, 'lat2'=NA)
    return(out)
  })
  l2 <- do.call(rbind, l2)
  
  
  ### Extract SE information ###
  Sdreport = Fit$parameter_estimates$SD
  
  Index_SE <- array( TMB::summary.sdreport( Sdreport )[which( 
    rownames( TMB::summary.sdreport( Sdreport ) ) == "Index_gctl" ),2], 
    dim = c( dim( Report$Index_gctl ) ), dimnames = list( NULL, NULL, NULL, NULL ) )[,1,,]
  
  #Convert to POC
  POC_SE <- Index_SE/(Fit$spatial_list$a_gl[,1]) #This gives SE of density, as R2 is all 1, density = probability of encounter in model
  #all(Fit$Report$R1_gct == Fit$Report$D_gct)
  
  save(POC_SE, file = file.path(path, "POC_SE.RData"))
  #load(file.path(path, "POC_SE.RData"))
  
  POC_CV <- POC_SE/Probability_of_encounter
  
  ## Uncertainty in probability of encounter plots - Standard error ##
  SE_xct <- lapply(1:length(Fit$year_labels), function(x){
    out <- data.frame('value'=POC_SE[,x], 'year'=Fit$year_labels[x], Fit$spatial_list$latlon_g)
    return(out)
  })
  SE_xct <- do.call(rbind, SE_xct)
  
  #Set so only complete cases are plotted (no NAs) - NAs produced in root nodes
  SE_xct <- SE_xct[complete.cases(SE_xct),]
  
  #Set limits for plotting
  zlim_inp1 <- c(0, round_any(max(SE_xct$value), 0.1, f=ceiling))
  Xlim = c(min(SE_xct$Lon),max(SE_xct$Lon))
  Ylim = c(min(SE_xct$Lat),max(SE_xct$Lat))
  
  ## Yearly
  p <- ggplot(SE_xct)
  p <- p + geom_segment(data=l2, aes(x = Lon,y = Lat, xend = long2, yend = lat2), col="gray92")
  
  p <- p +
    geom_point(aes(x = Lon, y = Lat, color = value), cex = 0.5, pch = 19) +
    scale_color_distiller(palette = "RdYlGn", limits = zlim_inp1) + #, direction = 1) +
    coord_cartesian(xlim = Xlim, ylim = Ylim) +
    scale_x_continuous(breaks=quantile(SE_xct$Lon, prob=c(0.1,0.5,0.9)), labels=round(quantile(SE_xct$Lon, prob=c(0.1,0.5,0.9)),0)) +
    # guides(color=guide_legend(title=plot_codes[plot_num])) +
    facet_wrap(~year) + 
    mytheme() +
    xlab("Longitude") + ylab("Latitude")
  
  #Save without title
  ggsave(file.path(path_figs, "POE_SE_lf_yearly_v2.png"), p, width=8,height=8)
  
  #Save with title
  p <- p + ggtitle(wrapper("Longfin eel yearly SE in probability of encounter in Taranaki, NZ", width = 50))
  ggsave(file.path(path_figs, "POE_SE_lf_yearly.png"), p, width=8,height=8)
  
  
  ## Uncertainty in probability of encounter plots - Standard error ##
  CV_xct <- lapply(1:length(Fit$year_labels), function(x){
    out <- data.frame('value'=POC_CV[,x], 'year'=Fit$year_labels[x], Fit$spatial_list$latlon_g)
    return(out)
  })
  CV_xct <- do.call(rbind, CV_xct)
  
  #Set so only complete cases are plotted (no NAs) - NAs produced in root nodes
  CV_xct <- CV_xct[complete.cases(CV_xct),]
  
  #Set limits for plotting
  zlim_inp2 <- c(floor(min(CV_xct$value)),ceiling(max(CV_xct$value)))
  Xlim = c(min(CV_xct$Lon),max(CV_xct$Lon))
  Ylim = c(min(CV_xct$Lat),max(CV_xct$Lat))
  
  ## Yearly
  p <- ggplot(CV_xct)
  p <- p + geom_segment(data=l2, aes(x = Lon,y = Lat, xend = long2, yend = lat2), col="gray92")
  
  p <- p +
    geom_point(aes(x = Lon, y = Lat, color = value), cex = 0.5, pch = 19) +
    scale_color_distiller(palette = "RdYlGn", limits = zlim_inp2) + #, direction = 1) +
    coord_cartesian(xlim = Xlim, ylim = Ylim) +
    scale_x_continuous(breaks=quantile(CV_xct$Lon, prob=c(0.1,0.5,0.9)), labels=round(quantile(CV_xct$Lon, prob=c(0.1,0.5,0.9)),0)) +
    # guides(color=guide_legend(title=plot_codes[plot_num])) +
    facet_wrap(~year) + 
    mytheme() +
    xlab("Longitude") + ylab("Latitude")
  
  #Save without title
  ggsave(file.path(path_figs, "POE_CV_lf_yearly_v2.png"), p, width=8,height=8)
  
  #Save with title
  p <- p + ggtitle(wrapper("Longfin eel yearly coefficient of variation (CV) in probability of encounter in Taranaki, NZ", width = 50))
  ggsave(file.path(path_figs, "POE_CV_lf_yearly.png"), p, width=8,height=8)
  
}


#######################################
# Percentage of river length occupied #
#######################################

Effective_area <- plot_range_index_SN(Sdreport = Fit$parameter_estimates$SD,
                                      Report = Fit$Report,
                                      TmbData = Fit$data_list,
                                      year_labels = as.numeric(Fit$year_labels),
                                      Znames = colnames(Fit$data_list$Z_gm),
                                      PlotDir = path_figs,
                                      use_biascorr = TRUE,
                                      category_names = "",
                                      total_river_length = (sum(network$length)))
saveRDS(Effective_area, file.path(path, paste0("Effective_area.rds")))


##########################
# Simulate data if an OM #
##########################

if(scenario %in% c("OM_1a", "OM_1b", "OM_2a", "OM_2b", "OM_3a", "OM_3b", "OM_4a", "OM_4b")){
  path_sim_data <- file.path(path, "Simulated_data")
  dir.create(path_sim_data, showWarnings = F)
  
  Nrep <- 20
  pgBar <- txtProgressBar( min = 1, max = Nrep, style = 3 )
  for(rI in 1:Nrep){
    setTxtProgressBar( pgBar, rI )
    Data_sim <- Obj$simulate( complete = TRUE )
    saveRDS(Data_sim, file.path(path_sim_data, paste0("Data_sim_", rI, ".rds")))
  }
  close(pgBar)
}



##############################################################################
##############################################################################