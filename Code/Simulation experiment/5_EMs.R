###################################################
##               Estimating models               ##
##                                               ##
##               Anthony Charsley                ##
##                September 2024                 ##
###################################################



rm(list=ls())



##############
#  Packages  #
##############

#packageurl <- "https://cran.r-project.org/src/contrib/Archive/RANN/RANN_2.6.1.tar.gz" 
#install.packages(packageurl, repos=NULL, type="source")
#library(RANN)
#need to use RANN version 2.6.1 or else there are problems building spatial_list

library(tidyverse)
library(VAST)
library(splines)  # Used to include basis-splines



####################
#  Call functions  #
####################

source(paste0(getwd(), "/Code/funcs.R"))



########################
#  Modelling scenario  #
########################

# Operating models
scenarios_all <- c("OM_1a", 
                   "OM_2a", 
                   "OM_3a", "OM_3b",
                   "OM_4a")

# Estimating models
data_sample_all <- c("sample_rand1n",
                     "sample_rand2n",
                     "sample_rand5n",
                     
                     "sample_unsuithab1n",
                     "sample_unsuithab2n",
                     "sample_unsuithab5n",
                     
                     "sample_nearroads1n",
                     "sample_nearroads2n",
                     "sample_nearroads5n",
                     
                     "sample_unsuithab_nearroads1n",
                     "sample_unsuithab_nearroads2n",
                     "sample_unsuithab_nearroads5n")

# Replications
reps_all <- c(1:100)


tasks <- data.frame("scenarios_all" = rep(scenarios_all, each = length(data_sample_all)*length(reps_all)),
                    "data_sample_all" = rep(rep(data_sample_all, each = length(reps_all)), length(scenarios_all)),
                    "reps_all" = rep(reps_all, length(scenarios_all)*length(data_sample_all)))
# nrow(unique(tasks)) == length(scenarios_all)*length(data_sample_all)*length(reps_all)

task_id = as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")) #task_id=675

# set scenario, data_sample, and rep
scenario <- tasks[task_id,"scenarios_all"] ; print(scenario)
data_sample <- tasks[task_id,"data_sample_all"] ; print(data_sample)
rep <- tasks[task_id,"reps_all"] ; print(rep)



###################
# Set output path #
###################

# Set data input path
VAST_input_data_dir <- paste0(getwd(), "/Data_processed/VAST_input_data")

#Set model path
model_path <- paste0(getwd(), "/Models")

# Set EM directory
EM_path <- file.path(model_path, "EMs")

# EM data path
EM_data_path <- file.path(EM_path, "EM_data")

# Set output path for scenario
path <- file.path(EM_path, paste0("EM_output_", scenario))
dir.create(path, showWarnings = F)
path <- file.path(EM_path, paste0("EM_output_", scenario), paste0("Rep_", rep, "_", data_sample))
#path <- file.path(EM_path, paste0("EM_output_", scenario), data_sample)
dir.create(path, showWarnings = F)



##############################################
#  Load VAST input data and set network data #
##############################################

VAST_input_data <- readRDS(file.path(VAST_input_data_dir, "VAST_input_data_OMs.rds"))

#Set network data
network = VAST_input_data$network
Network_sz = network %>% select(parent_s,child_s,dist_s)
Network_sz_LL = network %>% select(parent_s, child_s, dist_s, Lat, Lon)



###############
# Load OM fit #
###############

load(file.path(model_path, paste0("Model_", scenario, "/Fit.RData")))



################
# Load EM data #
################

EM_data_list <- readRDS(file.path(EM_data_path, paste0("EM_data_",scenario,"_rep",rep, ".rds")))
Data_inp <- EM_data_list[[data_sample]]

table(Data_inp$Data_source)


# ## Plot catchment data ##
# plotting_data <- Data_inp %>%
#   mutate("Present" = ifelse(round(Catch_KG) == 1, "Present", "Absent"))
# 
# catchmap <- ggplot(plotting_data) +
#   geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
#   geom_point(aes(x = Lon, y = Lat, col = Present), alpha = 0.8, size=3) +
#   facet_wrap(.~Year) +
#   xlab("Longitude (°E)") + ylab("Latitude (°N)") +
#   #ggtitle("Longfin eel data by year") +
#   guides(color = guide_legend(title = "")) +
#   scale_colour_manual(values = c("#E41A1C", "chartreuse4")) +
#   theme_bw(base_size = 14) +
#   theme(axis.text = element_text(size = rel(1.25)),
#         axis.title=element_text(size = rel(1.5),face="bold"),
#         axis.text.x = element_text(angle = 90),
#         legend.text=element_text(size = rel(1)))
# ggsave(file.path(path, paste0("Catchment_data_map_rep",rep,"_",data_sample,".png")), catchmap, height = 12, width = 15)
# ####



####################
# Set model inputs #
####################

# Set covariate input
covariate_df <- Fit$covariate_data

X1_formula_inp = ~ bs(std_log_loc_elev, degree = 3, intercept = FALSE ) + 
  bs(std_FWENZ_SegRipShade, degree = 3, intercept = FALSE ) + bs(std_FWENZ_segSubstrate, degree = 3, intercept = FALSE )
X2_formula_inp = ~0


# Set up catchability input
table(Data_inp$Data_source) 

Data_inp$Data_source_inp <- as.factor(ifelse(Data_inp$Data_source == "Structured_EF", 0, 
                                             ifelse(Data_inp$Data_source == "Structured_NetTrap", 1, 2)) )
#Structured_EF = 0, Structured_NetTrap = 1, Unstructured = 2

Q1_formula <- ~ Data_source_inp
Q1config_k <- c(3,3)
Q2_formula <- ~ 0
Q2config_k <- NULL
catchability_data <- Data_inp[,c("Lat", "Lon", "Data_source_inp")]


# Set model settings
Version = Fit$settings$Version ; print(Version)

FieldConfig <- Fit$settings$FieldConfig ; print(FieldConfig)
RhoConfig <- Fit$settings$RhoConfig ; print(RhoConfig)

ObsModel <- Fit$settings$ObsModel ; print(ObsModel)
OverdispersionConfig <- Fit$settings$OverdispersionConfig ; print(OverdispersionConfig)
Options <- Fit$settings$Options ; print(Options)

## Turn off bias correction to minimise run time
# bias_correct = T
# bias_correct_control = list( sd = FALSE, split = NULL, nsplit = 1, vars_to_correct = c( "Index_cyl", "Index_ctl" ) )
bias_correct = F
bias_correct_control = list( sd = FALSE, split = NULL, nsplit = NULL, vars_to_correct = NULL)

# Make settings - this isn't required but will do for saving later
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



########################
# Build the TMB object #
########################
TmbData = make_data(b_i = as_units(Data_inp$Catch_KG, unitless), 
                    a_i = as_units(rep(1, nrow(Data_inp)), unitless), 
                    t_i = Data_inp$Year,
                    
                    Version = Version,
                    FieldConfig = FieldConfig, 
                    OverdispersionConfig = OverdispersionConfig,
                    RhoConfig = RhoConfig, 
                    ObsModel_ez = ObsModel, 
                    Options = Options, 
                    
                    covariate_data = covariate_df,
                    #X_gctp = X_gctp,
                    #X_itp = X_itp,
                    X1_formula = X1_formula_inp,
                    #X1config_cp = X1config_inp,
                    X2_formula = X2_formula_inp,
                    #X2config_cp = X2config_inp,
                    
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
                         bias.correct.control = bias_correct_control, 
                         getJointPrecision = TRUE) 
Report = Obj$report()
time = Sys.time() - start_time ; print(paste0("Model run time: ", time))



######################################
# Calculate probability of encounter #
######################################

## Probability of encounter
Probability_of_encounter<- matrix(Report$R1_gct, nrow = dim(Report$R1_gct)[1], ncol = dim(Report$R1_gct)[3],
                                  dimnames = list(Network_sz_LL$child_s, min(Data_inp$Year):max(Data_inp$Year)))

## Probability of encounter standard error - takes about 4 mins to run
n_samples <- 100
samples <- sample_variable( Sdreport = Opt$SD, 
                            Obj = Obj, 
                            variable_name = "R1_gct", 
                            n_samples = n_samples, 
                            seed = sample(1:1000,1))

# POE_SE <- matrix(nrow = n_samples, ncol = dim(Report$R1_gct)[3],
#                  dimnames = list(c(1:n_samples), min(Data_inp$Year):max(Data_inp$Year)))
# 
# for(x in 1:n_samples){
#   POE_SE[x,] <- apply(samples[,1,,x],2,sd)
#   }
# POE_SE <- colMeans(POE_SE)

POE_SE <- matrix(nrow = dim(Report$R1_gct)[1], ncol = dim(Report$R1_gct)[3],
                 dimnames = list(Network_sz_LL$child_s, min(Data_inp$Year):max(Data_inp$Year)))

for(x in 1:dim(Report$R1_gct)[3]){
  POE_SE[,x] <- apply(samples[,1,x,],1,sd)
}



###################
# Save POE output #
###################

# Save probability of encounter data
POE_list <- list("Probability_of_encounter" = Probability_of_encounter, "SE" = POE_SE)

save(POE_list, file = file.path(path, "POE_list.RData"))
#load(file.path(path, "POE_list.RData"))



##########################################
##########################################