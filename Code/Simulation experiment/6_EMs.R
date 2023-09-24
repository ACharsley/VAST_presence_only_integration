###################################################
##               Estimating models               ##
##                                               ##
##               Anthony Charsley                ##
##                   May 2023                    ##
###################################################



rm(list=ls())



##############
#  Packages  #
##############

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

inputArgs <- commandArgs(trailingOnly=TRUE)
run_command <- inputArgs[1] ; print(run_command)
data_sample <- inputArgs[2] ; print(data_sample)
#data_sample "sample_1" = Random generation, n
#data_sample "sample_2" = Random generation, 10n
#data_sample "sample_3" = Random generation at locations with unsuitable longfin eel habitat, n
#data_sample "sample_4" = Random generation at locations with unsuitable longfin eel habitat, 10n
#data_sample "sample_5" = Random generation at locations within 2km of a registered road, n
#data_sample "sample_6" = Random generation at locations within 2km of a registered road, 10n
#data_sample "sample_7" = Random generation at locations within 2km of a registered road and with unsuitable longfin eel habitat, n
#data_sample "sample_8" = Random generation at locations within 2km of a registered road and with unsuitable longfin eel habitat, 10n

# For testing:
#run_command = "HPC"
#data_sample = "sample_8"

if(is.na(run_command)){stop("Error: Run command is not specified.")}
if(is.na(data_sample)){stop("Error: Data sample is not specified.")}

if(run_command == "HPC"){
  task_id = as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")) #task_id=5
  
  if(task_id %in% c(1:20)){scenario <- "OM_1a" ; rep <- task_id}
  if(task_id %in% c(21:40)){scenario <- "OM_1b" ; rep <- task_id - 20}
  if(task_id %in% c(41:60)){scenario <- "OM_2a" ; rep <- task_id - 40}
  if(task_id %in% c(61:80)){scenario <- "OM_2b" ; rep <- task_id - 60}
  if(task_id %in% c(81:100)){scenario <- "OM_3a" ; rep <- task_id - 80}
  if(task_id %in% c(101:120)){scenario <- "OM_3b" ; rep <- task_id - 100}
  if(task_id %in% c(121:140)){scenario <- "OM_4a" ; rep <- task_id - 120}
  if(task_id %in% c(141:160)){scenario <- "OM_4b" ; rep <- task_id - 140}
  
}



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
path <- file.path(EM_path, paste0("Output_", scenario))
dir.create(path, showWarnings = F)
path <- file.path(EM_path, paste0("Output_", scenario), paste0("Rep_", rep, "_", data_sample))
dir.create(path, showWarnings = F)



##############################################
#  Load VAST input data and set network data #
##############################################

VAST_input_data <- readRDS(file.path(VAST_input_data_dir, "VAST_input_data.rds"))

#Set network data
network = VAST_input_data[[scenario]]$network
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


## Plot catchment data ##
plotting_data <- Data_inp %>%
  mutate("Present" = ifelse(round(Catch_KG) == 1, "Present", "Absent"))

catchmap <- ggplot(plotting_data) +
  geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
  geom_point(aes(x = Lon, y = Lat, col = Present), alpha = 0.6) +
  facet_wrap(.~Year) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Longfin eel data by year") +
  guides(color = guide_legend(title = "")) +
  scale_colour_manual(values = c("#E41A1C", "green")) +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = rel(0.5)),
        axis.text.x = element_text(angle = 90))
ggsave(file.path(path, "Catchment_data_map.png"), catchmap, height = 12, width = 15)
####



####################
# Set model inputs #
####################


# Set covariate input
covariate_df <- Fit$covariate_data

covars_all <- colnames(covariate_df)[4:ncol(covariate_df)]

covariate_df <- covariate_df %>%
  select(c(Year, Lon, Lat, all_of(covars_all[covars_all != "std_Years_since_barrier"])))

#using bsplines with 2 degrees of freedom based on the Graynoth & Booker (2009) biomass model
# X1_formula_inp = ~ bs(std_log_loc_elev, degree = 2, intercept = FALSE ) + 
#   bs(std_SegRipShade_sqrd, degree = 2, intercept = FALSE ) + bs(std_log_MeanFlowCumecs, degree = 2, intercept = FALSE ) + 
#   bs(std_FWENZ_segSubstrate, degree = 2, intercept = FALSE ) + bs(std_local_twarm, degree = 2, intercept = FALSE ) + 
#   factor(Barrier_present)
X1_formula_inp = ~ bs(std_log_loc_elev, degree = 3, intercept = FALSE ) + 
  bs(std_SegRipShade_sqrd, degree = 3, intercept = FALSE ) + bs(std_log_MeanFlowCumecs, degree = 3, intercept = FALSE ) + 
  bs(std_FWENZ_segSubstrate, degree = 3, intercept = FALSE ) + bs(std_local_twarm, degree = 3, intercept = FALSE ) + 
  factor(Barrier_present)
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

bias_correct = T
bias_correct_control = list( sd = FALSE, split = NULL, nsplit = 1, vars_to_correct = c( "Index_cyl", "Index_ctl" ) )


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
                         newtonsteps = 1, 
                         bias.correct.control = bias_correct_control, 
                         getJointPrecision = TRUE) 
Report = Obj$report()
time = Sys.time() - start_time ; print(paste0("Model run time: ", time))



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
             #"X1config_cp" = X1config_inp,
             #"X2config_cp" = X2config_inp,
             #"X_gctp" = X_gctp,
             #"X_itp" = X_itp,
             "covariate_data" = covariate_df,
             "X1_gctp" = TmbData$X1_gctp,
             #"X2_gctp" = TmbData$X2_gctp, #No affect on 2nd linear predictor
             #"X1_formula" = X1_formula_inp,
             #"X2_formula" = X2_formula_inp,
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



#######################################
# Percentage of river length occupied #
#######################################

Effective_area <- plot_range_index_SN(Sdreport = Fit$parameter_estimates$SD,
                                      Report = Fit$Report,
                                      TmbData = Fit$data_list,
                                      year_labels = as.numeric(Fit$year_labels),
                                      Znames = colnames(Fit$data_list$Z_gm),
                                      PlotDir = path,
                                      use_biascorr = TRUE,
                                      category_names = "",
                                      total_river_length = (sum(network$length)))
saveRDS(Effective_area, file.path(path, paste0("Effective_area.rds")))



##########################################
##########################################