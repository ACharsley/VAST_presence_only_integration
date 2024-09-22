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

# For testing:
#data_sample = "sample_rand1n"

task_id = as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")) #task_id=1

if(task_id %in% c(1:10)){scenario <- "OM_1a" ; data_sample = "sample_rand1n"}
if(task_id %in% c(11:20)){scenario <- "OM_1a" ; data_sample = "sample_rand5n"}
if(task_id %in% c(21:30)){scenario <- "OM_1a" ; data_sample = "sample_unsuithab1n"}
if(task_id %in% c(31:40)){scenario <- "OM_1a" ; data_sample = "sample_unsuithab5n"}
if(task_id %in% c(41:50)){scenario <- "OM_1a" ; data_sample = "sample_nearroads1n"}
if(task_id %in% c(51:60)){scenario <- "OM_1a" ; data_sample = "sample_nearroads5n"}
if(task_id %in% c(61:70)){scenario <- "OM_1a" ; data_sample = "sample_unsuithab_nearroads1n"}
if(task_id %in% c(71:80)){scenario <- "OM_1a" ; data_sample = "sample_unsuithab_nearroads5n"}

if(task_id %in% c(81:90)){scenario <- "OM_1b" ; data_sample = "sample_rand1n"}
if(task_id %in% c(91:100)){scenario <- "OM_1b" ; data_sample = "sample_rand5n"}
if(task_id %in% c(101:110)){scenario <- "OM_1b" ; data_sample = "sample_unsuithab1n"}
if(task_id %in% c(111:120)){scenario <- "OM_1b" ; data_sample = "sample_unsuithab5n"}
if(task_id %in% c(121:130)){scenario <- "OM_1b" ; data_sample = "sample_nearroads1n"}
if(task_id %in% c(131:140)){scenario <- "OM_1b" ; data_sample = "sample_nearroads5n"}
if(task_id %in% c(141:150)){scenario <- "OM_1b" ; data_sample = "sample_unsuithab_nearroads1n"}
if(task_id %in% c(151:160)){scenario <- "OM_1b" ; data_sample = "sample_unsuithab_nearroads5n"}

if(task_id %in% c(161:170)){scenario <- "OM_2a" ; data_sample = "sample_rand1n"}
if(task_id %in% c(171:180)){scenario <- "OM_2a" ; data_sample = "sample_rand5n"}
if(task_id %in% c(181:190)){scenario <- "OM_2a" ; data_sample = "sample_unsuithab1n"}
if(task_id %in% c(191:200)){scenario <- "OM_2a" ; data_sample = "sample_unsuithab5n"}
if(task_id %in% c(201:210)){scenario <- "OM_2a" ; data_sample = "sample_nearroads1n"}
if(task_id %in% c(211:220)){scenario <- "OM_2a" ; data_sample = "sample_nearroads5n"}
if(task_id %in% c(221:230)){scenario <- "OM_2a" ; data_sample = "sample_unsuithab_nearroads1n"}
if(task_id %in% c(231:240)){scenario <- "OM_2a" ; data_sample = "sample_unsuithab_nearroads5n"}

if(task_id %in% c(241:250)){scenario <- "OM_2b" ; data_sample = "sample_rand1n"}
if(task_id %in% c(251:260)){scenario <- "OM_2b" ; data_sample = "sample_rand5n"}
if(task_id %in% c(261:270)){scenario <- "OM_2b" ; data_sample = "sample_unsuithab1n"}
if(task_id %in% c(271:280)){scenario <- "OM_2b" ; data_sample = "sample_unsuithab5n"}
if(task_id %in% c(281:290)){scenario <- "OM_2b" ; data_sample = "sample_nearroads1n"}
if(task_id %in% c(291:300)){scenario <- "OM_2b" ; data_sample = "sample_nearroads5n"}
if(task_id %in% c(301:310)){scenario <- "OM_2b" ; data_sample = "sample_unsuithab_nearroads1n"}
if(task_id %in% c(311:320)){scenario <- "OM_2b" ; data_sample = "sample_unsuithab_nearroads5n"}

if(task_id %in% c(321:330)){scenario <- "OM_3a" ; data_sample = "sample_rand1n"}
if(task_id %in% c(331:340)){scenario <- "OM_3a" ; data_sample = "sample_rand5n"}
if(task_id %in% c(341:350)){scenario <- "OM_3a" ; data_sample = "sample_unsuithab1n"}
if(task_id %in% c(351:360)){scenario <- "OM_3a" ; data_sample = "sample_unsuithab5n"}
if(task_id %in% c(361:370)){scenario <- "OM_3a" ; data_sample = "sample_nearroads1n"}
if(task_id %in% c(371:380)){scenario <- "OM_3a" ; data_sample = "sample_nearroads5n"}
if(task_id %in% c(381:390)){scenario <- "OM_3a" ; data_sample = "sample_unsuithab_nearroads1n"}
if(task_id %in% c(391:400)){scenario <- "OM_3a" ; data_sample = "sample_unsuithab_nearroads5n"}

if(task_id %in% c(401:410)){scenario <- "OM_3b" ; data_sample = "sample_rand1n"}
if(task_id %in% c(411:420)){scenario <- "OM_3b" ; data_sample = "sample_rand5n"}
if(task_id %in% c(421:430)){scenario <- "OM_3b" ; data_sample = "sample_unsuithab1n"}
if(task_id %in% c(431:440)){scenario <- "OM_3b" ; data_sample = "sample_unsuithab5n"}
if(task_id %in% c(441:450)){scenario <- "OM_3b" ; data_sample = "sample_nearroads1n"}
if(task_id %in% c(451:460)){scenario <- "OM_3b" ; data_sample = "sample_nearroads5n"}
if(task_id %in% c(461:470)){scenario <- "OM_3b" ; data_sample = "sample_unsuithab_nearroads1n"}
if(task_id %in% c(471:480)){scenario <- "OM_3b" ; data_sample = "sample_unsuithab_nearroads5n"}

if(task_id %in% c(481:490)){scenario <- "OM_4a" ; data_sample = "sample_rand1n"}
if(task_id %in% c(491:500)){scenario <- "OM_4a" ; data_sample = "sample_rand5n"}
if(task_id %in% c(501:510)){scenario <- "OM_4a" ; data_sample = "sample_unsuithab1n"}
if(task_id %in% c(511:520)){scenario <- "OM_4a" ; data_sample = "sample_unsuithab5n"}
if(task_id %in% c(521:530)){scenario <- "OM_4a" ; data_sample = "sample_nearroads1n"}
if(task_id %in% c(531:540)){scenario <- "OM_4a" ; data_sample = "sample_nearroads5n"}
if(task_id %in% c(541:550)){scenario <- "OM_4a" ; data_sample = "sample_unsuithab_nearroads1n"}
if(task_id %in% c(551:560)){scenario <- "OM_4a" ; data_sample = "sample_unsuithab_nearroads5n"}

if(task_id %in% c(561:570)){scenario <- "OM_4b" ; data_sample = "sample_rand1n"}
if(task_id %in% c(571:580)){scenario <- "OM_4b" ; data_sample = "sample_rand5n"}
if(task_id %in% c(581:590)){scenario <- "OM_4b" ; data_sample = "sample_unsuithab1n"}
if(task_id %in% c(591:600)){scenario <- "OM_4b" ; data_sample = "sample_unsuithab5n"}
if(task_id %in% c(601:610)){scenario <- "OM_4b" ; data_sample = "sample_nearroads1n"}
if(task_id %in% c(611:620)){scenario <- "OM_4b" ; data_sample = "sample_nearroads5n"}
if(task_id %in% c(621:630)){scenario <- "OM_4b" ; data_sample = "sample_unsuithab_nearroads1n"}
if(task_id %in% c(631:640)){scenario <- "OM_4b" ; data_sample = "sample_unsuithab_nearroads5n"}


rep <- task_id %% 10
if(rep == 0) rep <- 10



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


## Plot catchment data ##
plotting_data <- Data_inp %>%
  mutate("Present" = ifelse(round(Catch_KG) == 1, "Present", "Absent"))

catchmap <- ggplot(plotting_data) +
  geom_point(data = network, aes(x = Lon, y = Lat), col = "gray") +
  geom_point(aes(x = Lon, y = Lat, col = Present), alpha = 0.8, size=3) +
  facet_wrap(.~Year) +
  xlab("Longitude (°E)") + ylab("Latitude (°N)") +
  #ggtitle("Longfin eel data by year") +
  guides(color = guide_legend(title = "")) +
  scale_colour_manual(values = c("#E41A1C", "chartreuse4")) +
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = rel(1.25)),
        axis.title=element_text(size = rel(1.5),face="bold"),
        axis.text.x = element_text(angle = 90),
        legend.text=element_text(size = rel(1)))
ggsave(file.path(path, paste0("Catchment_data_map_rep",rep,"_",data_sample,".png")), catchmap, height = 12, width = 15)
####



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
                         newtonsteps = 3, 
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

#save(Fit, file = file.path(path, "Fit.RData"))
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