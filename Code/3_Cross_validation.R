###################################################
##              Model cross-validation           ##
##                                               ##
##               Anthony Charsley                ##
##                   May 2023                    ##
###################################################


rm(list=ls())
start_time <- Sys.time()


##############
#  Packages  #
##############

library(tidyverse)
library(VAST)
#library(DHARMa)
#library(ROCR) #auc function
#library(cvAUC) #auc CI's


#################
#  Directories  #
#################

model_path <- paste0(getwd(), "/Models")

####################
#  Call functions  #
####################

source(paste0(getwd(), "/Code/funcs.R"))


########################
#  Modelling scenario  #
########################

inputArgs <- commandArgs(trailingOnly=TRUE)
run_command <- inputArgs[1] ; print(run_command)

if(!is.na(run_command)){
  
  if(run_command == "HPC"){
    task_id = as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
    
    if(task_id %in% c(1:10)){scenario <- "Taranaki data" ; k <- task_id}
    if(task_id %in% c(11:20)){scenario <- "1a" ; k <- task_id - 10}
    if(task_id %in% c(21:30)){scenario <- "1b" ; k <- task_id - 20}
    if(task_id %in% c(31:40)){scenario <- "1c" ; k <- task_id - 30}
    if(task_id %in% c(41:50)){scenario <- "1d" ; k <- task_id - 40}
    if(task_id %in% c(51:60)){scenario <- "2a" ; k <- task_id - 50}
    if(task_id %in% c(61:70)){scenario <- "2b" ; k <- task_id - 60}
    if(task_id %in% c(71:80)){scenario <- "2c" ; k <- task_id - 70}
    if(task_id %in% c(81:90)){scenario <- "2d" ; k <- task_id - 80}
    if(task_id %in% c(91:100)){scenario <- "3a" ; k <- task_id - 90}
    if(task_id %in% c(101:110)){scenario <- "3b" ; k <- task_id - 100}
    if(task_id %in% c(111:120)){scenario <- "3c" ; k <- task_id - 110}
    if(task_id %in% c(121:130)){scenario <- "3d" ; k <- task_id - 120}
    if(task_id %in% c(131:140)){scenario <- "4a" ; k <- task_id - 130}
    if(task_id %in% c(141:150)){scenario <- "4b" ; k <- task_id - 140}
    if(task_id %in% c(151:160)){scenario <- "4c" ; k <- task_id - 150}
    if(task_id %in% c(161:170)){scenario <- "4d" ; k <- task_id - 160}
    
    network_type <- "full"
  }
  
}else{
  # "Taranaki data", 
  # "1a", "1b", "1c", "1d", 
  # "2a", "2b", "2c", "2d",
  # "3a", "3b", "3c", "3d",
  # "4a", "4b", "4c", "4d"
  
  scenario <-   "1a"
  k <- 4
  network_type <- "downstream"
}

print(scenario) ; print(network_type) ; print(k)


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

#Load model
load(file.path(path, "Fit.RData"))


###########################
# K-fold Cross-validation #
###########################

# Partition data
set.seed(280423) #Ensures partitioning is always the same
n_fold = 10

## Partition EF and NetTrap data separately so that there is approximately an even number from each
Partition_i_EF = sample(1:n_fold,
                        size=sum(Fit$catchability_data$Data_source_inp == 0),
                        replace=TRUE)
table(Partition_i_EF)

Partition_i_NetTrap = sample(1:n_fold,
                             size=sum(Fit$catchability_data$Data_source_inp == 1),
                             replace=TRUE)
table(Partition_i_NetTrap)

#Set a new path for cv
path_cv <- file.path(path, "CV")
dir.create(path_cv, showWarnings = F)


###################################
# Run cross-validation for fold k #
###################################

start_time <- Sys.time()
####
#Set Data_inp to new dataframe to be edited
Data_inp_new <- cbind(Fit$data_frame, "Data_source_inp" =Fit$catchability_data$Data_source_inp)

#Initially set PredTF_i to zero, indicating to include all in model (ensures all Unstructured data is in model)
Data_inp_new$PredTF_i <- 0

#Now set the data to be excluded for cross-validation
Data_inp_new[Data_inp_new$Data_source_inp == 0,"PredTF_i"] = ifelse(Partition_i_EF==k, 1, 0)
Data_inp_new[Data_inp_new$Data_source_inp == 1,"PredTF_i"] = ifelse(Partition_i_NetTrap==k, 1, 0)
#table(Data_inp_new$PredTF_i, Data_inp_new$Data_source_inp)

#Set a new path for cv
path_cv_k <- file.path(path_cv, paste0("Fold_", k))
dir.create(path_cv_k, showWarnings = F)

#Build new data object
TmbData_new <- Fit$data_list
TmbData_new$PredTF_i <- Data_inp_new$PredTF_i
TmbData_new$Options_list$Options["SD_site_density"] <- 0

#Build the VAST model 
TmbList_new = make_model(build_model = TRUE, 
                         TmbData = TmbData_new, 
                         RunDir = path_cv_k,
                         Version = Fit$settings$Version,
                         RhoConfig = TmbData_new$RhoConfig,
                         Method = "Stream_network")

#Modify the Map and Random lists 
Map = TmbList_new$Map
Random = TmbList_new$Random
ParHat = TmbList_new$Obj$env$parList()
Map[["beta2_ft"]] = factor( rep( NA, length( ParHat$beta2_ft ) ) )

#Rebuild the VAST model
TmbList_new = make_model(build_model = TRUE,
                         TmbData = TmbData_new,
                         RunDir = path_cv_k,
                         Version = Fit$settings$Version,
                         RhoConfig = TmbData_new$RhoConfig,
                         Method = "Stream_network",
                         Map = Map,
                         Random = Random)
Obj_new <- TmbList_new[["Obj"]]


#Estimate fixed effects and predict random effects
Opt_new = TMBhelper::fit_tmb(obj = Obj_new,
                             lower = TmbList_new[["Lower"]],
                             upper = TmbList_new[["Upper"]],
                             getsd = TRUE, 
                             savedir = path_cv_k, 
                             bias.correct = Fit$settings$bias.correct, 
                             newtonsteps = 1, 
                             bias.correct.control = list( sd = FALSE, split = NULL, nsplit = 1, vars_to_correct = c( "Index_cyl", "Index_ctl" ) ), 
                             getJointPrecision = TRUE) 
Report_new = Obj_new$report()

#Save new data 
saveRDS(Data_inp_new, file.path(path_cv_k, paste0("Data_input.rds")))

#Save important model stuff
Save_new = list( "Opt" = Opt_new, "Report" = Report_new, 
                 "ParHat" = Obj_new$env$parList( Opt_new$par ), "TmbData" = TmbData_new)
saveRDS(Save_new, file.path(path_cv_k, paste0("Save.rds")))



####
time = Sys.time() - start_time ; time

###################################
###################################