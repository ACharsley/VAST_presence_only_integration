###################################################
##     Generate cross-validation statistics      ##
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
library(DHARMa)
library(ROCR) #auc function
library(cvAUC) #auc CI's


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

# inputArgs <- commandArgs(trailingOnly=TRUE)
# run_command <- inputArgs[1] ; print(run_command)
# 
# if(!is.na(run_command)){
#   
#   if(run_command == "HPC"){
#     task_id = as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
#     
#     if(task_id==1){scenario <- "Taranaki data"}
#     if(task_id==2){scenario <- "1a"}
#     if(task_id==3){scenario <- "1b"}
#     if(task_id==4){scenario <- "1c"}
#     if(task_id==5){scenario <- "1d"}
#     if(task_id==6){scenario <- "2a"}
#     if(task_id==7){scenario <- "2b"}
#     if(task_id==8){scenario <- "2c"}
#     if(task_id==9){scenario <- "2d"}
#     if(task_id==10){scenario <- "3a"}
#     if(task_id==11){scenario <- "3b"}
#     if(task_id==12){scenario <- "3c"}
#     if(task_id==13){scenario <- "3d"}
#     if(task_id==14){scenario <- "4a"}
#     if(task_id==15){scenario <- "4b"}
#     if(task_id==16){scenario <- "4c"}
#     if(task_id==17){scenario <- "4d"}
#     
#     network_type <- "full"
#   }
#   
# }else{
#   # "Taranaki data", 
#   # "1a", "1b", "1c", "1d", 
#   # "2a", "2b", "2c", "2d",
#   # "3a", "3b", "3c", "3d",
#   # "4a", "4b", "4c", "4d"
#   
#   scenario <-   "1a"
#   network_type <- "downstream"
# }

# sce <- c("Taranaki data",
#               "1a", "1b", "1c", 
#          #"1d",  #This model failed
#               "2a", "2b", "2c", "2d",
#               "3a", "3b", 
#          #"3c",  #This model failed
#          "3d",
#               "4a", "4b", "4c", "4d")
sce <-   c("Taranaki data",
           "1a", 
           #"1b", #strange result
           #"1c","1d", # Both worked but currently not Cross validated and results look strange
           "2a", 
           #"2b", #strange result
           #"2c","2d", #failed model
           "3a", 
           "3b", #strange result
           #"3c", #failed model
           #"3d", #Worked but currently not Cross validated and results look strange
           "4a"
           #, "4b", # Model failed
           # "4c",  #worked but currently not Cross validated and ...
           #"4d" # Model failed
)

network_type <- "full"

# Loop over all modelling scenarios
for(scenario in sce){
  
  print(scenario) ; print(network_type)
  
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
  
  #CV settings
  path_cv <- file.path(path, "CV")
  n_fold = 10
  
  #Objects to save cross-validation stuff in 
  cv_list <- list()
  i <- 1 #Counter for storing objects
  
  for(k in 1:n_fold){
    
    print(k)
    
    #Load cross-validation save
    path_cv_k <- file.path(path_cv, paste0("Fold_", k))
    
    tryCatch({
      Save_new <- readRDS(file.path(path_cv_k, paste0("Save.rds")))
      
      #Load data input
      Data_inp_new <- readRDS(file.path(path_cv_k, paste0("Data_input.rds")))
      
      #Save fit to out-of-bag data
      cv_list$prednll_f[[i]] = Save_new$Report$pred_jnll
      
      
      #Make predictions
      pred = Save_new$Report$R1_i #ALL prediction values
      pred_train = pred[Data_inp_new$PredTF_i==0] #predicted POC, for training data
      pred_eval = pred[Data_inp_new$PredTF_i==1] #predicted POC, For evaluation (test) data
      obs = round(Data_inp_new[Data_inp_new$PredTF_i==1,"b_i"]) #observed data, for test data
      
      #Save 
      cv_list$predictions_train[[i]] <- pred_train
      cv_list$predictions[[i]] <- pred_eval #store predictions
      cv_list$obs[[i]] <- obs #store test data
      cv_list$labels[[i]] <- factor(as.logical(obs))
      levels(cv_list$labels[[i]]) <- c("FALSE", "TRUE") #ensure the levels are F/T
      
      #Set up tables according to two thresholds - 0.5 and mean predicted POE from training data
      
      misc_table <- table(factor(cv_list$obs[[i]], levels = c(0,1)), factor(round(cv_list$predictions[[i]]), levels = c(0,1))) #assumption is that >=0.5 = 1 and <0.5 = 0
      
      new_pa <- factor(ifelse(pred_eval <= mean(pred_train), 0, 1), levels = c(0,1)) #using mean of training set as threshold
      misc_table_thres <- table(factor(cv_list$obs[[i]], levels = c(0,1)), new_pa) #assumption is that >=0.5 = 1 and <0.5 = 0
      
      #TSS
      cv_list$TSS[[i]] <- TSS_func(misc_table = misc_table)
      
      cv_list$TSS_thres[[i]] <- TSS_func(misc_table = misc_table_thres)
      
      #Sensitivity and specificity
      cv_list$sn[[i]] <- misc_table["1","1"] / (misc_table["1","1"] + misc_table["1","0"])
      cv_list$sp[[i]] <- misc_table["0","0"] / (misc_table["0","0"] + misc_table["0","1"])
      
      cv_list$sn_thres[[i]] <- misc_table_thres["1","1"] / (misc_table_thres["1","1"] + misc_table_thres["1","0"])
      cv_list$sp_thres[[i]] <- misc_table_thres["0","0"] / (misc_table_thres["0","0"] + misc_table_thres["0","1"])
      
      #RMSE
      cv_list$RMSE[[i]] <- RMSE(m=cv_list$predictions[[i]],o=cv_list$obs[[i]])
      
      
      
      
      #Update counter:
      i <- i+1
    },
    error=function(e) e) #If something fails then TryCatch will return an error then move to the next
    
  }
  
  
  #######
  # AUC #
  #######
  
  # Format predictions and test data for calculating AUC
  pred_AUC <- prediction(cv_list$predictions, cv_list$labels)
  
  #Area under the receiver operator characteristic curve
  AUC <- performance(pred_AUC, "auc")@y.values #find and store AUC values
  AUC_ests <- ci.cvAUC(predictions = cv_list$predictions, labels = cv_list$labels) #mean AUC, SE and 95CI's
  
  #Add to object
  cv_list$AUC[["estimates"]] <- unlist(AUC)
  cv_list$AUC[["mean_est"]] <- AUC_ests$cvAUC
  cv_list$AUC[["se"]] <- AUC_ests$se
  cv_list$AUC[["ci"]] <- AUC_ests$ci
  
  
  
  ###################
  # Save cv results #
  ###################
  
  save(cv_list, file = file.path(path_cv, "CV_results.RData"))
  
  
}

time = Sys.time() - start_time ; print(paste0("Generating CV statistics time: ", time))
