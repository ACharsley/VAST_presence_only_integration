#############
# Check EMs #
#############

rm(list=ls())



###############
# Directories #
###############

#Set EM path
EM_path <- paste0(getwd(), "/Models/EMs")



########################
#  Modelling scenario  #
########################

# Operating models
scenarios <- c("OM_1a", 
               "OM_2a", 
               "OM_3a", "OM_3b",
               "OM_4a")

# Estimating models
data_sample <- c("sample_rand1n",
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
reps <- c(1:100)



#########
# Check #
#########

EM_failed <- list()

# Operating model
print("#############################  Percentage of failed EMs:  #############################")
for(sce in scenarios){ # sce = "OM_1a"
  
  #print(sce)
  
  # Estimating model
  for(mod in data_sample){ # mod = "sample_rand2n"
    
    i <- 1 #counter
    
    # Replicate
    for(r in reps){ # r = 1
      
      # Path with simulation study model results 
      path <- file.path(EM_path, paste0("EM_output_", sce), paste0("Rep_",r,"_",mod))
      
      #If file doesn't exist save
      if(!file.exists(file.path(path, "POE_list.RData"))){
        EM_failed[[paste0(sce, "_", mod)]][i] <- r
        i <- i + 1
      }
    }
    
    per_failed <- length(EM_failed[[paste0(sce, "_", mod)]])/length(reps) *100 
    
    print(paste0(sce, "_", mod))
    #print(EM_failed[[paste0(sce, "_", mod)]])
    print(paste0(per_failed,"%"))
    
  }
}

print("#############################  Failed EMs:  #############################")
print(EM_failed)


