
# Assemble all the data sets together


#########################################################
#   Load NZFFD observations to join to customary data   #
#########################################################

NZFFD_data <- NZFFD_data %>%
  select(catchname, nzsegment, parent_i, child_i, dist_i, fishmeth, angdie, Year,
         org, Lon, Lat) %>%
  rename("CatName"=catchname, "Data_value"=angdie, "Data_source"=org, "Fishmethod"=fishmeth) %>%
  mutate(Data_type="p_a")

table(NZFFD_data$Fishmethod, NZFFD_data$Year) #Very little data outside of EF

# #Keep only EF
# NZFFD_data <- NZFFD_data %>% 
#   filter(Fishmethod=="Electric fishing")


######################################
#   Join NZFFD and customary data    #
######################################

NZFFD_cust_data <- rbind(NZFFD_data, cust_data)

table(NZFFD_cust_data$Data_value, NZFFD_cust_data$Data_type)
table(NZFFD_cust_data$Data_value, NZFFD_cust_data$Year)