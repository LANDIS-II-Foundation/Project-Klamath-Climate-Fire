#######
################## SUMMARIZE OUTPUT IN CENTURY, HARVEST AND FIRE LOG FILES FROM LANDIS-II RUNS #####################
########################## Summarizes across multiple LANDIS-II scenarios and replicates ################################
## Note that each replicate is a separate folder because the landscape is too large to run replicates in a single model run

# Load libraries
library(sqldf)
library(plyr)
library(raster)


# Set working directory
dir <- "E:/K-S_Project/Scenarios/"
setwd(dir)
p <- "E:/K-S_Project/Scenarios/"
setwd(p)

# Load lookup tables for joining to data
LUT_Scenario_NineReps <- read.csv ("Klamath_Lookups/LUT_Scenarios_NineReps.csv")  # Includes replicates in scenario name
LUT_Scenario_4Reps <- read.csv ("Klamath_Lookups/LUT_Scenarios_4Reps.csv")  #Includes 4 replicates
LUT_Scenario_1Rep <- read.csv ("Klamath_Lookups/LUT_Scenarios_NoReps.csv")         # Excludes replicates
LUT_Scenario_Basic <- read.csv("Klamath_Lookups/LUT_Scenarios_Basic.csv")   #Base scenarios under contemporary climate
LUT_20Year <- read.csv  ("Klamath_Lookups/LUT_Age_20yr.csv")
LUT_10Year <- read.csv ("Klamath_Lookups/LUT_NECN_10yr.csv")                # Ten year time steps for Century/harvest simulations
LUT_5Year <- read.csv ("Klamath_Lookups/LUT_Harvest_5yr.csv") 
LUT_1Year <- read.csv ("Klamath_Lookups/LUT_Fire_1yr.csv")  # Annual time steps for fire simulations
LUT_Eco <- read.csv ("Klamath_Lookups/LUT_Ecoregion_Area.csv")  # Ecoreigon lookup table with weights based on landscape proportion
LUT_Wood_Density <- read.csv ("Klamath_Lookups/Wood_Densities.csv") # Various wood densities to covert mass to volume

#Species List
species_list <- c("ABGRC","ABPRSH", "ARME", "CADE27", "CHCH7", "FX_Resp_Deciduous", "FX_Seed_Deciduous", "FX_Seed_Evergreen", "LIDE3",
                  "NoFX_Resp_Deciduous", "NoFX_Resp_Evergreen", "NoFX_Seed_Deciduous", "NoFX_Seed_Evergreen", "PILA", "PIMO3", "PIPO", "PSME", "QUCH2", "QUGA4", "QUKE")

#Timesteps
timeteps <- c(0,10,20,30,40,50,60,70,80,90,100)

# List out scenario folder names in scen_list object
scen_list_full <- c("BAU_Contemporary_0_1", "BAU_Contemporary_0_2", "BAU_Contemporary_0_3", "BAU_Contemporary_0_4", "BAU_Contemporary_0_5", "BAU_Contemporary_0_6", "BAU_Contemporary_0_7", "BAU_Contemporary_0_8", "BAU_Contemporary_0_9",  
                    "CCA_Contemporary_0_1", "CCA_Contemporary_0_2", "CCA_Contemporary_0_3", "CCA_Contemporary_0_4", "CCA_Contemporary_0_5", "CCA_Contemporary_0_6", "CCA_Contemporary_0_7", "CCA_Contemporary_0_8", "CCA_Contemporary_0_9", 
                    "LIB_Contemporary_0_1", "LIB_Contemporary_0_2", "LIB_Contemporary_0_3", "LIB_Contemporary_0_4", "LIB_Contemporary_0_5", "LIB_Contemporary_0_6", "LIB_Contemporary_0_7", "LIB_Contemporary_0_8", "LIB_Contemporary_0_9", 
                    "PRIV_Contemporary_0_1", "PRIV_Contemporary_0_2", "PRIV_Contemporary_0_3", "PRIV_Contemporary_0_4", "PRIV_Contemporary_0_5", "PRIV_Contemporary_0_6", "PRIV_Contemporary_0_7", "PRIV_Contemporary_0_8", "PRIV_Contemporary_0_9", 
                    "SFT_Contemporary_0_1", "SFT_Contemporary_0_2", "SFT_Contemporary_0_3", "SFT_Contemporary_0_4", "SFT_Contemporary_0_5", "SFT_Contemporary_0_6", "SFT_Contemporary_0_7", "SFT_Contemporary_0_8", "SFT_Contemporary_0_9", 
                    "Rx_Contemporary_0_1", "Rx_Contemporary_0_2", "Rx_Contemporary_0_3", "Rx_Contemporary_0_4", "Rx_Contemporary_0_5", "Rx_Contemporary_0_6", "Rx_Contemporary_0_7", "Rx_Contemporary_0_8", "Rx_Contemporary_0_9", 
                    "BAU_ACCESS_85_1", "BAU_ACCESS_85_2", "BAU_ACCESS_85_3", "BAU_ACCESS_85_4", "BAU_ACCESS_85_5", "BAU_ACCESS_85_6", "BAU_ACCESS_85_7", "BAU_ACCESS_85_8", "BAU_ACCESS_85_9", 
                    "CCA_ACCESS_85_1", "CCA_ACCESS_85_2", "CCA_ACCESS_85_3", "CCA_ACCESS_85_4", "CCA_ACCESS_85_5", "CCA_ACCESS_85_6", "CCA_ACCESS_85_7", "CCA_ACCESS_85_8", "CCA_ACCESS_85_9", 
                    "LIB_ACCESS_85_1", "LIB_ACCESS_85_2", "LIB_ACCESS_85_3", "LIB_ACCESS_85_4", "LIB_ACCESS_85_5", "LIB_ACCESS_85_6", "LIB_ACCESS_85_7", "LIB_ACCESS_85_8", "LIB_ACCESS_85_9", 
                    "PRIV_ACCESS_85_1", "PRIV_ACCESS_85_2", "PRIV_ACCESS_85_3", "PRIV_ACCESS_85_4", "PRIV_ACCESS_85_5", "PRIV_ACCESS_85_6", "PRIV_ACCESS_85_7", "PRIV_ACCESS_85_8", "PRIV_ACCESS_85_9", 
                    "SFT_ACCESS_85_1", "SFT_ACCESS_85_2", "SFT_ACCESS_85_3", "SFT_ACCESS_85_4", "SFT_ACCESS_85_5", "SFT_ACCESS_85_6", "SFT_ACCESS_85_7", "SFT_ACCESS_85_8", "SFT_ACCESS_85_9", 
                    "Rx_ACCESS_85_1", "Rx_ACCESS_85_2", "Rx_ACCESS_85_3", "Rx_ACCESS_85_4", "Rx_ACCESS_85_5", "Rx_ACCESS_85_6", "Rx_ACCESS_85_7", "Rx_ACCESS_85_8", "Rx_ACCESS_85_9", 
                    "BAU_CanESM_85_1", "BAU_CanESM_85_2", "BAU_CanESM_85_3", "BAU_CanESM_85_4", "BAU_CanESM_85_5", "BAU_CanESM_85_6", "BAU_CanESM_85_7", "BAU_CanESM_85_8", "BAU_CanESM_85_9", 
                    "CCA_CanESM_85_1", "CCA_CanESM_85_2", "CCA_CanESM_85_3", "CCA_CanESM_85_4", "CCA_CanESM_85_5", "CCA_CanESM_85_6", "CCA_CanESM_85_7", "CCA_CanESM_85_8", "CCA_CanESM_85_9", 
                    "LIB_CanESM_85_1", "LIB_CanESM_85_2", "LIB_CanESM_85_3", "LIB_CanESM_85_4", "LIB_CanESM_85_5", "LIB_CanESM_85_6", "LIB_CanESM_85_7", "LIB_CanESM_85_8", "LIB_CanESM_85_9", 
                    "PRIV_CanESM_85_1", "PRIV_CanESM_85_2", "PRIV_CanESM_85_3", "PRIV_CanESM_85_4", "PRIV_CanESM_85_5", "PRIV_CanESM_85_6", "PRIV_CanESM_85_7", "PRIV_CanESM_85_8", "PRIV_CanESM_85_9", 
                    "SFT_CanESM_85_1", "SFT_CanESM_85_2", "SFT_CanESM_85_3", "SFT_CanESM_85_4", "SFT_CanESM_85_5", "SFT_CanESM_85_6", "SFT_CanESM_85_7", "SFT_CanESM_85_8", "SFT_CanESM_85_9", 
                    "Rx_CanESM_85_1", "Rx_CanESM_85_2", "Rx_CanESM_85_3", "Rx_CanESM_85_4", "Rx_CanESM_85_5", "Rx_CanESM_85_6", "Rx_CanESM_85_7", "Rx_CanESM_85_8", "Rx_CanESM_85_9", 
                    "BAU_CNRM_45_1", "BAU_CNRM_45_2", "BAU_CNRM_45_3", "BAU_CNRM_45_4", "BAU_CNRM_45_5", "BAU_CNRM_45_6", "BAU_CNRM_45_7", "BAU_CNRM_45_8", "BAU_CNRM_45_9", 
                    "CCA_CNRM_45_1", "CCA_CNRM_45_2", "CCA_CNRM_45_3", "CCA_CNRM_45_4", "CCA_CNRM_45_5", "CCA_CNRM_45_6", "CCA_CNRM_45_7", "CCA_CNRM_45_8", "CCA_CNRM_45_9", 
                    "LIB_CNRM_45_1", "LIB_CNRM_45_2", "LIB_CNRM_45_3", "LIB_CNRM_45_4", "LIB_CNRM_45_5", "LIB_CNRM_45_6", "LIB_CNRM_45_7", "LIB_CNRM_45_8", "LIB_CNRM_45_9", 
                    "PRIV_CNRM_45_1", "PRIV_CNRM_45_2", "PRIV_CNRM_45_3", "PRIV_CNRM_45_4", "PRIV_CNRM_45_5", "PRIV_CNRM_45_6", "PRIV_CNRM_45_7", "PRIV_CNRM_45_8", "PRIV_CNRM_45_9", 
                    "SFT_CNRM_45_1", "SFT_CNRM_45_2", "SFT_CNRM_45_3", "SFT_CNRM_45_4", "SFT_CNRM_45_5", "SFT_CNRM_45_6", "SFT_CNRM_45_7", "SFT_CNRM_45_8", "SFT_CNRM_45_9", 
                    "Rx_CNRM_45_1", "Rx_CNRM_45_2", "Rx_CNRM_45_3", "Rx_CNRM_45_4", "Rx_CNRM_45_5", "Rx_CNRM_45_6", "Rx_CNRM_45_7", "Rx_CNRM_45_8", "Rx_CNRM_45_9", 
                    "BAU_MIROC5_26_1", "BAU_MIROC5_26_2", "BAU_MIROC5_26_3", "BAU_MIROC5_26_4", "BAU_MIROC5_26_5", "BAU_MIROC5_26_6", "BAU_MIROC5_26_7", "BAU_MIROC5_26_8", "BAU_MIROC5_26_9", 
                    "CCA_MIROC5_26_1", "CCA_MIROC5_26_2", "CCA_MIROC5_26_3", "CCA_MIROC5_26_4", "CCA_MIROC5_26_5", "CCA_MIROC5_26_6", "CCA_MIROC5_26_7", "CCA_MIROC5_26_8", "CCA_MIROC5_26_9", 
                    "LIB_MIROC5_26_1", "LIB_MIROC5_26_2", "LIB_MIROC5_26_3", "LIB_MIROC5_26_4", "LIB_MIROC5_26_5", "LIB_MIROC5_26_6", "LIB_MIROC5_26_7", "LIB_MIROC5_26_8", "LIB_MIROC5_26_9", 
                    "PRIV_MIROC5_26_1", "PRIV_MIROC5_26_2", "PRIV_MIROC5_26_3", "PRIV_MIROC5_26_4", "PRIV_MIROC5_26_5", "PRIV_MIROC5_26_6", "PRIV_MIROC5_26_7", "PRIV_MIROC5_26_8", "PRIV_MIROC5_26_9", 
                    "SFT_MIROC5_26_1", "SFT_MIROC5_26_2", "SFT_MIROC5_26_3", "SFT_MIROC5_26_4", "SFT_MIROC5_26_5", "SFT_MIROC5_26_6", "SFT_MIROC5_26_7", "SFT_MIROC5_26_8", "SFT_MIROC5_26_9", 
                    "Rx_MIROC5_26_1", "Rx_MIROC5_26_2", "Rx_MIROC5_26_3", "Rx_MIROC5_26_4", "Rx_MIROC5_26_5", "Rx_MIROC5_26_6", "Rx_MIROC5_26_7", "Rx_MIROC5_26_8", "Rx_MIROC5_26_9")

#########################################################################################################################################
#########################################################################################################################################
#################################################### CENTURY LOG CARBON OUTPUT ##########################################################
# Set up empty object
all_data <- NULL

# Loop through each scenario and replicate and bring together selected NECN log output into a single data frame
for (Scenario in scen_list_full)   # for each scenario and replicate, compiles data into a single data frame
{
  log_file <- paste(dir, Scenario, "/", "NECN-succession-log.csv", sep="")
  log_data <- read.csv(log_file)
  select_data <- (log_data[,c(1:2,5:9,12:28,54:55)])  # select the column numbers of interest
  scenar <- rep(Scenario, nrow(select_data))     # create a vector of the length of your input file to record the scenario using the rep (replicate) function
  scen_data <- cbind(select_data, Scenario)        # bind the scenario name to the data
  all_data <- rbind(all_data, scen_data)         # append the scenarios together
}

write.csv (all_data, "all_data.csv")


##### Summarize each output of interest across replicates for historic and grouped future climate #####
# Subset data to select only current harvest scenario, then summarize by climate group (historic climate and climate change)
#data_curharv <- subset(all_data1, HarvestScenario=="Current")
# Note that some units are in Mg (TotalC, AGB, AGC, SOC) and some are in g (ANPP, NEE)
NECN_climgrp <- sqldf('SELECT Time, EcoregionName, Scenario,
                      AVG((C_Leaf)+(C_FRoot)+(C_Wood)+(C_CRoot)+(C_DeadWood)+(C_DeadCRoot)+(C_DeadLeaf_Struc)+(C_DeadLeaf_Meta)+(C_DeadFRoot_Struc)+(C_DeadFRoot_Meta)+(C_SOM1surf)+(C_SOM1soil)+(C_SOM2)+(C_SOM3)) AS TotalC_Avg,
                      SQRT(VARIANCE((C_Leaf)+(C_FRoot)+(C_Wood)+(C_CRoot)+(C_DeadWood)+(C_DeadCRoot)+(C_DeadLeaf_Struc)+(C_DeadLeaf_Meta)+(C_DeadFRoot_Struc)+(C_DeadFRoot_Meta)+(C_SOM1surf)+(C_SOM1soil)+(C_SOM2)+(C_SOM3))) AS TotalC_SD,
                      AVG(AGB) AS AGB_Avg,
                      SQRT(VARIANCE(AGB)) AS AGB_SD,
                      AVG((AGB)*0.47) AS AGC_Avg,
                      SQRT(VARIANCE((AGB)*0.47)) AS AGC_SD,
                      AVG(SOMTC) AS SOC_Avg,
                      SQRT(VARIANCE(SOMTC)) AS SOC_SD,
                      AVG(AG_NPPC) AS ANPP_Avg,
                      SQRT(VARIANCE(AG_NPPC)) AS ANPP_SD,
                      AVG(NEEC) AS NEE_Avg,
                      SQRT(VARIANCE(NEEC)) AS NEE_SD
                      FROM all_data
                      GROUP BY Time, EcoregionName, Scenario')
# Weight output by ecoregion to summarize across whole landscape
NECN_climgrp1 <- merge(NECN_climgrp, LUT_Eco, all=TRUE)
## Weight the values by the area occupied by each ecoregion
# This code multiplies each value by the ecoregion weight (0-1), then sums across all ecoregions
NECN_climgrp_wtd <- sqldf('SELECT Time, EcoregionName, Scenario,
                          SUM(TotalC_Avg*Weight) AS TotalC_Avg_wtd,
                          SUM(TotalC_SD*Weight) AS TotalC_SD_wtd,
                          SUM(AGB_Avg*Weight) AS AGB_Avg_wtd,
                          SUM(AGB_SD*Weight) AS AGB_SD_wtd,
                          SUM(AGC_Avg*Weight) AS AGC_Avg_wtd,
                          SUM(AGC_SD*Weight) AS AGC_SD_wtd,
                          SUM(SOC_Avg*Weight) AS SOMC_Avg_wtd,
                          SUM(SOC_SD*Weight) AS SOMC_SD_wtd,
                          SUM(ANPP_Avg*Weight) AS ANPPC_Avg_wtd,
                          SUM(ANPP_SD*Weight) AS ANPPC_SD_wtd,
                          SUM(NEE_Avg*Weight) AS NEEC_Avg_wtd,
                          SUM(NEE_SD*Weight) AS NEEC_SD_wtd
                          FROM NECN_climgrp1
                          GROUP BY Time, EcoregionName, Scenario ')
## Generate values +- 1SD
NECN_climgrp_wtd2 <- sqldf('SELECT Time, EcoregionName, Scenario,
                           SUM(TotalC_Avg_wtd+TotalC_SD_wtd) AS TotalC_Plus_SD,
                           SUM(TotalC_Avg_wtd-TotalC_SD_wtd) AS TotalC_Minus_SD,
                           SUM(AGC_Avg_wtd+AGC_SD_wtd) AS AGC_Plus_SD,
                           SUM(AGC_Avg_wtd-AGC_SD_wtd) AS AGC_Minus_SD,
                           SUM(SOMC_Avg_wtd+SOMC_SD_wtd) AS SOMC_Plus_SD,
                           SUM(SOMC_Avg_wtd-SOMC_SD_wtd) AS SOMC_Minus_SD,
                           SUM(ANPPC_Avg_wtd+ANPPC_SD_wtd) AS ANPP_Plus_SD,
                           SUM(ANPPC_Avg_wtd-ANPPC_SD_wtd) AS ANPP_Minus_SD,
                           SUM(NEEC_Avg_wtd+NEEC_SD_wtd) AS NEE_Plus_SD,
                           SUM(NEEC_Avg_wtd-NEEC_SD_wtd) AS NEE_Minus_SD
                           FROM NECN_climgrp_wtd
                           GROUP BY Time, EcoregionName, Scenario ')
NECN_climgrp_wtd3 <- merge(NECN_climgrp_wtd2, LUT_10Year)
write.csv(NECN_climgrp_wtd, "NECN_climate_group_summary_withSD.csv")

##### Summarize each output of interest across replicates for each individual climate model #####
# Note that some units are in Mg (TotalC, AGB, AGC, SOC) and some are in g (ANPP, NEE)
NECN_summary <- sqldf('SELECT Time, EcoregionName, Scenario,
                      AVG((C_Leaf)+(C_FRoot)+(C_Wood)+(C_CRoot)+(C_DeadWood)+(C_DeadCRoot)+(C_DeadLeaf_Struc)+(C_DeadLeaf_Meta)+(C_DeadFRoot_Struc)+(C_DeadFRoot_Meta)+(C_SOM1surf)+(C_SOM1soil)+(C_SOM2)+(C_SOM3)) AS TotalC_Avg,
                      SQRT(VARIANCE((C_Leaf)+(C_FRoot)+(C_Wood)+(C_CRoot)+(C_DeadWood)+(C_DeadCRoot)+(C_DeadLeaf_Struc)+(C_DeadLeaf_Meta)+(C_DeadFRoot_Struc)+(C_DeadFRoot_Meta)+(C_SOM1surf)+(C_SOM1soil)+(C_SOM2)+(C_SOM3))) AS TotalC_SD,
                      AVG(AGB) AS AGB_Avg,
                      SQRT(VARIANCE(AGB)) AS AGB_SD,
                      AVG((AGB)*0.47) AS AGC_Avg,
                      SQRT(VARIANCE((AGB)*0.47)) AS AGC_SD,
                      AVG(SOMTC) AS SOC_Avg,
                      SQRT(VARIANCE(SOMTC)) AS SOC_SD,
                      AVG(AG_NPPC) AS ANPP_Avg,
                      SQRT(VARIANCE(AG_NPPC)) AS ANPP_SD,
                      AVG(NEEC) AS NEE_Avg,
                      SQRT(VARIANCE(NEEC)) AS NEE_SD
                      FROM all_data1
                      GROUP BY Time, EcoregionName, Scenario')
# Weight output by ecoregion to summarize across whole landscape
NECN_summary1 <- merge(NECN_summary, LUT_Eco, all=TRUE)
#write.csv(output_summary1, "output_summary1.csv")
## Weight the values by the area occupied by each ecoregion
# This code multiplies each value by the ecoregion weight (0-1), then sums across all ecoregions
NECN_summary_wtd <- sqldf('SELECT Time, Scenario,
                          SUM(TotalC_Avg*Weight) AS TotalC_Avg_wtd,
                          SUM(TotalC_SD*Weight) AS TotalC_SD_wtd,
                          SUM(AGB_Avg*Weight) AS AGB_Avg_wtd,
                          SUM(AGB_SD*Weight) AS AGB_SD_wtd,
                          SUM(AGC_Avg*Weight) AS AGC_Avg_wtd,
                          SUM(AGC_SD*Weight) AS AGC_SD_wtd,
                          SUM(SOC_Avg*Weight) AS SOMC_Avg_wtd,
                          SUM(SOC_SD*Weight) AS SOMC_SD_wtd,
                          SUM(ANPP_Avg*Weight) AS ANPPC_Avg_wtd,
                          SUM(ANPP_SD*Weight) AS ANPPC_SD_wtd,
                          SUM(NEE_Avg*Weight) AS NEEC_Avg_wtd,
                          SUM(NEE_SD*Weight) AS NEEC_SD_wtd
                          FROM NECN_summary1
                          GROUP BY Time, Scenario')
NECN_summary_wtd1 <- merge(NECN_summary_wtd, LUT_Scenario_1Rep)
NECN_summary_wtd2 <- merge(NECN_summary_wtd1, LUT_10Year)
write.csv(NECN_summary_wtd2, "NECN_annual_summary_weighted.csv")


##################################################################################################################################
##################################################################################################################################
##################################################### MONTHLY LOG OUTPUT #########################################################
# Set up empty object
all_month_data <- NULL

# Loop through each scenario and bring together monthly log output into a single data frame
for (Scenario in scen_list_WUE)   # for each scenario and replicate, compiles data into a single data frame
{
  month_file <- paste(dir, Scenario, "/", "NECN-succession-monthly-log.csv", sep="")
  month_data <- read.csv(month_file)
  select_month_data <- month_data [,c(1:3,6:12)]
  scenar <- rep(Scenario, nrow(select_month_data))     # create a vector of the length of your input file to record the scenario using the rep (replicate) function
  scen_data <- cbind(select_month_data, scenar)        # bind the scenario name to the data
  all_month_data <- rbind(all_month_data, scen_data)         # append the scenarios together
}
colnames(all_month_data)<-c("Time", "Month", "EcoregionName", "ppt", "airtemp", "avgNPPtc", "avgResp", "avgNEE", "Ndep", "StreamN", "Scenario")
write.csv(all_month_data, "NECN_monthly_log.csv")
all_month_data1 <- merge(all_month_data, LUT_Scenario_FullReps)

## Summarize each output of interest across replicates
monthly_summary <- sqldf('SELECT Time, Month, EcoregionName, Scenario,
                         AVG(avgNPPtc) AS NPPC_Avg,
                         SQRT(VARIANCE(avgNPPtc)) AS NPPC_SD,
                         AVG(avgNEE) AS NEEC_Avg,
                         SQRT(VARIANCE(avgNEE)) AS NEEC_SD,
                         AVG(airtemp) AS Temp_Avg,
                         AVG(ppt) AS Ppt_Avg,
                         AVG(avgResp) AS Resp_Avg,
                         AVG(Ndep) AS Ndep_Avg
                         FROM all_month_data
                         GROUP BY Time, Month, EcoregionName, Scenario')
write.csv(monthly_summary, "monthly_summary.csv")

# WEIGHT OUTPUT BY ECOREGION
monthly_summary1 <- merge(monthly_summary, LUT_Eco, all=TRUE)
## Weight the values by the area occupied by each ecoregion
# This code multiplies each value by the ecoregion weight (0-1), then sums across all ecoregions
monthly_summary_wtd <- sqldf('SELECT Time, Month, Scenario,
                             SUM(NPPC_Avg*Weight) AS NPPC_Avg_wtd,
                             SUM(NPPC_SD*Weight) AS NPPC_SD_wtd,
                             SUM(NEEC_Avg*Weight) AS NEEC_Avg_wtd,
                             SUM(NEEC_SD*Weight) AS NEEC_SD_wtd,
                             SUM(Temp_Avg*Weight) AS Temp_Avg_wtd,
                             SUM(Ppt_Avg*Weight) AS Ppt_Avg_wtd,
                             SUM(Resp_Avg*Weight) AS Resp_Avg_wtd,
                             SUM(Ndep_Avg*Weight) AS Ndep_Avg_wtd
                             FROM monthly_summary1
                             GROUP BY Time, Month, Scenario')
monthly_summary_wtd1 <- merge(monthly_summary_wtd, LUT_Scenario_1Rep)
monthly_summary_wtd2 <- merge(monthly_summary_wtd1, LUT_10Year)
write.csv(monthly_summary_wtd, "NECN_monthly_summary_by_scenario.csv")


#####################################################################################################################################
#####################################################################################################################################
###################################################### HARVEST OUTPUT ###############################################################
# Set up empty object
all_harv_data <- NULL

# Loop through each scenario and bring together selected harvest log output into a single data frame
for (Scenario in scen_list_full)   # for each scenario and replicate, compiles data into a single data frame
{
  harv_file <- paste(dir, Scenario,"/","harvest-event-test-log.csv", sep="")
  harv_data <- read.csv(harv_file)
  harv_data1 <- cbind(harv_data,(((harv_data[,c(34)]*1000)/862)*423.776*.8))  #conversion of Mg to volume by converting to kilograms, dividing by density, converting from m^3 to bdft, and assuming that only 80% of what's harvested is taken off site.
  harv_data1 <- cbind(harv_data1,(((harv_data[,c(35)]*1000)/826)*423.776*.8))
  harv_data1 <- cbind(harv_data1,(((harv_data[,c(36)]*1000)/1105)*423.776*.8))
  harv_data1 <- cbind(harv_data1,(((harv_data[,c(37)]*1000)/775)*423.776*.8))
  harv_data1 <- cbind(harv_data1,(((harv_data[,c(38)]*1000)/1073)*423.776*.8))
  harv_data1 <- cbind(harv_data1,(((harv_data[,c(39)]*1000)/1230)*423.776*.8))
  harv_data1 <- cbind(harv_data1,(((harv_data[,c(40)]*1000)/1230)*423.776*.8))
  harv_data1 <- cbind(harv_data1,(((harv_data[,c(41)]*1000)/1230)*423.776*.8))
  harv_data1 <- cbind(harv_data1,(((harv_data[,c(42)]*1000)/1230)*423.776*.8))
  harv_data1 <- cbind(harv_data1,(((harv_data[,c(43)]*1000)/1230)*423.776*.8))
  harv_data1 <- cbind(harv_data1,(((harv_data[,c(44)]*1000)/1230)*423.776*.8))
  harv_data1 <- cbind(harv_data1,(((harv_data[,c(45)]*1000)/1230)*423.776*.8))
  harv_data1 <- cbind(harv_data1,(((harv_data[,c(46)]*1000)/1230)*423.776*.8))
  harv_data1 <- cbind(harv_data1,(((harv_data[,c(47)]*1000)/950)*423.776*.8))
  harv_data1 <- cbind(harv_data1,(((harv_data[,c(48)]*1000)/670)*423.776*.8))
  harv_data1 <- cbind(harv_data1,(((harv_data[,c(49)]*1000)/840)*423.776*.8))
  harv_data1 <- cbind(harv_data1,(((harv_data[,c(50)]*1000)/753)*423.776*.8))
  harv_data1 <- cbind(harv_data1,(((harv_data[,c(51)]*1000)/1412)*423.776*.8))
  harv_data1 <- cbind(harv_data1,(((harv_data[,c(52)]*1000)/1176)*423.776*.8))
  harv_data1 <- cbind(harv_data1,(((harv_data[,c(53)]*1000)/1212)*423.776*.8))
  harv_data2 <- cbind(harv_data1, rowSums(harv_data1[,c(54:73)] ))
  colnames(harv_data2)[74] <- "Total_BDFT"
  select_harv_data <- (harv_data2[,c(1:4,10,11,34:74)]) # select the column numbers of interest
  scenar <- rep(Scenario, nrow(select_harv_data))         # create a vector of the length of your input file to record the scenario using the rep (replicate) function
  scen_harv_data <- cbind(select_harv_data, Scenario)       # bind the scenario name to the data
  all_harv_data <- rbind(all_harv_data, scen_harv_data)   # append the replicates together
}
write.csv(all_harv_data, "all_harv_data.csv")
all_harv_data1 <- merge(all_harv_data, LUT_Scenario_4Reps)
write.csv(all_harv_data1, "all_harv_data1.csv")


##### Calculate harvested carbon by year for each major ownership type ####
harvest_summary <- sqldf('SELECT Time, Harvest_Climate, ManagementArea, Prescription,
                         SUM(Total_BDFT) AS Total_BDFT
                         FROM all_harv_data1
                         GROUP BY Time, Harvest_Climate, ManagementArea, Prescription')


write.csv(harvest_summary, "Harvested_total_summary.csv")

##### Calculate harvested carbon by year for whole landscape ####
harvest_summary_all <- sqldf('SELECT Time, Harvest_Climate, 
                             SUM((MgBiomassRemoved)*0.47) AS HarvC_Mg
                             FROM all_harv_data1
                             GROUP BY Time, Harvest_Climate')
harvest_summary_all1 <- sqldf('SELECT Time, Harvest_Climate,
                              SUM(HarvC_Mg)/2997220 AS HarvC_Avg_MgHa
                              FROM harvest_summary_all
                              GROUP BY Time, Harvest_Climate')
harvest_summary_all2 <- merge(harvest_summary_all1, LUT_Scenario_4Reps)
harvest_summary_all3 <- merge(harvest_summary_all2, LUT_5Year)
write.csv(harvest_summary_all3, "Harvested_carbon_summary_whole_landscape.csv")


#####################################################################################################################################
#####################################################################################################################################
###################################################### FIRE OUTPUT ###############################################################
# Set up empty object
all_fire_data <- NULL
active.sites <-418625*7.29 
# Loop through each scenario and replicate and bring together fire event log output into a single data frame
for (Scenario in scen_list_full)   # for each scenario, compiles data into a single data frame
{
  fire_file <- paste(dir, Scenario, "/dynamic-fire-events-log.csv", sep="")
  fire_data <- read.csv(fire_file)
  frp.simulated <- ((max(fire_data$Time) )/ sum(fire_data$TotalSites*7.29)) *active.sites
  select_fire_data <- (fire_data[,c(1,16,17,19)]) # select the column numbers of interest  
  select_fire_data1 <- cbind(select_fire_data, (select_fire_data[,3]*7.29))
  scenar <- rep(Scenario, nrow(select_fire_data1))         # create a vector of the length of your input file to record the scenario using the rep (replicate) function
  scen_fire_data <- cbind(select_fire_data1, Scenario, frp.simulated)     # bind the scenario name to the data
  all_fire_data <- rbind(all_fire_data, scen_fire_data)   # append the replicates together
}
cols <- c("Time", "FWI", "CellsBurned", "Severity", "TotalArea", "Scenario", "FRP")
colnames(all_fire_data) <- cols

all_fire_data1 <- merge(all_fire_data, LUT_Scenario_FullReps)

#all_fire_data2 <- merge(all_fire_data1, LUT_1Year)
write.csv(all_fire_data1, "all_fire_data.csv")

fire.sizes.total <- data.frame (
  Q10=quantile (all_fire_data1$CellsBurned,probs = 0.1)*((270*270)/10000),
  Q25=quantile (all_fire_data1$CellsBurned,probs = 0.25)*((270*270)/10000),
  Q40=quantile (all_fire_data1$CellsBurned,probs = 0.40)*((270*270)/10000),
  Mean.Size.ha = mean (all_fire_data1$CellsBurned)*((270*270)/10000),
  Median.Size.ha = median (all_fire_data1$CellsBurned)*((270*270)/10000),
  Q60=quantile (all_fire_data1$CellsBurned,probs = 0.6)*((270*270)/10000),
  Q75=quantile (all_fire_data1$CellsBurned,probs = 0.75)*((270*270)/10000),
  Q90=quantile (all_fire_data1$CellsBurned,probs = 0.90)*((270*270)/10000),
  Q95=quantile (all_fire_data1$CellsBurned,probs = 0.95)*((270*270)/10000),
  Q99=quantile (all_fire_data1$CellsBurned,probs = 0.99)*((270*270)/10000),
  
  Max.Size.ha = max (all_fire_data1$CellsBurned)*((270*270)/10000),
  SD.Size.ha = sd(all_fire_data1$CellsBurned)*((270*270)/10000)
)

if(all_fire_data1$Climate_GCM = "Contemporary"){
  fire.sizes.total_baseline <- data.frame (
    Q10=quantile (all_fire_data1$CellsBurned,probs = 0.1)*((270*270)/10000),
    Q25=quantile (all_fire_data1$CellsBurned,probs = 0.25)*((270*270)/10000),
    Q40=quantile (all_fire_data1$CellsBurned,probs = 0.40)*((270*270)/10000),
    Mean.Size.ha = mean (all_fire_data1$CellsBurned)*((270*270)/10000),
    Median.Size.ha = median (all_fire_data1$CellsBurned)*((270*270)/10000),
    Q60=quantile (all_fire_data1$CellsBurned,probs = 0.6)*((270*270)/10000),
    Q75=quantile (all_fire_data1$CellsBurned,probs = 0.75)*((270*270)/10000),
    Q90=quantile (all_fire_data1$CellsBurned,probs = 0.90)*((270*270)/10000),
    Q95=quantile (all_fire_data1$CellsBurned,probs = 0.95)*((270*270)/10000),
    Q99=quantile (all_fire_data1$CellsBurned,probs = 0.99)*((270*270)/10000),
    
    Max.Size.ha = max (all_fire_data1$CellsBurned)*((270*270)/10000),
    SD.Size.ha = sd(all_fire_data1$CellsBurned)*((270*270)/10000))}


###### SUMMARIZE BY DECADE: Calculate sum of ha burned, average severity, and average FWI #####
fire_summary_decade <- sqldf('SELECT Time, Harvest_Climate, 
                             SUM((CellsBurned)*7.29) AS Ha_Burned,
                             AVG(Severity) AS Severity,
                             AVG(FWI) AS FWI
                             FROM all_fire_data1
                             GROUP BY Time, Harvest_Climate')
fire_summary_decade1 <- sqldf('SELECT Time, Harvest_Climate,
                              AVG(Ha_Burned) AS Total_Ha_Burned_Avg,
                              SQRT(VARIANCE(Ha_Burned)) AS Total_Ha_Burned_SD,
                              AVG(Severity) AS Severity_Avg,
                              SQRT(VARIANCE(Severity)) AS Severity_SD,
                              AVG(FWI) AS FWI_Avg,
                              SUM(Total_Ha_Burned_Avg)/(418625*7.29) AS FRP
                              FROM fire_summary_decade
                              GROUP BY Harvest_Climate')
fire_summary_decade2 <- merge(fire_summary_decade1, LUT_Scenario)
write.csv(fire_summary_decade2, "Fire_summary_by_decade.csv")

##### SUMMARIZE BY TIME SLICE: Calculate sum of ha burned, average severity, and average FWI #####
fire_summary_timeslice <- sqldf('SELECT Time, TimeSlice, Scenario, 
                                SUM((CellsBurned)*7.29) AS Ha_Burned,
                                AVG(Severity) AS Severity,
                                AVG(FWI) AS FWI
                                FROM all_fire_data2
                                GROUP BY Time, TimeSlice, Scenario')
fire_summary_timeslice1 <- sqldf('SELECT TimeSlice, Scenario2,
                                 AVG(Ha_Burned) AS Total_Ha_Burned_Avg,
                                 SQRT(VARIANCE(Ha_Burned)) AS Total_Ha_Burned_SD,
                                 AVG(Severity) AS Severity_Avg,
                                 SQRT(VARIANCE(Severity)) AS Severity_SD,
                                 AVG(FWI) AS FWI_Avg
                                 FROM fire_summary_timeslice
                                 GROUP BY TimeSlice, Scenario')
fire_summary_timeslice2 <- merge(fire_summary_timeslice1, LUT_Scenario)
write.csv(fire_summary_timeslice2, "Fire_summary_by_timeslice.csv")

###### Plotting Extreme fire events ######
library(ggplot2)

largefires <- subset(all_fire_data2, CellsBurned>250)   # >1000 hectares
# may also want to subset fires that are pst 2050?

#histogram
ggplot2.histogram(data=largefires, xName='CellsBurned', groupName='ClimateScenario', binwidth=50, alpha=0.0, addDensity=TRUE)

# density plot
densityplot( ~ CellsBurned, groups=ClimateScenario, data=largefires)

#ggplot(largefires, aes(CellsBurned,fill=ClimateScenario) + stat_density(fill=NA))

##################################################################################################################################
##################################################################################################################################
##################################################### SPP Biomass LOG OUTPUT #####################################################
# Set up empty object
all_spp_data <- NULL

# Loop through each scenario and bring together monthly log output into a single data frame
for (Scenario in scen_list_4)   # for each scenario and replicate, compiles data into a single data frame
{
  spp_biomass_file <- paste(dir, Scenario, "/", "spp-biomass-log.csv", sep="")
  sppbiomass_data <- read.csv(spp_biomass_file)
  scenar <- rep(Scenario, nrow(sppbiomass_data))     # create a vector of the length of your input file to record the scenario using the rep (replicate) function
  scen_data <- cbind(sppbiomass_data, scenar)        # bind the scenario name to the data
  all_spp_data <- rbind(all_spp_data, scen_data)         # append the scenarios together
}
colnames(all_spp_data)<-c("Time", "Ecoregion", "NumSites", "ABGRC", "ABPRSH", "ARME", "CADE27", "CHCH7", "FX_Resp_Deciduous","FX_Seed_Deciduous","FX_Seed_Evergreen",
                          "LIDE3","NoFX_Resp_Deciduous","NoFX_Resp_Evergreen","NoFX_Seed_Deciduous","NoFX_Seed_Evergreen","PILA","PIMO3","PIPO","PSME","QUCH2","QUGA4","QUKE","","Scenario")
all_spp_data1 <- all_spp_data[-c(24)]
na.omit(all_spp_data1)
all_spp_data1 <- merge(all_spp_data1, LUT_Scenario_4Reps)
write.csv(all_spp_data1, "spp_summary.csv")

## Summarize each output of interest across replicates
spp_summary <- sqldf('SELECT Time, Ecoregion, Harvest_Climate,
                     AVG(ABGRC) AS ABGRC_Avg,
                     AVG(ABPRSH) AS ABPRSH_Avg,
                     AVG(ARME) AS ARME_Avg,
                     AVG(CADE27) AS CADE27_Avg,
                     AVG(CHCH7) AS CHCH7_Avg,
                     AVG(FX_Resp_Deciduous) AS FX_Resp_Deciduous,
                     AVG(FX_Seed_Evergreen) AS FX_Seed_Evergreen,
                     AVG(FX_Seed_Deciduous) AS FX_Seed_Deciduous,
                     AVG(LIDE3) AS LIDE3_Avg,
                     AVG(NoFX_Resp_Deciduous) AS NoFX_Resp_Deciduous_Avg,
                     AVG(NoFX_Resp_Evergreen) AS NoFX_Resp_Evergreen_Avg,
                     AVG(NoFX_Seed_Deciduous) AS NoFX_Seed_Deciduous_Avg,
                     AVG(NoFX_Seed_Evergreen) AS NoFX_Seed_Evergreen_Avg,
                     AVG(PILA) AS PILA_Avg,
                     AVG(PIMO3) AS PIMO3_Avg,
                     AVG(PIPO) AS PIPO_Avg,
                     AVG(PSME) AS PSME_Avg,
                     AVG(QUCH2) AS QUCH2_Avg,
                     AVG(QUGA4) AS QUGA4_Avg,
                     AVG(QUKE) AS QUKE_Avg
                     FROM all_spp_data1
                     GROUP BY Time, Ecoregion, Harvest_Climate')
write.csv(spp_summary, "spp_summary.csv")

###########################################MFRI MAPPING#####################################
#script goes through DFFS output, converts fire severity rating to a simple count and adds them through time (and then divided by the simulation length) to return Mean Fire Return Interval
library(raster)

scen_name <- as.list(c("BAU", "CCA", "LIB", "PRIV", "SFT", "Rx"))

clim_name <- as.list(c("_Contemporary_0", "_ACCESS_85", "_CanESM_85", "_CNRM_45", "_MIROC5_26"))

scen_lis <- (NULL)
scen_list <- (NULL)
for(i in seq_along(scen_name)){
  for(j in seq_along(clim_name)){
    scen_lis <- paste0(scen_name[i],clim_name[j])
    scen_list <- as.list(c(scen_list,scen_lis))
  }
}

print(scen_list)


#across scenario and projection for loop
setwd(g)
timesteps <- 1:100
sta <- stack()
sta2<- stack()

for(i in seq_along(scen_list)){
  lf <- list.files(g)
  head(lf)
  scen_list_3 <- paste0('^',scen_list[i],'_.$')
  print(scen_list_3)
  current_harv_clim <- grep(scen_list_3, lf, value = TRUE)
  print(current_harv_clim)
  
  sta <- stack()
  sta2<- stack()
  
  for(j in seq_along(current_harv_clim)){    
    print(current_harv_clim[j])
    #r <- paste0(g,current_harv_clim[j],'/fire/','severity-',timesteps,'.img')
    #print(r)
    
    binary.burn.map <- lapply (timesteps, function (timesteps){
      r <- raster (paste0(g,current_harv_clim[j],'/fire/','severity-',timesteps,'.img'))
      r[r[]<2]<-0
      r[r[]>2]<-1
      return(r)
    })
    
    binary.burn.map2 <- Reduce (stack, binary.burn.map)
    reburn.times.map  <- sum (binary.burn.map2)
    plot(reburn.times.map)
    #    freq(reburn.times.map)
    sta <- stack(sta, reburn.times.map)
    s <- 100/reburn.times.map
    sta2<-stack(sta2, s)
    plot(s)
    #    freq(s)
  }  
  
  current_scen <- sum(sta)/length(current_harv_clim)
  plot(current_scen)
  current_scen_mfri <- 100/current_scen
  plot(current_scen_mfri)
  
  current_name <- paste0(scen_list[i],"_avg.tif")
  print(current_name)
  current_name_mfri <- paste0(scen_list[i],"_fri.tif")
  writeRaster(current_scen, current_name, overwrite=TRUE )
  writeRaster(current_scen_mfri, current_name_mfri, overwrite=TRUE)
}

library(doParallel)
registerDoParallel(cores = 6) 
getDoParWorkers()

#parallelized version to calculate areas with highest fire severity
foreach(i=1:length(scen_list)) %dopar% {
  library(raster)
  lf <- list.files(g)
  print(lf)
  scen_list_3 <- paste0('^',scen_list[i],'_.$')
  print(scen_list_3)
  current_harv_clim <- grep(scen_list_3, lf, value = TRUE)
  print(current_harv_clim)
  
  sta <- stack()
  sta2<- stack()
  
  for(j in seq_along(current_harv_clim)){    
    print(current_harv_clim[j])
    binary.burn.map <- lapply (timesteps, function (timesteps){
      r <- raster (paste0(g,current_harv_clim[j],'/fire/','severity-',timesteps,'.img'))
      r[r[]<6]<-0
      r[r[]>6]<-1
      return(r)
    })
    
    binary.burn.map2 <- Reduce (stack, binary.burn.map)
    reburn.times.map  <- sum (binary.burn.map2)
    #plot(reburn.times.map)
    sta <- stack(sta, reburn.times.map)
    s <- 100/reburn.times.map
    sta2<-stack(sta2, s)
    #plot(s)
  }  
  
  current_scen <- sum(sta)/length(current_harv_clim)
  plot(current_scen)
  current_scen_mfri <- 100/current_scen
  plot(current_scen_mfri)
  
  current_name <- paste0(scen_list[i],"_avg_0-9_hs.tif")
  current_name_mfri <- paste0(scen_list[i],"_fri_0-9_hs.tif")
  writeRaster(current_scen, current_name, overwrite=TRUE )
  writeRaster(current_scen_mfri, current_name_mfri, overwrite=TRUE)
}

###################Biomass Reclass to Forest Type Map####################
#Calculates most frequent cover type on a cell by cell basis and returns a single map
for(i in seq_along(scen_list_full)){
  su <- paste0(scen_list_full[i],"/outputs/biomass-reclass/")
  print(su)
    for(j in seq_along(timesteps)){
    rec <- paste0(p,su,"biomass-reclass-reclass1-",timesteps[j],".img")
    print(rec)
    ras <- raster(rec)
    rnameout <- paste0(p,"scen_reclass/", scen_list_full[i],"_biom-rec_",timesteps[j],".tif")
    print(rnameout)
    writeRaster(ras, rnameout,  overwrite = TRUE)
  }
}


lf <- list.files(p)
print(lf)

#for calculating mode
for(i in seq_along (scen_list)){
  for(j in seq_along (timesteps)){
    sta <- stack()
    ras <- raster()
    name <- paste0(scen_list[i],"_",".","_","biom-rec_",timesteps[j],".tif")
    print(name)
    su <- grep(paste0(scen_list[i],"_",".","_","biom-rec_",timesteps[j],".tif"), lf, value = TRUE)
    print(su)
    sta <- stack(su)
    maj.class <- function(x) { as.numeric( names( which.max( table(x) ) ) ) }
    mclass <- calc(sta, maj.class)
    #    plot(mclass)
    rname_out <- paste0(scen_list[i],"_","biom-rec-mode_",timesteps[j],".tif")
    print(rname_out)
    writeRaster(mclass, rname_out, overwrite=TRUE)
  }
}

p1 <- paste0(p,"scen_reclass/")
fn <- list.files(p1)
print(fn)


all_data <- NULL
scen_data <- NULL
for (i in seq_along(fn)){
  scen_data <- cbind(freq(raster(fn[i])), fn[i])        # bind the scenario name to the data
  all_data <- rbind(all_data, scen_data)         # append the scenarios together
}
write.csv (all_data, "reclass_mode_count.csv")

#####################Consensus Map#################################

setwd(p1)
lf <- list.files(p1)

head(lf)

maj.class <- function(x) { as.numeric( names( which.max( table(x) ) ) )}
f.class <- function(x) { 
  maxm <- as.numeric(names(which.max( table(x)))) ##calculates the mode
  lenm <- length(which((x)==maxm)) ##counts how often that mode occurred 
  percag <- 100 * (lenm/length(x))
  return(percag)
}
scen_list <- as.list(c("BAU", "CCA", "LIB", "PRIV", "SFT", "Rx"))
clim_list <- as.list(c("ACCESS", "CanESM", "CNRM", "Contemporary", "MIROC5"))
timesteps <- as.list(c(40))

for(i in seq_along (scen_list)){
  for(j in seq_along (timesteps)){
    name <- paste0(".*",clim_list[i],".*biom-rec_",timesteps[j],".tif")
    name <- paste0(".*biom-rec_","40",".tif$")
    print(name)
    su <- grep(name, lf, value = TRUE)
    print(su)
    sta <- stack(su)
    #maj.class <- function(x) { as.numeric( names( which.max( table(x) ) ) ) }
    mclass <- calc(sta, maj.class)
    fclass <- calc(sta, f.class)
    mname_out <- paste0(clim_list[i],"_mclass_",timesteps[j],".tif")
    fname_out <- paste0(clim_list[i],"_fclass_",timesteps[j],".tif")
    print(mname_out)
    print(fname_out)
    writeRaster(mclass, mname_out, overwrite=TRUE)
    writeRaster(fclass, fname_out, overwrite=TRUE)
    writeRaster(mclass, "year_40_mclass.tif", overwrite=TRUE)
    writeRaster(fclass, "year_40_fclass.tif", overwrite=TRUE)
  }
}

plot(mclass)
plot(fclass)

for(i in seq_along (scen_list)){
  name <- paste0(scen_list[i],".*biom-rec_","40",".tif")
  print(name)
  su <- grep(name, lf, value = TRUE)
  print(su)
  sta <- stack(su)
  #maj.class <- function(x) { as.numeric( names( which.max( table(x) ) ) ) }
  fclass <- calc(sta, f.class)
  fname_out <- paste0(scen_list[i],"_fclass_40",".tif")
  print(fname_out)
  writeRaster(fclass, fname_out, overwrite=TRUE)
}

for(i in seq_along (scen_list)){
  name <- paste0(scen_list[i],".*biom-rec_40",".tif")
  print(name)
  su <- grep(name, lf, value = TRUE)
  print(su)
  sta <- stack(su)
  #maj.class <- function(x) { as.numeric( names( which.max( table(x) ) ) ) }
  mclass <- calc(sta, maj.class)
  mname_out <- paste0(scen_list[i],"_mclass_40",".tif")
  print(fname_out)
  writeRaster(mclass, mname_out, overwrite=TRUE)
}

plot(fclass)
print(lf)