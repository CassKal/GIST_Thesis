#######################################################################################
# 
# FILE Thesis_Kalinski_AppendixB_R_Script_ModelTuning_vXX.R
# SEE ALSO Thesis_Kalinski_AppendixB_R_Script_ModelTuning_vXX.R
# 
# USC SSCI 594 - Masters Thesis in GIST
# Fall 2018/Spring 2019
# "Impacts of Covariate Scale on MaxEnt Model Outputs: An Example Using Bristlecone Pine Data"
# Author: Cass Kalinski, ckalinsk@usc.edu
# 
# Following code builds the initial tuning evaluation models for the thesis study
# The ENMeval package is used to facilitate this and generate the necessary metrics for parameter selection
# ENMeval uses either the MaxNet package to create the MaxEnt models or the MaxEnt java jar file.
# The latter is used in this study.
#
# Subsequent work will generate tuned and default models for comparison
#
# Code within this module was copied from or modeled after code provided by 
# Muscarella, Robert, Peter J. Galante, Mariano Soley-Guardia, Robert A. Boria, Jamie M. Kass, María Uriarte, and Robert P. Anderson. 2014. 
#     "ENMeval: An R package for conducting spatially independent evaluations and estimating optimal model complexity for Maxent ecological niche models."  
#     Methods in Ecology and Evolution 5 (11):1198-1205. doi: 10.1111/2041-210X.12261.
#     R Vignette: https://cran.r-project.org/web/packages/ENMeval/vignettes/ENMeval-vignette.html
#
#######################################################################################

#######################################################################################
#
# Methods that will be used in the study:
#    jackknife
#    randomkfold with k=5
#    block 
#    checkerboard2 with cell count = 5km grids (5 for 800m, 167 for 30m DEMs)
#    
#######################################################################################

# Setup java footpring for MaxEnt. Needs to be run before any packages load. 
rm(list = ls())
# options(java.parameters = "-Xmxlg")
options(java.parameters = "-d64")
options(java.parameters = "-Xmx4g")

#######################################################################################
# SET THESE FLAGS BEFORE DATA RUN

# Flag to run or not run the four models. Allows a bypass on the model code block once models are built
modelRun <- TRUE

# NOTE: Models take about 70 minutes to run per model on an 8 Core Intel box. Memory may constrain running
# all four in sequence as well. I ran them individually and restarted RStudio after each run. Have to rerun
# the preliminary code each time. Or save off an RDATA file of the RStudio environment.

# Using three different computers to build the models. Flags to switch path structures unique to each.
# Set flag to 0 for unknown, 1 for HP desktop, 2 for HP laptop2, 3 for Dell laptop1
pathFlag <- 0

# Set flag for 30m versus 800m data run
dataRun <- "BOMB" 
#dataRun <- "30m"
#dataRun <- "800m"

#######################################################################################

# General environment setup

# Default repo
local({r <- getOption("repos")
r["CRAN"] <- "https://cloud.r-project.org" 
options(repos=r)
})


# Install needed packages
if (!requireNamespace("ade4", quietly = TRUE))
  install.packages("ade4")
if (!requireNamespace("raster", quietly = TRUE))
  install.packages("raster")
if (!requireNamespace("rgeos", quietly = TRUE))
  install.packages("rgeos")
if (!requireNamespace("maptools", quietly = TRUE))
  install.packages("maptools")
if (!requireNamespace("dismo", quietly = TRUE))
  install.packages("dismo")
if (!requireNamespace("ENMeval", quietly = TRUE))
  install.packages("ENMeval")


# Load libraries
library(ade4)
library(raster)
library(rgeos)
library(maptools)
library(dismo)
library(ENMeval)
library(arcgisbinding)
arc.check_product()

#######################################################################################
#
# Load data files for Laptop1 if the models have already been run
if (pathFlag == 3 & !modelRun ){
  print("Start Load")
  load("C:/Users/CKaya/Dropbox/School/_Coursework/SSCI_594ab_MastersThesis/Data/_R_Snapshots/Desktop/30m/bcone_checkerboard2.RData")
  load("C:/Users/CKaya/Dropbox/School/_Coursework/SSCI_594ab_MastersThesis/Data/_R_Snapshots/Desktop/30m/bcone_jackknife.RData")
  load("C:/Users/CKaya/Dropbox/School/_Coursework/SSCI_594ab_MastersThesis/Data/_R_Snapshots/Laptop1/30m/bcone_randomkfold.RData")
  load("C:/Users/CKaya/Dropbox/School/_Coursework/SSCI_594ab_MastersThesis/Data/_R_Snapshots/Laptop2/30m/bcone_block.RData")
  print("Load completed")
}
#
#######################################################################################

#######################################################################################
# Create paths for various folders

# Set path prefix per modeling environment
if (pathFlag == 1) {
  #HP Desktop
    pathPrefix <- file.path("E:")
    ModelEnvironment <- "Desktop"
    pathGDB <- file.path("C:","594_Thesis","ThesisProject_v3","ThesisProject_v3.gdb")
} else if (pathFlag == 2) {
  # HP Laptop2
    pathPrefix <- file.path("C:","Users","CKaya")
    ModelEnvironment <- "Laptop2"
} else if (pathFlag == 3) {
  # Dell laptop1
    pathPrefix <- file.path("C:","Users","CKaya")
    ModelEnvironment <- "Laptop1"
    pathGDB <- file.path("C:","Users","CKaya","Documents","ArcGIS","Packages","ThesisProject_v3_181201_1800_A1BD7D83-A2D7-4FC8-8CE8-2A6150F3FB57","p20","thesisproject_v3.gdb")
} else {
    stop("Path not set properly. Program terminated")
}

# Set path target per 30m versus 800m run
if (dataRun == "30m") {
    dataSource <- file.path("ASCII_30m","BConeClimateAdjusted")
} else if (dataRun == "800m") {
    dataSource <- file.path("ASCII_800m","BConeClimateAdjusted")
} else {
    stop("Data run resolution not set properly. Program terminated")
}

# Dropbox path shared by modeling environments
pathDropboxModelData <- file.path("Dropbox","School","_Coursework","SSCI_594ab_MastersThesis","Data","_ModelDataFiles")
pathDropboxSnapshots <- file.path("Dropbox","School","_Coursework","SSCI_594ab_MastersThesis","Data","_R_Snapshots")
pathDropboxResults <- file.path("Dropbox","School","_Coursework","SSCI_594ab_MastersThesis","Data","_Results")
SpeciesInfo <- file.path("SpeciesCSV","BConeLocationUTM.csv")

# Set final data input and output paths
pathSpecies <- file.path(pathPrefix,pathDropboxModelData,SpeciesInfo)
pathEnvVar <- file.path(pathPrefix,pathDropboxModelData,dataSource)
pathSaveTo <- file.path(pathPrefix,pathDropboxSnapshots,ModelEnvironment,dataRun)
pathResults <- file.path(pathPrefix,pathDropboxResults, dataRun)

#######################################################################################

# Create a list of parameters for the models 
RMlist <- c(1, 2, 3, 4, 5)
FClist <- c("L", "Q", "P", "LQ", "LQP")
size_kfold <- 5
if (dataRun == "30m") {
    size_checkerboard2 <- c(167,5)
} else if (dataRun == "800m") {
    size_checkerboard2 <- c(5,5)
} else {
    stop("Data run resolution not set properly. Program terminated")
}

#######################################################################################

# Get the Bristlecone presence data 
bcone <- read.csv(pathSpecies, header = TRUE, sep = ",")
occsBCone <- as.data.frame(bcone[,2:3])

#######################################################################################

# Get the list of environment files for the Bristlecone study 
envFiles <- list.files(path=pathEnvVar, pattern = "*.asc", full.names=TRUE)

# Put the rasters into a RasterStack:
envRaster <- stack(envFiles)

# Rename rasters to match
envFileNames <- list.files(path=pathEnvVar, pattern = "*.asc", full.names=FALSE)
names(envRaster) <- envFileNames

# FYI...listing of rasters
print(names(envRaster))

#######################################################################################

# <<<<<<<<<<  Model Runs  >>>>>>>>>>
# Time stamps at beginning and end of model runs. Run time data appended to a text file for each model method

if (modelRun) {
  
  #  <<<<  JACKKNIFE  >>>>>
  startTime <- Sys.time()
  bcone.jackknife <- ENMevaluate(occsBCone, envRaster,
                              RMvalues = RMlist,
                              fc = FClist,
                              n.bg = 10000,
                              method = "jackknife",
                              algorithm = "maxent.jar",
                              overlap = TRUE,
                              parallel = TRUE)

  endTime <- Sys.time()
  runTime <- difftime(endTime, startTime, units = "mins")
  
  RTjackknife <- as.character(c(as.numeric(endTime), as.character(startTime), as.character(endTime), runTime))
  write.table(RTjackknife, file = file.path(pathSaveTo, "RTJacknife.txt"), sep = "\t", append = TRUE)
  
  save(bcone.jackknife, file=(file.path(pathSaveTo, "bcone_jackknife.RData")))
  # Completed in 


  #  <<<<  RANDOM K-FOLD  >>>>>
  startTime <- Sys.time()
  bcone.randomkfold <- ENMevaluate(occsBCone, envRaster, 
                                RMvalues = RMlist, 
                                fc = FClist,
                                n.bg = 10000,
                                method = "randomkfold",
                                algorithm = "maxent.jar",
                                kfolds = size_kfold,
                                overlap = TRUE,
                                parallel = TRUE)
  endTime <- Sys.time()
  runTime <- difftime(endTime, startTime, units = "mins")
  
  RTrandomkfold <- as.character(c(as.numeric(endTime), as.character(startTime), as.character(endTime), runTime))
  write.table(RTrandomkfold, file = file.path(pathSaveTo, "RTrandomkfold.txt"), sep = "\t", append = TRUE)
  
  save(bcone.randomkfold, file=file.path(pathSaveTo, "bcone_randomkfold.RData"))
  # Completed in


  #  <<<<  CHECKERBOARD2  >>>>>
  startTime <- Sys.time()
  bcone.checkerboard2 <- ENMevaluate(occsBCone, envRaster, 
                                RMvalues = RMlist, 
                                fc = FClist,
                                n.bg = 10000,
                                method = "checkerboard2",
                                algorithm = "maxent.jar",
                                aggregation.factor = size_checkerboard2,
                                overlap = TRUE,
                                parallel = TRUE)
  endTime <- Sys.time()
  runTime <- difftime(endTime, startTime, units = "mins")
  
  RTcheckerboard2 <- as.character(c(as.numeric(endTime), as.character(startTime), as.character(endTime), runTime))
  write.table(RTcheckerboard2, file = file.path(pathSaveTo, "RTcheckerboard2.txt"), sep = "\t", append = TRUE)
  
  save(bcone.checkerboard2, file=file.path(pathSaveTo, "bcone_checkerboard2.RData"))
  # Completed in 


  #  <<<<  BLOCK  >>>>>
  startTime <- Sys.time()
  bcone.block <- ENMevaluate(occsBCone, envRaster, 
                                  RMvalues = RMlist, 
                                  fc = FClist,
                                  n.bg = 10000,
                                  method = "block",
                                  algorithm = "maxent.jar",
                                  overlap = TRUE,
                                  parallel = TRUE)
  endTime <- Sys.time()
  runTime <- difftime(endTime, startTime, units = "mins")
  
  RTblock <- as.character(c(as.numeric(endTime), as.character(startTime), as.character(endTime), runTime))
  write.table(RTblock, file = file.path(pathSaveTo, "RTblock.txt"), sep = "\t", append = TRUE)
  
  save(bcone.block, file=file.path(pathSaveTo, "bcone_block.RData"))
  # Completed in  
  
  # Saving all environment variables
  postDate <- paste(as.numeric(endTime))
  save(bcone,file = file.path(pathSaveTo, paste(postDate,"_bcone",".RData", sep = "")))
  save(occsBCone,file = file.path(pathSaveTo, paste(postDate,"_occsBCone", ".RData", sep = "")))
  save(envRaster,file = file.path(pathSaveTo, paste(postDate,"_envRaster", ".RData", sep = "")))

} #END if(modelRun) 

#######################################################################################

# Write results and overlap data to CSV files

write.csv(bcone.randomkfold@results, file = file.path(pathResults,"randomkfold_results.csv"))
write.csv(bcone.jackknife@results, file = file.path(pathResults,"jackknife_results.csv"))
write.csv(bcone.checkerboard2@results, file = file.path(pathResults,"checkerboard2_results.csv"))
write.csv(bcone.block@results, file = file.path(pathResults,"block_results.csv"))

write.csv(bcone.randomkfold@overlap, file = file.path(pathResults,"randomkfold_overlap.csv"))
write.csv(bcone.jackknife@overlap, file = file.path(pathResults,"jackknife_overlap.csv"))
write.csv(bcone.checkerboard2@overlap, file = file.path(pathResults,"checkerboard2_overlap.csv"))
write.csv(bcone.block@overlap, file = file.path(pathResults,"block_overlap.csv"))

#######################################################################################

# Write variable importance metrics to CSV files
modelNames <- names(bcone.checkerboard2@predictions) #Same list for all partition methods

randomkfold.importance <- lapply(bcone.randomkfold@models,var.importance)[]
randomkfold.importance <- structure(randomkfold.importance, names = modelNames)
write.csv(randomkfold.importance,file = file.path(pathResults,"randomkfold_importance.csv"))

jackknife.importance <- lapply(bcone.jackknife@models,var.importance)[]
jackknife.importance <- structure(jackknife.importance, names = modelNames)
write.csv(jackknife.importance,file = file.path(pathResults,"jackknife_importance.csv"))

checkerboard2.importance <- lapply(bcone.checkerboard2@models,var.importance)[]
checkerboard2.importance <- structure(checkerboard2.importance, names = modelNames)
write.csv(checkerboard2.importance,file = file.path(pathResults,"checkerboard2_importance.csv"))

block.importance <- lapply(bcone.block@models,var.importance)[]
block.importance <- structure(block.importance, names = modelNames)
write.csv(block.importance,file = file.path(pathResults,"block_importance.csv"))


#######################################################################################

# Plot partition patterns for randomkfold, checkerboard2, and block. (No pattern on jackknife.)

# Plot bcone.checkerboard2 pattern
jpeg(filename = file.path(pathResults, "CHECKERBOARD2_PATTERN.jpg"), width = 720, height = 960, pointsize = 24)
plot(envRaster[[1]], col="gray", legend = FALSE) #Any raster in the stack would work. Just background
points(occsBCone, pch=21, bg=bcone.checkerboard2@occ.grp, col="white", cex=1)
dev.off()

# Plot bcone.block pattern
jpeg(filename = file.path(pathResults, "BLOCK_PATTERN.jpg"), width = 720, height = 960, pointsize = 24)
plot(envRaster[[1]], col="gray", legend = FALSE) #Any raster in the stack would work. Just background
points(occsBCone, pch=21, bg=bcone.block@occ.grp, col="white", cex=1)
dev.off()

# Plot bcone.randomkfold pattern
jpeg(filename = file.path(pathResults, "RANDOMKFOLD_PATTERN.jpg"), width = 720, height = 960, pointsize = 24)
plot(envRaster[[1]], col="gray", legend = FALSE) #Any raster in the stack would work. Just background
points(occsBCone, pch=21, bg=bcone.randomkfold@occ.grp, col="white", cex=1)
dev.off()

# Can add this line of code to each of the above if background points desired. Suggust placement after plot.
#points(bcone.checkerboard2@bg.pts, pch=23, bg=bcone.checkerboard2@bg.grp) 

#######################################################################################

# Plot metrics
resultsMetricName <- c("train.AUC", "avg.test.AUC", "avg.diff.AUC", "avg.test.orMTP", "avg.test.or10pct", "AICc")

for (metricName in resultsMetricName) {
  jpeg(filename = file.path(pathResults, paste("randomkfold_", metricName,".jpg", sep = "")))
  eval.plot(bcone.randomkfold@results, value = metricName, legend = TRUE)
  dev.off()
  
  jpeg(filename = file.path(pathResults, paste("jackknife_", metricName,".jpg", sep = "")))
  eval.plot(bcone.jackknife@results, value = metricName, legend = TRUE)
  dev.off()
  
  jpeg(filename = file.path(pathResults, paste("checkerboard2_", metricName,".jpg", sep = "")))
  eval.plot(bcone.checkerboard2@results, value = metricName, legend = TRUE)
  dev.off()

  jpeg(filename = file.path(pathResults, paste("block_", metricName,".jpg", sep = "")))
  eval.plot(bcone.block@results, value = metricName, legend = TRUE)
  dev.off()
}

#######################################################################################

# Plot individual rasters

for (rasterName in modelNames) {
  jpeg(filename = file.path(pathResults, "Plots", paste("Plot_randomkfold_", rasterName,".jpg", sep = "")))
  plot(bcone.randomkfold@predictions[[rasterName]])
  dev.off()

  jpeg(filename = file.path(pathResults, "Plots", paste("Plot_jackknife_", rasterName,".jpg", sep = "")))
  plot(bcone.jackknife@predictions[[rasterName]])
  dev.off()
  
  jpeg(filename = file.path(pathResults, "Plots", paste("Plot_checkerboard2_", rasterName,".jpg", sep = "")))
  plot(bcone.checkerboard2@predictions[[rasterName]])
  dev.off()
  
  jpeg(filename = file.path(pathResults, "Plots", paste("Plot_block_", rasterName,".jpg", sep = "")))
  plot(bcone.block@predictions[[rasterName]])
  dev.off()
  
}

#######################################################################################
# Export rasters back to ArcGIS Pro

crsProjection <- CRS("+proj=utm +zone=11 +north +datum=NAD83 +units=m")
crs(bcone.randomkfold@predictions) <- crsProjection
crs(bcone.jackknife@predictions) <- crsProjection
crs(bcone.checkerboard2@predictions) <- crsProjection
crs(bcone.block@predictions) <- crsProjection

modelNames <- names(bcone.checkerboard2@predictions) #Same list for all partition methods
for (rasterName in modelNames) {
  
  arc.write(file.path(pathGDB, paste("randomkfold_", dataRun, "_", rasterName, sep = "")), bcone.randomkfold@predictions[[rasterName]])
  arc.write(file.path(pathGDB, paste("jackknife_", dataRun, "_", rasterName, sep = "")), bcone.jackknife@predictions[[rasterName]])
  arc.write(file.path(pathGDB, paste("checkerboard2_", dataRun, "_", rasterName, sep = "")), bcone.checkerboard2@predictions[[rasterName]])
  arc.write(file.path(pathGDB, paste("block_", dataRun, "_", rasterName, sep = "")), bcone.block@predictions[[rasterName]])

}

#######################################################################################
#### END OF CODE ######################################################################
#######################################################################################
#######################################################################################


