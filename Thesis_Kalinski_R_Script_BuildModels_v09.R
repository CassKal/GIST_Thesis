#######################################################################################
# 
# FILE: Thesis_Kalinski_R_Script_BuildModels_vXX
#
# USC SSCI 594 - Masters Thesis in GIST
# Fall 2018/Spring 2019
# "Impacts of Covariate Scale on MaxEnt Model Outputs: An Example Using Bristlecone Pine Data"
# Author: Cass Kalinski, ckalinsk@usc.edu
# 
#
# Following code builds the tuned and default MaxEnt models for the thesis study
# Some of this code is copied over from the tuning exercise. 
#
# Run time for this module is about 30 minutes for the 800m data, about 34 hours for the 30m data.
#
# Code from the Oliveira Brunno blog was used throughout this module
# https://oliveirabrunno.wordpress.com/2016/12/04/compare-the-performance-of-ecological-niche-models-enms/
#
# Code within this module was copied from or modeled after code provided by 
#   Muscarella, Robert, Peter J. Galante, Mariano Soley-Guardia, Robert A. Boria, Jamie M. Kass, María Uriarte, and Robert P. Anderson. 2014. 
#     "ENMeval: An R package for conducting spatially independent evaluations and estimating optimal model complexity for Maxent ecological niche models."  
#     Methods in Ecology and Evolution 5 (11):1198-1205. doi: 10.1111/2041-210X.12261.
#     R Vignette: https://cran.r-project.org/web/packages/ENMeval/vignettes/ENMeval-vignette.html
#
#######################################################################################


# Setup java footpring for MaxEnt. Needs to be done before any related packages load or variables set
rm(list = ls())
options(java.parameters = "-d64")
options(java.parameters = "-Xmx8g")

#######################################################################################
#
# >>>>>>  SET THESE FLAGS BEFORE DATA RUN  <<<<<<<<

# Flag to run or not run the models. Bypass code to build models, using data saved from prior run instead
modelRun <- TRUE

# Set flag to 0 for unknown, 1 for HP desktop, 2 for HP laptop2, 3 for Dell laptop1
pathFlag <- 0

# Set flag for 30m versus 800m data run
dataRun <- "BOMB" 
#dataRun <- "30m"
#dataRun <- "800m"

# Create parameters for the tuned model. Insert results from the tuning study

if (dataRun == "30m") {
  RMlist <- 2
  FClist <- "LQ"
  TunedMethod <- "jackknife"
} else if (dataRun == "800m") { 
  RMlist <- 2
  FClist <- "LQ"
  TunedMethod <- "jackknife"
} else {
  stop("Data run resolution not set properly. Program terminated.")
}

#######################################################################################

# General environment setup

options(warnings = 1000)

# Default repo
local({r <- getOption("repos")
r["CRAN"] <- "https://cloud.r-project.org" 
options(repos=r)
})


# Install needed packages
if (!requireNamespace("raster", quietly = TRUE))
  install.packages("raster")
if (!requireNamespace("rgeos", quietly = TRUE))
  install.packages("rgeos")
if (!requireNamespace("maptools", quietly = TRUE))
  install.packages("maptools")
if (!requireNamespace("rJava", quietly = TRUE))
  install.packages("rJava")
if (!requireNamespace("dismo", quietly = TRUE))
  install.packages("dismo")
if (!requireNamespace("rgdal", quietly = TRUE))
  install.packages("rgdal")

# Load libraries
library(raster)
library(rgeos)
library(maptools)
library(dismo)
library(rJava)
library(rgdal)


## HOLD THIS. See comments at end of file about r-Bridge issues
# Assumes ArcGIS Pro has been setup with the r-Bridge
#library(arcgisbinding)
#arc.check_product()

#######################################################################################

# Function to retrieve feature classes from default MaxEnt model

getFCs <- function(html) {
  htmlRead <- readLines(html)
  featureTypes <- htmlRead[grep("Feature types", htmlRead)]
  substr(featureTypes, start=21, stop=nchar(featureTypes)-4)
}

# Use these if needed...
#getFCs(file.path(pathMaxEntOutputsDefault,"maxent.html"))
#getFCs(file.path(pathMaxEntOutputsTuned,"maxent.html"))


#######################################################################################

# Create paths for various folders. Data and results all stored on a Dropbox site so that they 
# can be shared between the three different machines used to process the datasets.

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
    # Not GDB on HP Laptop2. Only R processing on that machine.
} else if (pathFlag == 3) {
  # Dell laptop1
    pathPrefix <- file.path("C:","Users","CKaya")
    ModelEnvironment <- "Laptop1"
    pathGDB <- file.path("C:","Users","CKaya","Documents","ArcGIS","Packages","ThesisProject_v3_181201_1800_A1BD7D83-A2D7-4FC8-8CE8-2A6150F3FB57","p20","thesisproject_v3.gdb")
} else {
    stop("Path not set properly. Program terminated")
}

# Set path target per 30m versus 800m environmental data
if (dataRun == "30m") {
    envData <- file.path("ASCII_30m","BConeClimateAdjusted")
    envDataUtah <- file.path("ASCII_30m","Utah")
} else if (dataRun == "800m") {
    envData <- file.path("ASCII_800m","BConeClimateAdjusted")
    envDataUtah <- file.path("ASCII_800m","Utah")
} else {
    stop("Data run resolution not set properly. Program terminated")
}

# Path stub for the presence data
SpeciesInfo <- file.path("SpeciesCSV","BConeLocationUTM.csv")
SpeciesInfoUtah <- file.path("SpeciesCSV","UtahBcone.csv")

# Dropbox path shared by modeling environments
pathDropboxModelData <- file.path("Dropbox","School","_Coursework","SSCI_594ab_MastersThesis","Data","_ModelDataFiles")
pathDropboxSnapshots <- file.path("Dropbox","School","_Coursework","SSCI_594ab_MastersThesis","Data","_R_Snapshots")
pathDropBoxResults <- file.path("Dropbox","School","_Coursework","SSCI_594ab_MastersThesis","Data","_Results")
pathDropBoxMaxEntOutputs <- file.path("Dropbox","School","_Coursework","SSCI_594ab_MastersThesis","Data","_MaxEntOutputs")


# Set final data input and output paths 

# Model inputs
pathSpecies <- file.path(pathPrefix,pathDropboxModelData,SpeciesInfo)
pathSpeciesUtah <- file.path(pathPrefix,pathDropboxModelData,SpeciesInfoUtah)
pathEnvVar <- file.path(pathPrefix,pathDropboxModelData,envData)
pathEnvVarUtah <- file.path(pathPrefix,pathDropboxModelData,envDataUtah)

# Model outputs
pathMaxEntOutputs <- file.path(pathPrefix, pathDropBoxMaxEntOutputs, dataRun)
pathMaxEntOutputsDefault <- file.path(pathPrefix, pathDropBoxMaxEntOutputs, dataRun,"Default")
pathMaxEntOutputsTuned <- file.path(pathPrefix, pathDropBoxMaxEntOutputs, dataRun,"Tuned")
pathMaxEntOutputsRasters <- file.path(pathPrefix, pathDropBoxMaxEntOutputs, dataRun,"Rasters")
pathMaxEntOutputsUtah <- file.path(pathPrefix, pathDropBoxMaxEntOutputs, dataRun,"Utah")


#######################################################################################

# Set projections
crsProjection <- CRS("+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs")
crsProjectionUtah <- CRS("+proj=utm +zone=12 +datum=NAD83 +units=m +no_defs")

#######################################################################################
#
# Load data files if models have already been run before
# Changed. Now the whole environment is saved at the end of the run
# Need to choose which file to reload. 

if (!modelRun ){

  print("Start Load")
  
  # UNCOMMENT AND UPDATE THE FOLLOWING LINE ONLY
  #load(file.path(pathMaxEntOutputs, "<<<RDATA FILE NAME HERE>>>"))
  
  print("Load completed")
}


#######################################################################################
# Data for the California area

# Get the Bristlecone presence data 
bcone <- read.csv(pathSpecies, header = TRUE, sep = ",")
occsBCone <- as.data.frame(bcone[,2:3])

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
# Data for the Utah area

# Get the Bristlecone presence data 
bconeUtah <- read.csv(pathSpeciesUtah, header = TRUE, sep = ",")
occsBConeUtah <- as.data.frame(bconeUtah[,2:3])

# Get the list of environment files for the Bristlecone study 
envFilesUtah <- list.files(path=pathEnvVarUtah, pattern = "*.asc", full.names=TRUE)

# Put the rasters into a RasterStack:
envRasterUtah <- stack(envFilesUtah)

# Rename rasters to match
envFileNamesUtah <- list.files(path=pathEnvVarUtah, pattern = "*.asc", full.names=FALSE)
names(envRasterUtah) <- envFileNamesUtah

# FYI...listing of rasters
print(names(envRasterUtah))


#######################################################################################
# Build MaxEnt models


# If you hit this error on the default MaxEnt model...
#    Loading required namespace: rJava
#    Dec 18, 2018 8:42:47 PM java.util.prefs.WindowsPreferences <init>
#    WARNING: Could not open/create prefs root node Software\JavaSoft\Prefs at root 0x80000002. Windows RegCreateKeyEx(...) returned error code 5.
# Try this fix...https://github.com/julienvollering/MIAmaxent/issues/1

if (modelRun) {
  
  # Start timer (FYI only. Will capture run time of the model iterations.)
  startTime <- Sys.time()
  
  # Lists of metrics to be captured
  metric_AICc_Default <- c()
  metric_AICc_Tuned <- c()
  metric_AUC_Default <- c()
  metric_AUC_Tuned <- c()
  metric_orMTP_Default <-  c()
  metric_or10_Default <-  c()
  metric_orMTP_Tuned <-  c()
  metric_or10_Tuned <-  c()
  metric_NicheOverlap <- c()

  metric_Utah_AICc_Default <- c()
  metric_Utah_AICc_Tuned <- c()
  metric_Utah_AUC_Default <- c()
  metric_Utah_AUC_Tuned <- c()
  metric_Utah_NicheOverlap <- c()
  
  # # Raster stacks NOT USED BECAUSE OF MEMORY ISSUES WITH 30m DATA SIZE
  # stackMaxentDefault <- stack()
  # stackMaxentTuned <- stack()
  # stackUtahDefault <- stack()
  # stackUtahTuned <- stack()
  
  # Set a time stamped folders to be used to hold data outputs
  snapshotTime <- format(Sys.time(), "%Y%m%d_%H%M")
  
  # Create output folders for the iterations
  pathOutputDefault <- file.path(pathMaxEntOutputsDefault,paste(snapshotTime, sep = ""))
  pathOutputDefaultRasters <- file.path(pathOutputDefault, "_RastersRAW")
  pathOutputDefaultRastersUtah <- file.path(pathOutputDefault, "_RastersUtahRAW")
  pathOutputDefaultRastersLOG <- file.path(pathOutputDefault, "_RastersLOG")
  pathOutputDefaultRastersUtahLOG <- file.path(pathOutputDefault, "_RastersUtahLOG")
  
  dir.create(pathOutputDefault, showWarnings = FALSE, recursive = TRUE)
  dir.create(pathOutputDefaultRasters, showWarnings = FALSE, recursive = TRUE)
  dir.create(pathOutputDefaultRastersUtah, showWarnings = TRUE, recursive = TRUE)
  dir.create(pathOutputDefaultRastersLOG, showWarnings = TRUE, recursive = TRUE)
  dir.create(pathOutputDefaultRastersUtahLOG, showWarnings = TRUE, recursive = TRUE)
  
  pathOutputTuned <- file.path(pathMaxEntOutputsTuned,paste(snapshotTime, sep = ""))
  pathOutputTunedRasters <- file.path(pathOutputTuned, "_RastersRAW")
  pathOutputTunedRastersUtah <- file.path(pathOutputTuned, "_RastersUtahRAW")
  pathOutputTunedRastersLOG <- file.path(pathOutputTuned, "_RastersLOG")
  pathOutputTunedRastersUtahLOG <- file.path(pathOutputTuned, "_RastersUtahLOG")
  
  dir.create(pathOutputTuned, showWarnings = FALSE, recursive = TRUE)
  dir.create(pathOutputTunedRasters, showWarnings = FALSE, recursive = TRUE)
  dir.create(pathOutputTunedRastersUtah, showWarnings = TRUE, recursive = TRUE)
  dir.create(pathOutputTunedRastersLOG, showWarnings = TRUE, recursive = TRUE)
  dir.create(pathOutputTunedRastersUtahLOG, showWarnings = TRUE, recursive = TRUE)
  
  pathMaxEntOutputsResults <- file.path(pathMaxEntOutputs, "_DataRuns", snapshotTime)
  dir.create(pathMaxEntOutputsResults, showWarnings = FALSE, recursive = TRUE)
  pathMaxEntOutputsResultsMeanRasters <- file.path(pathMaxEntOutputsResults, "Mean Rasters")
  dir.create(pathMaxEntOutputsResultsMeanRasters, showWarnings = FALSE, recursive = TRUE)
  
  
  # Going to loop 25 times to get averages for metrics
  for (i in 1:25) {
    
    print(paste("Begin loop #",i, Sys.time()))
    print(paste("[Model run start time was ", startTime,"]"))
    
    # Clear structures
    maxentDefault <- NULL
    rasterDefault <- NULL
    rasterDefaultLOG <- NULL
    maxentTuned <- NULL
    rasterTuned <- NULL
    rasterTunedLOG <- NULL
    rasterUtahDefault <- NULL
    rasterUtahDefaultLOG <- NULL
    rasterUtahTuned <- NULL
    rasterUtahTunedLOG <- NULL
    
    #Build set of random background points for each iteration. Will be used for both tuned and default model.
    bgPts <- randomPoints(envRaster, 10000)
    
    #######################################################################################
    # Build default MaxEnt model
    
    # Was getting ERROR2 (see the end of the code list) when Maxent was running in the loop
    # on the desktop machine. Occasionally see it on the laptop1. Appears to be the threads stepping
    # on each other when writing the plots. Putting all MaxEnt outputs into seperate loop folders.
    pathOutputDefaultLoop <- file.path(pathOutputDefault,i)
    dir.create(pathOutputDefaultLoop, showWarnings = FALSE, recursive = TRUE)
    
    # Running with defaults for the model itself. "Args" is for output and threads.
    maxentDefault <- maxent(envRaster, occsBCone,
                            path = pathOutputDefaultLoop,
                            a = bgPts,
                            args = c("-P", #turn on response curves
                                     "-J", #jackknife of variable impacts
                                     "outputformat=raw",
                                     "-x", #turn off output grids
                                     "threads=32")
    )
    
    # Create a prediction raster of the model
    rasterDefault <- predict(maxentDefault, envRaster,
                             args = c("outputformat=raw", "threads=32"),
                             progress = "text"
    )

    crs(rasterDefault) <- crsProjection
    writeRaster(rasterDefault, file.path(pathOutputDefaultRasters, paste(dataRun,"_", i,"_rasterRAW_Default.bil", sep = "")), 
                format="EHdr", 
                overwrite=TRUE, 
                progress="text",
                prj=TRUE)

    # Create a prediction raster of the model with logistics output
    rasterDefaultLOG <- predict(maxentDefault, envRaster,
                                args = c("outputformat=logistic", "threads=32"),
                                progress = "text"
    )
    crs(rasterDefaultLOG) <- crsProjection
    writeRaster(rasterDefaultLOG, file.path(pathOutputDefaultRastersLOG, paste(dataRun,"_", i,"_rasterDefaultLOG.bil", sep = "")), 
                format="EHdr", 
                overwrite=TRUE, 
                progress="text",
                prj=TRUE)
    
    
    # May cause memory issues on 30m run...
    #crs(rasterDefault) <- crsProjection
    #stackMaxentDefault <- stack(stackMaxentDefault, rasterDefault)
    
    #######################################################################################
    #  Build tuned MaxEnt model
    
    pathOutputTunedLoop <- file.path(pathOutputTuned,i)
    dir.create(pathOutputTunedLoop, showWarnings = FALSE, recursive = TRUE)
    
    maxentTuned <- maxent(envRaster, occsBCone,
                          path = pathOutputTunedLoop,
                          a = bgPts,
                          removeDuplicates = FALSE,
                          args = c(
                            "-P", #turn on response curves
                            "-J", #jackknife of variable impacts
                            "outputformat=raw",
                            "randomseed=TRUE",
                            paste("betamultiplier=", RMlist, sep = ""), #RM value for tuned model
                            "-A", #turn off the auto feature selection
                            "-p", #turn off product feature. linear and quadratic TRUE by default
                            "-h", #turn off hinge feature
                            "-x", #turn off output grids
                            "maximumiterations=5000", #Generally stops at ~700
                            "threads=32"
                          )
    )
    # Create a prediction raster of the model
    rasterTuned <- predict(maxentTuned, envRaster,
                           args = c("outputformat=raw", "threads=32"),
                           progress = "text"
    )
    
    crs(rasterTuned) <- crsProjection
    writeRaster(rasterTuned, file.path(pathOutputTunedRasters, paste(dataRun,"_", i,"_rasterRAW_Tuned.bil", sep = "")), 
                format="EHdr", 
                overwrite=TRUE, 
                progress="text",
                prj=TRUE)
    
    # Create a prediction raster of the model with logistics output
    rasterTunedLOG <- predict(maxentTuned, envRaster,
                                args = c("outputformat=logistic", "threads=32"),
                                progress = "text"
    )
    
    crs(rasterTunedLOG) <- crsProjection
    writeRaster(rasterTunedLOG, file.path(pathOutputTunedRastersLOG, paste(dataRun,"_", i,"_rasterTunedLOG.bil", sep = "")), 
                format="EHdr", 
                overwrite=TRUE, 
                progress="text",
                prj=TRUE)
    
    # May cause memory issues on 30m run...
    #crs(rasterDefault) <- crsProjection
    #stackMaxentTuned <- stack(stackMaxentTuned, rasterTuned)

    #######################################################################################
    # Calculate metrics: AICc, AUC, niche overlap. Get orMTP and or10 from MaxEnt output folder. 
    
    metric_AICc_Default <- rbind(metric_AICc_Default, calc.aicc(get.params(maxentDefault), occsBCone, rasterDefault))
    metric_AICc_Tuned <- rbind(metric_AICc_Tuned, calc.aicc(get.params(maxentTuned), occsBCone, rasterTuned))
    
    evalDefault <- evaluate(maxentDefault, p=occsBCone, a=bgPts, x=envRaster)
    metric_AUC_Default <- rbind(metric_AUC_Default, evalDefault@auc)
    
    evalTuned <- evaluate(maxentTuned, p=occsBCone, a=bgPts, x=envRaster)
    metric_AUC_Tuned <- rbind(metric_AUC_Tuned, evalTuned@auc)
    
    metric_NicheOverlap <- rbind(metric_NicheOverlap, nicheOverlap(rasterDefault, rasterTuned,stat = "D"))
    
    maxentResultsCSV <- read.csv(file.path(pathOutputDefaultLoop,"maxentResults.csv"), header = TRUE)
    metric_orMTP_Default <-  rbind(metric_orMTP_Default, maxentResultsCSV$Minimum.training.presence.training.omission)
    metric_or10_Default <-  rbind(metric_or10_Default, maxentResultsCSV$X10.percentile.training.presence.training.omission)
    maxentResultsCSV <- NULL
    
    maxentResultsCSV <- read.csv(file.path(pathOutputTunedLoop,"maxentResults.csv"), header = TRUE)
    metric_orMTP_Tuned <-  rbind(metric_orMTP_Tuned, maxentResultsCSV$Minimum.training.presence.training.omission)
    metric_or10_Tuned <-  rbind(metric_or10_Tuned, maxentResultsCSV$X10.percentile.training.presence.training.omission)
    maxentResultsCSV <- NULL
    
    #######################################################################################
    # Build Utah prediction
    
    # Predict default model to Utah
    rasterUtahDefault <- predict(maxentDefault, envRasterUtah,
                                 args = c("outputformat=raw", "threads=32"),
                                 progress = "text"
    )
    
    crs(rasterUtahDefault) <- crsProjectionUtah
    writeRaster(rasterUtahDefault, file.path(pathOutputDefaultRastersUtah, paste(dataRun,"_", i,"_rasterRAW_Utah_Default.bil", sep = "")), 
                format="EHdr", 
                overwrite=TRUE, 
                progress="text",
                prj=TRUE)
   
    # Create a prediction raster of the model with logistics output
    rasterUtahDefaultLOG <- predict(maxentDefault, envRasterUtah,
                                args = c("outputformat=logistic", "threads=32"),
                                progress = "text"
    )

    crs(rasterUtahDefaultLOG) <- crsProjectionUtah
    writeRaster(rasterUtahDefaultLOG, file.path(pathOutputDefaultRastersUtahLOG, paste(dataRun,"_", i,"_rasterDefaultUtahLOG.bil", sep = "")), 
                format="EHdr", 
                overwrite=TRUE, 
                progress="text",
                prj=TRUE)
    
    #stackUtahDefault <- stack(stackUtahDefault, rasterUtahDefault)
    
    # Predict tuned model to Utah
    rasterUtahTuned <- predict(maxentTuned, envRasterUtah,
                               args = c("outputformat=raw", "threads=32"),
                               progress = "text"
    )
    
    crs(rasterUtahTuned) <- crsProjectionUtah
    writeRaster(rasterUtahTuned, file.path(pathOutputTunedRastersUtah, paste(dataRun,"_", i,"_rasterRAW_Utah_Tuned.bil", sep = "")), 
                format="EHdr", 
                overwrite=TRUE, 
                progress="text",
                prj=TRUE)

    # Create a prediction raster of the model with logistics output
    rasterUtahTunedLOG <- predict(maxentTuned, envRasterUtah,
                                    args = c("outputformat=logistic", "threads=32"),
                                    progress = "text"
    )
    
    crs(rasterUtahTunedLOG) <- crsProjectionUtah
    writeRaster(rasterUtahTunedLOG, file.path(pathOutputTunedRastersUtahLOG, paste(dataRun,"_", i,"_rasterTunedUtahLOG.bil", sep = "")), 
                format="EHdr", 
                overwrite=TRUE, 
                progress="text",
                prj=TRUE)
    
    #stackUtahTuned <- stack(stackUtahTuned, rasterUtahTuned)
    
    #######################################################################################
    
    # Calc Utah AIC metrics
    # Note: Getting NA for default is expected as parameters > presence locations
    metric_Utah_AICc_Default <- rbind(metric_Utah_AICc_Default, calc.aicc(get.params(maxentDefault), occsBConeUtah, rasterUtahDefault))
    metric_Utah_AICc_Tuned <- rbind(metric_Utah_AICc_Tuned, calc.aicc(get.params(maxentTuned), occsBConeUtah, rasterUtahTuned))
    
    # Calc Utah AUC metrics. Used 5k instead of 10k points as the raster is small on the 800m. 
    bgPtsUtah <- randomPoints(envRasterUtah, 5000)
    
    evalDefaultUtah <- evaluate(maxentDefault, p=occsBConeUtah, a=bgPtsUtah, x=envRasterUtah)
    metric_Utah_AUC_Default <- rbind(metric_Utah_AUC_Default,  evalDefaultUtah@auc)
    
    evalTunedUtah <- evaluate(maxentTuned, p=occsBConeUtah, a=bgPtsUtah, x=envRasterUtah)
    metric_Utah_AUC_Tuned <- rbind(metric_Utah_AUC_Tuned, evalTunedUtah@auc)
    
    metric_Utah_NicheOverlap<- rbind(metric_Utah_NicheOverlap, nicheOverlap(rasterUtahDefault, rasterUtahTuned,stat = "D"))
    
    #######################################################################################
    
  } #end for(the25 loops)
  
  print("Loops completed. Consolidated metric calc in progress.")
  print(Sys.time())
  print(paste("[Model run start time was ", startTime,"]"))
  
  
  #######################################################################################
  # Consolidate metrics
  
  # Assign column headings
  colnames(metric_AUC_Default) <- c("AUC Default") 
  colnames(metric_AUC_Tuned)  <- c("AUC Tuned")
  colnames(metric_or10_Default) <-c("or10 Default")
  colnames(metric_or10_Tuned) <- c("or10 Tuned")
  colnames(metric_orMTP_Default)  <- c("orMTP Default")
  colnames(metric_orMTP_Tuned) <- c("orMTP Tuned")
  colnames(metric_NicheOverlap)  <- c("Niche Overlap") 
  colnames(metric_Utah_AUC_Default)  <- c("Utah AUC Default") 
  colnames(metric_Utah_AUC_Tuned)  <- c("Utah AUC Tuned") 
  colnames(metric_Utah_NicheOverlap)  <- c("Utah Niche Overlap") 
  
  # Consolidate metrics and save to file
  metric_Consolidated <- cbind(metric_AICc_Default,
                               metric_AICc_Tuned, 
                               metric_AUC_Default,
                               metric_AUC_Tuned, 
                               metric_orMTP_Default,
                               metric_orMTP_Tuned,
                               metric_or10_Default,
                               metric_or10_Tuned,
                               metric_NicheOverlap,
                               metric_Utah_AUC_Default, 
                               metric_Utah_AUC_Tuned, 
                               metric_Utah_AICc_Default, 
                               metric_Utah_AICc_Tuned,
                               metric_Utah_NicheOverlap)
  
  # Save results of last run to file so that the model does not need to be rerun later.
  #snapshotTime <- format(Sys.time(), "%Y%m%d_%H%M")
  write.csv(metric_Consolidated, file.path(pathMaxEntOutputsResults, paste(snapshotTime,"_", ModelEnvironment,"_ConsolidatedMetrics",".csv",sep = "")))
  save.image(file.path(pathMaxEntOutputsResults, paste(snapshotTime,"_", ModelEnvironment,"_SnapShot", ".RData", sep = "")))
  
  #######################################################################################
  # Delete extraneous XML files from directory
  
  unlink(file.path(pathOutputDefaultRasters,"*.xml"))
  unlink(file.path(pathOutputDefaultRastersUtah,"*.xml"))
  unlink(file.path(pathOutputDefaultRastersLOG,"*.xml"))
  unlink(file.path(pathOutputDefaultRastersUtahLOG,"*.xml"))
  unlink(file.path(pathOutputTunedRasters,"*.xml"))
  unlink(file.path(pathOutputTunedRastersUtah,"*.xml"))
  unlink(file.path(pathOutputTunedRastersLOG,"*.xml"))
  unlink(file.path(pathOutputTunedRastersUtahLOG,"*.xml"))
  
  #######################################################################################
  # Process rasters
  
  print("Processing raster means...")
  print(Sys.time())
  print(paste("[Model run start time was ", startTime,"]"))
  
  
  # Get the list of logistics raster files 
  rasFilesDefaultLOG <- list.files(path=pathOutputDefaultRastersLOG, pattern = ".bil", full.names = TRUE)
  rasFilesDefaultUtahLOG <- list.files(path=pathOutputDefaultRastersUtahLOG, pattern = ".bil", full.names = TRUE)
  rasFilesTunedLOG <- list.files(path=pathOutputTunedRastersLOG, pattern = ".bil", full.names = TRUE)
  rasFilesTunedUtahLOG <- list.files(path=pathOutputTunedRastersUtahLOG, pattern = ".bil", full.names = TRUE)
  
  # Put the rasters into a RasterStack:
  stackDefaultLOG <- stack(rasFilesDefaultLOG)
  stackDefaultUtahLOG <- stack(rasFilesDefaultUtahLOG)
  stackTunedLOG <- stack(rasFilesTunedLOG)
  stackTunedUtahLOG <- stack(rasFilesTunedUtahLOG)
  
  # Calculate mean of raster stack
  rasterDefaultMean <- mean(stackDefaultLOG)
  rasterDefaultUtahMean <- mean(stackDefaultUtahLOG)
  rasterTunedMean <- mean(stackTunedLOG)
  rasterTunedUtahMean <- mean(stackTunedUtahLOG)
  
  # Save mean rasters
  writeRaster(rasterDefaultMean, 
              file.path(pathMaxEntOutputsResultsMeanRasters, paste(snapshotTime,"_", ModelEnvironment, "_rasterDefaultMean.bil", sep = "")), 
              format="EHdr", 
              overwrite=TRUE, 
              progress="text",
              prj=TRUE)
  
  writeRaster(rasterDefaultUtahMean, 
              file.path(pathMaxEntOutputsResultsMeanRasters, paste(snapshotTime,"_", ModelEnvironment, "_rasterDefaultUtahMean.bil", sep = "")), 
              format="EHdr", 
              overwrite=TRUE, 
              progress="text",
              prj=TRUE)
  
  writeRaster(rasterTunedMean, 
              file.path(pathMaxEntOutputsResultsMeanRasters, paste(snapshotTime,"_", ModelEnvironment, "_rasterTunedMean.bil", sep = "")), 
              format="EHdr", 
              overwrite=TRUE, 
              progress="text",
              prj=TRUE)
  
  writeRaster(rasterTunedUtahMean, 
              file.path(pathMaxEntOutputsResultsMeanRasters, paste(snapshotTime,"_", ModelEnvironment, "_rasterTunedUtahMean.bil", sep = "")), 
              format="EHdr", 
              overwrite=TRUE, 
              progress="text",
              prj=TRUE)
  
  #######################################################################################
  
  # Extract values at locations
  occsBcone_DefaultMean <- as.matrix(extract(rasterDefaultMean, occsBCone))
  occsBconeUtah_DefaultMean <- as.matrix(extract(rasterDefaultUtahMean, occsBConeUtah))
  occsBcone_TunedMean <- as.matrix(extract(rasterTunedMean, occsBCone))
  occsBconeUtah_TunedMean <- as.matrix(extract(rasterTunedUtahMean, occsBConeUtah))
  
  names(occsBcone_DefaultMean) <- c("Default Mean")
  names(occsBconeUtah_DefaultMean) <- c("Utah Default Mean")
  names(occsBcone_TunedMean) <- c("Tuned Mean")
  names(occsBconeUtah_TunedMean) <- c("Utah Tuned Mean")
  
  pointValues_CA <- cbind(occsBCone,
                          occsBcone_DefaultMean,
                          occsBcone_TunedMean)
  
  pointValues_UT <-cbind(occsBConeUtah,
                         occsBconeUtah_DefaultMean,
                         occsBconeUtah_TunedMean)
  
  write.csv(pointValues_CA, file.path(pathMaxEntOutputsResults, paste(snapshotTime,"_", ModelEnvironment,"_pointValues_CA",".csv",sep = "")))
  write.csv(pointValues_UT, file.path(pathMaxEntOutputsResults, paste(snapshotTime,"_", ModelEnvironment,"_pointValues_UT",".csv",sep = "")))
  
  
  save.image(file.path(pathMaxEntOutputsResults, paste(snapshotTime,"_", ModelEnvironment,"_SnapShot2", ".RData", sep = "")))
  
  #######################################################################################
  # Simple runtime calc. Info just FYI. Not used in analysis
  endTime <- Sys.time()
  runTime <- difftime(endTime, startTime, units = "mins")
  
  RTloops <- cbind(snapshotTime,
                   dataRun,
                   format(startTime, "%Y-%m-%d_%H:%M"),
                   format(endTime, "%Y-%m-%d_%H:%M"),
                   runTime)
  
  if (file.exists(file.path(pathMaxEntOutputs, "RTLoops.csv"))) {
    write.table(RTloops, file.path(pathMaxEntOutputs, "RTLoops.csv"), 
                append = TRUE, 
                sep = ",", 
                row.names = FALSE,
                col.names = FALSE)
  } else {
    write.table(RTloops, file.path(pathMaxEntOutputs, "RTLoops.csv"), 
                append = TRUE, 
                sep = ",", 
                row.names = FALSE,
                col.names = c("Snapshot Time", "Data Run", "Start", "End", "Minutes"))
  }
  
  
} # END if(modelRun)

print("<<<<<  END OF MODEL RUN >>>>>")  
print(Sys.time())
print(paste("[Model run start time was ", startTime,"]"))



#######################################################################################
#####  CODE ENDS HERE #################################################################
#######################################################################################



#######################################################################################
# SCRATCH SCRATCH area for testing code before moving it to "production" above
#######################################################################################




#######################################################################################
# END  SCRATCH SCRATCH
#######################################################################################

#######################################################################################
#######################################################################################
# Export model rasters to ArcGIS Pro

#  <<<<<<<<<<<<<<<<<<<<PROBLEM>>>>>>>>>>>>>
#  write failing because of spatial reference. Not sure why. Worked in before!!
# Post on GeoNet
# https://community.esri.com/thread/227071-spatial-reference-error-with-arcwrite

## This pattern for both Default and Tuned models
#crs(rasterDefault) <- crsProjection
#rasterNameDefault <- paste(dataRun, "_Default", sep = "")
#arc.write(file.path(pathGDB, rasterNameDefault), 
#          rasterDefault, 
#          overwrite = TRUE)

## Export Utah model rasters to ArcGIS Pro

# crs(rasterUtahDefault) <- crsProjectionUtah
# crs(rasterUtahTuned) <- crsProjectionUtah
#arc.write(file.path(pathGDB, paste("Utah_Default_", dataRun, sep = "")), rasterUtahDefault, overwrite = TRUE)
#arc.write(file.path(pathGDB, paste("Utah_Tuned_", dataRun, sep = "")), rasterUtahTuned, overwrite = TRUE)

#######################################################################################
#######################################################################################
#ERROR2 - from the maxent.log file. RStudio had a fatal crash. Nothing in its log. 
#         Did see a reference to a plot write erro on the screen but could not access
#         the console session as the app was frozen. Directory that I could see was 
#         to the plots folder of the tuning model. Suspect the threads were running
#         into each other, but just a guess.
#######################################################################################
# Error in parallel 1-var response curves: java.util.concurrent.ExecutionException: java.lang.NullPointerException
# java.util.concurrent.ExecutionException: java.lang.NullPointerException
# at java.util.concurrent.FutureTask.report(Unknown Source)
# at java.util.concurrent.FutureTask.get(Unknown Source)
# at density.ParallelRun.runall(ParallelRun.java:56)
# at density.Runner.createProfiles(Runner.java:1298)
# at density.Runner.start(Runner.java:581)
# at mebridge.fit(mebridge.java:27)
# Caused by: java.lang.NullPointerException
# at javax.imageio.ImageIO.write(Unknown Source)
# at density.ResponsePlot.makeplot(ResponsePlot.java:72)
# at density.Runner.createProfiles(Runner.java:1439)
# at density.Runner.oneVarResponseRun(Runner.java:1325)
# at density.Runner$2.run(Runner.java:1289)
# at density.ParallelRun$1.run(ParallelRun.java:44)
# at java.util.concurrent.Executors$RunnableAdapter.call(Unknown Source)
# at java.util.concurrent.FutureTask.run(Unknown Source)
# at java.util.concurrent.ThreadPoolExecutor.runWorker(Unknown Source)
# at java.util.concurrent.ThreadPoolExecutor$Worker.run(Unknown Source)
# at java.lang.Thread.run(Unknown Source)
#######################################################################################
#######################################################################################





