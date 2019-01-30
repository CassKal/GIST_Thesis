#######################################################################################
# 
# FILE Thesis_Kalinski_AppendixD_R_Script_Histograms_vXX.R
# SEE ALSO FILE Thesis_R_Script_POINTS_vXX.R
#
# USC SSCI 594 - Masters Thesis in GIST
# Fall 2018/Spring 2019
# "Impacts of Covariate Scale on MaxEnt Model Outputs: An Example Using Bristlecone Pine Data"
# Author: Cass Kalinski, ckalinsk@usc.edu
# 
#
# Code to read in the point values of the averaged logistics rasters created in the 
# Thesis_R_Script_COMPARE_vXX.R script. This code then calculates the delta between the
# Default and Tuned models, creates a histogram of the data, and then plots it.
#
# Code borrowed heavily from the histogram tutorial 
# http://t-redactyl.io/blog/2016/02/creating-plots-in-r-using-ggplot2-part-7-histograms.html
# http://www.cookbook-r.com/Graphs/Plotting_distributions_(ggplot2)/
#
#
#######################################################################################

# >>>>>>  SET THESE FLAGS BEFORE DATA RUN  <<<<<<<<

# Set flag to 0 for unknown, 1 for HP desktop, 2 for HP laptop2, 3 for Dell laptop1
pathFlag <- 0
dataResolution <- c("30m", "800m")

#######################################################################################


# Default repo
local({r <- getOption("repos")
r["CRAN"] <- "https://cloud.r-project.org" 
options(repos=r)
})


# Install needed packages
if (!requireNamespace("ggplot2", quietly = TRUE))
  install.packages("ggplot2")
if (!requireNamespace("ggthemes", quietly = TRUE))
  install.packages("ggthemes")
if (!requireNamespace("stats", quietly = TRUE))
  install.packages("stats")

library(ggplot2)
library(ggthemes)
library(stats)


#######################################################################################

# Setup paths

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


pathDropbox <- file.path("Dropbox","School","_Coursework","SSCI_594ab_MastersThesis","Data","_MaxEntOutputs")
pathPoints <- file.path(pathPrefix, pathDropbox)


#######################################################################################


for (dataRES in dataResolution) {
  
  # Get point data
  dataFile <- paste("pointValues_CA_", dataRES, ".csv", sep = "")
  dataPts <- read.csv(file.path(pathPoints, dataFile), header = TRUE, sep = ",")
  
  #Calcuate stats
  ptsDeltaMean <- mean(dataPts$Delta, na.rm = TRUE)
  ptsDeltaSD <- sd(dataPts$Delta, na.rm = TRUE)
  ptsDeltaMedian <- median(dataPts$Delta, na.rm = TRUE)
  
  ptsDeltaMin <- min(dataPts$Delta, na.rm = TRUE)
  ptsDeltaMax <- max(dataPts$Delta, na.rm = TRUE)
  ptsDeltaRange <- ptsDeltaMax - ptsDeltaMin
  
  ptsDeltaLowerSD <- ptsDeltaMean - ptsDeltaSD
  ptsDeltaUpperSD <- ptsDeltaMean + ptsDeltaSD
  
  # Plot the histogram 
  ptsHisto <- ggplot(dataPts, aes(x = Delta)) +
    geom_histogram(color="darkgray", fill="white", bins = 10) +
    labs(x= 'Default - Tuned Delta', 
         y = "Count", 
         title = paste("Histogram of ",dataRES, " Logistic Value Deltas", sep = "")) +
    geom_vline(aes(xintercept = ptsDeltaMean, 
                   color = "Mean", 
                   linetype = "Mean"), 
               show.legend = TRUE, 
               size = 1) +
    geom_vline(aes(xintercept = ptsDeltaMedian, 
                   color = "Median", 
                   linetype = "Median"), 
               show.legend = TRUE, 
               size = 1) +
    geom_vline(aes(xintercept = ptsDeltaLowerSD, 
                   color = "StdDev", 
                   linetype = "StdDev"), 
               show.legend = TRUE, 
               size = 1) +
    geom_vline(aes(xintercept = ptsDeltaUpperSD, 
                   color = "StdDev", 
                   linetype = "StdDev"), 
               show.legend = TRUE, 
               size = 1) +
    scale_linetype_manual(name = "Statistics", values = c(Mean = "solid", Median = "dashed", StdDev = "dotted")) +
    scale_color_manual(name = "Statistics", values = c(Mean = "red", Median = "green4", StdDev = "medium blue")) +
    theme_stata(scheme = "s1mono")
  
  # Save histogram to disk
  ggsave(paste("Histogram_", dataRES,".jpg", sep = ""), 
         path = pathPoints, 
         plot = ptsHisto, 
         width = 5, 
         height = 4.4, 
         dpi = 100 )
  
  # Save stats to disk
  ptsStats <- cbind(dataRES,
                    ptsDeltaMean,
                    ptsDeltaMedian,
                    ptsDeltaSD,
                    ptsDeltaLowerSD,
                    ptsDeltaUpperSD,
                    ptsDeltaMax,
                    ptsDeltaMin,
                    ptsDeltaRange,
                    format(Sys.time(), "%Y-%m-%d %H:%M")
  )
  
  if (file.exists(file.path(pathPoints, "Stats_PointDelta.csv"))) {
    write.table(ptsStats, file.path(pathPoints, "Stats_PointDelta.csv"), 
                append = TRUE, 
                sep = ",", 
                row.names = FALSE,
                col.names = FALSE)
  } else {
    write.table(ptsStats, file.path(pathPoints, "Stats_PointDelta.csv"), 
                append = TRUE, 
                sep = ",", 
                row.names = FALSE,
                col.names = c("Resolution", "Mean", "Median", "SD", "Lower SD", "Upper SD", "Max", "Min", "Range", "Timestamp"))
  }
  
                        
}



#######################################################################################
####### END OF CODE ###################################################################
#######################################################################################


#######################################################################################
## SCRATCH CODE - TESTING
#######################################################################################



#######################################################################################
## END OF SCRATCH SECTION
#######################################################################################
