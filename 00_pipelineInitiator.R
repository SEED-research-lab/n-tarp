## ===================================================== ##
# Title:        Pipeline Initiator ####
# Project:      n-TARP clustering
#               https://github.com/SEED-research-lab/n-tarp
# 
# Copyright 2017-2019 Taylor Williams
# 
#     Licensed under the Apache License, Version 2.0 (the "License");
#     you may not use this file except in compliance with the License.
#     You may obtain a copy of the License at
#     
#     http://www.apache.org/licenses/LICENSE-2.0
#     
#     Unless required by applicable law or agreed to in writing, software
#     distributed under the License is distributed on an "AS IS" BASIS,
#     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#     See the License for the specific language governing permissions and
#     limitations under the License.
#
#
#
# Authors:      Taylor Williams
# Affiliation:  Purdue University
# 
# Description:  
# 
# Package dependancies: readr, tidyr, stringr, writexl
#
# Changelog:
#     2019.09.19. forked from other SEED lab projects
#                   
# Feature wish list:  (*: planned but not complete)
#                   * have user provide paths to the data files here (once)
#                   * extract the foldername for the course and create an output folder for it
#                   * pass selected data file paths between sourced scripts
#                   * output data in normalized form for use in Tableau/Excel/R
## ===================================================== ##




## _Clean the environment ####
rm(list=ls()) 


######### Internal functions ########## 
#Function: Check to see if the current working directory contains an expected file.  
# If not then prompt user to select the correct directory
WorkingDirectoryCheck <- function(expectedFile) {
  #set directory variables
  curDir <- getwd()
  
  if(file.exists(file.path(curDir, expectedFile))){
    #if file does exists in the current WD, exit the funtion returning TRUE
    return(TRUE)
  } else{
    #if the file does not exist in the current WD, return FALSE
    return(FALSE)
  }
}


######### Setup ########## 

## _Check for correct working directory ########## 
#check for correct expected working directory, inform user if incorrect and stop running script
current.dir <- getwd()
thisFile = "00_pipelineInitiator.R"
expectedFile = file.path(thisFile)

if(!WorkingDirectoryCheck(expectedFile)){
  message("\nThe current working directory is NOT CORRECT.
          It is currently set to '", current.dir, "'
          Please set it to the directory containing the '", thisFile, 
          "' file and rerun this script.\n")
  
  #stop running current script
  break
}


## _External function sourcing ########## 
#(none_))######### Main ########## 

#start a timer to track how long the pipeline takes to execute
start <-  proc.time() #save the time (to report the pipeline's running time at the end of the script)
save("start", file = file.path("output", "00_pipelineStartTime.RData"))

##source (run) the pipeline script files in sequence
# cat("*****Starting: 05_processCourseQuestions.R*****\n\n")
# source(file.path(getwd(), "R", "05_processCourseQuestions.R"))
# cat("*****Complete: 05_processCourseQuestions.R*****\n\n")
# 
# cat("*****Starting: 10_makeProbMatrix.R*****\n\n")
# source(file.path(getwd(), "R", "10_makeProbMatrix.R"))
# cat("*****Complete: 10_makeProbMatrix.R*****\n\n")

# cat("*****Starting: 20_populateSummaryTable.R*****\n\n")
# source(file.path(getwd(), "R", "20_populateSummaryTable.R"))
# cat("*****Complete: 20_populateSummaryTable.R*****\n\n")

cat("*****Starting: 28_generateRandVectors.R*****\n\n")
source(file.path(getwd(), "R", "28_generateRandVectors.R"))
cat("*****Complete: 28_generateRandVectors.R*****\n\n")

cat("*****Starting: 30_calcProjections.R*****\n\n")
source(file.path(getwd(), "R", "30_calcProjections.R"))
cat("*****Complete: 30_calcProjections.R*****\n\n")

cat("*****Starting: 40_findBestThreshold.R*****\n\n")
source(file.path(getwd(), "R", "40_findBestThreshold.R"))
cat("*****Complete: 40_findBestThreshold.R*****\n\n")

# cat("*****Starting: 45_plotProjectionHistograms.R*****\n\n")
# source(file.path(getwd(), "R", "45_plotProjectionHistograms.R"))
# cat("*****Complete: 45_plotProjectionHistograms.R*****\n\n")

cat("*****Starting: 50_seperateGroups.R*****\n\n")
source(file.path(getwd(), "R", "50_seperateGroups.R"))
cat("*****Complete: 50_seperateGroups.R*****\n\n")
# 
# cat("*****Starting: 53_populateSummaryTable.R*****\n\n")
# source(file.path(getwd(), "R", "53_populateSummaryTable.R"))
# cat("*****Complete: 53_populateSummaryTable.R*****\n\n")
# 
# cat("*****Starting: 55_buildNormalizedResultsTable.R*****\n\n")
# source(file.path(getwd(), "R", "55_buildNormalizedResultsTable.R"))
# cat("*****Complete: 55_buildNormalizedResultsTable.R*****\n\n")
# 
# message("\n**** Summary chart created! ****\n")


# 
# #ask user if additional summaries are desired, if so run '53_populateSummaryTable.R' again
# repeat{
#   #beepr::beep(sound = 10)   #notify user to provide input
#   continue <- readline(prompt="Would you like to summarize another probability table from this data? (Y/N): ");
#   
#   #if user selected to create an additional cluster graph
#   if(continue == "y" || continue == "Y"){  
#     source(file.path(getwd(), "R", "53_populateSummaryTable.R"))
#     message("\n**** _____ created! ****\n")
#   }
#   else if(continue == "n" || continue == "N"){  
#     break
#   }
#   else{
#     message("Please enter either 'Y' or 'N'.\n")
#   }
#   
#   #repeat *OR* end if user requested
# }




## Script conclusion
#print the amount of time the script required
load("output/00_pipelineStartTime.RData")
cat("\n\n\nComplete pipeline processing runtime details (in sec):\n")
print(proc.time() - start)


#Indicate pipeline completion
message("\n**** Pipeline complete! ****\n")

#Clear environment variables
rm(list=ls())   

