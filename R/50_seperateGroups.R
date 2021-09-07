## ===================================================== ##
# Title:        Separate IDs into cluster groups ####
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
# Input stack:
#     30_projections.RData
#     40_best_RP_names.RData
#     40_minW_and_threshold.RData
#     110v2_stuFeatureVector-*.RData (CLEAN PROBABILITY MATRIX (feature vector))
# 
# Package dependencies: 
#
# Changelog:
#     2019.09.19. forked from other SEED lab projects
#     2021.08.02. commented out the file loading code--a bug with grepl is suspected.  Workaround: manually load the 3 RData files

#                   
# Feature wishlist:  (*: planned but not complete)
#     *need a robust way to select what object in the loaded PROJECTIONS file is 
#       the one that should be renamed as probMatrix
## ===================================================== ##


## Clean the environment ########## 
varsToRetain <- c("filenameFV", "dataFolderPath")
rm(list=setdiff(ls(), varsToRetain))



## Required libraries ########## 
require("readr")
require("tcltk")
require("tidyr")
require("dplyr")
require("tibble")


#custom function(s)
source(file.path(getwd(), "R", "functions", "DisplayPercentComplete.R"))
source(file.path(getwd(), "R", "functions", "file-structure-functions.R"))



#Read data from files ####
## WORKAROUND
probMatrix <- stu_LO_FV #run this and insert the appropriate clean feature vector (aka, probability matrix)
# 
# ## Check for pre-defined starting directory and course prefix ####
# if(!exists("filenamePrefix")) filenamePrefix <- NULL
# if(!exists("dataFolderPath")) dataFolderPath <- NULL
# if(!exists("filenameFV")) filenameFV <- NULL
# 
# 
# ## get data file locations from user ####
# #Locate the CLEAN probability matrix (feature vector) file
# if(!exists("filenameFV")){
#   #read the CLEAN probability matrix (feature vector) file
#   prompt <- "*****Select the CLEAN PROBABILITY MATRIX (feature vector) file*****\n    (The file picker window may have opened in the background.  Check behind this window if you do not see it.)\n"
#   cat("\n", prompt)
#   filenameFV <- tcltk::tk_choose.files(caption = prompt,
#                                        default = file.path(getwd(), "output", ""),
#                                        filter = matrix(c("CSV", ".csv",
#                                                          "RData", ".RData",
#                                                          "All files", ".*"),
#                                                        3, 2, byrow = TRUE),
#                                        multi = FALSE)
# }
# 
# #load in the data based on the type of data file provided
# if(grepl(x = filenameFV, pattern = "\\.RData$")){
#   load(file = filenameFV)
#   probMatrix <- stu_LO_FV
# }else if(grepl(x = filenameFV, pattern = "\\.(csv|CSV)$")){
#   probMatrix <- read_csv(file = filenameFV)
# }else {
#   message("Invalid Data Filetype.")
#   break
# }
# 
# 
# 
# 
# 
# #read the MIN_W and THRESHOLD data file
# filenameMinW <-
#   SelectFile(prompt = "*****Select the MIN_W and THRESHOLD data file*****\n    (The file picker window may have opened in the background.  Check behind this window if you do not see it.)\n",
#              defaultFilename = "40_minW_and_threshold.RData",
#              # filenamePrefix = ifelse(exists("filenamePrefix") & !is.null(filenamePrefix),
#              #                         yes = filenamePrefix, no = ""),
#              fileTypeMatrix = matrix(c("RData", ".RData", "CSV", ".csv", "All files", ".*"),
#                                      3, 2, byrow = TRUE),
#              dataFolderPath = ifelse(exists("dataFolderPath") & !is.null(dataFolderPath),
#                                      yes = dataFolderPath, no = ""))
# 
# 
# #load in the data based on the type of data file provided
# if(grepl(x = filenameMinW, pattern = "\\.RData$")){
#   load(file = filenameMinW)
# }else if(grepl(x = filenameMinW, pattern = "\\.(csv|CSV)$")){
#   minW_RandVec_sort <- read_csv(file = filenameMinW)
# }else{
#   message("Invalid Data Filetype.")
#   return
# }
# 
# 
# 
# #read the BEST RANDOM PROJECTIONS data file
# filenameBestRP <-
#   SelectFile(prompt = "*****Select the BEST RANDOM PROJECTIONS data file*****\n    (The file picker window may have opened in the background.  Check behind this window if you do not see it.)\n",
#              defaultFilename = "40_best_RP_names.RData",
#              # filenamePrefix = ifelse(exists("filenamePrefix") & !is.null(filenamePrefix),
#              #                         yes = filenamePrefix, no = ""),
#              fileTypeMatrix = matrix(c("RData", ".RData", "CSV", ".csv", "All files", ".*"),
#                                      3, 2, byrow = TRUE),
#              dataFolderPath = ifelse(exists("dataFolderPath") & !is.null(dataFolderPath),
#                                      yes = dataFolderPath, no = ""))
# 
# 
# #load in the data based on the type of data file provided
# if(grepl(x = filenameBestRP, pattern = "\\.RData$")){
#   load(file = filenameBestRP)
# }else if(grepl(x = filenameBestRP, pattern = "\\.(csv|CSV)$")){
#   sortedCandidateNames <- read_csv(file = filenameBestRP)
# }else{
#   message("Invalid Data Filetype.")
#   return
# }
# 
# 
# 
# 
# #read the PROJECTIONS data file
# filenameProj <-
#   SelectFile(prompt = "*****Select the PROJECTIONS data file*****\n    (The file picker window may have opened in the background.  Check behind this window if you do not see it.)\n",
#              defaultFilename = "30_projections.RData",
#              # filenamePrefix = ifelse(exists("filenamePrefix") & !is.null(filenamePrefix),
#              #                         yes = filenamePrefix, no = ""),
#              fileTypeMatrix = matrix(c("RData", ".RData", "CSV", ".csv", "All files", ".*"),
#                                      3, 2, byrow = TRUE),
#              dataFolderPath = ifelse(exists("dataFolderPath") & !is.null(dataFolderPath),
#                                      yes = dataFolderPath, no = ""))
# 
# 
# #load in the data based on the type of data file provided
# if(grepl(x = filenameProj, pattern = "\\.RData$")){
#   load(file = filenameProj)
# }else if(grepl(x = filenameProj, pattern = "\\.(csv|CSV)$")){
#   projections <- read_csv(file = filename)
# }else{
#   message("Invalid Data Filetype.")
#   break
# }




## MAIN ------------------------------------

## Extract length(sortedCandidateNames) candidate projections from original projection data (includes orig ID) ####
# Check that there is at least one recommended random vector to use
if(length(sortedCandidateNames) == 0)
{
  #print error message####
  message("-----Script ERROR. No useable random vectors were identified in prior step. Exiting.-----")
  
}else
{
  #loop through all of the candidate projections and add them to a new data frame 
  CandidateProjections <- data.frame(userID = probMatrix$'User ID')
  for(i in 1:length(sortedCandidateNames))
  {
    #add projection values to the data frame
    CandidateProjections <- cbind(CandidateProjections, projection[sortedCandidateNames[i]])
  }
  
  
  ## Seperate and save Group1 and Group2 based on threshold ####
  #identify how many of the cluster candidates to move forward with
  if(interactive()){
    numnTARPgroupsToAnalyze <- readline(prompt=paste0(
      "How many n-TARP cluster criteria to use in clustering? 
      (dafault = 10; max = ", length(sortedCandidateNames), "): "))
    
    #convert input to number (function returns NA for non-number inputs)
    numnTARPgroupsToAnalyze <- as.numeric(numnTARPgroupsToAnalyze)    
    
    #if no or invalid value entered then set to default
    numnTARPgroupsToAnalyze <- ifelse(is.na(numnTARPgroupsToAnalyze), yes = 10, no = numnTARPgroupsToAnalyze) 
    
  }else{
    numnTARPgroupsToAnalyze <- min(3, length(sortedCandidateNames))   #max value could be "length(sortedCandidateNames)"
  }
  
  #for each of the top cluster candidates, find and save the 2 groups' ID lists and their probability matricies
  cluster_assignments <- c()
  for(i in 1:numnTARPgroupsToAnalyze)
  {
    #find column name for current itteration
    curColName <- sortedCandidateNames[i]
    
    #extract the RespondantIDs for each of the 2 groups
    group1IDs <- data.frame(ID = CandidateProjections$userID[CandidateProjections[curColName] < 
                                                                     minW_RandVec_sort[curColName, "Group Threshold"]])
    group2IDs <- data.frame(ID = CandidateProjections$userID[CandidateProjections[curColName] >= 
                                                                     minW_RandVec_sort[curColName, "Group Threshold"]])
    
    
    
    #extract each groups' probability matrix
    group1probMatrix <- merge(probMatrix, group1IDs, by.x="userID", by.y = "ID")
    group2probMatrix <- merge(probMatrix, group2IDs, by.x="userID", by.y = "ID")
    
    
    ##|Save group IDs to file ####
    cat(paste("\n\nSaving cluster group ID files for", curColName))
    #write to CSV files
    write.csv(file = file.path("output", paste0("50_", curColName, 
                                                "_group1IDs.csv")), 
              x = group1IDs)  
    write.csv(file = file.path("output", paste0("50_", curColName, 
                                                "_group2IDs.csv")), 
              x = group2IDs)  
    #write to RData files
    save(list = c("group1IDs", "group2IDs"), file = file.path("output", paste0("50_", curColName, 
                                                             "_group_IDs.RData"),
                                            fsep = "/"), 
         precheck = TRUE, compress = TRUE)

    
    

    ##|Save grouped probability matricies to file ####
    cat(paste("\nSaving probability matrix files for", curColName))
    #write to CSV files
    write.csv(file = file.path("output", paste0("50_", curColName, 
                                                "_group1probMatrix.csv")), 
              x = group1probMatrix)  
    write.csv(file = file.path("output", paste0("50_", curColName, 
                                                "_group2probMatrix.csv")), 
              x = group2probMatrix)  
    #write to RData files
    save(list = c("group1probMatrix" , "group2probMatrix"), 
         file = file.path("output", paste0("50_", curColName, "_group_probMatrices.RData"),
                          fsep = "/"), 
         precheck = TRUE, compress = TRUE)

    
  }
  
  ##Script complete message####
  message("-----Script complete-----")
}#end else
