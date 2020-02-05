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
# Package dependancies: 
#
# Changelog:
#     2019.09.19. forked from other SEED lab projects
#                   
# Feature wishlist:  (*: planned but not complete)
#     *need a robust way to select what object in the loaded PROJECTIONS file is 
#       the one that should be renamed as probMatrix
## ===================================================== ##


## Clean the environment ########## 
rm(list=ls())  


## Required libraries ########## 
require("readr")
require("tcltk")
require("tidyr")
require("dplyr")
require("tibble")



## Read data from file(s) ####
#read the CLEAN probability matrix CSV file
prompt <- "*****Select the CLEAN PROBABILITY MATRIX file*****\n    (The file picker window may have opened in the background.  Check behind this window if you do not see it.)\n"
cat("\n", prompt)
filename <- tcltk::tk_choose.files(caption = prompt,
                                   default = file.path(getwd(),
                                                       "output",
                                                       "10_passForwardData_CPM.RData"),
                                   filter = matrix(c("RData", ".RData",
                                                     "CSV", ".csv",
                                                     "All files", ".*"),
                                                   3, 2, byrow = TRUE),
                                   multi = FALSE)
#load in the data based on the type of data file provided
if(grepl(x = filename, pattern = "\\.RData$"))
{
  objs <- load(file = filename, verbose = T)
  probMatrix <- stu_LO_FV
  probMatrix <- rownames_to_column(probMatrix)
  names(probMatrix)[1] <- "userID"
  
}else if(grepl(x = filename, pattern = "\\.(csv|CSV)$"))
{
  probMatrix <- read_csv(file = filename)
  
}else
{
  message("Invalid Data Filetype.")
  break
}


#read the MIN_W and THRESHOLD data file
prompt <- "*****Select the MIN_W and THRESHOLD data file*****\n    (The file picker window may have opened in the background.  Check behind this window if you do not see it.)\n"
cat("\n", prompt, "\n\n")
filename <- tcltk::tk_choose.files(caption = prompt,
                                   default = file.path(getwd(),
                                                       "output",
                                                       "40_minW_and_threshold.RData"),
                                   filter = matrix(c("RData", ".RData",
                                                     "CSV", ".csv",
                                                     "All files", ".*"),
                                                   3, 2, byrow = TRUE),
                                   multi = FALSE)
#load in the data based on the type of data file provided
if(grepl(x = filename, pattern = "\\.RData$"))
{
  load(file = filename)
}else if(grepl(x = filename, pattern = "\\.(csv|CSV)$"))
{
  minW_RandVec_sort <- read_csv(file = filename)
}else
{
  message("Invalid Data Filetype.")
  return
}
# filename <-  file.path(getwd(),"output","40_minW_and_threshold.RData")
# load(file = filename)

#read the BEST RANDOM PROJECTIONS data file
prompt <- "*****Select the BEST RANDOM PROJECTIONS data file*****\n    (The file picker window may have opened in the background.  Check behind this window if you do not see it.)\n"
cat("\n", prompt, "\n\n")
filename <- tcltk::tk_choose.files(caption = prompt,
                                   default = file.path(getwd(),
                                                       "output",
                                                       "40_best_RP_names.RData"),
                                   filter = matrix(c("RData", ".RData",
                                                     "CSV", ".csv",
                                                     "All files", ".*"),
                                                   3, 2, byrow = TRUE),
                                   multi = FALSE)
#load in the data based on the type of data file provided
if(grepl(x = filename, pattern = "\\.RData$"))
{
  load(file = filename)
}else if(grepl(x = filename, pattern = "\\.(csv|CSV)$"))
{
  sortedCandidateNames <- read_csv(file = filename)
}else
{
  message("Invalid Data Filetype.")
  return
}
# filename <- file.path(getwd(),"output","40_best_RP_names.RData")
# load(file = filename)

#read the PROJECTIONS data file
prompt <- "*****Select the PROJECTIONS data file*****\n    (The file picker window may have opened in the background.  Check behind this window if you do not see it.)\n"
cat("\n", prompt, "\n\n")
filename <- tcltk::tk_choose.files(caption = prompt,
                                   default = file.path(getwd(),
                                                       "output",
                                                       "30_projections.RData"),
                                   filter = matrix(c("RData", ".RData",
                                                     "CSV", ".csv",
                                                     "All files", ".*"),
                                                   3, 2, byrow = TRUE),
                                   multi = FALSE)
#load in the data based on the type of data file provided
if(grepl(x = filename, pattern = "\\.RData$"))
{
  load(file = filename)
}else if(grepl(x = filename, pattern = "\\.(csv|CSV)$"))
{
  projections <- read_csv(file = filename)
}else
{
  message("Invalid Data Filetype.")
  return
}
# filename <- file.path(getwd(),"output","30_projections.RData")
# load(file = filename)



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
  CandidateProjections <- data.frame(userID = probMatrix$userID)
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
  
  #for each of the top cluster candidates, find and save the 2 groups' probability matricies
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
    

    ##|Save grouped probability matricies to file ####
    cat("\nSaving files")
    #write to CSV files
    write.csv(file = file.path("output", paste0("50_", curColName, 
                                                "_group1probMatrix.csv")), 
              x = group1probMatrix)  
    write.csv(file = file.path("output", paste0("50_", curColName, 
                                                "_group2probMatrix.csv")), 
              x = group2probMatrix)  
    #write to RData files
    save(group1probMatrix, file = file.path("output", paste0("50_", curColName, 
                                                             "_group1probMatrix.RData"),
                                            fsep = "/"), 
         precheck = TRUE, compress = TRUE)
    save(group2probMatrix, file = file.path("output", paste0("50_", curColName, 
                                                             "_group2probMatrix.RData"),
                                            fsep = "/"), 
         precheck = TRUE, compress = TRUE)
    
  }
  
  ##Script complete message####
  message("-----Script complete-----")
}#end else
