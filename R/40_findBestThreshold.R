## ===================================================== ##
# Title:        Find best threshold values for n-TARP method ####
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
# Package dependencies: 
#
# Changelog:
#     2019.09.19. forked from other SEED lab projects
#                   
# Feature wishlist:  (*: planned but not complete)
#     *look for a more efficient/smarter way to identify than through an exhaustive search
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
source(file.path(getwd(), "R", "functions", "ExtractRVnumsAndNames.R"))
source(file.path(getwd(), "R", "functions", "DisplayPercentComplete.R"))
source(file.path(getwd(), "R", "functions", "file-structure-functions.R"))


#start a timer to track how long the script takes to execute
start40 <-  proc.time() #save the time 


# ## Read data from file(s) ####
# #read the PROJECTIONS data file
# prompt <- "*****Select the PROJECTIONS data file*****\n    (The file picker window may have opened in the background.  Check behind this window if you do not see it.)\n"
# cat("\n", prompt, "\n\n")
# filename <- tcltk::tk_choose.files(caption = prompt,
#                                    default = file.path(getwd(),
#                                                        "output",
#                                                        "30_projections.RData"),
#                                    filter = matrix(c("RData", ".RData",
#                                                      "CSV", ".csv",
#                                                      "All files", ".*"),
#                                                    3, 2, byrow = TRUE),
#                                    multi = FALSE)
# #load in the data based on the type of data file provided
# if(grepl(x = filename, pattern = "\\.RData$"))
# {
#   load(file = filename)
# }else if(grepl(x = filename, pattern = "\\.(csv|CSV)$"))
# {
#   projections <- read_csv(file = filename)
# }else
# {
#   message("Invalid Data Filetype.")
#   return
# }




#Read data from files ####
cat("Loading data from files\n")
proc.time() - start40

## Check for pre-defined starting directory and course prefix ####
if(!exists("filenamePrefix")) filenamePrefix <- NULL
if(!exists("dataFolderPath")) dataFolderPath <- NULL
if(!exists("filenameFV")) filenameFV <- NULL


## get data file locations from user ####


# #try to automatically get the other files (ask for them if fails)
#read the PROJECTIONS data file
filenameProj <-
  SelectFile(prompt = "*****Select the PROJECTIONS data file*****\n    (The file picker window may have opened in the background.  Check behind this window if you do not see it.)\n",
             defaultFilename = "30_projections.RData",
             # filenamePrefix = ifelse(exists("filenamePrefix") & !is.null(filenamePrefix),
             #                         yes = filenamePrefix, no = ""),
             fileTypeMatrix = matrix(c("RData", ".RData", "CSV", ".csv", "All files", ".*"),
                                     3, 2, byrow = TRUE),
             dataFolderPath = ifelse(exists("dataFolderPath") & !is.null(dataFolderPath),
                                     yes = dataFolderPath, no = ""))


#load in the data based on the type of data file provided
if(grepl(x = filenameProj, pattern = "\\.RData$")){
  load(file = filenameProj)
}else if(grepl(x = filenameProj, pattern = "\\.(csv|CSV)$")){
  projections <- read_csv(file = filename)
}else{
  message("Invalid Data Filetype.")
  break
}

cat("Loading data complete\n\n")
proc.time() - start40




## Extract string values for the random vectors ####
cat("Extracting string values for the random vectors\n")
proc.time() - start40
# check if already complete and saved
fileExistCheck <- FileExistCheck_workingDir(filename = "40_RVnumsAndNames.RData", subDir = "output")
if(fileExistCheck != FALSE){
  load(fileExistCheck)
}else{
  
  #get the string values for the random vectors 
  numsAndNames <- ExtractRVnumsAndNames(RP_names = names(projection))
  
  #split the returned list into two separate variables.  Convert to matrices for use in the next step
  RV_nums <- as.matrix(numsAndNames$nums)
  RV_names <- as.matrix(numsAndNames$names)
  
  #save String extraction to a RData file
  save(numsAndNames, file = file.path("output", "40_RVnumsAndNames.RData"),
       precheck = TRUE, compress = TRUE)
}
cat("String extraction complete\n\n")
proc.time() - start40


## Sort all of the projection columns ####
cat("Sorting projection columns\n")
# check if the projections have already been sorted and saved
fileExistCheck <- FileExistCheck_workingDir(filename = "40_projectionSort.RData", subDir = "output")
if(fileExistCheck != FALSE){
  load(fileExistCheck)
}else{
  #duplicate the projection data frame to store results of sorted projections
  projectionSort <- projection
  #replace the row names with an ascending integer 
  #   (after sorting, the rows will no longer refer to a single user's projection)
  rownames(projectionSort) <- c(1:nrow(projectionSort))
  
  #sort each column 
  for(i in 1:ncol(projection))
  {
    projectionSort[,i] <- projection[order(projection[i]),i]
    
    
    #| print completion progress to console   ####
    #during first iteration, create progress status variables for main processing loop
    if(i==1){
      iCount <- 0 #loop counter for completion updates
      pct <- 0  #percentage complete tracker
    }
    #print function
    updateVars <- DisplayPercentComplete(projectionSort, iCount, pct, displayText = "Sorting projections: ")
    #update status variables
    iCount <- updateVars$iCount
    pct <- updateVars$pct
    #print update
    cat(updateVars$toPrint)
  }
  
  #save sorted projections to a RData file
  save(projectionSort, file = file.path("output", "40_projectionSort.RData"),
       precheck = TRUE, compress = TRUE)
}
cat("Sorting projection columns complete\n\n")
proc.time() - start40



## Test each projection value as the threshold to find best threshold value (minimizing withinSS, W) ####
cat("Finding best threshold value for each projection (minimizing withinSS, W)")
#create empty data frame to store results of matrix multiplication
minW_RandVec <- data.frame(matrix(nrow = 2, ncol = length(projection)))
#set names
#set column names
for(i in 1:length(minW_RandVec))
{
  if(i==1)
  {
    minW_RandVecColNames <- paste0("RP1D_",RV_nums[i])
  }else
  {
    minW_RandVecColNames[i] <- paste0("RP1D_",RV_nums[i])
  }
}
colnames(minW_RandVec) <- minW_RandVecColNames

# 
# colnames(minW_RandVec) <- paste0("RP1D_",1:length(projection))
# set row names
rownames(minW_RandVec) <- c("Min WithinSS (W)", "Group Threshold")


## loop through all the projections
for(i in 1:ncol(projectionSort))
{
  #build current column name
  curColName <- paste0("RP1D_",RV_nums[i])
  
  #reset the minimimum withinSS value to an abserdly large value
  minW <- 1e10
  
  
  #| print completion progress to console   ####
  #durring first iteration, create progress status variables for main processing loop
  if(i==1){
    iCount <- 0 #loop counter for completion updates
    pct <- 0  #percentage complete tracker
  }
  #print function
  updateVars <- DisplayPercentComplete(projectionSort, iCount, pct, displayText = "Locating best threshold values: ")
  #update status variables
  iCount <- updateVars$iCount
  pct <- updateVars$pct
  #print update
  cat(updateVars$toPrint)
  
  
  ## brute-force search for the global minimum
  for(j in 1:nrow(projectionSort))
  {
    #test each of the projection values (x_j) as the threshold 
    testThresh <- projectionSort[j,i]
    
    #split ith sorted projection into two groups
    #subset of projection values less than the threshold
    group1_ltThresh <- projectionSort[1:(j-1),curColName]
    
    #subset of projection values greater than or equal to the threshold
    group2_gteThresh <- projectionSort[j:nrow(projectionSort),curColName]
    
    #calculate variance for each group and for the whole population of projections 
    varGroup1 <- var(group1_ltThresh)
    varGroup2 <- var(group2_gteThresh)
    varAll    <- var(projectionSort[,curColName])
    
    #calculate the withinSS value  
    curW <- (varGroup1 + varGroup2)/varAll
    
    #see if current withinSS value is less than previous minimum, 
    #  if so, set current value to be the minimum  
    if((curW < minW) & !is.na(curW))
    {
      minW <- curW
      bestThresh <- testThresh
    }
  }
  
  #save the min withinSS value and associated threshold for each projection set 
  minW_RandVec["Min WithinSS (W)", curColName] <- minW
  minW_RandVec["Group Threshold", curColName]  <- bestThresh
  
  #find number of learners in groups 1 and 2
  minW_RandVec["Group 1 Count", curColName] <- sum(projectionSort[,curColName] < as.numeric(bestThresh))
  minW_RandVec["Group 2 Count", curColName] <- sum(projectionSort[,curColName] >= as.numeric(bestThresh))
  
  if (i %% 250==0) {
    #write to a RData file
    save(minW_RandVec, file = file.path("output", paste0("40_minW_and_threshold-",i,".RData")),
         precheck = TRUE, compress = TRUE)
    cat("\nSaving 250 file")
    proc.time() - start40
    
  }
  

}

#transpose and sort the min. withinSS and group threshold data
minW_RandVec_sort <- as.data.frame(t(minW_RandVec))
minW_RandVec_sort <- minW_RandVec_sort[order(minW_RandVec_sort$`Min WithinSS (W)`),]


## |Save min. withinSS and group threshold data to file ####
#write a CSV file
cat("\nSaving CSV file.")
proc.time() - start40

write.csv(file = file.path("output", "40_minW_and_threshold_sorted.csv"), 
          x = minW_RandVec_sort)  
#write to a RData file
save(minW_RandVec_sort, file = file.path("output", "40_minW_and_threshold_sorted.RData"),
     precheck = TRUE, compress = TRUE)


## Identify projections that cluster ####
#set the threshold value for W
clusterWThreshold <- 0.36

#identify the projections that resulted in a W below the threshold
clusterCandidates <- minW_RandVec[1,minW_RandVec["Min WithinSS (W)",] < clusterWThreshold]
sortedCandidates <- clusterCandidates[1,order(clusterCandidates)]


#report percentage of promising projections 
cat("percentage of promising projections:", length(clusterCandidates)/length(minW_RandVec))


#save the names of the cluster candidates
sortedCandidateNames <- names(sortedCandidates)

##|Save data to file ####
#write to a CSV file
cat("\nSaving files.")
write.csv(file = file.path("output", 
                           paste0("40_best_RP_names (W-lt-", clusterWThreshold, ").csv")), 
          x = sortedCandidateNames)  
#write to a RData file
save(sortedCandidateNames, 
     file = file.path("output", 
                      paste0("40_best_RP_names (W-lt-", clusterWThreshold, ").RData")),
     precheck = TRUE, compress = TRUE)

