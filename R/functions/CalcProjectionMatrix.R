## ===================================================== ##
# Title:        Calculate the Projection Matrix ####
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
#       2017.09.20. extracted from 30_
#       2019.09.13. forked from other SEED lab projects
#                   
# Feature wishlist:  (*: planned but not complete)
#     *              
## ===================================================== ##

CalcProjectionMatrix <- function(probMatrix, randomVectors, RV_nums){ 
  
  #custom function(s)
  source(file.path(getwd(), "R", "functions", "DisplayPercentComplete.R"))
  
  #find the number of dimensions for the projection
  numDims <- nrow(randomVectors)
  
  ##Dot product of user vectors (in probMatrix) with random vector (in randomVectors) ####
  #create matrix containing only the probabilities (converted to matrix to do matrix multiplation below) 
  userVector <- as.matrix(probMatrix[,2:(numDims+1)])
  #create empty data frame to store results of matrix multiplication
  projection <- data.frame(matrix(nrow = nrow(userVector), 
                                  ncol = length(randomVectors)))
  #set column names
  for(i in 1:length(randomVectors))
  {
    if(i==1)
    {
      projectionColNames <- paste0("RP1D_",RV_nums[i])
    }else
    {
      projectionColNames[i] <- paste0("RP1D_",RV_nums[i])
    }
  }
  colnames(projection) <- projectionColNames
  
  #set row names to be the users' ID
  rownames(projection) <- probMatrix$RespondentID
  
  # calculate dot products for all users with all random vectors
  for(i in 1:length(randomVectors))
  {
    #build current column name
    curColName <- paste0("RP1D_",RV_nums[i])
    
    #calculate dot product and store into projection data frame (dot 
    #   product returns a single scalar by taking the sum of the 
    #   products of the two vectors)
    projection[curColName] <- userVector %*% as.matrix(randomVectors[[i]])
    
    
    #| print completion progress to console   ####
    #durring first iteration, create progress status variables for main processing loop
    if(i==1)
    {
      iCount <- 0 #loop counter for completion updates
      pct <- 0  #percentage complete tracker
    }
    
    #print function
    updateVars <- DisplayPercentComplete(randomVectors, iCount, pct, displayText = "Calculating projections: ")
    
    #update status variables
    iCount <- updateVars$iCount
    pct <- updateVars$pct
    
    #print update
    cat(updateVars$toPrint)
    
  }
  return(projection)
}