## ===================================================== ##
# Title:        Calculate Vector Projections ####
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
#     2019.09.13. forked from other SEED lab projects
#                   
# Feature wishlist:  (*: planned but not complete)
#     * Should I scale the projected values to [-1,1]? I don't know if this is important or not.  It shouldn't change any of the results going forward
## ===================================================== ##


## Clean the environment ########## 
rm(list=ls())  

## Required libraries ########## 
#packages
require("readr")
require("tcltk")
require("tidyr")
require("dplyr")

#custom functions
source(file.path(getwd(), "R", "functions", "CalcProjectionMatrix.R"))
source(file.path(getwd(), "R", "functions", "ExtractRVnumsAndNames.R"))


#Read data from files ####
#read the CLEAN probability matrix CSV file
prompt <- "*****Select the CLEAN PROBABILITY MATRIX CSV file*****\n    (The file picker window may have opened in the background.  Check behind this window if you do not see it.)\n"
cat("\n", prompt)
filename <- tcltk::tk_choose.files(caption = prompt,
                                   default = file.path(getwd(),
                                                       "output",
                                                       ""),
                                   filter = matrix(c("CSV", ".csv",
                                                     "RData", ".RData",
                                                     "All files", ".*"),
                                                   3, 2, byrow = TRUE),
                                   multi = FALSE)
#load in the data based on the type of data file provided
if(grepl(x = filename, pattern = "\\.RData$"))
{
  load(file = filename)
}else if(grepl(x = filename, pattern = "\\.(csv|CSV)$"))
{
  probMatrix <- read_csv(file = filename)
}else
{
  message("Invalid Data Filetype.")
  break
}

#read the RANDOM VECTORS data file (from 28_passForwardData.RData)
prompt <- "*****Select the RANDOM VECTORS data file*****\n    (The file picker window may have opened in the background.  Check behind this window if you do not see it.)\n"
cat("\n", prompt, "\n\n")
filename <- tcltk::tk_choose.files(caption = prompt,
                                   default = file.path(getwd(),
                                                       "output",
                                                       "28_passForwardData_RV.RData"),
                                   filter = matrix(c("RData", ".RData",
                                                     "All files", ".*"),
                                                   2, 2, byrow = TRUE),
                                   multi = FALSE)
#load in the data based on the type of data file provided
if(grepl(x = filename, pattern = "\\.RData$"))
{
  load(file = filename)
}else
{
  message("Invalid Data Filetype.")
  break
}


#get the string values for the random vectors 
numsAndNames <- ExtractRVnumsAndNames(RP_names = names(randomVectors))

#split the returned list into two seperate variables.  Convert to matricies for use in the next step
RV_nums <- as.matrix(numsAndNames$nums)
RV_names <- as.matrix(numsAndNames$names)


##Calc Projections (Dot product of user vectors (in probMatrix) with random vectors (in randomVectors)) ####
projection <- CalcProjectionMatrix(probMatrix = probMatrix, 
                                   randomVectors = randomVectors, 
                                   RV_nums = RV_nums)



##| Save data to file ####
#Save projection values
#write a CSV file
write.csv(file = file.path("output", "30_projections.csv"), 
          x = projection)  
#write to a RData file
save(projection, file = file.path("output", "30_projections.RData"),
     precheck = TRUE, compress = TRUE)
