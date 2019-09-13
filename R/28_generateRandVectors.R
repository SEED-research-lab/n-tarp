## ===================================================== ##
# Title:        Generate Random Vectors ####
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
#     2019.09.13. forked from other SEED projects
#                   
# Feature wishlist:  (*: planned but not complete)
#     *              
## ===================================================== ##


## Clean the environment ########## 
rm(list=ls())  

## Required libraries ########## 
require("readr")
require("tcltk")
require("tidyr")
require("dplyr")
require("tibble")
require("beepr")

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



##Generate random vectors ####
#set the number of random vectors to generate
numRandVectors <- 1000

## check for saved RNG seeds; ask user which to restore (if any)
# save the available seed filenames
if(interactive()){
  seedSaveFilenames <- list.files(path = "output", pattern = "28_randomSeed.*RData$")
  
  # if saved seeds exist, ask user if they would like to use one
  if (length(seedSaveFilenames) > 0) {
    message(paste0("Number of saved random seeds found: ", length(seedSaveFilenames)))
    
    # ask user which RND seed to restore (if any)
    seedSelection <- menu(choices = c("New random seed", seedSaveFilenames), 
                          title = "What random number generator seed would you like to use?", 
                          graphics = FALSE)
  }
}

# if user selected a saved seed, restore it; else generate and save a new seed
if (exists("seedSelection") && seedSelection > 1 && interactive()) {
  curSeedFilename <- seedSaveFilenames[seedSelection-1]
  
  # load the data into the working environment
  load(file = file.path("output", curSeedFilename))
  
  # restore the random number generator method (must be before assign()!)
  do.call("RNGkind", as.list(oldRNGkind))  
  # restore the seed to the global environment
  assign(".Random.seed", oldSeed, .GlobalEnv)  
  
} else { # create and save new seed
  # initalize a new seed based on the current system time
  set.seed(seed = Sys.time())
  # save the state of the random number generator's seed 
  oldSeed <- .Random.seed  #save the current seed 
  oldRNGkind <- RNGkind()  #save the current random number generator method
  
  # create the output subdirectory if it doesn't already exist
  if(!dir.exists(file.path("output"))) dir.create(file.path("output"))
  # save new seed filename
  curSeedFilename <- paste0("28_randomSeed (", 
                            format(Sys.time(), "%Y.%m.%d %Hh%Mm%Ss"), 
                            ").RData")
  # save new seed to file
  save("oldSeed", "oldRNGkind", 
       file = file.path("output", curSeedFilename),
       compress = TRUE)
}


# find and delete any previous IN USE flag files
inUseFilenames <- list.files(path = "output", 
                             pattern = "RData IS IN USE.txt$", 
                             full.names = TRUE)
file.remove(inUseFilenames)
# save file indicating which seed in currently in use
inUseMsg <- paste0("'", curSeedFilename, "' is the RNG seed most recently used in the pipeline.")
save(inUseMsg, file = file.path("output", paste0(curSeedFilename, " IS IN USE.txt")))


#find the number of dimensions present in the probability matrix (one fewer than the total number of columns)
numDims <- ncol(probMatrix) - 1
#generate numRandVectors vectors with numDims dimensions (values range uniformly from -1:1, generated by runif() function)
randomVectors <- replicate(numRandVectors, 
                           runif(numDims, min = -1, max = 1), 
                           simplify=FALSE)
#convert to a data frame
randomVectors <- as.data.frame(randomVectors)
#set column names
colnames(randomVectors) <- paste0("RV_", 1:length(randomVectors))

##| Save data to file ####
cat("\nSaving files.\n\n")
#Save the random vectors
#write a CSV file
write.csv(file = file.path("output", "28_randomVectors.csv"), 
          x = randomVectors)  
#write to a RData file
save(list = c("randomVectors","numDims"), 
     file = file.path("output", "28_passForwardData_RV.RData"),
     precheck = TRUE, compress = TRUE)

beep()
cat("Done producing random directions for n-TARP \n\n\n")