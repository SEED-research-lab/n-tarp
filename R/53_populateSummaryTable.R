## ===================================================== ##
# Title:        Build table of code/response pair averages for all users ####
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
# Feature wishlist:  (*: planned but not complete)
#     * generalize the "response options" holdover from prior implementation      
## ===================================================== ##



## Clean the environment ########## 
varsToRetain <- c("filenameFV")
rm(list=setdiff(ls(), varsToRetain))


## Required libraries ########## 
require("readr")
require("tcltk")


##Read data from files ####
#read the probability matrix CSV file
# prompt <- "*****Select a CLEAN or GROUP PROBABILITY MATRIX CSV file*****\n    (The file picker window may have opened in the background.  Check behind this window if you do not see it.)\n"
# cat("\n", prompt)
# filename <- tcltk::tk_choose.files(caption = prompt,
#                                    default = file.path(getwd(),
#                                                        "output",
#                                                        "10_probability_matrix_clean.csv"),
#                                    filter = matrix(c("CSV", ".csv",
#                                                      "RData", ".RData",
#                                                      "All files", ".*"),
#                                                    3, 2, byrow = TRUE),
#                                    multi = FALSE)
# # filename <- file.path(getwd(),"output","10_probability_matrix_clean.csv")
# probMatrix <- read_csv(file = filename)  
# #extract the data source's filename without its extension
# dataSourceFilenameSansExt <- tools::file_path_sans_ext(basename(filename))

#read the response options CSV file
# prompt <- "*****Select the RESPONSE OPTIONS CSV file*****\n    (The file picker window may have opened in the background.  Check behind this window if you do not see it.)\n"
# cat("\n", prompt)
# filename <- tcltk::tk_choose.files(caption = prompt, 
#                                    default = file.path(getwd(), "output", "10_response_options.csv"),
#                                    filter = matrix(c("CSV", ".csv", "All files", ".*"),
#                                                    2, 2, byrow = TRUE),
#                                    multi = FALSE)
filename <- file.path(getwd(), "output", "10_response_options.csv")
responseOptions <- read_csv(file = filename)



# Process all probability matrix files in working directory ------------------
# Read filesnames of all clean probability matrix files in the working directory
probMatrixfileNames <- list.files(path = file.path(getwd(), "output"), 
                                  pattern = "^(10_probability_matrix_clean|50_).*csv$",
                                  recursive = FALSE) 
numProbMatrixFiles <- length(probMatrixfileNames) # Counts the number of matching files


# loop through all probability matricies ------------------------------------
for (curFileNum in 1:length(probMatrixfileNames)) {
  
  # Read probability matrix data from file ----------------------------------
  filename <- probMatrixfileNames[curFileNum]
  probMatrix <- read_csv(file = file.path(getwd(), "output", filename))
  #extract the data source's filename without its extension
  dataSourceFilenameSansExt <- tools::file_path_sans_ext(basename(filename))
  
  
  ##Averages table ----------------------------------
  #build empty averages table
  questionCodes <- c("MI", "ME", "RM")
  responses <- c()
  for(i in 1:length(responseOptions$response))
  {
    responses[i] <- as.character(responseOptions$response[i])
  }
  
  avgTable <- data.frame()  
  
  #populate the averages table 
  #loop through the question codes
  for(i in 1:length(questionCodes))  
  {
    #loop through the possible responses 
    #   (Note, this only works when the questions from the outer loop have the same respones)
    for(j in 1:length(responseOptions$response))
    {
      #build the column name to read from the probability matrix for the current response and code pair
      #   (e.g., "P(Strongly disagree|RM)")
      probMatrixCol <- paste0("P(", responseOptions$response[j],"|",questionCodes[i],")")
      
      #store the mean value of all the responses in the avgTable
      #   (Note, for some reason the mean() function is not working, so the mean is calculated by sum/count)
      avgTable[questionCodes[i],
               responseOptions$response[j]] <-  sum(probMatrix[probMatrixCol], na.rm = TRUE)/
        sum(!is.na(probMatrix[probMatrixCol]))
      
    }
  }
  
  
  
  ##Save data to file ####
  cat("\nSaving file.")
  
  #write a CSV file
  write.csv(file = file.path("output", paste0("20_avgTable. using ", 
                                              dataSourceFilenameSansExt, ".CSV")), 
            x = avgTable)  
  #write to RData files
  save(avgTable, file = file.path("output", paste0("20_avgTable. using ", 
                                                   dataSourceFilenameSansExt, ".RData")), 
       precheck = TRUE, compress = TRUE)
}