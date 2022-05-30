## ===================================================== ##
# Title:        Build normalized table of code/response pair averages for user groups ####
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
# Package dependencies: readr, tidyr, stringr, writexl
#
# Changelog:
#     2019.09.19. forked from other SEED lab projects
#                   
# Feature wishlist:  (*: planned but not complete)
#     *
## ===================================================== ##


## Clean the environment ########## 
varsToRetain <- c("filenameFV")
rm(list=setdiff(ls(), varsToRetain))


## Required libraries ########## 
require("readr")
require("tcltk")
require("stringr")
require("writexl")


##Read data from files ####
#read the response options CSV file
prompt <- "*****Select the RESPONSE OPTIONS CSV file*****\n    (The file picker window may have opened in the background.  Check behind this window if you do not see it.)\n"
cat("\n", prompt)
filename <- tcltk::tk_choose.files(caption = prompt,
                                   default = file.path(getwd(), "output", "10_response_options.csv"),
                                   filter = matrix(c("CSV", ".csv", "All files", ".*"),
                                                   2, 2, byrow = TRUE),
                                   multi = FALSE)
# filename <- file.path(getwd(), "output", "10_response_options.csv")
responseOptions <- read_csv(file = filename)


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
# filename <- file.path(getwd(), "output", "40_minW_and_threshold.RData")
# load(file = filename)

## read course name
courseName <- list.files(path = "output", pattern = "^_courseName_")
courseName <- read_csv(file = file.path("output", courseName))[[1,1]]



# hard code question codes ----------------------------------------------
questionCodes <- c("MI", "ME", "RM")
# read possible responses
responses <- c()
for(i in 1:length(responseOptions$response))
{
  responses[i] <- as.character(responseOptions$response[i])
}


# build normalized averages table ----------------------------
# create empty data frame for the normalized data table
normalizedTable <- data.frame(matrix(nrow = 0, ncol = 9))
names(normalizedTable) <- c("RV source",
                            "RV number",
                            "Data applied to",
                            "Group",
                            "W",
                            "Question code",
                            "Response option",
                            "Response mean",
                            "Group n")


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
  
  
  # extract group name and RV number (if appliciable) -----------------------
  if(str_detect(dataSourceFilenameSansExt, "group")){
    #find group number
    groupName <- str_extract(dataSourceFilenameSansExt, "group\\d") # extract group name
    groupName <- str_extract(groupName, ".$")  # only retain group number
    
    #find RV number
    RV_number <- str_extract(dataSourceFilenameSansExt, "RP1D_\\d+") # extract RV name
    RV_number <- str_extract(RV_number, "\\d+$") # only retain RV number
  }else{
    groupName <- "All"  # if 'group' not found then same as all students
    RV_number <- "None" # RV not applicable if working with all students
  }
  
  
  
  # Build normalized group averages table -----------------------------------
  for(i in 1:length(questionCodes))  
  {
    #loop through the possible responses 
    #   (Note, this only works when the questions from the outer loop have the same response)
    for(j in 1:length(responseOptions$response))
    {
      #build the column name to read from the probability matrix for the current response and code pair
      #   (e.g., "P(Strongly disagree|RM)")
      probMatrixCol <- paste0("P(", responseOptions$response[j],"|",questionCodes[i],")")
      
      #calculate the group mean
      #   (Note, for some reason the mean() function is not working, so the mean is calculated by sum/count)
      curMean <- sum(probMatrix[probMatrixCol], na.rm = TRUE)/
        sum(!is.na(probMatrix[probMatrixCol]))
      
      # find W and number of students in the group 
      # pull info from file if currently working with a sub-group of students
      if(str_detect(dataSourceFilenameSansExt, "group")){
        # find current W value and student count in group
        curW <- minW_RandVec_sort[paste0("RP1D_", RV_number), "Min WithinSS (W)"]
        curN <- minW_RandVec_sort[paste0("RP1D_", RV_number), paste0("Group ", groupName, " Count")]
      }else{
        curW <- NA
        curN <- nrow(probMatrix)
      }
      
      # save current values to the table
      nextRowNum <- nrow(normalizedTable) + 1
      
      normalizedTable[nextRowNum,"RV source"] <- courseName
      normalizedTable[nextRowNum,"RV number"] <- RV_number
      normalizedTable[nextRowNum,"Data applied to"] <- "self"
      normalizedTable[nextRowNum,"Group"] <- groupName
      normalizedTable[nextRowNum,"W"] <- curW
      normalizedTable[nextRowNum,"Question code"] <- questionCodes[i]
      normalizedTable[nextRowNum,"Response option"] <- responseOptions$response[j]
      normalizedTable[nextRowNum,"Response mean"] <- curMean
      normalizedTable[nextRowNum,"Group n"] <- curN
      
      
    }
  }
}




##Save data to file ####
message("\nSaving files.\n")

#write to CSV file
write_csv(path = file.path("output", paste0("55_normalizedResultsTable.csv")), 
          x = normalizedTable) 

#write to Excel xlsx file
writexl::write_xlsx(x = normalizedTable, path = file.path("output", paste0("55_normalizedResultsTable.xlsx")))

#write to RData file
save(normalizedTable, file = file.path("output", paste0("55_normalizedResultsTable.RData")),
     precheck = TRUE, compress = TRUE)
