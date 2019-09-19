## ===================================================== ##
# Title:        Create histograms for the selected projections ####
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
#     * figure out the error in showing the figure
## ===================================================== ##


## Clean the environment ########## 
rm(list=ls())  

## Required libraries ########## 
require("readr")
require("tcltk")
require("tidyr")
require("dplyr")
require("ggplot2")


#custom functions
source(file.path(getwd(), "R", "functions", "DisplayPercentComplete.R"))


#Read data from files ####
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


## User input: Generate all histograms or only W<.36? ####
repeat{
  beepr::beep(sound = 10)   #notify user to provide input
  histogramSet <- readline(prompt=paste0("Which set of histograms would you like to generate?
                                     (1) All ", ncol(projection), " projections or
                                     (2) only the ", length(sortedCandidateNames)," projections that cluster?: "))
  
  #set the names of the projections to plot based on user selection
  #if user selected ALL projections
  if(histogramSet == "1" || histogramSet == "(1)"){  
    histogramSetNames <- colnames(projection)
    break
    
    #if the user selected only CLUSTERING projections
  }else if(histogramSet == "2" || histogramSet == "(2)"){ 
    histogramSetNames <- sortedCandidateNames
    break
    
  }else{
    message("Please enter either '1' or '2'.\n")
  }
  
  #repeat *OR* end if user requested
}


##Plot histograms ####
message(paste0("===Plotting ", length(histogramSetNames), " histograms==="))
for(i in 1:length(histogramSetNames))
{
  #set the histogram bin width
  binWidth = 0.2 
  #store name of current random projection
  curRP_name <- histogramSetNames[i]
  #store name of current random variable being used
  curRV_num <- regmatches(x = curRP_name, regexpr(pattern = "\\d+$", text =  curRP_name))
  
  #| build histogram ####
  #create the ggplot2 object
  ggHist <- ggplot(data = projection[curRP_name], aes(projection[curRP_name])) 
  
  #print the histogram bin count above each bar
  ggHist <- ggHist + geom_text(binwidth = binWidth, stat= "bin", vjust=-0.2, aes(label = ..count..))
  
  #create the histogram bars
  #   "aes(fill=..count..)" will color each bar a shade of blue according to its count
  #   "guides(fill=FALSE)" turns off the legend
  ggHist <- ggHist + geom_histogram(binwidth = binWidth, aes(fill=..count..)) +
    guides(fill=FALSE)
  
  #set x-axis values, adjust to match bin boundaries 
  ggHist <- ggHist + scale_x_continuous(breaks = seq(from = binWidth/2, to = 3, by = binWidth)) 
  
  #add and customize lables (e.g., title, axies)
  ggHist <- ggHist + labs(title=paste0("Histogram for Projection ", curRP_name),  
                          subtitle = paste0("Projecting user vectors (n = ", nrow(projection), 
                                            ") onto random vector #", curRV_num, ". W = ", 
                                            sprintf("%.4f", minW_RandVec_sort[curRP_name, "Min WithinSS (W)"])), 
                          x="Projected value", y="Count of learners") 
  
  #set visual theme options (doc: https://www.rdocumentation.org/packages/ggplot2/versions/1.0.1/topics/theme)
  # ggHist <- ggHist + theme(panel.grid.major = element_line(color = "white"))
  
  #| show histogram ####
  ggHist
  
  #| save histogram to file ####
  ggsave(filename = file.path("output", paste0("rank ", i,"__rv", curRV_num, ". bin width of ", binWidth,".png")), 
         width = 7, height = 7)
  
  #| print completion progress to console   ####
  #   if more than 10 histograms will be created
  if (length(histogramSetNames) > 10) {
    #durring first iteration, create progress status variables for main processing loop
    if(i==1)
    {
      iCount <- 0 #loop counter for completion updates
      pct <- 0  #percentage complete tracker
    }
    
    #print function
    updateVars <- DisplayPercentComplete(histogramSetNames, iCount, pct, displayText = "Saving histograms: ")
    
    #update status variables
    iCount <- updateVars$iCount
    pct <- updateVars$pct
    
    #print update
    cat(updateVars$toPrint)
  }
}

