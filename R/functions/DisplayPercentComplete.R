## ===================================================== ##
# Title:        Print Percent Complete to Console ####
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
#       2017.09.20. extracted from 45_
#       2019.09.13. forked from other SEED lab projects
#                   
# Feature wishlist:  (*: planned but not complete)
#     *              
## ===================================================== ##

DisplayPercentComplete <- function(dataFrame, iCount, pct, displayText = ""){ 
 
  ## Add the following code in the loop where this function is being called:
    #     #| print completion progress to console   ####
    # #durring first iteration, create progress status variables for main processing loop
    # if(i==1)
    # {
    #   iCount <- 0 #loop counter for completion updates
    #   pct <- 0  #percentage complete tracker
    # }
    # 
    # #print function
    # updateVars <- DisplayPercentComplete(dataFrame = [update], iCount, pct, displayText = "[UPDATE with what process is happening]: ")
    # 
    # #update status variables
    # iCount <- updateVars$iCount
    # pct <- updateVars$pct
    # 
    # #print update
    # cat(updateVars$toPrint)
  #########
 
  
  
  #| print completion progress to console   ####
  if(length(dataFrame) > 10)
  {

      iCount <- iCount + 1  
      if(length(dataFrame) < 10 &&
         iCount%%as.integer((length(dataFrame))/1) == 0 && 
         pct <= 100)
      {
        pct <- pct + 1
        toPrint <- paste0("\r", displayText, pct, "% complete")
      }else if(length(dataFrame) < 100 && 
         iCount%%as.integer((length(dataFrame))/10) == 0 && 
         pct <= 100)
      {
        pct <- pct + 10
        toPrint <- paste0("\r", displayText, pct, "% complete")
      }else if(length(dataFrame) >= 100 &&
               iCount%%as.integer((length(dataFrame))/100) == 0 && 
               pct <= 100)
      {
        pct <- pct + 1
        toPrint <- paste0("\r", displayText, pct, "% complete")
      }else
      {
        toPrint <- paste0("\r", displayText)
      }
      
    # }
  }
  
  #return both variables in a list (to be extracted after the function call)
    #extract using two lines like the following:
    # var1 <- returnedList$var1
    # var2 <- returnedList$var2
  return(list("iCount" = iCount,"pct" = pct, "toPrint" = toPrint))
}
  