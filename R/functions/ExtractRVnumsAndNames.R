## ===================================================== ##
# Title:        Extract Random Vector Numbers and Names ####
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


ExtractRVnumsAndNames <- function(RP_names){ 
  for(i in 1:length(RP_names)) 
  {
    #store name of current random projection
    curRP_name <- RP_names[i]
    
    #store number of current random variable being used
    if(i==1)
    {
      #create the data frames during first iteration
      RV_nums = data.frame("RV" = regmatches(x = curRP_name, regexpr(pattern = "\\d+$", text =  curRP_name)))
      RV_names = data.frame("RV"= paste0("RV_", regmatches(x = curRP_name, 
                                                              regexpr(pattern = "\\d+$", text =  curRP_name))))
    }else
    {
      #add rows during remaining iterations
      RV_nums <- add_row(RV_nums, "RV"= paste0(regmatches(x = curRP_name, 
                                                          regexpr(pattern = "\\d+$", text =  curRP_name))))
      RV_names <- add_row(RV_names, 
                             "RV"= paste0("RV_",regmatches(x = curRP_name, 
                                                           regexpr(pattern = "\\d+$", text =  curRP_name))))
    }

  }
  
  #return both variables in a list (to be extracted after the function call)
    #extract using two lines like the following:
    # var1 <- returnedList$var1
    # var2 <- returnedList$var2
  return(list("nums" = RV_nums, "names" = RV_names))
}