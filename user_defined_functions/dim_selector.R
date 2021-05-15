
library(dplyr)

dimension_selector <- function(.phrase){
  
  len = length(strsplit(.phrase," ")[[1]])
  
  dt <- switch(len,dt2,dt3,dt4)
  
  dt_log <- dt[,lapply(.SD, function(x) x %in% .phrase)]
  
  dt_row <- 
  
  
}

dimension_selector("zzzs")

