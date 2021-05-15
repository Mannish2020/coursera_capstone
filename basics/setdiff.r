library(data.table)

# dt1 <- readRDS('dat/data/ngrams/long/with_sw/14_nov/dt1')
# dt2 <- readRDS('dat/data/ngrams/long/with_sw/14_nov/dt2')
# dt3 <- readRDS('dat/data/ngrams/long/with_sw/14_nov/dt3')
# dt4 <- readRDS('dat/data/ngrams/long/with_sw/14_nov/dt4')
# dt5 <- readRDS('dat/data/ngrams/long/with_sw/14_nov/dt5')

# Let's try to obtain all rows in which all variables begin with the same
## specific variable 'aa'
library(magrittr)
library(data.table)

DT1 = as.data.table(readRDS('dat/data/ngrams/long/with_sw/14_nov/dt1'))
setnames(DT1, new = c("Predicted","Count"))

# DT2 = as.data.table(readRDS('data/ngrams/long/no_sw/19/dt2'))
DT2 = as.data.table(readRDS('dat/data/ngrams/long/no_sw/14_nov/dt2'))
setnames(DT2, new = c("Predicted","Count"))

# DT3 = as.data.table(readRDS('data/ngrams/long/no_sw/19/dt3'))
DT3 = as.data.table(readRDS('dat/data/ngrams/long/no_sw/14_nov/dt3'))
setnames(DT3, new = c("Predicted","Count"))

# DT4 = as.data.table(readRDS('data/ngrams/long/no_sw/19/dt4'))
DT4 = as.data.table(readRDS('dat/data/ngrams/long/no_sw/14_nov/dt4'))
setnames(DT4, new = c("Predicted","Count"))

DT5 = as.data.table(readRDS('dat/data/ngrams/long/no_sw/14_nov/dt5'))
setnames(DT5, new = c("Predicted","Count"))

dt1 = DT1[,tstrsplit(Predicted,'_')]
dt2 = DT2[,tstrsplit(Predicted,'_')];setkey(dt2)
dt3 = DT3[,tstrsplit(Predicted,'_')];setkey(dt3)
dt4 = DT4[,tstrsplit(Predicted,'_')];setkey(dt4)
dt5 = DT5[,tstrsplit(Predicted,'_')];setkey(dt5)

excess_char_repeat = function(.dimension,.pattern,.lower,.upper=.lower,Logical) {
  
  if(Logical == '|') Logical = 1 else Logical = 2
  
  .str = paste0("'",.pattern,"'")
  
  p1 = paste(sprintf("V%i",.lower:.upper),'%like%',.str,collapse = " | ")
  p2 = paste(sprintf("V%i",.lower:.upper),'%like%',.str,collapse = " & ")
  
  p = switch(Logical,p1,p2)
  
  p = paste0("dt",.dimension,"[",p,"]")
  s2e = str2expression(p)
  eval(s2e)
}

excess_char_repeat(2,"(.)(.)\\\\1{2}",1,2,Logical='&')



excessive_string_length = function(.dimension,Logical){
  
  if(Logical == "|") Logical = 1 else Logical = 2
  
  p1 = paste0("nchar(V",seq(.dimension),") >= 10",collapse = " | ")
  p2 = paste0("nchar(V",seq(.dimension),") >= 10",collapse = " & ")
  
  p = switch(Logical,p1,p2)
  
  # String interpolation might be a good idea here
  .d = sprintf("dt%i",.dimension) 
  s2e = str2expression(paste0(.d,"[",p,"]"))
  eval(s2e)
  
};excessive_string_length(4,"|")


setdiff(dt4,excessive_string_length(4,"|"))
