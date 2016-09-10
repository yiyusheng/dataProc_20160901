# Statistic for partition. Laura Ye send me on 09/02/2016
# statistic for each svr_id,disk number and item number.
rm(list = ls())
source('head.R')

tarDir <- file.path(dir_data,'compIO')
fileName <- list.files(tarDir)
fileNamesmp <- fileName
# fileNamesmp <- fileName[sample(seq_len(length(fileName)),40)]
####################################
sta_io <- function(dir){
  dt <- tryCatch(read.table(file.path(tarDir,dir),header = F),error = function(e) NULL)
  if(is.null(dt))return(0)
  names(dt) <- c('date','svr_id','attr','timepoint','value')
  cat(sprintf("PROCESS: %s count: %s\n",dir,nrow(dt)))
  
  
  dt$svr_id <- factor(dt$svr_id)
  
  sta <- tapply(dt$attr,dt$svr_id,function(x){
    tmp <- unique(x)
    tmpBytesUnit <- intersect(tmp,c(902,903,999))
    list(length(tmpBytesUnit),length(tmp) - length(tmpBytesUnit),length(x))
  })

  sta <- list2df(sta)
  names(sta) <- c('len_bytesUtil','len_iops','len_data','svr_id')
  sta$date <- dt$date[1]
  sta
}

# doParallel
require(doParallel)
ck <- makeCluster(35,outfile = '')
registerDoParallel(ck)
staIO <- foreach(i = fileNamesmp,.verbose = T) %dopar% sta_io(i)
save(staIO,file = file.path(dir_data,'staIOAllFile.Rda'))
stopCluster(ck)
