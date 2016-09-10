# IO compress. Laura Ye send me on 09/02/2016
rm(list = ls())
source('head.R')

compDir <- file.path(dir_data,'diskio')
dirName <- list.files(compDir)
sigCompDir <- file.path(compDir,dirName)
tarDir <- file.path(dir_data,'compIO')
####################################
comp_dir <- function(dirN){
  fileName <- list.files(file.path(compDir,dirN))
  fileNameWithoutGz <- gsub('\\.gz','',fileName)
  len <- length(fileName)
  cat(sprintf('PROCESSING: [dir: %s;count: %d]\n',dirN,len))
  
  if(len == 1){
    str <- sprintf('zcat %s > %s',
                   file.path(compDir,dirN,fileName),
                   file.path(tarDir,dirN))
    system(str)
  }else if(len > 1){
    # comp
    str <- sprintf('zcat %s > %s',
                  file.path(compDir,dirN,fileName),
                  file.path(tarDir,fileNameWithoutGz))
    sapply(str,system)
    # rename
    str <- sprintf('mv %s %s',
                   file.path(tarDir,fileNameWithoutGz[1]),
                   file.path(tarDir,dirN))
    system(str)
    # cat
    str <- sprintf('cat %s >> %s',
                   file.path(tarDir,fileNameWithoutGz[2]),
                   file.path(tarDir,dirN))
    system(str)
    # rm 
    str <- sprintf('rm %s',
                   file.path(tarDir,fileNameWithoutGz[2]))
    system(str)
  }
}

# doParallel
require(doParallel)
numCore <- floor(detectCores()*0.9)
ck <- makeCluster(min(numCore,length(dirName)),outfile = '')
registerDoParallel(ck)
foreach(i = dirName,.verbose = T) %dopar% comp_dir(i)
stopCluster(ck)
