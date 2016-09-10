# Merge svrid partition. Laura Ye send me on 09/02/2016
# Merge svrid partition based on file name.
rm(list = ls())
source('head.R')

load(file.path(dir_data,'partIO.Rda'))
sDir <- file.path(dir_data,'partSvrid')
tDir <- file.path(dir_data,'mergePartSvrid')
# sDir <- file.path('/mnt/window/partSvrid')
# tDir <- file.path('/mnt/window/mergePartSvrid')

fileName <- list.files(sDir)
fileNamePre <- gsub('-.*','',fileName)
fn <- unique(fileNamePre)
fn <- fn[grepl('a|b',fn)]

merge_file <- function(fname){
  fset <- sort(fileName[fname == fileNamePre])
  # create file
  dataset <- do.call(rbind,lapply(fset,function(f){
    cat(sprintf('PROCESS: %s\n',f))
    read.table(file.path(sDir,f),header = F)
  }))
  names(dataset) <- c('date','svr_id','attrid','timepoint','value')
  data <- subset(dataset,!is.na(value))
  cat(sprintf('SAVING: %s\n',f))
  save(data,file = file.path(tDir,paste(fname,'.Rda',sep='')))
  cat(sprintf('SAVED: %s\n',f))
}

# backup
# merge_file <- function(fname){
#   fset <- sort(fileName[fname == fileNamePre])
#   # create file
#   str <- sprintf('touch %s',file.path(tDir,fname))
#   system(str)
#   # cat file
#   a <- sapply(fset,function(x){
#     str <- sprintf('cat %s >> %s',
#                    file.path(sDir,x),
#                    file.path(tDir,fname))
#     system(str)
#   })
# }

# doParallel
require(doParallel)
ck <- makeCluster(40,outfile = '')
registerDoParallel(ck)
foreach(i = fn,.verbose = T) %dopar% merge_file(i)
stopCluster(ck)