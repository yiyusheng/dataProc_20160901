# svrid partition. Laura Ye send me on 09/02/2016
# Partion svrid.
rm(list = ls())
source('head.R')

load(file.path(dir_data,'partIO.Rda'))
dDir <- file.path(dir_data,'compIO')
fileName <- list.files(dDir)
tDir <- file.path('/mnt/window/partSvrid')
#tDir <- file.path(dir_data,'partSvrid')
####################################
part_svrid <- function(fname){
  dt <- tryCatch(read.table(file.path(dDir,fname),header = F),error = function(e) NULL)
  names(dt) <- c('date','svr_id','attr','timepoint','value')
  if(is.null(dt))return(0)
  
  fileIdx <- data.frame(id = seq_len(nrow(dt)),
                        file = paste(partSvrid$idx[match(dt$svr_id,partSvrid$svr_id)],fname,sep='-'))
  fileIdx <- split(fileIdx$id,fileIdx$file)
  # fileIdx <- fileIdx[1:5]
  
  for (i in 1:length(fileIdx)){
    fn <- names(fileIdx)[i]
    cat(sprintf('PROCESS: %s\tlines:%d\n',fn,length(fileIdx[[i]])))
    write.table(dt[fileIdx[[i]],],file = file.path(tDir,fn),row.names = F,col.names = F)
  }
}

# doParallel
require(doParallel)
ck <- makeCluster(40,outfile = '')
registerDoParallel(ck)
foreach(i = fileName,.verbose = T) %dopar% part_svrid(i)
stopCluster(ck)
