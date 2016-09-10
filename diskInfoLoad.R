# Disk Info Load. Laura Ye send me on 09/02/2016
rm(list = ls())
source('head.R')

####################################
data <- read.table(file.path(dir_data,'diskInfo_20160902'),header = F,fill = T,sep='\t')
names(data) <- c('id','use_time','drive','model','svr_id')
data$use_time <- as.POSIXct(data$use_time,tz = 'UTC')
diskInfo0902 <- factorX(data)
save(diskInfo0902,file = file.path(dir_data,'diskInfo0902.Rda'))

