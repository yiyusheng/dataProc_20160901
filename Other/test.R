#test
rm(list = ls())
source('head.R')

# T1.partSvrid and file in partSvrid
# load(file.path(dir_data,'partIO.Rda'))
# load(file.path(dir_data,'mergePartSvrid','d2.Rda'))
# svr_idD1 <- unique(data$svr_id)
# partD1 <- subset(partSvrid,svr_id %in% svr_idD1)

# T2.extract some failed svr and find feature of iops of their disks.
load(file.path(dir_data,'failRecord_diskParse.Rda'))
load(file.path(dir_data,'partIO.Rda'))
load(file.path(dir_data,'restoreSvrid','d1.Rda'))
failRecordWithDev$svr_id <- factor(failRecordWithDev$svr_id)
partSvrid$svr_id <- factor(partSvrid$svr_id)

# add file name to failure Record
frecord <- failRecordWithDev
frecord$idx <- factor(partSvrid$idx[match(frecord$svr_id,partSvrid$svr_id)])

# plot

iopsPlot <- function(df){
  # fail info
  ddf <- data[[45]];df <- ddf
  curSvr <- as.character(df$svr_id[1])
  fr <- subset(frecord,svr_id == curSvr)
  lenFr <- nrow(fr)
  startID <- c(36810,36834)
  faultID <- matrix(t(paste('a',sapply(fr$faultDiskId,function(x)startID + x - 1),sep='')),
                    ncol = 2,byrow = T)
  faultID <- data.frame(faultID)
  faultID$f_time <- fr$f_time
  faultID$fid <- nrow(faultID):1
  
  # data prepare to plot
  lenDf <- ncol(df)
  
  tmp <- df[,3:lenDf]
  tmp[tmp > 1e5|tmp < 0] <- 0
  df[,3:lenDf] <-tmp
  
  df1 <- subset(df,as.numeric(abs(difftime(time,faultID$f_time[1],units = 'days'))) < 2)
  ggplot(df1,aes(time,a36816)) + geom_line()
  meltDf <- melt(df1[,-1],id.vars = 'time')
  
  # add fail info for meltDf
  meltDf$fid <- 0
  for(i in 1:nrow(faultID)){
    meltDf$fid[meltDf$variable == as.character(faultID$X1[i]) & 
                 meltDf$time < faultID$f_time[i]] <- faultID$fid[i]
    meltDf$fid[meltDf$variable == as.character(faultID$X2[i]) & 
                 meltDf$time < faultID$f_time[i]] <- faultID$fid[i]
  }
  
  meltDfsmp <- smp_df(meltDf,0.5)
  p <- ggplot(meltDfsmp,aes(x = time,y = value,group = variable,
                            color = variable,size = factor(fid))) + 
    geom_line() + ylim(c(0,100))
  ggsave(p,file = file.path(dir_data,'p.jpg'))
}
