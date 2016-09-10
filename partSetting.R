# svrid partition setting. Laura Ye send me on 09/02/2016
# Partion svrid based on number of attr of iops and the count of items.
rm(list = ls())
source('head.R')

load(file.path(dir_data,'staIO.Rda'))
days <- length(staIO)
####################################
# S1.merge
lenIOPS <- data.frame(svr_id = 1:22393)
for(i in 1:length(staIO)){
  x <- staIO[[i]]
  lenIOPS[[as.character(x$date[i])]] <- x$len_iops[match(lenIOPS$svr_id,x$svr_id)]
}
lenIOPS$maxLenIOPS <- apply(lenIOPS[,2:41],1,function(x)max(x,na.rm = T))

lenItem <- data.frame(svr_id = 1:22393)
for(i in 1:length(staIO)){
  x <- staIO[[i]]
  lenItem[[as.character(x$date[i])]] <- x$len_data[match(lenItem$svr_id,x$svr_id)]
}
lenItem$maxLenItem <- apply(lenItem[,2:(days + 1)],1,function(x)max(x,na.rm = T))
lenItem$countItemPerDay <- apply(lenItem[,2:(days + 1)],1,function(x)sum(x,na.rm = T))/days

# S2.filter some server
lenSvrid <- merge(lenIOPS[,c('svr_id','maxLenIOPS')],lenItem[,c('svr_id','maxLenItem','countItemPerDay')],by = 'svr_id')
lenSvrid <- subset(lenSvrid,maxLenIOPS != -Inf)
lenSvrid$avgNumAttr <- lenSvrid$maxLenItem/(lenSvrid$maxLenIOPS + 3)

# S3.partition
partA <- list2df(tapply(lenSvrid$countItemPerDay,lenSvrid$maxLenIOPS,function(x){list(mean(x),length(x))}),names = c('numPerDay','count','lenIOPS'))
partA$svrNum <- 2.5e8/partA$numPerDay/365
partA$svrNum <- ceiling(partA$svrNum/10)*10
partA$fileNum <- floor(partA$count/partA$svrNum)
diskNumNeed <- c(0,4,6,24,26,48)
partA <- subset(partA,lenIOPS %in% diskNumNeed)
# number of severs of 0,4,6,24,48 disk drives is set to 1600,500,450,200,120
numTable <- data.frame(lenIOPS = diskNumNeed,
                       num = c(1600,500,450,200,150,120),
                       sign = letters[1:length(diskNumNeed)])

# S4.And Index for sever of 0,4,6,24,26,48 disks
lenSvridA <- subset(lenSvrid,maxLenIOPS %in% diskNumNeed)
lenSvridA <- lenSvridA[order(lenSvridA$maxLenIOPS,lenSvridA$svr_id),]

add_idx <- function(x){
  len <- length(x)
  numMatch <- numTable$num[numTable$lenIOPS == x[1]]
  idx <- ceiling(seq_len(len)/numMatch)
  maxIdx <- max(idx)
  lenMaxIdx <- length(idx[idx == maxIdx])
  
  if(round(lenMaxIdx/numMatch) == 0){
    idx[idx == maxIdx] <- maxIdx - 1
  }
  
  idx <- paste(numTable$sign[numTable$lenIOPS == x[1]],idx,sep='')
}

idx <- unlist(tapply(lenSvridA$maxLenIOPS,lenSvridA$maxLenIOPS,add_idx))
lenSvridA$idx <- idx

# S5.And Index for sever of not 0,4,6,24,26,48 disks
lenSvridB <- subset(lenSvrid,!(maxLenIOPS  %in% c(0,4,6,24,26,48)))
lenSvridB <- lenSvridB[order(lenSvridB$maxLenIOPS,lenSvridB$svr_id),]
idxNum <- table(cut(lenSvridB$maxLenIOPS,c(0,12,18,24,48)))
lenSvridB$idx <- c(rep('g1',idxNum[1]),rep('g2',idxNum[2]),rep('g3',idxNum[3]),rep('g4',idxNum[4]))

# S6.merge
partSvrid <- rbind(lenSvridA,lenSvridB)
save(partSvrid,file = file.path(dir_data,'partIO.Rda'))