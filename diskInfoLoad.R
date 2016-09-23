# Disk Info Load. Laura Ye send me on 09/02/2016
rm(list = ls())
source('head.R')

####################################
# S1. disk info
data <- read.table(file.path(dir_data,'files','diskInfo_20160902'),header = F,fill = T,sep='\t')
names(data) <- c('id','use_time','drive','model','svr_id')
data$use_time <- as.POSIXct(data$use_time,tz = 'UTC')
data$svr_id <- factor(data$svr_id)
data$id <- NULL
data <- data[,c('svr_id','use_time','drive','model')]
diskInfo0902 <- factorX(data)

dataTime <- read.table(file.path(dir_data,'files','diskInfo_20160912A'),header = F,fill = T,sep='\t')
names(dataTime) <- c('svr_id','use_time')
dataTime$use_time <- fct2ori(dataTime$use_time)
dataTime$use_time[dataTime$use_time == ''] <- '2016-09-01'
dataTime$use_time <- as.POSIXct(dataTime$use_time,tz = 'UTC',na.rm = T)
dataTime$svr_id <- factor(dataTime$svr_id)

#filter disk onshelf after 2015-07-01
diskInfo0902$use_time_SMART <- diskInfo0902$use_time
diskInfo0902$use_time <- dataTime$use_time[match(diskInfo0902$svr_id,dataTime$svr_id)]
diskInfo0902 <- factorX(subset(diskInfo0902,use_time < as.POSIXct('2015-07-01')))

# S2. svr info
svrInfo0902 <- data.frame(svr_id = levels(diskInfo0902$svr_id),
                          use_time = tapply(diskInfo0902$use_time,diskInfo0902$svr_id,min),
                          count = as.numeric(tapply(diskInfo0902$svr_id,diskInfo0902$svr_id,length)),
                          numModel = as.numeric(tapply(diskInfo0902$model,diskInfo0902$svr_id,
                                                        function(x)length(unique(x)))),
                          model = unlist(tapply(diskInfo0902$model,diskInfo0902$svr_id,
                                                 function(x){paste(unique(x),collapse = '_')})),
                          numDev = as.numeric(tapply(diskInfo0902$drive,diskInfo0902$svr_id,
                                                      function(x)length(unique(x))))
                          )
svrInfo0902$use_time <- as.POSIXct.numeric(as.vector(svrInfo0902$use_time),
                                           tz = 'UTC',origin = '1970-01-01')
svrInfo0902$model <- factor(as.vector(svrInfo0902$model))

 # S3. SAVE
save(diskInfo0902,svrInfo0902,file = file.path(dir_data,'diskInfo0902.Rda'))

