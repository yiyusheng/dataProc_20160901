# Read data sample
rm(list = ls())
source('head.R')
# data <- read.table(file.path(dir_data,'IO_14-15','attempt_1470375164689_37607881_r_000000_0.1472268248502'),
#                    header = F)
# names(data) <- c('date','svr_id','attrid','timepoint','value')
# save(data,file = file.path(dir_data,'sample.Rda'))
load(file.path(dir_data,'sample.Rda'))

####################################
# how many items a server have
table.attr <- melt(table(data$attrid))
ggplot(table.attr,aes(x = factor(Var1),y = value)) + geom_bar(stat = 'identity')

# How many servers generate more than one non-zero value for each attribute.
svrNum <- tapply(data$svr_id,data$attr,function(x){length(unique(x))})
svrNum <- melt(svrNum)

# expand data
attrSet <- split(data,data$attrid)
attrSetNew <- lapply(attrSet,function(df){
  attrid <- df$attrid[1]
  date <- df$date[1]
  svrid <- sort(unique(df$svr_id))
  cat(sprintf('Processing attrid [%i] on [%s]. There are %i svrids\n',
              attrid,date,length(svrid)))
  timepoint <- 0:287
  r <- expand.grid(svrid,timepoint,KEEP.OUT.ATTRS = F)
  names(r) <- c('svrid','timepoint')
  r <- merge(r,df[,c('svr_id','timepoint','value')],
             by.x = c('svrid','timepoint'),
             by.y = c('svr_id','timepoint'),
             all.x = T)
  r$value[is.na(r$value)] <- 0
  r
})
save(attrSetNew,file = file.path(dir_data,'attrSample.Rda'))
