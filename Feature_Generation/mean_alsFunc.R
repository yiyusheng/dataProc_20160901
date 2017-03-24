# F1. cut for each attr
cutNum <- function(df,attr,div = 10,ori = T){
  tmp <- df[[attr]]
  tmpCut <- cut(tmp,div,include.lowest = T,dig.lab = 10)
  if(ori == T){
    tmpCut <- as.numeric(gsub('\\[|\\(|,.*','',tmpCut))
  }
  tmpCut
}

# F2. failure rate inherted from ioAFR
calcRate<- function(allData,fData,attr,diskCount = 1){
  t1 <- melt(table(allData[,attr]))
  t2 <- melt(table(fData[,attr]))
  tMerge <- merge(t1,t2,by = 'Var1',all = T)
  names(tMerge) <- c(attr,'count','fCount')
  tMerge$AFR <- tMerge$fCount/tMerge$count/diskCount*100
  tMerge <- subset(tMerge,!is.na(AFR))
}