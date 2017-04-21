rm(list = ls());setwd('/home/yiyusheng/Code/R/Load_Data_2015/');source('~/rhead')
load_save <- function(i){
  fn <- fname[i]
  cat(sprintf('[%s]\t%s SATRT!!!\n',date(),fn))
  load(file.path(dir_dataset,fn))
  cat(sprintf('[%s]\t%s END!!!\n',date(),fn))
  return(sta_zero)
}

dir_dataset <- dir_data15ADC
fname <- list.files(dir_dataset)
idx <- seq_len(length(fname))
r <- foreachX(idx,'load_save')