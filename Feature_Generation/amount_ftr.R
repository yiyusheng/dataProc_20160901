#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: amount_ftr.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-09-12 12:46:29
#
# Last   modified: 2016-09-12 13:05:49
#
#
#


rm(list = ls())
source('head.R')

# Prepare
sDir <- file.path(dir_data,'mergePartSvrid')
fileName <- list.files(sDir)
fileNameWithoutRda <- gsub('\\.Rda','',fileName)

fileInfo <- data.frame(id = c('a','b','c','d','e','f'),
                       diskNum = c(0,2,3,12,13,24))
startIOPSAttr <- c(36810,36834)

# Function
amount_func <- function(fn){
    dt <- tryCatch(load(file.path(sDir,fn)),error = function(e) NULL)
    if(is.null(dt))return(0)

    cat(sprintf('PROCESS[amount_ftr]: %s\tcount: %d',fn,nrow(dt)))
    len <- tapply(data$value,list(data$svr_id,data$attrid),length)
    amt <- tapply(data$value,list(data$svr_id,data$attrid),length)
    len[is.na(len)] <- 0
    amt[is.na(amt)] <- 0
    list(len,amt)
}

# doParallel
require(doParallel)
numCore <- floor(detectCores()*0.9)
ck <- makeCluster(min(numCore,length(fileName)),outfile = '')
registerDoParallel(ck)
amount_ftr <- foreach(i = fileName,.combine = rbind,.verbose = T) %dopar% amount_func(i)
save(amount_ftr,file = file.path(dir_data,'amount_ftr.Rda'))
stopCluster(ck)    
