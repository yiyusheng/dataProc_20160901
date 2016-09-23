#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: mean_als.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-09-14 09:13:47
#
# Last   modified: 2016-09-14 09:13:48

rm(list = ls())
source('head.R')
source('mean_alsFunc.R')

#Load feature
load(file.path(dir_data,'mean_ftr.Rda'))
mean_ftr$a3680 <- mean_ftr$a3681 + mean_ftr$a3686
colAttr <- names(mean_ftr)[4:(4+7)]

#Load failure record
load(file.path(dir_data,'failRecord_diskParse.Rda'))
frecord <- rbind(failRecordWithoutDev,failRecordWithDev[,1:4])

#Load svr_id Info
load(file.path(dir_data,'diskInfo0902.Rda'))
load(file.path(dir_data,'partIO.Rda'))

####################################
# S1.clear svr_id + add f_time and use_time + add iopsread and iopswrite
startIdx <- regexpr('[A-Z]{1}.*',mean_ftr$svr_id)
mean_ftr$mon_id <- factor(regmatches(mean_ftr$svr_id,startIdx))
mean_ftr$svr_id <- factor(gsub('[N|F].*','',mean_ftr$svr_id))

mean_ftr$f_time <- frecord$f_time[match(mean_ftr$svr_id,frecord$svr_id)]

mean_ftr$use_time <- svrInfo0902$svr_id[match(mean_ftr$svr_id,svrInfo0902$svr_id)]
mean_ftr$idx <- partSvrid$idx[match(mean_ftr$svr_id,partSvrid$svr_id)]
mean_ftr <- factorX(subset(mean_ftr,svr_id %in% svrInfo0902$svr_id))

mean_ftr$diskNum <- svrInfo0902$count[match(mean_ftr$svr_id,frecord$svr_id)]
mean_ftr <- mean_ftr[,c('svr_id','mon_id','idx','f_time','use_time','maxTime','count',colAttr)]

# S2. cut iops-r iops-w iops-all
mean_ftr$a902cut <- cutNum(mean_ftr,'a902',quantile(mean_ftr$a902,seq(0,1,0.1)))
mean_ftr$a903cut <- cutNum(mean_ftr,'a903',quantile(mean_ftr$a903,seq(0,1,0.1)))
mean_ftr$a999cut <- cutNum(mean_ftr,'a999',quantile(mean_ftr$a999,seq(0,1,0.1)))
mean_ftr$a3681cut <- cutNum(mean_ftr,'a3681',quantile(mean_ftr$a3681,seq(0,1,0.1)))
mean_ftr$a3686cut <- cutNum(mean_ftr,'a3686',quantile(mean_ftr$a3686,seq(0,1,0.1)))
mean_ftr$a3680cut <- cutNum(mean_ftr,'a3680',quantile(mean_ftr$a3680,seq(0,1,0.1)))

# S3. compute failure rate for all cut range
fData <- mean_ftr[grepl('F',mean_ftr$mon_id),]
nData <- mean_ftr[grepl('N',mean_ftr$mon_id),]
expandN <- smp_df(n,20,T)
allData <- rbind(fData,expandN)

frA902 <- calcRate(allData,fData,'a902cut')
frA903 <- calcRate(allData,fData,'a903cut')
frA999 <- calcRate(allData,fData,'a999cut')
frA3681 <- calcRate(allData,fData,'a3681cut')
frA3686 <- calcRate(allData,fData,'a3686cut')
frA3680 <- calcRate(allData,fData,'a3680cut')
# S4. plot

######
# S1.scatter plot 
sca_plot <- function(mean_ftr,attr1,attr2){
  eval(parse(text = sprintf('p <- ggplot(mean_ftr,aes(x = log2(%s), y = log2(%s))) + 
                            geom_point() + stat',attr1,attr2)))
  print(p)
  p
}
mean_ftr$a9023 <- mean_ftr$a902 + mean_ftr$a903
p_read <- sca_plot(smp_df(mean_ftr,0.1),'a902','a3681')
p_write <- sca_plot(smp_df(mean_ftr,0.1),'a903','a3686')
p_amount <- sca_plot(smp_df(mean_ftr,0.1),'a9023','a3680')

# S2.the rate of 3682 and 3681
rate_iopsread_maxiopsread <- mean_ftr[,c('a3681','a3682')]
rate_iopsread_maxiopsread$rate <- rate_iopsread_maxiopsread$a3682/rate_iopsread_maxiopsread$a3681
p_readrate <- ggplot(rate_iopsread_maxiopsread,aes(x = a3681,y = rate)) + geom_point()
print(p_readrate)
# S3.the rate of 3681 and 3685
