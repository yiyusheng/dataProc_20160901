#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: IOclearFunc.R
#
# Description: functions of clear IO attrid.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-04-14 16:50:53
#
# Last   modified: 2017-04-14 16:50:54
#
#
#

invalid_item_clear <- function(DT,fn){
  col_ps <- names(DT)[grepl('ps',names(DT))]
  
  attrNameLocal <- names(DT)[names(DT) %in% attrName]
  idx_NA <- which(rowSums(is.na(DT[,attrNameLocal])) > 0)
  if(length(idx_NA) != 0)DT <- DT[-idx_NA,]
  
  idx_util <- which(with(DT, util < 0 | util > 110))
  idx_ps <- which(apply(DT[,col_ps],1,function(x)any(x < 0)|any(x > 1e6)))
  idx_remove <- c(idx_util,idx_ps)
  
  cat(sprintf('[%s]invalid_item_clear: %d NA items\t%d util overflow\t%d xps/iops overflow\t\tfn:%s[%d|%d|%.4f]\n',
              date(),length(idx_NA),length(idx_util),length(idx_ps),fn,length(idx_remove),nrow(DT),length(idx_remove)/nrow(DT)))
  if(length(idx_remove) != 0)DT <- DT[-idx_remove,]
  DT$util[DT$util > 100] <- 100
  return(DT)
}

invalid_relation_clear <- function(DT,fn){
  idx_wps <- which(DT$wps == 0)
  idx_util_xps <- with(DT,which(util == 0 & wps + rps > 300))
  idx_iopsw <- which(apply(DT[,grepl('iopsw',names(DT))],1,function(x)all(x == 0))) 
  idx_remove <- c(idx_wps,idx_util_xps|idx_iopsw)
  cat(sprintf('[%s]invalid_relation_clear: %d zero wps\t\t%d bad_util_xps\t%d bad iopsw\tfn:%s[%d|%d|%.4f]\n',
              date(),length(idx_wps),length(idx_util_xps),length(idx_iopsw),fn,length(idx_remove),nrow(DT),length(idx_remove)/nrow(DT)))
  if(length(idx_remove) != 0)DT <- DT[-idx_remove,]
  return(DT)
}

sta_zero_sum <- function(DT,fn){
  attrNameLocal <- names(DT)[names(DT) %in% attrName]
  splitDT <- split(DT,factor(DT$svrid))
  
  sta_zero <- lapplyX(splitDT,function(df){
    lab <- data.frame(t(c(nrow(df),apply(df[,attrNameLocal],2,function(x)sum(x[!is.na(x)] == 0)))))
    names(lab)[1] <- 'count'
    lab
  })
  sta_zero <- cbind(names(splitDT),sta_zero)
  names(sta_zero)[1] <- 'svrid'
  sta_zero$fn <- fn
  
  sta_sum <- lapplyX(splitDT,function(df){
    lab <- c(nrow(df),colSums(df[,attrNameLocal]))
    names(lab)[1] <- 'count'
    lab
  })
  sta_sum <- cbind(names(splitDT),data.frame(sta_sum))
  names(sta_sum)[1] <- 'svrid'
  sta_sum$fn <- fn
  
  row.names(sta_zero) <- NULL;row.names(sta_sum) <- NULL
  return(list(sta_zero,sta_sum))
}

iops_aggragate <- function(df){
  x <- ifelse(sum(grepl('iopsr',names(df))) != 1,df$iopsr <- rowSums(df[,grepl('iopsr',names(df))]),names(df)[grepl('iopsr',names(df))] <- 'iopsr')
  x <- ifelse(sum(grepl('iopsw',names(df))) != 1,df$iopsw <- rowSums(df[,grepl('iopsw',names(df))]),names(df)[grepl('iopsw',names(df))] <- 'iopsw')
  
  df$iops <- rowSums(df[,c('iopsr','iopsw')])
  df$xps <- rowSums(df[,c('rps','wps')])
  
  df[,grepl('iops.*_.*',names(df))] <- NULL
  df
}