#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: amount_proc.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-09-12 16:09:32
#
# Last   modified: 2016-09-12 16:09:37
#
#
#

rm(list = ls())
source('head.R')
load(file.path(dir_data,'amount_ftr.Rda'))
sDir <- file.path(dir_data,'mergePartSvrid')
fileName <- list.files(sDir)
fileNameChar <- substr(fileName,1,1)
uniFile <- unique(fileNameChar)
colNum <- data.frame(ch = uniFile,
                     num = c(0,4,6,24,26,48,48) + 3)

limColNum <- function(matrix,cn){
  matrix[,1:cn]
}

listLen <- list()
listAmt <- list()
for(i in 1:length(uniFile)){
  ch <- uniFile[i]
  if(ch == 'g') next
  curColNum <- colNum$num[colNum$ch == ch]
  
  idxLen <- which(fileNameChar == ch)
  listLen[[i]] <- do.call(rbind,lapply(amount_ftr[idxLen],limColNum,cn = curColNum))
  
  idxAmt <- idxLen + length(fileName)
  listAmt[[i]] <- do.call(rbind,lapply(amount_ftr[idxAmt],limColNum,cn = curColNum))
}
names(listAmt) <- uniFile[1:6]
names(listLen) <- uniFile[1:6]
