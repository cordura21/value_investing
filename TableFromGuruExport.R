library(readxl)
library(dplyr)
library(formattable)
options("scipen"=999, "digits"=4)
downlad.dir <- "/Users/andresp/Downloads/"
dataFile.name <- "Financials_GuruFocus_2016-08-5-10-53.xls"

company.name <- names(read_excel(path = file.path(downlad.dir,dataFile.name))[1,1])

xx <- read_excel(path = file.path(downlad.dir,dataFile.name),skip = 20)
names(xx)[1] <- company.name
ttm.col <-  which(names(xx) == 'TTM/current')
xx <- xx[,1:ttm.col]
xx <- cbind(xx[,1],xx[,ttm.col],xx[2:(ttm.col-1)])
formattable(xx)
xx