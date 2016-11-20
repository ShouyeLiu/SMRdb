##---------------------------------------------------------------------------------------
## SMRdb global.R
## Author: Jenny Whitehead - CNSG
## Date Written: February 2016
##
## Updates:
## 2016-05-18 Regional Association plot functionality added - J Whitehead
##
##---------------------------------------------------------------------------------------

# load in required packages

library("shiny")
library("data.table")
library("DT")
library("qqman")

# load the data into a data table

readdat <- data.table(read.csv("Data/SMR_Database.csv",header=TRUE,stringsAsFactors = FALSE))
dat <- readdat[order(Trait, ProbeChr)]
gwaspubdat <- data.table(read.csv("Data/GWASpubdata.csv",header=TRUE,stringsAsFactors = FALSE))
regassocdat <- data.table(read.csv("Data/RegAssocData.csv", header=TRUE,stringsAsFactors = FALSE))




                  