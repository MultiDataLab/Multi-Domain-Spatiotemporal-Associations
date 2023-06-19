#' Author:
#' Project Title: Discovering Anomalous Spatio-temporal Associations (W9132V-15-C-0004-P00001)
#' Principal Investigator: Vandana Janeja, PhD, UMBC
#' Code Author: Prathamesh Walkikar



#' global.R
library(rsatscan)
library(shiny)
library(shinydashboard)

#'@details:
#'refresh all the global program variables on application load

#' @description:
#' satscan seetings for running satscan from r satscan

#create temperory directory
td = tempdir()

#'@decleartion:
#' global variabls to be used in the application

#' pairwise approach variables considering primary and seconday both
domain1Obj <- list()
domain2Obj <- list()
domain1text <- "abc"
domain2text <- "abc"

#' pairwise approach considering primary windows only

domain1ObjPrim <- list()
domain2ObjPrim <- list()
domain1textPrim <- "abc"
domain2textPrim <- "abc"




#' Windows center distance approach variables
dataCenter <- "abc"
windowCenter1 <- "abc"
windowCenter2 <- "abc"
windowCenterObj1 <- list()
windowCenterObj2<- list()

#' map visulization shapefiles from each domains
shapeFile1 <- NA
shapeFile2 <- NA


#' Program Variables
pairwiseconfidence <- 0
pairwisesupport <-  0