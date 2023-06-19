#' Author:
#' Project Title: Discovering Anomalous Spatio-temporal Associations (W9132V-15-C-0004-P00001)
#' Principal Investigator: Vandana Janeja, PhD, UMBC
#' Code Author: Prathamesh Walkikar


#' @description: Install locally all the packages required for this application

# packages required for running this application locally

pckg <- c("shiny","shinydashboard","leaflet","infotheo","raster","rsatscan","rlist","rgeos","ggmap","dplyr","sqldf","RgoogleMaps","cluster","igraph")


# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# run the function to install packages at once
ipak(pckg)



