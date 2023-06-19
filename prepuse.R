#' Author:
#' Project Title: Discovering Anomalous Spatio-temporal Associations (W9132V-15-C-0004-P00001)
#' Principal Investigator: Vandana Janeja, PhD, UMBC
#' Code Author: Prathamesh Walkikar



# steps to use this application
#' List of Applications which needd to be donwloaded :
#' 1. R 
#' 2. R Studio - an open access GUI to run R
#' 3. SatSCan software - free , easy to access scan -statistic softare to detect spatio-temporal anamolies


#' @description :
#' Step 1: Open R Studio and open installPackages.R , ui.R and server.R from the extracted MDAApp folder
#' Step 2: Firs-time users run installPackages.R script 
#' Step 3: In setting up the workdirectory as your apps directory, we use setwd() command with arguments as
#' the app's folder current location in the system
#' Step 4: we cross check the current work director setting using the getwd() command
#' Step 5: We initialize the shiny library using library(shiny) command and use the runApp() command to launch the app.