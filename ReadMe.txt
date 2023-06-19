ReadMe for the endusers of this application
-------------------------------------------
  
What Is This?
-------------
  
This set of documentation is intended to provide working examples of Multi-domain unsual association discover application
features and APIs.  The modules strive to be simple, well documented and modification friendly and exhibit complete 
step by step description of using this application in order to help end-users quickly learn their inner workings 
and application usage.


How To Install The Modules
--------------------------
In order to use this application, there are currently two main pre-requisites which needs to be satisfied:
1.	Users need to install R on the local machine which can be easily done from the CRAN website https://cran.r-project.org/ 
according to the system OS requirements.
2.	In order to detect scan-statistic results, the user needs to locally install SatScan software which is a free tool which 
can be easily downloaded from http://www.satscan.org/download_satscan.html

We are in process to remove the first dependency and would target this in the upcoming versions of the software.

How To Use The Application:
---------------------------

1. After all the pre-requisites have been fullfilled , the user can set the current working directory (using setwd()) of his R to the location of application directory where this file was downloaded. Cross check the working directory by typing the getwd() command.

example : setwd("G:/<folder_name_of_app>")


2. There is an easy-to-use script named installPackages.R which is been provided with this application which has to be run only first time the user installs R
to load all the concerned R packages required for analysis. It will install all the packages if they are not present in the loacl R system.

3. User can then easily run this application now by running the follwing commands:

library(shiny)
runApp()



