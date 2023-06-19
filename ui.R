#' Author
#' Project Title: Discovering Anomalous Spatio-temporal Associations (W9132V-15-C-0004-P00001)
#' Principal Investigator: Vandana Janeja, PhD, UMBC
#' Code Author: Prathamesh Walkikar


## ui.R ##

#'  Pre-requisites:
#' Loads the concerned application libraries when the application is invoked

library(shiny)
library(shinydashboard)
library(leaflet)
library(infotheo)
library(raster)
library(rlist)
library(rsatscan)
library(rgeos)
library(maptools)
library(ggmap)
library(maps)
library(dplyr)
library(igraph)
library(sqldf)
library(geosphere)
library(RgoogleMaps)
library(cluster)

ui <- dashboardPage(
  
  dashboardHeader(title = "Unusual Anomalous Window Associations dashboard", titleWidth = 425),
  dashboardSidebar(width = 275,
                   sidebarMenu(
                     menuItem("Data Explorer",tabName = "dataexplorer",icon = icon("database")),
                     menuItem("Pairwise Influence Approach",tabName = "pairwiseassociations",icon = icon("th")),
                     menuItem("Individual Temporal Analysis",tabName = "spacetime",icon = icon("clock-o")),
                     menuItem("Window Center Influence Approach",tabName = "windowcenters",icon = icon("signal",lib = "glyphicon"))
                     
                   )
  ),
  
  
  #' @description: UI layout of the application dashboad
  #' start of the dashboard body
  
  dashboardBody( 
    tabItems(
      
      tabItem("dataexplorer",
              sidebarPanel(
                
                tags$style(type='text/css', ".well { max-width: 25em; }"),
                tags$style(".well {background-color:#FFFFFF;}"),
                # Tags:
                tags$head(
                  tags$style(type="text/css", "select[multiple] { width: 100%; height:10em}"),
                  tags$style(type="text/css", "select { width: 100%}"),
                  tags$style(type="text/css", "input { width: 19em; max-width:100%}")
                ),
                
                h4(tags$b("Domain 1: ")),
                h6("Upload Case and Geo files in SatScan format for the first domain data to analyze"),
                
                #' @details: Upload the case fils and geo file (.csv files) in the satscan format 
                #' with the case attribute to be the second column in the dataset.
                
                # Upload Case data:
                fileInput("case", "Upload Case-file: 1"),
                # Upload Case data:
                fileInput("geo", "Upload Geo-file: 1"),
                
                
                # Input Date for Domain 1
                
                #Start Date Selection
                dateInput("satscanStartDate","Study Period Start Date for Domain 1:",value = '1997/01/01',format = "yyyy/mm/dd",language = "en"),
                
                #End Date Selection
                dateInput("satscanEndDate","Study Period End Date for Domain 1:",value = '2012/12/31',format = "yyyy/mm/dd",language = "en"),
                
                
                h4(tags$b("Domain 2: ")),  
                h6("Upload Case and Geo files in SatScan format for the second domain data to analyze"),
                # Upload Case data:
                fileInput("case1", "Upload Case-file: 2"),
                # Upload Case data:
                fileInput("geo1", "Upload Geo-file: 2"),
                
                
                # Date for Domain 2 
                
                #Start Date Selection
                dateInput("satscanStartDate1","Study Period Start Date for Domain 2:",value = '1997/01/01',format = "yyyy/mm/dd",language = "en"),
                
                #End Date Selection
                dateInput("satscanEndDate1","Study Period End Date for Domain 2:",value = '2012/12/31',format = "yyyy/mm/dd",language = "en"),
                
                
                #########################################################################################################################
                
                
                
                # Select filetype:
                selectInput("readFunction", "Function to read data:", c(
                  # Base R:
                  "read.csv"
                )),
                
                #' @description : Select the binning strategy to bin the case file
                
                #Selecting the binning startegy
                selectInput("selectBinStrategy","Select Binning Strategy",c("Equal Frequency Binning","Equal Width Binning","Hierarchical Timeseries Clustering")),
                
                #Bin size specification
                
                numericInput(inputId = "n_breaks",
                             label = "Number of bins:",
                             value = 1,
                             min = 1,
                             max = 100
                )
                
                #actionButton("savedata","Save"),
                #                 
                #                 numericInput(inputId = "nbin",
                #                              label = "Bin select",
                #                              value = 1,
                #                              min = 1,
                #                              max = 100
                #                 )
              ),
              
              
              # display of the case file and geo file data
              
              fluidRow(
                box(width =7,title = "Case Files and Geo Files",
                    conditionalPanel("input.n_breaks == 1",{
                      tabsetPanel(
                        
                        tabPanel(" Original Case File for Domain 1", dataTableOutput('caseTable')),
                        tabPanel(" Original Case File for Domain 2", dataTableOutput('caseTable1'))
                        
                      )
                    }),
                    
                    conditionalPanel("input.selectBinStrategy == 'Equal Frequency Binning' && input.n_breaks > 1",{
                      tabsetPanel(
                        tabPanel("Case Data for Domain 1", dataTableOutput('caseTableBin')),
                        tabPanel("Case Data for Domain 2", dataTableOutput('caseTableBinDomain2'))
                        
                      )
                    }),
                    
                    conditionalPanel("input.n_breaks == 1",{
                      tabsetPanel(
                        
                        tabPanel(" Original Geo File for Domain 1", dataTableOutput('geoTable')),
                        tabPanel(" Original Geo File for Domain 2", dataTableOutput('geoTable1'))
                        
                      )   
                      
                    }),
                    
                    conditionalPanel("input.selectBinStrategy == 'Equal Frequency Binning' && input.n_breaks > 1",{
                      tabsetPanel(
                        tabPanel("Geo Data for Domain 1", dataTableOutput('geoTableBin')),
                        tabPanel("Geo Data for Domain 2", dataTableOutput('geoTableBinDomain2'))
                        
                      )
                    }),
                    
                    conditionalPanel("input.selectBinStrategy == 'Equal Width Binning' && input.n_breaks > 1",{
                      tabsetPanel(
                        tabPanel("Case Data for Domain 1", dataTableOutput('caseTableWidthBin')),
                        
                        tabPanel("Case Data for Domain 2", dataTableOutput('caseTableWidthBin2'))
                      )
                    }),
                    
                    conditionalPanel("input.selectBinStrategy == 'Equal Width Binning' && input.n_breaks > 1",{
                      tabsetPanel(
                        tabPanel("Geo File for Domain 1", dataTableOutput('geoTableWidthBin')),
                        tabPanel("Geo File for Domain 2", dataTableOutput('geoTableWidthBin2'))                                 
                        
                      )
                    }),
                    
                    conditionalPanel("input.selectBinStrategy == 'Hierarchical Timeseries Clustering' && input.n_breaks > 1",{
                      tabsetPanel(
                        tabPanel("Case File for Domain 1", dataTableOutput('caseTableHCBin')),
                        tabPanel("Case File for Domain 2", dataTableOutput('caseTableHCBin2'))
                      )
                    }),
                    
                    conditionalPanel("input.selectBinStrategy == 'Hierarchical Timeseries Clustering' && input.n_breaks > 1",{
                      tabsetPanel(
                        tabPanel("Geo File", dataTableOutput('geoTableHCBin')),
                        tabPanel("Geo File", dataTableOutput('geoTableHCBin2'))                                  
                      )
                    })
                    
                    
                )
                
              )
              
              
              
      ),
      
      tabItem("pairwiseassociations",
              
              # Header:
              sidebarPanel(
                
                h4(tags$b("SatScan Control Pannel ")),
                
                h6("Approach : Detection of Single Domain Anamolous Windows using SatScan software"),
                
                #' @description : Reset the satscan parameters on every satscan run by clicking the reset button
                #Reset parameters
                tags$h5(tags$b(" 1. Reset SatScan Parametes")),
                actionButton("reset",label = "Reset Parameters"),
                
                #Selecting the binnig as input
                selectInput("binStrategy","Analysis Output Strategy",c("Equal Frequency Binning","Equal Width Binning","Hierarchical Timeseries Clustering","Full Data Set")),
                
                #selecting input for maximum spatial size option
                selectInput("clusterSize","Use Maximum Spatial Cluster Size Option",c("y","n")),
                
                #Distance From Center Option
                numericInput(inputId = "distanceSelect",
                             label = "Maximum Spatial Cluster Size",
                             value = 1,
                             min = 1,
                             max = 200
                ),
                
                #selecting input for maximum spatial size option
                selectInput(
                  "secondaryCluster","Report Secondary Cluster Options",c("0","1")
                ),
                
                
                
                #Run SatScan
                actionButton("runsatscan1","Analyze Domain 1"),
                actionButton("runsatscan2","Analyze Domain 2"),
                
                #Remove Files from OS Temp Location
                
                h6("Delete Files after analysis"),
                actionButton("resetfiles","Delete Files"),
                
                br(),
                
                # Jaccard Similarity coefficient threshold to select the graph edges to be taken into the consideration
                numericInput(inputId = "jaccardValue",
                             label = "Jaccard Similarity Threshold",
                             value = 0.1,
                             min = 0,
                             max = 1
                ),
                
                # Input for the Influence Decay Factor 
                numericInput(inputId = "Idf",
                             label = "Influence Decay Factor",
                             value = 10,
                             min = 1,
                             max = 100
                ),
                
                #Input threshold for influencne score association rules
                numericInput(inputId = "input_threshold",
                             label = "Input Threshold",
                             value = 0.9,
                             min = 0,
                             max = 500
                ),
                
                # action button to associate the domains
                h6("Associate between the domains"),
                actionButton("associteDomains","Associate"),
                
                h6("Monte-Carlo Simulation"),
                #Input threshold for influencne score association rules
                numericInput(inputId = "no_of_replicas",
                             label = "No of Replicas",
                             value = 10,
                             min = 1,
                             max = 10000
                ),
                
                # action button to associate the domains
                actionButton("simulate","Significance Test")
                
              ),
              
              box( title = "Map Visualization",width = 7,
                   leafletOutput("map")
              ),
              
              ### dispaly the pairwise influence approach results
              fluidRow (
                box(title = "Detected Anomalies (Both Primary & Secondary)",
                    tabsetPanel(
                      tabPanel("All Anomalies for Domain 1", verbatimTextOutput('textDisp')),
                      tabPanel("All Anomalies for Domain 2", verbatimTextOutput('textDisp1'))
                      
                    )
                    
                ),
                
                box(title = "Significant Primary Anamolies",
                    tabsetPanel(
                      tabPanel("Primary Anamoly for Domain 1", verbatimTextOutput('textDisp1Primary')),
                      tabPanel("Primary Anamoly for Domain 2", verbatimTextOutput('textDisp2Primary'))    
                    )
                ),
                ###
                
                box(title = "Associated Influence Relationship Measures",
                    
                    valueBox(verbatimTextOutput("windowAnomalouesScore2"),"Influence Score",color = "light-blue"
                    ),
                    
                    valueBox(verbatimTextOutput("windowAnomalouesScore1"),"Confidence",color = "light-blue"
                    ),
                    
                    valueBox(verbatimTextOutput("windowAnomalouesScore3"),"Support",color = "light-blue"
                    ),
                    
                    valueBox(verbatimTextOutput("WindowLift"),"Lift Measure",color = "light-blue"
                    ),
                    
                    valueBox(verbatimTextOutput("pvalue"),"Monte Carlo Simulation",color = "green"
                    )
                )
                
                
              )
              ####
              
              
      ),
      
      ### dispaly the window centers influence approach results
      tabItem("windowcenters", 
              
              box( title = " Associated Window Centers",
                   
                   tabsetPanel(
                     tabPanel("Window Centers for Domain 1", verbatimTextOutput('anamolyCenterdomain1')),
                     tabPanel("Window Centers for Domain 2", verbatimTextOutput('anamolyCenterdomain2'))
                   )
              ),
              
              fluidRow(
                
                box(title = "Associated Influence Relationship Measures",
                    valueBox(verbatimTextOutput('windowAnomalouesCenterInfluence'),"Center based Influence Score",color = "light-blue"
                    ),
                    
                    valueBox(verbatimTextOutput("centerAssociatedmeasures"),"Confidence",color = "light-blue"
                    ),
                    
                    valueBox(verbatimTextOutput("centerAssociatedmeasures2"),"Support",color = "light-blue"
                    ),
                    
                    valueBox(verbatimTextOutput("centerAssociatedmeasures3"),"Lift",color = "light-blue"
                    )
                ),
                
                
                box(title = "Control Pannel",
                    h6("Associat the domain Centers"),
                    actionButton("associateCenters","Associate Centers")
                )
                
              )
              
              
      ),
      
      tabItem("spacetime",
              
              box(title = "Control Pannel",width = 3,
                  h6("Individual Time Period Analysis"),
                  actionButton("spacetime","Analyze")
              ),
              
              box(title = "Individual Time Period Analysis",
                  tabsetPanel(
                    tabPanel("Potential Intersecting Anamolies", verbatimTextOutput('spacetimeData')),
                    tabPanel("Influence Score in Each Time Frame", plotOutput('spacetimeIScore'))
                  )
                  
              )
              
              
      )
      
      
    )
    
  )
)  
