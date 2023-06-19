#' Author:
#' Project Title: Discovering Anomalous Spatio-temporal Associations (W9132V-15-C-0004-P00001)
#' Principal Investigator: Vandana Janeja, PhD, UMBC
#' Code Author: Prathamesh Walkikar


library(rsatscan)
library(shiny)
library(shinydashboard)

#' @description  : Server Function code for the application

server <- function(input, output) { 
  
  
  # code to get the concerned function for the data
  
  ### Argument names:
  ArgNames <- reactive({
    Names <- names(formals(input$readFunction)[-1])
    Names <- Names[Names!="..."]
    return(Names)
  })
  
  # Argument selector:
  output$ArgSelect <- renderUI({
    if (length(ArgNames())==0) return(NULL)
    
    selectInput("arg","Argument:",ArgNames())
  })
  
  # Input case file function for processing of case file of domain 1

  ### Case Data 1 import:
  
  CaseDataset <- reactive({
    if (is.null(input$case)) {
      # User has not uploaded a file yet
      return(data.frame())
    }
    
    args <- grep(paste0("^",input$readFunction,"__"), names(input), value = TRUE)
    
    argList <- list()
    for (i in seq_along(args))
    {
      argList[[i]] <- eval(parse(text=input[[args[i]]]))
    }
    names(argList) <- gsub(paste0("^",input$readFunction,"__"),"",args)
    
    argList <- argList[names(argList) %in% ArgNames()]
    CaseDataset <- as.data.frame(do.call(input$readFunction,c(list(input$case$datapath),argList)))
    return(CaseDataset) 
  })
  
  
  # Input Geo file function for processing of the geo file of domain 1
  
  ### Geo Data 1 import:
  
  GeoDataset <- reactive({
    if (is.null(input$geo)) {
      # User has not uploaded a file yet
      return(data.frame())
    }
    
    args <- grep(paste0("^",input$readFunction,"__"), names(input), value = TRUE)
    
    argList <- list()
    for (i in seq_along(args))
    {
      argList[[i]] <- eval(parse(text=input[[args[i]]]))
    }
    names(argList) <- gsub(paste0("^",input$readFunction,"__"),"",args)
    
    argList <- argList[names(argList) %in% ArgNames()]
    GeoDataset <- as.data.frame(do.call(input$readFunction,c(list(input$geo$datapath),argList)))
    return(GeoDataset)
  })
  
  
  #display of case file and geo file of domain 1 on upload
  
  # Show table Case Data:
  
  output$caseTable <- renderDataTable({
    return(CaseDataset())
  })	
  
  # Show table Geo Data:
  output$geoTable <- renderDataTable({
    return(GeoDataset())
  })
  
  ###########################################################################################################################  
  
  # Input Case file function for processing of the case file of domain 2
  
  ### Case Data 2 import:
  
  CaseDataset1 <- reactive({
    if (is.null(input$case1)) {
      # User has not uploaded a file yet
      return(data.frame())
    }
    
    args <- grep(paste0("^",input$readFunction,"__"), names(input), value = TRUE)
    
    argList <- list()
    for (i in seq_along(args))
    {
      argList[[i]] <- eval(parse(text=input[[args[i]]]))
    }
    names(argList) <- gsub(paste0("^",input$readFunction,"__"),"",args)
    
    argList <- argList[names(argList) %in% ArgNames()]
    CaseDataset1 <- as.data.frame(do.call(input$readFunction,c(list(input$case1$datapath),argList)))
    # CaseDataset$Date <- as.Date(CaseDataset$Date,"%m/%d/%Y")
    # CaseDataset$Date <- as.numeric(CaseDataset$Date)
    return(CaseDataset1) 
  })
  
  
  # Input Geo file function for processing of the Geo file of domain 2
  
  ### Geo Data 2 import:
  
  GeoDataset1 <- reactive({
    if (is.null(input$geo1)) {
      # User has not uploaded a file yet
      return(data.frame())
    }
    
    args <- grep(paste0("^",input$readFunction,"__"), names(input), value = TRUE)
    
    argList <- list()
    for (i in seq_along(args))
    {
      argList[[i]] <- eval(parse(text=input[[args[i]]]))
    }
    names(argList) <- gsub(paste0("^",input$readFunction,"__"),"",args)
    
    argList <- argList[names(argList) %in% ArgNames()]
    GeoDataset1 <- as.data.frame(do.call(input$readFunction,c(list(input$geo1$datapath),argList)))
    return(GeoDataset1)
  })
  
  
  #display of case file and geo file of domain 2 on upload
  
  # Show table Case Data:
  
  output$caseTable1 <- renderDataTable({
    return(CaseDataset1())
  })	
  
  # Show table Geo Data:
  output$geoTable1 <- renderDataTable({
    return(GeoDataset1())
  })
  
  ######################################################################################################################################################
  #     EQUAL FREQUENCY BINNING  #
  ######################################################################################################################################################  
  
  #Data Binning for Domain 1 
  
  # function to perform equal frequcney binning of data
  EqualFreq2 <- function(x,n){
    nx <- length(x)
    nrepl <- floor(nx/n)
    nplus <- sample(1:n,nx - nrepl*n)
    nrep <- rep(nrepl,n)
    nrep[nplus] <- nrepl+1
    x[order(x)] <- rep(seq.int(n),nrep)
    x
  }
  
  # logic for equal frequency bin data
  
  bind_data_func_caseData <- reactive({
    
    inputStartDate <- input$satscanStartDate
    inputStartDate <- as.Date(inputStartDate,"%Y/%m/%d")
    minDateForOrigin <- inputStartDate
    
    if(is.null(input$n_breaks))
      return(NULL)
    
    if (is.null(CaseDataset()$Date))
      return(NULL)
    
    CaseDataset <- CaseDataset()[order(CaseDataset()$Date),]
    
    bin <- EqualFreq2(as.numeric(CaseDataset()$Date),input$n_breaks)
    bind_data_func_caseData <- cbind(CaseDataset(),bin)
    bind_data_func_geoData <- GeoDataset()
    
    
    #logic for subsetting the bins and creating files for running satsacan
    
    bin_data_EF_Casesubset <- list()
    
    for(i in (1:(input$n_breaks))){
      
      bin_data_EF_Casesubset[[i]] <- subset(bind_data_func_caseData, bind_data_func_caseData$bin == i)
      
    }  
    
    return(list(bind_data_func_caseData = bind_data_func_caseData,bind_data_func_geoData=bind_data_func_geoData,bin_data_EF_Casesubset =bin_data_EF_Casesubset))
    
  })
  
  
  # Data Bin based on equal frequency strategy for Domain 2
  
  bind_data_func_caseData1 <- reactive({
    
    inputStartDate <- input$satscanStartDate1
    inputStartDate <- as.Date(inputStartDate,"%Y/%m/%d")
    minDateForOrigin <- inputStartDate
    
    if(is.null(input$n_breaks))
      return(NULL)
    
    if (is.null(CaseDataset1()$Date))
      return(NULL)
    
    CaseDataset1 <- CaseDataset1()[order(CaseDataset1()$Date),]
    bin <- EqualFreq2(as.numeric(CaseDataset1()$Date),input$n_breaks)
    bind_data_func_caseData1 <- cbind(CaseDataset1(),bin)
    bind_data_func_geoData <- GeoDataset1()
    
    #logic for subsetting the bins and creating files for running satsacan
    
    bin_data_EF_Casesubset1 <- list()
    
    for(i in (1:(input$n_breaks))){
      
      bin_data_EF_Casesubset1[[i]] <- subset(bind_data_func_caseData1, bind_data_func_caseData1$bin == i)
      
    }  
    
    return(list(bind_data_func_caseData1 = bind_data_func_caseData1,bind_data_func_geoData=bind_data_func_geoData,bin_data_EF_Casesubset1=bin_data_EF_Casesubset1))
    
  })
  
  #Domain 1 display of equal frequency binned data
  
  output$caseTableBin <- renderDataTable({
    bind_data_func_caseData()$bind_data_func_caseData
  })
  
  # Show table for Geo Bin Data:
  
  output$geoTableBin <- renderDataTable({
    bind_data_func_caseData()$bind_data_func_geoData
  })
  

  #Domain 2 display of equal frequency binned data
  
  
  # Show table Case Bin Data:
  
  output$caseTableBinDomain2 <- renderDataTable({
    bind_data_func_caseData1()$bind_data_func_caseData
  })
  
  # Show table for Geo Bin Data:
  
  output$geoTableBinDomain2 <- renderDataTable({
    bind_data_func_caseData1()$bind_data_func_geoData
  })
  
  ##########################################################################################################################################################
  
  #     EQUAL WIDTH BINNING  #
  
  #' @description : Equal Width descritization is based on the descritize function used in infotheo package
  ##########################################################################################################################################################  
  
  #Data Binning for Domain 1
  
  # logic for equal width bin data display
  
  bind_data_func_width_caseData <- reactive({
    
    inputStartDate <- input$satscanStartDate
    inputStartDate <- as.Date(inputStartDate,"%Y/%m/%d")
    minDateForOrigin <- inputStartDate
    
    if(is.null(input$n_breaks))
      return(NULL)
    if (is.null(CaseDataset()$Date))
      return(NULL)
    
    CaseDataset <- CaseDataset()[order(CaseDataset()$Date),]
    bin <- discretize(CaseDataset()$Date,disc="equalwidth",input$n_breaks)
    bind_data_func_width_caseData <- cbind(CaseDataset(),bin)
    bind_data_func_width_geoData <- GeoDataset()
    
    #logic for subsetting the bins and creating files for running satsacan
    
    bin_data_EW_Casesubset <- list()
    
    for(i in (1:(input$n_breaks))){
      
      bin_data_EW_Casesubset[[i]] <-  subset(bind_data_func_width_caseData, bind_data_func_width_caseData$X == i) 
      
    }
    return(list(bind_data_func_width_caseData = bind_data_func_width_caseData,bind_data_func_width_geoData=bind_data_func_width_geoData,bin_data_EW_Casesubset=bin_data_EW_Casesubset))
    
  })
  
  
  
  #Data Binning for Domain 2
  
  # logic for equal width bin data display
  
  bind_data_func_width_caseData1 <- reactive({
    
    inputStartDate <- input$satscanStartDate
    inputStartDate <- as.Date(inputStartDate,"%Y/%m/%d")
    minDateForOrigin <- inputStartDate
    
    if(is.null(input$n_breaks))
      return(NULL)
    if (is.null(CaseDataset1()$Date))
      return(NULL)
    
    CaseDataset1 <- CaseDataset1()[order(CaseDataset1()$Date),]
    bin <- discretize(CaseDataset1()$Date,disc="equalwidth",input$n_breaks)
    bind_data_func_width_caseData1 <- cbind(CaseDataset1(),bin)
    bind_data_func_width_geoData <- GeoDataset1()
    
    #logic for subsetting the bins and creating files for running satsacan
    
    bin_data_EW_Casesubset1 <- list()
    
    for(i in (1:(input$n_breaks))){
      
      bin_data_EW_Casesubset1[[i]] <-  subset(bind_data_func_width_caseData1, bind_data_func_width_caseData1$X == i) 
      
    }
    
    return(list(bind_data_func_width_caseData1 = bind_data_func_width_caseData1,bind_data_func_width_geoData=bind_data_func_width_geoData,bin_data_EW_Casesubset1=bin_data_EW_Casesubset1))
    
  })
  
  # Domain 1 display of equal width binned data
  
  # Show table Case Bin Data:
  
  output$caseTableWidthBin <- renderDataTable({
    bind_data_func_width_caseData()$bind_data_func_width_caseData
  })
  
  # Show table for Geo Bin Data:
  
  output$geoTableWidthBin <- renderDataTable({
    bind_data_func_width_caseData()$bind_data_func_width_geoData
  })
  
  
  #Domain 2 display of equal width binned data
  
  
  # Show table Case Bin Data:
  
  output$caseTableWidthBin2 <- renderDataTable({
    bind_data_func_width_caseData1()$bind_data_func_width_caseData
  })
  
  # Show table for Geo Bin Data:
  
  output$geoTableWidthBin2 <- renderDataTable({
    bind_data_func_width_caseData1()$bind_data_func_width_geoData
  })
  
  ##########################################################################################################################################################
  
  # HIERARCHICAL TIME SERIES cLURTERING BASED BINNING  #
  #' @description : Binning strategy obtained based on hierarchical time series clustering strategy which is done by clustering
  #' the Date values into hierarchical clusters using DIANA algorithm strategy implemented in diana() function from cluster package
  
  ##########################################################################################################################################################  
  
  #Data Binning for Domain 1
  
  # logic for hierarchical timeseries clustering based binning data display
  
  bind_data_func_hc_caseData <- reactive({
    
    inputStartDate <- input$satscanStartDate
    inputStartDate <- as.Date(inputStartDate,"%Y/%m/%d")
    minDateForOrigin <- inputStartDate
    
    if(is.null(input$n_breaks))
      return(NULL)
    if (is.null(CaseDataset()$Date))
      return(NULL)
    
    CaseDataset <- CaseDataset()[order(as.vector(CaseDataset()$Date)),]
    
    #diana clustering
    cluster_data <- diana(as.numeric(CaseDataset()$Date), metric = "manhattan", stand = TRUE)
    
    #getting clusters and subsetting the bins out of it
    
    getclust <- cutree(cluster_data,input$n_breaks)
    
    bind_data_func_hc_caseData <- cbind(CaseDataset(),getclust)
    bind_data_func_hc_geoData <- GeoDataset()
    
    #logic for subsetting the bins and creating files for running satsacan
    bin_data_HC_Casesubset <- list()
    
    for(i in (1:(input$n_breaks))){
      
      bin_data_HC_Casesubset[[i]] <-  subset(bind_data_func_hc_caseData, bind_data_func_hc_caseData$getclust == i) 
      
    }
    
    return(list(bind_data_func_hc_caseData = bind_data_func_hc_caseData,bind_data_func_hc_geoData=bind_data_func_hc_geoData,bin_data_HC_Casesubset=bin_data_HC_Casesubset))
  })
  
  
  #Data Binning for Domain 2
  
  # logic for hierarchical timeseries clustering based binning data display
  
  bind_data_func_hc_caseData1 <- reactive({
    
    inputStartDate <- input$satscanStartDate
    inputStartDate <- as.Date(inputStartDate,"%Y/%m/%d")
    minDateForOrigin <- inputStartDate
    
    if(is.null(input$n_breaks))
      return(NULL)
    if (is.null(CaseDataset1()$Date))
      return(NULL)
    
    CaseDataset1 <- CaseDataset1()[order(as.vector(CaseDataset1()$Date)),]
    
    #diana clustering
    cluster_data <- diana(as.numeric(CaseDataset1()$Date), metric = "manhattan", stand = TRUE)
    
    #getting clusters and subsetting the bins out of it
    
    getclust <- cutree(cluster_data,input$n_breaks)
    
    bind_data_func_hc_caseData1 <- cbind(CaseDataset1(),getclust)
    bind_data_func_hc_geoData <- GeoDataset1()
    
    #logic for subsetting the bins and creating files for running satsacan
    bin_data_HC_Casesubset1 <- list()
    
    for(i in (1:(input$n_breaks))){
      
      bin_data_HC_Casesubset1[[i]] <-  subset(bind_data_func_hc_caseData1, bind_data_func_hc_caseData1$getclust == i) 
      
    }
    
    return(list(bind_data_func_hc_caseData1 = bind_data_func_hc_caseData1,bind_data_func_hc_geoData=bind_data_func_hc_geoData,bin_data_HC_Casesubset1=bin_data_HC_Casesubset1))
  })
  
  # Domain 1 display of hierarchical timeseries clustering based binned data
  
  # Show table Case Bin Data:
  
  output$caseTableHCBin <- renderDataTable({
    if (is.null(bind_data_func_hc_caseData()$bind_data_func_hc_caseData))
      return(NULL)
    withProgress(message = 'Computing Results...', value = 0, {
      # Number of times we'll go through the loop
      n <- 10
      
      for (i in 1:n) {
        # Increment the progress bar, and update the detail text.
        incProgress(1 / n)
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)
      }
    })
    
    bind_data_func_hc_caseData()$bind_data_func_hc_caseData
  })
  
  # Show table for Geo Bin Data:
  
  output$geoTableHCBin <- renderDataTable({
    if (is.null(bind_data_func_hc_caseData()$bind_data_func_hc_geoData))
      return(NULL)
    withProgress(message = 'Computing Results...', value = 0, {
      # Number of times we'll go through the loop
      n <- 10
      
      for (i in 1:n) {
        # Increment the progress bar, and update the detail text.
        incProgress(1 / n)
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)
      }
    })
    
    bind_data_func_hc_caseData()$bind_data_func_hc_geoData
  })
  
  # Show table Binned Subset Data:
  
  output$subsetCaseDataHCBin <- renderDataTable({
    bind_data_func_hc_caseData()$bin_data_HC_Casesubset
  })	
  
  
  
  
  # Domain 2 display of hierarchical timeseries clustering based binned data
  
  # Show table Case Bin Data:
  
  output$caseTableHCBin2 <- renderDataTable({
    if (is.null(bind_data_func_hc_caseData1()$bind_data_func_hc_caseData))
      return(NULL)
    withProgress(message = 'Computing Results...', value = 0, {
      # Number of times we'll go through the loop
      n <- 10
      
      for (i in 1:n) {
        # Increment the progress bar, and update the detail text.
        incProgress(1 / n)
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)
      }
    })
    
    bind_data_func_hc_caseData1()$bind_data_func_hc_caseData
  })
  
  # Show table for Geo Bin Data:
  
  output$geoTableHCBin2 <- renderDataTable({
    if (is.null(bind_data_func_hc_caseData1()$bind_data_func_hc_geoData))
      return(NULL)
    withProgress(message = 'Computing Results...', value = 0, {
      # Number of times we'll go through the loop
      n <- 10
      
      for (i in 1:n) {
        # Increment the progress bar, and update the detail text.
        incProgress(1 / n)
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)
      }
    })
    
    bind_data_func_hc_caseData1()$bind_data_func_hc_geoData
  })
  
  # Show table Binned Subset Data:
  
  output$subsetCaseDataHCBin2 <- renderDataTable({
    bind_data_func_hc_caseData1()$bin_data_HC_Casesubset1
  })	
  
  ##########################################################################################################################################################
  
  # SPATIAL- OBJECT CLASS DEFINATION AND PAIRWISE COMPUTATION APPROACH ALGORITHM

  #' @description : This sections implements the concerned functions required for implementing the pairwise influence relationship
  #' apprach.  
  ##########################################################################################################################################################
  
  #class defination
  setClass( Class = "SpatialObject",
            representation = representation(
              location_post = "character"
            )
  )
  
  
  # function to create object of class SpatialObejct and assign value to mile post
  SpatialObject <-  function(location_name){
    mp <- new("SpatialObject")
    location_post(mp) <- location_name
    return(mp)
  }
  
  # Value getting method for location_post
  setGeneric("location_post", function(self) standardGeneric("location_post"))
  setMethod("location_post", 
            signature(self = "SpatialObject"), 
            function(self) {
              self@location_post
            }
  )
  
  #value setting methods
  #setting the values of location_post
  
  setGeneric("location_post<-", function(self, value) standardGeneric("location_post<-"))
  setReplaceMethod("location_post", 
                   "SpatialObject", 
                   function(self, value) {
                     self@location_post <- value
                     self
                   }
  )
  
  # Method that finds the influence distance
  setGeneric("get_influence_distance", function(self1,self2, SPMatrix) { standardGeneric("get_influence_distance")})
  setMethod("get_influence_distance", 
            signature(self1 = "SpatialObject",self2 = "SpatialObject", SPMatrix = "matrix"), 
            function(self1,self2, SPMatrix) {
              x <-  SPMatrix
              
              for(i in colnames(x)){
                for(j in rownames(x)){
                  if(i == self1@location_post && j == self2@location_post){
                    return(x[i,j])
                  }
                  
                }
              }
              
            }
  )
  
  
  #method to generate influence score between the spatial objects
  setGeneric("get_influence_score", function(self1,self2,SPMatrix,influence_decay_factor) { standardGeneric("get_influence_score")})
  setMethod("get_influence_score", 
            signature(self1 = "SpatialObject",self2 = "SpatialObject",SPMatrix = "matrix" ,influence_decay_factor = "numeric"), 
            function(self1,self2,SPMatrix,influence_decay_factor) {
              distance <- get_influence_distance(self1,self2,SPMatrix) # getting the influence distance from the matrix
              influence_score <- exp(-(distance * (1 / influence_decay_factor)))
              influence_score
            }
  )
  
  #method to generate anamolous windows influence scores
  setGeneric("get_anamolous_influence_score", function(array1,array2,SPMatrix,influence_decay_factor) { standardGeneric("get_anamolous_influence_score")})
  setMethod("get_anamolous_influence_score", 
            signature(array1 = "list",array2 = "list",SPMatrix = "matrix", influence_decay_factor = "numeric"), 
            function(array1,array2,SPMatrix,influence_decay_factor) {
              sum_influence_score_max <- 0
              for(s_1 in array1)
              {
                max <- 0
                for(s_2 in array2)
                {
                  influence_score <- get_influence_score(s_1,s_2,SPMatrix,influence_decay_factor)
                  if (influence_score > max)
                    max <- influence_score
                }
                
                sum_influence_score_max <- sum_influence_score_max + max
                
              }
              
              x <- (sum_influence_score_max/length(array1))
              x
              
            }
  )
  
  #method for generating best pairing between anamolous windows
  
  setGeneric("get_best_paring_for_phenomena", function(array1,array2,SPMatrix,influence_decay_factor) {
    standardGeneric("get_best_paring_for_phenomena")
  })
  setMethod("get_best_paring_for_phenomena",
            signature(
              array1 = "list",array2 = "list",SPMatrix = "matrix",influence_decay_factor = "numeric"
            ),
            function(array1,array2,SPMatrix,influence_decay_factor) {
              best_paring <- list()
              index_p_1 <- 1
              index_p_2 <- 1
              
              for (a_1 in array1){
                
                while(index_p_1 <= length(array1)){
                  
                  index_p_2 <- 1
                  
                  max_influence_score <- 0
                  
                  while (index_p_2 <= length(array2)) {
                    
                    influence_score <- get_anamolous_influence_score(array1[index_p_1], array2[index_p_2],SPMatrix,influence_decay_factor)
                    
                    if (influence_score < max_influence_score) {
                      
                      break
                      
                    } else {
                      max_influence_score <- influence_score
                    }
                    
                    if (index_p_2 <= length(array2)) {
                      
                      best_paring <- c((best_paring),index_p_1,index_p_2,max_influence_score)
                    } 
                    else {
                      
                      break
                      
                    }
                    
                    index_p_2 <- index_p_2 + 1 
                  }
                  
                  index_p_1 <- index_p_1 + 1
                }
              }  
              
              best_paring
              
            })
  
  #confidence calculation for influence phenomenon rules
  
  setGeneric("get_confidence", function(array1,array2,SPMatrix,domain1count,influence_decay_factor,influence_threshold) {
    standardGeneric("get_confidence")
  })
  setMethod("get_confidence",
            signature(
              array1 = "list",array2 = "list",SPMatrix = "matrix",domain1count = "numeric",influence_decay_factor = "numeric",influence_threshold = "numeric"
            ),
            function(array1,array2,SPMatrix,domain1count,influence_decay_factor,influence_threshold) {
              
              best_paring <- get_best_paring_for_phenomena(array1,array2,SPMatrix,influence_decay_factor)
              
              count_influence <- 0
              count <- 0
              
              for (i in seq(3,length(best_paring),3)){
                influence_score <- best_paring[i]
                
                if(influence_score >= influence_threshold){
                  count_influence <- count_influence + 1
                  
                }
                
              }

              confidence1 <- count_influence / domain1count
              confidence1
              
            })
  
  
  #confidence calculation for influence phenomenon rules
  
  setGeneric("get_confidence_center", function(array1,array2,SPMatrix,influence_decay_factor,influence_threshold) {
    standardGeneric("get_confidence_center")
  })
  setMethod("get_confidence_center",
            signature(
              array1 = "list",array2 = "list",SPMatrix = "matrix",influence_decay_factor = "numeric",influence_threshold = "numeric"
            ),
            function(array1,array2,SPMatrix,influence_decay_factor,influence_threshold) {
              
              best_paring <- get_best_paring_for_phenomena(array1,array2,SPMatrix,influence_decay_factor)
              
              count_influence <- 0
              count <- 0
              
              for (i in seq(3,length(best_paring),3)){
                influence_score <- best_paring[i]
                
                if(influence_score >= influence_threshold){
                  count_influence <- count_influence + 1
                  
                }
                
              }
              
              count_best_pairs <- length(best_paring)/3
              confidence1 <- count_influence / count_best_pairs 
              confidence1
              
            })
  
  #support calculation for influence phenomenon rules
  
  setGeneric("get_support", function(array1,array2,SPMatrix,influence_decay_factor,influence_threshold) {
    standardGeneric("get_support")
  })
  setMethod("get_support",
            signature(
              array1 = "list",array2 = "list",SPMatrix = "matrix",influence_decay_factor = "numeric",influence_threshold = "numeric"
            ),
            function(array1,array2,SPMatrix,influence_decay_factor,influence_threshold) {
              
              best_paring <- get_best_paring_for_phenomena(array1,array2,SPMatrix,influence_decay_factor)
              
              count_influence <- 0
              all_pairs <- nrow(SPMatrix) * ncol(SPMatrix)
              
              for (i in seq(3,length(best_paring),3)){
                influence_score <- best_paring[i]
                
                if(influence_score >= influence_threshold){
                  count_influence <- count_influence + 1
                  
                }
                
              }
              
              support <-  count_influence / all_pairs
              support            
            })
  
  #lift calculation for influence phenomenon rules
  
  setGeneric("get_lift", function(array1,array2,SPMatrix,location_count_d1,location_count_d2,influence_decay_factor,influence_threshold) {
    standardGeneric("get_lift")
  })
  setMethod("get_lift",
            signature(
              array1 = "list",array2 = "list",SPMatrix = "matrix",location_count_d1 = "numeric",location_count_d2 = "numeric",influence_decay_factor = "numeric",influence_threshold = "numeric"
            ),
            function(array1,array2,SPMatrix,location_count_d1,location_count_d2,influence_decay_factor,influence_threshold) {
              
              best_paring <- get_best_paring_for_phenomena(array1,array2,SPMatrix,influence_decay_factor)
              
              count_influence <- 0
              count <- 0
              cd <- nrow(GeoDataset())
              cd1 <- nrow(GeoDataset1())
              
              for (i in seq(3,length(best_paring),3)){
                influence_score <- best_paring[i]
                
                if(influence_score >= influence_threshold){
                  count_influence <- count_influence + 1
                  
                }
                
              }
              
              confidence <- count_influence / location_count_d1
              support1 <-  location_count_d1 / cd
              support2 <- location_count_d2 / cd1
              lift <- confidence / (support1 * support2)
              lift
              
            })
  
  
  ##################################################################################################################################################
  CreateAdjecencyEdgeMatrix <- function(value){ # Creation of Adjacency Matrix for Influence Distance Calculation
    x <-  value
    location_id <- x[,1]
    mat <- matrix(location_id,nrow = NROW(x),ncol = NROW(x))
    mat[1:NROW(x),2:NROW(x)] <- 0 
    colnames(mat) <- mat[,1]
    rownames(mat) <- mat[,1]
    mat[,1:NROW(value)] <- NA
    
    for(i in (1:NROW(mat))){
      for(j in (1:NCOL(mat))){
        if(i == j){
          mat[i,j] <- NA      
        } else {
          mat[i,j] <- sample(0:1,1)
        }
      }
    }
    return(mat)
  }
  
  ##################################################################################################################################################
  
  CreateAdjecencyEdgeMatrixCenter <- function(value){ # Creation of Adjacency Matrix for Influence Distance Calculation for Centers
    x <-  value
    mat <- matrix(x,nrow = NROW(x),ncol = NROW(x))
    mat[1:NROW(x),2:NROW(x)] <- 0 
    colnames(mat) <- x[,1]
    rownames(mat) <- x[,1]
    mat[,1:NROW(x)] <- NA
    
    for(i in (1:NROW(mat))){
      for(j in (1:NCOL(mat))){
        if(i == j){
          mat[i,j] <- NA      
        } else {
          mat[i,j] <- sample(0:1,1)
        }
      }
    }
    return(mat)
  }
  
  
  ##################################################################################################################################################  
  checkNames <- function(value){ # checks for repetated locations in the adjacency matrix
    mat <- value
    xrow <- rownames(mat)
    ycol <- colnames(mat)
    
    for(i in 1:length(xrow)){
      for(j in 1:length(ycol)){
        if(xrow[i] == ycol[j]){
          mat[i,j] <- 0
        }
      }
    }
    return(mat)
  }
  
  ##################################################################################################################################################    
  createInfluenceDistanceMatrix <- function(value){ # Creation Influence Distance matrix by computing shortest path
    dataset <- value
    x <- CreateAdjecencyEdgeMatrix(dataset)
    x <-  checkNames(x)
    return(x)
  }
  
  
  ##################################################################################################################################################    
  
  createInfluenceDistanceMatrixCenter <- function(value){ # Creation Influence Distance matrix by computing shortest path for Center Method
    dataset <- value
    x <- CreateAdjecencyEdgeMatrixCenter(dataset)
    x <-  checkNames(x)
    return(x)
  }
  
  ######################################################################################################################  
  
  # function to compute jaccard simalarities values from influence matrix and select edges base don threshold 
  
  checkJaccardSimilarity <- function(adjMat,value){ 
    x <-  adjMat
    graph <- graph_from_adjacency_matrix(x,weighted = TRUE)
    graph_jaccard_similarity_matrix <- similarity(graph,vids = V(graph),mode = "all",method = "jaccard")
    rownames(graph_jaccard_similarity_matrix) <- rownames(x)
    colnames(graph_jaccard_similarity_matrix) <- colnames(x)
    
    for(i in (1:nrow(graph_jaccard_similarity_matrix))){
      for(j in (1:ncol(graph_jaccard_similarity_matrix))){
        if (graph_jaccard_similarity_matrix[i,j] >= value){
          graph_jaccard_similarity_matrix[i,j] <- 1
        } else {
          graph_jaccard_similarity_matrix[i,j] <- 0
        }
      }
    }
    
    gadjacency <- graph.adjacency(graph_jaccard_similarity_matrix,mode = "directed") 
    shortestPathMatrix <- shortest.paths(gadjacency)
    shortestPathMatrix <- checkNames(shortestPathMatrix)
    return(shortestPathMatrix)
  } 
  
  ##########################################################################################################################################################
  
  calculateGeographicDistance <-function(data_set){ # function to calculates Actual Geographic distance
    x <- data_set
    dat <- data.frame(x)
    
    location_id <- dat[,1]
    
    ll <- c("lat", "long")
    coords <- dat[ll]
    
    dist <- apply(coords, 1, function(eachPoint) spDistsN1(as.matrix(coords),eachPoint, longlat=TRUE))
    rownames(dist) <- location_id
    colnames(dist) <- location_id
    
    dist <- checkNames(dist)
    return(dist)
  }
  
  #########################################################################################################################################################
  
  calculateGeoConfidence <-  function(array1,distMatrix,tvalue){ # function to calculates confidence and support measures from actual geographic distances
    x <- distMatrix
    count_dist <-  0
    
    count_anamolies <- nrow(x)*ncol(x)
    
    for(i in (1: ncol(x))){
      for(j in (1: nrow(x))){
        if(x[i,j] > tvalue){
          count_dist <-  count_dist + 1
        }
      }
    }
    confidence <- count_dist/length(array1)
    support <- count_dist/count_anamolies
    
    result <-  c(confidence,support)
    return(result)
  }
  ##########################################################################################################################################################
  #SatScan analysis
  
  #' @details : This action button is used to reset the satscan parametrs and should be ideally used each time before invoking satscan run
  observeEvent(input$reset,{
    
    invisible(ss.options(reset=TRUE))
    
  })
  
  ##########################################################################################################################################################  #SatScan Run
  #' Code for domain 1 Analysis
  
  observeEvent(input$runsatscan1,{
    
    # local variables decleration for domain 1 
    e_location_id <- list()
    Locn_id <- list()
    time_frame <- list()
    e_time_frame <- list()
    extracted_data1 <- list()
    text_data <- list()
    
    #input case file name
    ss.options(list(CaseFile="ANAlysis.cas", PrecisionCaseTimes=1))
    
    startDateString <- "StartDate="
    endDateString <- "EndDate="
    
    # Domain 1
    
    inputStartDate <- input$satscanStartDate
    inputStartDate <- as.Date(inputStartDate,"%Y/%m/%d")
    inputStartDate <- format(strptime(inputStartDate,"%Y-%m-%d"),"%Y/%m/%d") 
    inputEndDate <- input$satscanEndDate
    inputEndDate <- as.Date(inputEndDate,"%Y/%m/%d")
    inputEndDate <- format(strptime(inputEndDate,"%Y-%m-%d"),"%Y/%m/%d") 
    
    startDate <- paste(startDateString,inputStartDate,"")
    endDate <- paste(endDateString,inputEndDate,"")
    ss.options(c(startDate,endDate))
    
    #input geo file name
    ss.options(list(CoordinatesFile="ANAlysis.geo", AnalysisType=3, ModelType=2, TimeAggregationUnits=1 , CriteriaForReportingSecondaryClusters = input$secondaryCluster, MonteCarloReps=99))
    if(input$clusterSize == 'y')
    {
      ss.options(list(UseDistanceFromCenterOption="y",MaxSpatialSizeInDistanceFromCenter=input$distanceSelect))
    }
    else
    {
      ss.options(list(UseDistanceFromCenterOption="n",MaxSpatialSizeInDistanceFromCenter=input$distanceSelect))
      
    }
    
    # declare list of objects which stores all the satscan runs from each bins
    ANAlysis <- list()
    
    #create temperory directory
    td = tempdir()
    
    write.ss.prm(td, "ANAlysis")
    
    
    for (i in 1:(input$n_breaks))
    {
      
      observe({
        if(input$n_breaks > i ){
          
          withProgress(message = 'Computing Results...', value = 0, {
            # Number of times we'll go through the loop
            n <- 10
            
            for (i in 1:n) {
              # Increment the progress bar, and update the detail text.
              incProgress(1 / n)
              
              # Pause for 0.1 seconds to simulate a long computation.
              Sys.sleep(0.1)
            }
          })
        }
      })
      
      #prepering the case and geo files as input to satscan instance based on binning strategy
      
      if(input$binStrategy == 'Equal Frequency Binning'){
        
        #Binned Uploaded Case File --- Equal Frequecny Binning    
        write.cas(bind_data_func_caseData()$bin_data_EF_Casesubset[[i]],td,"ANAlysis")
        write.geo(bind_data_func_caseData()$bind_data_func_geoData,td, "ANAlysis")  
        
      } 
      else if(input$binStrategy == 'Equal Width Binning') 
      {
        #Binned Uploaded Case File --- Equal Width Binning    
        write.cas(bind_data_func_width_caseData()$bin_data_EW_Casesubset[[i]],td, "ANAlysis")
        write.geo(bind_data_func_width_caseData()$bind_data_func_width_geoData,td, "ANAlysis")
        
      }
      else if(input$binStrategy == 'Hierarchical Timeseries Clustering') 
      {
        #Binned Uploaded Case File --- Hierarchical Timeseries Clustering Binning    
        write.cas(bind_data_func_hc_caseData()$bin_data_HC_Casesubset[[i]],td, "ANAlysis")
        write.geo(bind_data_func_hc_caseData()$bind_data_func_hc_geoData,td, "ANAlysis")
        
      }
      else {
        
        #Full Uploaded Case File Without Binning
        write.cas(CaseDataset(),td, "ANAlysis")
        write.geo(GeoDataset(),td, "ANAlysis") 
        
      }
      
      # Running SatScan on each individual object and storing it in ANAlysis list object
      
      tryCatch({
      ANAlysis[[i]] = satscan(td, "ANAlysis", sslocation="C:/Program Files/SaTScan",ssbatchfilename = "SatScanBatch",verbose = TRUE,cleanup = TRUE)
      }, error = function(e){print("NO features found")})
      
      withProgress(message = 'Plotting the Analysis Output for Domain 1', value = 0, {
        # Number of times we'll go through the loop
        n <- 10
        
        for (i in 1:n) {
          # Increment the progress bar, and update the detail text.
          incProgress(1 / n, detail = paste("Plotting the Results", i))
          
          # Pause for 0.1 seconds to simulate a long computation.
          Sys.sleep(0.1)
        }
      })
      
      #function to generate shapeFiles obtained from each bins satscan run and merge it into single file for domain 1
      getShapefile1 <- function(){
        result_shape_file <- list() 
        
        for (i in 1:(input$n_breaks)){
          result_shape_file <- gUnionCascaded(ANAlysis[[i]]$shapeclust)
        }
        
        result_shape <- fortify(result_shape_file)
        return(result_shape)
      }
      
      # display of anamolies from domain 1 based on satscan results
      
      output$textDisp <- renderPrint({
        
        ObjectList <- list()
        
        for (i in 1:(input$n_breaks)){
          
          text_data[[i]] <- ANAlysis[[i]]$main
          e_location_id[[i]]<- text_data[[i]][grepl("Location IDs included.:", text_data[[i]])]
          Locn_id[[i]]<-unlist(lapply(strsplit(as.character(e_location_id[[i]]),": "),"[",2))
          
          e_time_frame[[i]] <- text_data[[i]][grepl("Time frame............:", text_data[[i]])]
          time_frame[[i]] <-unlist(lapply(strsplit(as.character(e_time_frame[[i]]),": "),"[",2))
          
          extracted_data1[[i]] <- cbind.data.frame(Locn_id[[i]],time_frame[[i]])
          
        }
        
        df <- do.call("rbind",extracted_data1)
        colnames(df) <- c("Anamolies","Time")
        
        sdf <- strsplit(as.character(df$Anamolies), ',')
        sdf <- unique(data.frame(anamolies = unlist(sdf),time = rep(df$Time , sapply(sdf,FUN = length))))
        sdf <- distinct(sdf)
        location_data <-  as.character(sdf$anamolies)
        
        for(i in (1:nrow(sdf))){
          ObjectList[[i]] <- SpatialObject(location_data[i])
        }
        
        domain1Obj <<- ObjectList
        domain1text <<- sdf
        
        shapeFile1 <<- getShapefile1()
        
        sdf
        
      })
      
      # displaying only the primary windows from each bin
      
      output$textDisp1Primary <- renderPrint({
        
        for (i in 1:(input$n_breaks)){
          
          text_data[[i]] <- ANAlysis[[i]]$main
          e_location_id[[i]]<- text_data[[i]][grepl("1.Location IDs included.:", text_data[[i]])]
          Locn_id[[i]]<-unlist(lapply(strsplit(as.character(e_location_id[[i]]),": "),"[",2))
          
          #           e_time_frame1[[i]] <- text_data1[[i]][grepl("Time frame............:", text_data1[[i]])]
          #           time_frame1[[i]] <-unlist(lapply(strsplit(as.character(e_time_frame1[[i]]),": "),"[",2))
          #           
          # extracted_data2[[i]] <- cbind.data.frame(Locn_id1[[i]],time_frame1[[i]])
          
        }
        
        df1 <- do.call("rbind",Locn_id)
        df1 <-  as.data.frame(df1)
        colnames(df1) <- "anamolies"
        
        
        sdf1 <- strsplit(as.character(df1$anamolies), ',')
        
        sdf1 <- unique(data.frame(anamolies = unlist(sdf1)))
        
                if(is.null(sdf1))
                  return(NULL)
        
                ObjectList1 <- list()
                
                location_data <-  as.character(sdf1$anamolies)
                for(i in (1:nrow(sdf1))){
                  ObjectList1[[i]] <- SpatialObject(location_data[i])
                }
                
                domain1ObjPrim <<- ObjectList1
                
                domain1textPrim <<- sdf1
        
        sdf1
      })
      
      
      # display of anaomolus windows centers based on satscan run 
      
      output$anamolyCenterdomain1 <- renderPrint({
        
        text_data_center <- list()
        WindoCenters1 <- list()
        data_extracted <- list()
        
        for (i in 1:(input$n_breaks)){
          text_data_center[[i]] <-  ANAlysis[[i]]$col[,2] 
          data_extracted[[i]] <- as.data.frame(text_data_center[[i]])
        }
        
        df <- do.call("rbind",data_extracted)
        colnames(df) <- c("Anamolies Center")
        df <-  unique(df)
        
        windowCenter1 <<- df
        
        location_centers <- as.character(df$`Anamolies Center`)
        
        for(i in (1:nrow(df))){
          WindoCenters1[[i]] <- SpatialObject(location_centers[i])
        }
        
        windowCenterObj1 <<- WindoCenters1
        
        geodatafile <- GeoDataset()
        lat <- sqldf('select lat from geodatafile  where SpatialLocation in df')
        long <- sqldf('select long from geodatafile where SpatialLocation in df')
        
        data_with_lat_lng <- cbind.data.frame(df,lat,long)
        windowCenter1 <<- data_with_lat_lng
        df
        
      })
      
    } # end of for loop
    
  })
  
  ##########################################################################################################################################################

  #' Code for domain 2 Analysis

    
  #SatScan Run
  observeEvent(input$runsatscan2,{
    
    #variables decleration for domain 2 
    e_location_id1 <- list()
    Locn_id1 <- list()
    time_frame1 <- list()
    e_time_frame1 <- list()
    extracted_data2 <- list()
    text_data1 <- list()
    
    
    #input case file name
    ss.options(list(CaseFile="ANAlysis.cas", PrecisionCaseTimes=1))
    
    startDateString <- "StartDate="
    endDateString <- "EndDate="
    
    # Domain 2
    
    inputStartDate2 <- input$satscanStartDate1
    inputStartDate2 <- as.Date(inputStartDate2,"%Y/%m/%d")
    inputStartDate2 <- format(strptime(inputStartDate2,"%Y-%m-%d"),"%Y/%m/%d") 
    inputEndDate2 <- input$satscanEndDate1
    inputEndDate2 <- as.Date(inputEndDate2,"%Y/%m/%d")
    inputEndDate2 <- format(strptime(inputEndDate2,"%Y-%m-%d"),"%Y/%m/%d")
    
    
    startDate <- paste(startDateString,inputStartDate2,"")
    endDate <- paste(endDateString,inputEndDate2,"")
    ss.options(c(startDate,endDate))  
    
    #input geo file name
    ss.options(list(CoordinatesFile="ANAlysis.geo", AnalysisType=3, ModelType=2, TimeAggregationUnits=1 , CriteriaForReportingSecondaryClusters = input$secondaryCluster, MonteCarloReps=99))
    if(input$clusterSize == 'y')
    {
      ss.options(list(UseDistanceFromCenterOption="y",MaxSpatialSizeInDistanceFromCenter=input$distanceSelect))
    }
    else
    {
      ss.options(list(UseDistanceFromCenterOption="n",MaxSpatialSizeInDistanceFromCenter=input$distanceSelect))
      
    }
    
    #domain 2 
    ANAlysis1 <- list()
    
    #create temperory directory
    td = tempdir()
    
    
    write.ss.prm(td, "ANAlysis")
    
    if(is.null(input$n_breaks)){
      return(NULL)
    }
    
    for (i in 1:(input$n_breaks))
    {
      observe({
        if(input$n_breaks > i ){
          
          withProgress(message = 'Computing Results Please wait...', value = 0, {
            # Number of times we'll go through the loop
            n <- 10
            
            for (i in 1:n) {
              # Increment the progress bar, and update the detail text.
              incProgress(1 / n)
              
              # Pause for 0.1 seconds to simulate a long computation.
              Sys.sleep(0.1)
            }
          })
        }
      })
      #Domain 2 SatScan Run
      if(input$binStrategy == 'Equal Frequency Binning'){
        
        #Binned Uploaded Case File --- Equal Frequecny Binning    
        write.cas(bind_data_func_caseData1()$bin_data_EF_Casesubset1[[i]],td, "ANAlysis")
        write.geo(bind_data_func_caseData1()$bind_data_func_geoData,td, "ANAlysis")  
        
      } 
      else if(input$binStrategy == 'Equal Width Binning') 
      {
        #Binned Uploaded Case File --- Equal Frequecny Binning    
        write.cas(bind_data_func_width_caseData1()$bin_data_EW_Casesubset1[[i]],td, "ANAlysis")
        write.geo(bind_data_func_width_caseData1()$bind_data_func_width_geoData,td, "ANAlysis")
        
      }
      else if(input$binStrategy == 'Hierarchical Timeseries Clustering') 
      {
        #Binned Uploaded Case File --- Equal Frequecny Binning    
        write.cas(bind_data_func_hc_caseData1()$bin_data_HC_Casesubset1[[i]],td, "ANAlysis")
        write.geo(bind_data_func_hc_caseData1()$bind_data_func_hc_geoData,td, "ANAlysis")
        
      }
      else {
        
        #Full Uploaded Case File Without Binning
        write.cas(CaseDataset1(),td, "ANAlysis")
        write.geo(GeoDataset1(),td, "ANAlysis") 
        
      }
      
      # Running SatScan on each individual object and storing it in ANAlysis1 list object
      ANAlysis1[[i]] = satscan(td, "ANAlysis", sslocation="C:/Program Files/SaTScan",ssbatchfilename = "SatScanBatch",verbose = TRUE,cleanup = TRUE)   
      
      withProgress(message = 'Plotting the Analysis Output for Domain 2', value = 0, {
        # Number of times we'll go through the loop
        n <- 10
        
        for (i in 1:n) {
          # Increment the progress bar, and update the detail text.
          incProgress(1 / n, detail = paste("Plotting the Results", i))
          
          # Pause for 0.1 seconds to simulate a long computation.
          Sys.sleep(0.1)
        }
      })
      
      #function to generate shapeFiles obtained from each bins satscan run and merge it into single file for domain 2
      
      getShapefile2 <- function(){
        result_shape_file <- list() 
        
        for (i in 1:(input$n_breaks)){
          result_shape_file <- gUnionCascaded(ANAlysis1[[i]]$shapeclust)
        }
        
        result_shape <- fortify(result_shape_file)
        return(result_shape)
      }
      
      # display of anaomolus windows based on satscan run considering both primary and secondary 
      
      output$textDisp1 <- renderPrint({
        
        for (i in 1:(input$n_breaks)){
          
          text_data1[[i]] <- ANAlysis1[[i]]$main
          e_location_id1[[i]]<- text_data1[[i]][grepl("Location IDs included.:", text_data1[[i]])]
          Locn_id1[[i]]<-unlist(lapply(strsplit(as.character(e_location_id1[[i]]),": "),"[",2))
          
          e_time_frame1[[i]] <- text_data1[[i]][grepl("Time frame............:", text_data1[[i]])]
          time_frame1[[i]] <-unlist(lapply(strsplit(as.character(e_time_frame1[[i]]),": "),"[",2))
          
          extracted_data2[[i]] <- cbind.data.frame(Locn_id1[[i]],time_frame1[[i]])
          
        }
        
        df1 <- do.call("rbind",extracted_data2)
        colnames(df1) <- c("anamolies","time")
        
        
        sdf1 <- strsplit(as.character(df1$anamolies), ',')
        
        sdf1 <- unique(data.frame(anamolies = unlist(sdf1),time = rep(df1$time , sapply(sdf1,FUN = length))))
        
        if(is.null(sdf1))
          return(NULL)
        
        ObjectList1 <- list()
        
        location_data <-  as.character(sdf1$anamolies)
        for(i in (1:nrow(sdf1))){
          ObjectList1[[i]] <- SpatialObject(location_data[i])
        }
        
        domain2Obj <<- ObjectList1
        
        domain2text <<- sdf1
        
        shapeFile2 <<- getShapefile2()
        
        sdf1
      })
      
      # displaying only the primary windows from each bin
      
      output$textDisp2Primary <- renderPrint({
        
        for (i in 1:(input$n_breaks)){
          
          text_data1[[i]] <- ANAlysis1[[i]]$main
          e_location_id1[[i]]<- text_data1[[i]][grepl("1.Location IDs included.:", text_data1[[i]])]
          Locn_id1[[i]]<-unlist(lapply(strsplit(as.character(e_location_id1[[i]]),": "),"[",2))
          
        }
        
        df1 <- do.call("rbind",Locn_id1)
        df1 <- as.data.frame(df1)
        colnames(df1) <- "anamolies"
      
        
        sdf1 <- strsplit(as.character(df1$anamolies), ',')
        
        sdf1 <- unique(data.frame(anamolies = unlist(sdf1)))
        
        if(is.null(sdf1))
          return(NULL)
        
        ObjectList1 <- list()
        
        location_data <-  as.character(sdf1$anamolies)
        for(i in (1:nrow(sdf1))){
          ObjectList1[[i]] <- SpatialObject(location_data[i])
        }
        
        domain2ObjPrim <<- ObjectList1
        
        domain2textPrim  <<- sdf1
        
        # sdf1
        
        sdf1
      })
      
      
      # display of anaomolus windows centers based on satscan run 
      
      output$anamolyCenterdomain2 <- renderPrint({
        
        text_data_center1 <- list()
        WindoCenters2 <- list()
        data_extracted1 <- list()
        
        for (i in 1:(input$n_breaks)){
          
          text_data_center1[[i]] <-  ANAlysis1[[i]]$col[,2] 
          data_extracted1[[i]] <- as.data.frame(text_data_center1[[i]])
        }
        
        df1 <- do.call("rbind",data_extracted1)
        colnames(df1) <- c("Anamolies Center")
        df1 <- unique(df1)
        
        location_centers1 <- as.character(df1$`Anamolies Center`)
        
        for(i in (1:nrow(df1))){
          WindoCenters2[[i]] <- SpatialObject(location_centers1[i])
        }
        
        windowCenterObj2 <<- WindoCenters2
        
        geodatafile <- GeoDataset1()
        lat <- sqldf('select lat from geodatafile  where SpatialLocation in df1')
        long <- sqldf('select long from geodatafile where SpatialLocation in df1')
        
        data_with_lat_lng <- cbind.data.frame(df1,lat,long)
        windowCenter2 <<- data_with_lat_lng
        
        df1
        
      })
      
      
    } # end of for loop
    
  })

  ##########################################################################################################################################################
  
  # associate both the domains
  observeEvent(input$associteDomains,{
    
    #' count individual anamolies from each domain for unconditional support
    
    domain1Count <- function(){
      test <- domain1text
      test <- as.data.frame(test)
      anamolies <- test[1]
      anamolies$anamolies
      uni_ana <- anamolies[!duplicated(anamolies["anamolies"]),]
      uni_ana <- as.data.frame(uni_ana)
      count1 <- nrow(uni_ana)
      return(count1)
    }
    
    domain1CountPrim <- function(){
      test <- domain1textPrim
      test <- as.data.frame(test)
      anamolies <- test[1]
      anamolies$anamolies
      uni_ana <- anamolies[!duplicated(anamolies["anamolies"]),]
      uni_ana <- as.data.frame(uni_ana)
      count1 <- nrow(uni_ana)
      return(count1)
    }
    
    
    domain2Count <- function(){
      test <- domain2text
      test <- as.data.frame(test)
      anamolies <- test[1]
      anamolies$anamolies
      uni_ana <- anamolies[!duplicated(anamolies["anamolies"]),]
      uni_ana <- as.data.frame(uni_ana)
      count2 <- nrow(uni_ana)
      return(count2)
    }
    
    #function to combine code full results form both the domain into one
    prepareData <- function(){
      
      x <-  domain1text
      y <- domain2text
      
      combined_data <- rbind.data.frame(x,y)
      # combined_data <- unique(combined_data)
      return(combined_data)
    } 
    
    prepareDataPrim <- function(){
      
      x <-  domain1textPrim
      y <- domain2textPrim
      
      combined_data <- rbind.data.frame(x,y)
      # combined_data <- unique(combined_data)
      return(combined_data)
    } 
    
    # function to merge shape files into 1 and assign color codes based on d1 and d2 values
    mergeShapefiles <-  function(s1,s2){
      x <- s1
      y <- s2
      
      x$domain <- "d1"
      y$domain <- "d2"
      
      result_file <- rbind.data.frame(x,y)
      return(result_file)
      
    }
    
    #get anamolous window influce score and dislay it
    output$windowAnomalouesScore2 <- renderPrint({
      x <-  prepareDataPrim()
      matr <- createInfluenceDistanceMatrix(x)
      
      result <- checkJaccardSimilarity(matr,input$jaccardValue)
      
      resultA <- get_anamolous_influence_score(domain1ObjPrim,domain2ObjPrim,result,input$Idf)
      resultA
      
    })
    
    #get anamolous window confidence measures and dislay them
    output$windowAnomalouesScore1 <- renderPrint({
      x <-  prepareDataPrim()
      
      matr <- createInfluenceDistanceMatrix(x)
      count1 <- domain1Count()
      result <- checkJaccardSimilarity(matr,input$jaccardValue)
      
      resultB <- get_confidence(domain1ObjPrim,domain2ObjPrim,result,count1,input$Idf,input$input_threshold)
      pairwiseconfidence <<- resultB
      resultB
      
    })
    
    
    
    #get anamolous window support measures and dislay them
    output$windowAnomalouesScore3 <- renderPrint({
      x <-  prepareData()
      matr <- createInfluenceDistanceMatrix(x)
      result <- checkJaccardSimilarity(matr,input$jaccardValue)
      
      resultB <- get_support(domain1ObjPrim,domain2ObjPrim,result,input$Idf,input$input_threshold)
      resultB
      
    })
    
    #get lift measure and dislay it
    output$WindowLift <- renderPrint({
    x <-  prepareData()
    matr <- createInfluenceDistanceMatrix(x)
    result <- checkJaccardSimilarity(matr,input$jaccardValue)  
    count1 <- domain1Count()
    count2 <- domain2Count()
    
    res <- get_lift(domain1ObjPrim,domain2ObjPrim,result,count1,count2,input$Idf,input$input_threshold)
    res
    
    })
    
    # Create the map visulizations 
    output$map <- renderLeaflet({
      
      final_shapeFile <- mergeShapefiles(shapeFile1,shapeFile2)
      colorcode <- colorFactor(topo.colors(2),final_shapeFile$domain)
      
      (leaflet(final_shapeFile) %>%
        addTiles() %>%
        addCircles(lng = ~long ,lat = ~lat,weight = 2,color = ~colorcode(final_shapeFile$domain)) %>%
        setView(lng = -105.85, lat = 34.45, zoom = 5)
      )
      
    })
  })
  
  #' associate the domain centers based on influcen threshold and jaccard simalarity values from pairwise computation approach screen
  
  observeEvent(input$associateCenters,{
    
    #combine window centers to create adjacency matrix
    prepareDataCenter <- function(){
      
      x <- windowCenter1
      y <- windowCenter2
      
      combined_data <- rbind.data.frame(x,y)
      return(combined_data)
    } 
    
    domain1CenterCount <- function(){
      test <- windowCenter1
      test <- as.data.frame(test)
      anamolies <- test[1]
      anamolies$`Anamolies Center`
      uni_ana <- anamolies[!duplicated(anamolies["Anamolies Center"]),]
      uni_ana <- as.data.frame(uni_ana)
      count1 <- nrow(uni_ana)
      return(count1)
    }
    
    domain2CenterCount <- function(){
      test <- windowCenter2
      test <- as.data.frame(test)
      anamolies <- test[1]
      anamolies$`Anamolies Center`
      uni_ana <- anamolies[!duplicated(anamolies["Anamolies Center"]),]
      uni_ana <- as.data.frame(uni_ana)
      count1 <- nrow(uni_ana)
      return(count1)
    }
    
    
    #compute and display the anamolous window influence score for window centers
    output$windowAnomalouesCenterInfluence <- renderPrint({
      xc <-  prepareDataCenter()
      matr1 <- createInfluenceDistanceMatrixCenter(xc)
      
      resultA <- checkJaccardSimilarity(matr1,input$jaccardValue)
      
      result <- get_anamolous_influence_score(windowCenterObj1,windowCenterObj2,resultA,input$Idf)
      result
      
    })
    
    #compute the confidence measure based on window centers
    output$centerAssociatedmeasures <- renderPrint({
      xc <-  prepareDataCenter()
      matr1 <- createInfluenceDistanceMatrixCenter(xc)
      count1 <- domain1CenterCount()
      
      resultA <- checkJaccardSimilarity(matr1,input$jaccardValue)
      
      centerResult <- get_confidence_center(windowCenterObj1,windowCenterObj2,resultA,input$Idf,input$input_threshold)
      centerResult
    
    })
    
    #compute the support measure based on window centers
    output$centerAssociatedmeasures2 <- renderPrint({
      xc <-  prepareDataCenter()
      matr1 <- createInfluenceDistanceMatrixCenter(xc)
      
      resultA <- checkJaccardSimilarity(matr1,input$jaccardValue)
      
      centerResult <- get_support(windowCenterObj1,windowCenterObj2,resultA,input$Idf,input$input_threshold)
      centerResult
      
    })
    
    #' output the associated lift score
    
    output$centerAssociatedmeasures3 <-  renderPrint({
      x <-  prepareDataCenter()
      matr <- createInfluenceDistanceMatrix(x)
      result <- checkJaccardSimilarity(matr,input$jaccardValue)  
      count1 <- domain1CenterCount()
      count2 <- domain2CenterCount()
      
      res <- get_lift(windowCenterObj1,windowCenterObj2,result,count1,count2,input$Idf,input$input_threshold)
      res
      
    })
  })  
  
  
#   #' Monte Carlo Simulation Button
  observeEvent(input$simulate,{
  
    #function to combine code results form both the domain into 1
    prepareData <- function(){
      
      x <-  domain1text
      y <- domain2text
      
      combined_data <- rbind.data.frame(x,y)
      # combined_data <- unique(combined_data)
      return(combined_data)
    } 
    
    
    domain1Count <- function(){
      test <- domain1text
      test <- as.data.frame(test)
      anamolies <- test[1]
      anamolies$anamolies
      uni_ana <- anamolies[!duplicated(anamolies["anamolies"]),]
      uni_ana <- as.data.frame(uni_ana)
      count1 <- nrow(uni_ana)
      return(count1)
    }
    
    
    #' function to create objects from the obtained domain anamolies
    
    temp <- list()
    objectList_Domain1 <- list()
    objectList_Domain2 <- list()
    
    createDomain1Object <- function(domain1_anomalies){
      for(i in (1:nrow(domain1_anomalies))){
        d1_an <-  as.character(domain1_anomalies$anamolies)
        temp <- SpatialObject(d1_an[i])
        objectList_Domain1[[i]] <- temp 
        
      }
      return(objectList_Domain1)
    }
    
    createDomain2Object <- function(domain2_anomalies){
      
      for(i in (1:nrow(domain2_anomalies))){
        
        d2_an <-  as.character(domain2_anomalies$anamolies)
        temp <- SpatialObject(d2_an[i])
        objectList_Domain2[[i]] <- temp 
        
      }    
      return(objectList_Domain2)
    }
    
    
    #' Function to produce simulation with random edges

    MonteCarlosimulate <- function(domain1_anomalies,domain2_anomalies,IDmatrix,noOfReplicas,idf,threshold,jaccardvalue){
      x <- list()
      y <- list()
      d1_an <- list()
      d2_an <- list()
      combined_data <- list()
      matr <- list()
      result <- list()
      resultA <-list() 
      objectList_Domain1 <- list()
      objectList_Domain2 <- list()
      resultB <-list()
      resN <- list()
      s1 <- list()
      s2 <-  list()
      
      p <- 0
      count1 <-  domain1Count()
      
      for(i in (1:noOfReplicas)){
        
        s1 <- createDomain1Object(domain1_anomalies)
        s2 <- createDomain2Object(domain2_anomalies)
        
        result[[i]] <- apply(IDmatrix, c(1,2), function(x) sample(0:1,1)) #randomly reshuffle the edges of the matrix
        result[[i]] <- checkJaccardSimilarity(result[[i]],jaccardvalue) # calculate the jaccard simalarity and a
        
        resultB[[i]] <- get_confidence(s1,s2,result[[i]],count1,idf,threshold) 
        
      }
      
      dfDis <-  do.call("rbind",resultB)
      dfDis <- as.data.frame(dfDis)
      
      colnames(dfDis) <- "discovered"
      sortedDf <-  dfDis[order(dfDis$discovered),]
      
      p <- 0
      
      for(i in (1:length(sortedDf))){
        if( pairwiseconfidence >= sortedDf[[i]]){
          p <- i/noOfReplicas
          break
        }
      }
      p
    }
    
    output$pvalue <-  renderPrint({
      
      x <-  prepareData()
      matr <- createInfluenceDistanceMatrix(x)
      result <- checkJaccardSimilarity(matr,input$jaccardValue)
      
      res <- MonteCarlosimulate(domain1textPrim,domain2textPrim,result,input$no_of_replicas,input$Idf,input$input_threshold,input$jaccardValue)
      res      
    })
    
  })
  
  observeEvent(input$spacetime, {
    
    objectList_Domain1 <- list()
    d1_an <- list()
    
    createDomain1Object <- function(domain1_anomalies){
      for(i in (1:nrow(domain1_anomalies))){
        d1_an <-  as.character(domain1_anomalies$anamolies)
        temp <- SpatialObject(d1_an[i])
        objectList_Domain1[[i]] <- temp 
        
      }
      return(objectList_Domain1)
    }
    
    #' Function to split the obtained anomalies based on time and obtains significatnt portion
    #' of the windows where both domains have common intersecting anamolies for a specific time interval

    spatiotemporalSplit <-  function(domain1,domain2){
      
      d1case <- domain1
      d2case <- domain2
      
      d1case$domain <-  1
      d2case$domain <-  2
      
      merge_data <- rbind.data.frame(d1case,d2case)
      merge_data <-  merge_data[order(merge_data$time),]
      
      res <-  sqldf("select m1.time,m1.domain from merge_data m1
                    where m1.domain IN (1,2)
                    group by m1.time,m1.domain
                    ")
      
      re <- sqldf("select time from res where domain = '1' OR domain = '2' 
                  group by time
                  having count(*) > 1")
      
      finaloutPut <- sqldf("select anamolies,time,domain from merge_data 
                           where time IN (select time from res where domain = '1' OR domain = '2' 
                           group by time
                           having count(*) > 1)") 
      
      
      time_periods <- list()
      for ( j in unique(finaloutPut$time)){
        time_periods[[j]] <- subset(finaloutPut,finaloutPut$time == j)
      }
      
      return(time_periods)
    }
    
    #' caluclate influence score in each time period for respective anamolies
    
    calculateTemporalInfluence  <- function(spacetime){
      createsubsetD1 <- list()
      createsubsetD2 <- list()
      create1 <- list()
      create2 <-  list()
      combineData <- list()
      matr <- list()
      IDmatrix <- list()
      influenceScores <-  list()
      
      for(i in (1:length(spacetime))){
        createsubsetD1[[i]] <- subset(spacetime[[i]],spacetime[[i]]$domain == 1)  
        createsubsetD2[[i]] <- subset(spacetime[[i]],spacetime[[i]]$domain == 2)  
        
        
        #' calculating influence score for each time period
        create1[[i]] <-  createDomain1Object(createsubsetD1[[i]])
        create2[[i]] <-  createDomain1Object(createsubsetD2[[i]])
        
        #' forming the influence distance matrix for each time interval
        combineData[[i]] <- rbind.data.frame(createsubsetD1[[i]],createsubsetD2[[i]])
        matr[[i]] <- createInfluenceDistanceMatrix(combineData[[i]])
        IDmatrix[[i]] <- checkJaccardSimilarity(matr[[i]],input$jaccardValue)
        
        influenceScores[[i]] <- get_anamolous_influence_score(create1[[i]],create2[[i]],IDmatrix[[i]],input$Idf)
      }
      
      return(influenceScores)
      
    }
    
    
    output$spacetimeData <- renderPrint({
      significant_anamolies <- spatiotemporalSplit(domain1text,domain2text)
      significant_anamolies
    })
    
    output$spacetimeIScore <- renderPlot({
      significant_anamolies <- spatiotemporalSplit(domain1text,domain2text)
      
      y <-  calculateTemporalInfluence(significant_anamolies)
      
      
      d1case <- domain1text
      d2case <- domain2text
      
      d1case$domain <-  1
      d2case$domain <-  2
      
      merge_data <- rbind.data.frame(d1case,d2case)
      merge_data <-  merge_data[order(merge_data$time),]
      
      res <-  sqldf("select m1.time,m1.domain from merge_data m1
                    where m1.domain IN (1,2)
                    group by m1.time,m1.domain
                    ")
      
      re <- sqldf("select time from res where domain = '1' OR domain = '2' 
                  group by time
                  having count(*) > 1")
      
      
      
      result <- do.call("rbind",y)
      final_result <- cbind.data.frame(re,result)
      colnames(final_result) <- c("Time","Influence Score")
      
      ggplot(final_result, aes(x = final_result$Time , y = final_result$`Influence Score`,group = 1))+
        geom_point()+
        geom_line(color = "red")+
        labs(x = "Time Period" ,y = "Influence Score")+
        theme_bw()+
        ggtitle("Temporal Influence Trend Analysis")+
        theme(plot.title = element_text(color="#666666", face="bold", size=20, hjust=0.5))+
        theme(axis.title = element_text(color="#666666", face="bold", size=15))

    })
    
  })
  
  
  
  ##########################################################################################################################################################
  #' @description : It is advisiable to delete the created temp files after each satscan run
  
  observeEvent(input$resetfiles,{
    td = tempdir()
    file.remove(paste0(td,"/ANAlysis.prm"))
    file.remove(paste0(td,"/ANAlysis.cas"))
    file.remove(paste0(td,"/ANAlysis.geo"))
  })
  
  ##########################################################################################################################################################
  
}