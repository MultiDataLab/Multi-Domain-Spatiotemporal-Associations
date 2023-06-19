library(rsatscan)


###################################################################################################

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

###################################################################################################

#' sampling the case data 

randomSampleCaseData <- function(Dataset){
  CaseData <- Dataset
  CaseDataSample <-  CaseData[sample(1:nrow(CaseData),nrow(CaseData),replace = FALSE),]
  return (CaseDataSample)  
}

output <- list()

#' generate randomly sampled replicas
generateReplicats <- function(CaseDatasetInput,noOfReplicas){
  
  for(i in (1:noOfReplicas)){
    
    output[[i]] <-  randomSampleCaseData(CaseDatasetInput)
    
  }
  return(output)
}

domain2Case

domain1_anomalies <- generateReplicats(domain1text,10)
domain2_anomalies <- generateReplicats(domain2text,10)



###############################################################################################################

#' function to create objects from the obtained domain anamolies

temp <- list()

createDomain1Object <- function(domain1_anomalies,noOfReplicas){
  for(j in (1:noOfReplicas)){
    for(i in (1:nrow(domain1_anomalies[[j]]))){
      
      d1_an <-  as.character(domain1_anomalies[[j]]$anamolies)
      temp[[i]] <- SpatialObject(d1_an[i])
      objectList_Domain1[[j]] <- temp 
    
      }    
  }
  return(objectList_Domain1)
}



createDomain2Object <- function(domain2_anomalies,noOfReplicas){
  for(j in (1:noOfReplicas)){
    for(i in (1:nrow(domain2_anomalies[[j]]))){
      
      d2_an <-  as.character(domain2_anomalies[[j]]$anamolies)
      temp[[i]] <- SpatialObject(d2_an[i])
      objectList_Domain2[[j]] <- temp 
      
    }    
  }
  return(objectList_Domain2)
}

#################################################################################################################

#' function 
#'
#' 

MonteCarlosimulate <- function(domain1_anomalies,domain2_anomalies,noOfReplicas){
  x <- list()
  y <- list()
  d1_an <- list()
  d2_an <- list()
  combined_data <- list()
  matr <- list()
  result <- list()
  objectList_Domain1 <- list()
  objectList_Domain2 <- list()
  resultA <-list() 
  
  for(i in (1:noOfReplicas)){
    x[[i]] <- domain1_anomalies[[i]]
    y[[i]] <- domain2_anomalies[[i]]
    
    s1 <- createDomain1Object(domain1_anomalies,noOfReplicas)
    s2 <- createDomain2Object(domain2_anomalies,noOfReplicas)
    
    combined_data[[i]] <- rbind.data.frame(x[[i]],y[[i]])
    matr[[i]] <- createInfluenceDistanceMatrix(combined_data[[i]])
    result[[i]] <- checkJaccardSimilarity(matr[[i]],0.1)
    resultA[[i]] <- get_anamolous_influence_score(s1[[i]],s2[[i]],result[[i]],1)
    
  }
  return(resultA)
}

r <-  MonteCarlosimulate(domain1_anomalies,domain2_anomalies,10)




test <- domain2text
test <- as.data.frame(test)
anamolies <- test[1]
anamolies$anamolies
uni_ana <- anamolies[!duplicated(anamolies["anamolies"]),]
uni_ana <- as.data.frame(uni_ana)
nrow(uni_ana)