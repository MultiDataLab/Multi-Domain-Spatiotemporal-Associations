
#' Function to split the obtained anomalies based on time and obtains significatnt portion
#' of the windows where both domains have common intersecting anamolies for a specific time interval


library(sqldf)
library(dplyr)
library(igraph)


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

x <- spatiotemporalSplit(domain1text,domain2text)


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
    IDmatrix[[i]] <- checkJaccardSimilarity(matr[[i]],0.4)
    
    influenceScores[[i]] <- get_anamolous_influence_score(create1[[i]],create2[[i]],IDmatrix[[i]],10)
  }
  
  return(influenceScores)
  
}


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



y <-  calculateTemporalInfluence(x)


result <- do.call("rbind",y)
final_result <- cbind.data.frame(re,result)
colnames(final_result) <- c("Time","Influence Score")
final_result


ggplot(final_result, aes(x = final_result$Time , y = final_result$`Influence Score`,group = 1))+
  geom_point()+
  geom_line(color = "red")+
  labs(x = "Time Period" ,y = "Influence Score")+
  theme_bw()+
  ggtitle("Temporal Influence Trend Analysis")+
  theme(plot.title = element_text(color="#666666", face="bold", size=20, hjust=0.5))+
  theme(axis.title = element_text(color="#666666", face="bold", size=15))
  



