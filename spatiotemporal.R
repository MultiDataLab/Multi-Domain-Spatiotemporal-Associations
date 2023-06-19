#' Finding confidence and support for each time interval window

library(sqldf)

d1case <- domain1text
d2case <- domain2text

d1case$domain <-  1
d2case$domain <-  2

merge_data <- rbind.data.frame(d1case,d2case)
merge_data <-  merge_data[order(merge_data$time),]

#' option 1 to subset based on time
l <- merge_data$time
split_data_byTime <-  split(merge_data,l)

sample <- split_data_byTime$`2002/1/1 to 2002/12/31`

createAn1 <- subset(sample,sample$domain == 1)
createAn2 <- subset(sample,sample$domain == 2)

obj1 <-  createDomain1Object(createAn1)
obj2 <- createDomain1Object(createAn2)


cmbdata <-  rbind.data.frame(createAn1,createAn2)
matr <- createInfluenceDistanceMatrix(cmbdata)
matrix1 <- checkJaccardSimilarity(matr,0.2)

is <-  get_anamolous_influence_score(obj1,obj2,matrix1,10)
