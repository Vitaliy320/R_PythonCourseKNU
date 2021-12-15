ncol <- 5
nrow <- 10

#Task1
mat <- matrix(rnorm(50), ncol = ncol, nrow = nrow)

#Task2
for(col in 1:ncol){
  max(mat[,col])
}

#Task3
for(col in 1:ncol){
  mean(mat[,col])
}

#Task4
for(col in 1:ncol){
  min(mat[col,])
}

#Task5
for(col in 1:ncol){
  sort(mat[,col])
}

#Task6
find_zero_greater_count <- function(mat, ncol){
  for(col in 1:ncol){
    length(mat[,col][mat[,col]<0])
  }
}

#Task7
bool_arr <- vector()
for(col in 1:ncol){
  if(length(mat[,col][mat[,col]>2])){
    bool_arr<-c(bool_arr, TRUE)
  }
  else{
    bool_arr<-c(bool_arr, FALSE)
  }
}

#Task8
list1 <- list(observationA = c(1:5, 7:3),
              observationB =matrix(1:6, nrow=2))

lapply(list1, sum)

#Task9
lapply(list1, min)
lapply(list1, max)

sapply(list1, min)
sapply(list1, max)

#Task10
data <- InsectSprays

result <- aggregate(list(MeanCount=data$count), list(Spray=data$spray), mean)






















