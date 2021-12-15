read_file <- function(directory, file_id){
  file_name <- paste(file_id)
  
  while(nchar(file_name) < 3){
    file_name <- paste(0, file_name, sep = "")
  }
  file_name <- paste(file_name, ".csv", sep = "")
  
  file_path <- file.path(directory, file_name, fsep = .Platform$file.sep)
  
  read_data <- read.csv(file_path)
  
  return(read_data)
}

pmean <- function(directory, pollutant, id=1:332){
  combine_vector <- vector()
  
  for(file_id in id){
    
    read_data <- read_file(directory, file_id)
    
    combine_vector <- c(combine_vector, read_data[[pollutant]][!is.na(read_data[[pollutant]])])
  }
  
  return(mean(combine_vector))
}

complete <- function(directory, id=1:332){
  result <- data.frame(matrix(ncol = 2, nrow = 0))
  
  for(file_id in id){
    read_data <- read_file(directory, file_id)
    
    new_row <- c(file_id, nrow(read_data[!is.na(read_data$Date) & !is.na(read_data$sulfate) & !is.na(read_data$nitrate),]))
    
    result <- rbind(result, new_row)
  }
  
  col_names <- c("file_id", "number_of_completely_observed_cases")
  colnames(result) <- col_names
  
  return(result)
}


corr <- function(directory, treshhold=0){
  sulfates <- vector()
  nitrates <- vector()
  
  result <- vector()
  
  for(file_id in 1:332){
    if(complete(directory, file_id)[[2]][1] > treshhold){
      read_data <- read_file(directory, file_id)
      filled_row <- read_data[!is.na(read_data$Date) & !is.na(read_data$sulfate) & !is.na(read_data$nitrate),]
      sulfates <- filled_row$sulfate
      nitrates <- filled_row$nitrate
      
      result <- c(result, cor(sulfates, nitrates))
    }
  }
  
  return(result)
}


