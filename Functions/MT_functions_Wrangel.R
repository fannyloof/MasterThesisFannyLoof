
#######################################
### This script contains functions ####
### to get to know the data        ####
#######################################
library(dplyr)

### Used in scoring program ####
## Based on scoring rule in TIMSS: An item is considered not reached when the item itself and the item immediatly preceeding it are not answered
## and there are no other items completed on that block ("part of that booklet")

### Fanny's version
set_last_2_NA <- function(df){
  for (row in 1:nrow(df)) {
    for (response in 2:ncol(df)) {
      if (!is.na(df[row, response]) & df[row, response] == 0) {
        if (!is.na(df[row, response - 1]) & df[row, response - 1] == 0) {
          if (sum(df[row, response:ncol(df)], na.rm = TRUE) == 0) {
            df[row, c(response-1,response:ncol(df))] <- NA #if you want to change so that the preceeding zero is/isnt NA, here's the row to do that!
          } else {
            df[row, response] <- 0
          }
        } else {
          df[row, response] <- 0
        }
      }
    }
  }
  return(df)
}

### Nicolai's version
set_last_to_na <- function(row){
  # function reads a vector from right to left and sets 0 valuee  
  # from the right to NA if subsequent element to the left is also 0
  
  val <- 0 # creating a conditional value
  i <- length(row) # number of elements in the row
  mem <- c() # empty vector to store index of 0 values to set to NA
  while(val == 0 & i > 0){ # loop reads each value of vector row
    if(row[i] == 0){
      mem <- append(mem,i) # storing index of zero value
    } else {
      val <- 1 # breaking while loop
    }
    i <- i - 1 
  }
  if (length(mem) > 1){ #only sets values to NA if there are more than one 0 in a row
    row[mem] <- NA #using the stored indexes in vector mem to set values to NA in row vector
  }
  return(row) #returns updated row vector
}


###variable creating ####
immigration_fun <- function(df,v.e,v1, v2, v3){
  for(i in 1:length(v.e)){
    if (sum(v1[i],v2[i],v3[i],na.rm = T) > 5) {
      #print("I'm here")
      v.e[i] <- 2
    }
    else {
      #print("Not here")
      v.e[i] <- 1
    }
  }
  Imm_Stat <- v.e
  df <- cbind(df, Imm_Stat)
  return(df)
}

scienceitems <- function(data, block_start){
  ######### Function takes input data, context_data, block_start. 
  ######### Function creates a dataframe with only science items (paper or digital), 
  

  science_items <- data[,c( "IDSTUD", 
                            grep(block_start, 
                                 names(data), 
                                 value = T))] #science items introduced both in 2019 and 2015
  return(science_items)
}  

genderimmigration <- function(context_data){
#variables of interest
  gender <- data.frame(ASBG01 = context_data$ASBG01, IDSTUD = context_data$IDSTUD)
  
  immigration <- data.frame(ASBG06A = context_data$ASBG06A, 
                            ASBG06B = context_data$ASBG06B, 
                            ASBG07 = context_data$ASBG07, 
                            IDSTUD = context_data$IDSTUD)
  gender.immigration <- merge(gender,immigration, by = "IDSTUD")
  
  return(gender.immigration)
}

genderSES <- function(context_data){
  #variables of interest
  gender <- data.frame(ASBG01 = context_data$ASBG01, IDSTUD = context_data$IDSTUD)
  
  SES <- data.frame(ASBG04 = context_data$ASBG04, 
                            IDSTUD = context_data$IDSTUD)
  gender.SES <- merge(gender,SES, by = "IDSTUD")
  
  return(gender.SES)
}

merge_sciencecontext <- function(data, context_data){
  #merge w science items
  science_items.contextVar <- merge(data,context_data, by = "IDSTUD")
  
  
  
  return(science_items.contextVar)
} 

immigration_variable <- function(merged_data){
  ## Creating a immigration variable, 
  #value 1 if student have no immigration background, 
  #value 2 if student immigrated herself (ref item: ASBG07) or if either parents immigrated (ref items: ASBG06A/B)
  
  Imm_Stat <- rep(NA,dim(merged_data)[1])
  
  merged_data <- immigration_fun(df = merged_data, 
                                 v.e = Imm_Stat, 
                                 v1 = merged_data$ASBG07, 
                                 v2 = merged_data$ASBG06A, 
                                 v3 = merged_data$ASBG06B)  
  
  
  #return to object
  return(merged_data)
}



SES_variable <- function(merged_data){
    merged_data$SES_level <- ifelse(merged_data$ASBG04 > 3, 3,
                                    ifelse(merged_data$ASBG04 == 3, 2, 1))
    return(merged_data)
  }
  
  # Example usage:
  # merged_data <- data.frame(ASBG04 = c(2, 3, NA, 4, 1))
  # result <- SES_variable(merged_data)
  # print(result)
  



blockmerger <- function(scoredlist, blocks2merge){
  names2merge <- names(scoredlist[[1]][1:5])
  
  df <- scoredlist[[1]]
  for (block in blocks2merge){
    
    block2merge <- scoredlist[[block]]
    df <- merge(df, block2merge, by = names2merge, all = TRUE)
    
  }
  
  return(df)
}

## remove rows with all NA
remove_rows_all_na <- function(data, start_col, end_col) {
  # Select the specified range of columns
  selected_cols <- data[, start_col:end_col]
  
  # Find rows where all values are NA
  rows_all_na <- apply(selected_cols, 1, function(x) all(is.na(x)))
  
  # Remove rows with all NA values
  cleaned_data <- data[!rows_all_na, ]
  
  return(cleaned_data)
}

