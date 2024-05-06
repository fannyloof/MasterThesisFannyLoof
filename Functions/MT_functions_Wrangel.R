
#######################################
### This script contains functions ####
### to get to know the data        ####
#######################################
library(dplyr)

### Used in scoring program ####
## Based on scoring rule in TIMSS: An item is considered not reached when the item itself and the item immediatly preceeding it are not answered
## and there are no other items completed on that block ("part of that booklet")

### Fanny's version
set_last_2_NA <- function(df){ # USED IN: 2, 
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




scienceitems <- function(data, block_start){ # USED IN: 1, 
  ######### Function takes input data, context_data, block_start. 
  ######### Function creates a dataframe with only science items (paper or digital), 
  

  science_items <- data[,c( "IDSTUD", 
                            grep(block_start, 
                                 names(data), 
                                 value = T))] #science items introduced both in 2019 and 2015
  return(science_items)
}  

genderimmigration <- function(context_data){ # USED IN: 1, 
#variables of interest
  gender <- data.frame(ASBG01 = context_data$ASBG01, IDSTUD = context_data$IDSTUD)
  
  immigration <- data.frame(ASBG06A = context_data$ASBG06A, 
                            ASBG06B = context_data$ASBG06B, 
                            ASBG07 = context_data$ASBG07, 
                            IDSTUD = context_data$IDSTUD)
  gender.immigration <- merge(gender,immigration, by = "IDSTUD")
  
  return(gender.immigration)
}









  
  # Example usage:
  # merged_data <- data.frame(ASBG04 = c(2, 3, NA, 4, 1))
  # result <- SES_variable(merged_data)
  # print(result)
  



blockmerger <- function(scoredlist, blocks2merge){ # USED IN: 4, 5, 7, 7PUR, itemstats
  names2merge <- names(scoredlist[[1]][1:5])
  
  df <- scoredlist[[1]]
  for (block in blocks2merge){
    
    block2merge <- scoredlist[[block]]
    df <- merge(df, block2merge, by = names2merge, all = TRUE)
    
  }
  
  return(df)
}

resp_category <- function(list){ # USED IN: 5
  remove_item <- c()
  for (i in 3:length(list)){
    df <- list[[i]]
    
    for (i in 1:length(df)){
      index = sum(df[i], na.rm = T)/(nrow(df) - sum(is.na(df[i])))
      
      if (index == 1 | index == 0){
        remove_item <- append(remove_item, colnames(df)[i], after = length(remove_item))
      } else {
        remove_item <- remove_item
      }
      
    }
    
  }
  return(remove_item)
}

