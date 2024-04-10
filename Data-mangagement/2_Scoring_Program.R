

#####################
## Scoring program ##
#####################

library(haven)
library(readxl)
library(tidyverse)
library(dplyr)
library(openxlsx)

source("Functions/MT_functions_Wrangel.R") ##treatment of NA's

## Load data ##
result <- readRDS("R-objects/All_blocks_ver3_mars.rds")
codebook_M7 <- read_excel(path = "CODEBOOKS/Fanny_codebook.xlsx", sheet = "SCI_Etimss")

## Get scoring code
codebook_Scoring <- read_excel(path = "CODEBOOKS/Fanny_codebook.xlsx", sheet = "ResponseCode")

### only for science items
codebook_Scoring <- codebook_Scoring[grepl("^SE", codebook_Scoring$Variable), ]

#####################
### Identification
### of PS - items 
#####################

variables_with_partial_response <- codebook_Scoring$Variable[grep("Partially Correct Response", codebook_Scoring$`Value Scheme Detailed`)]

data <- result

codebook_Scoring <- read_excel(path = "CODEBOOKS/Fanny_codebook.xlsx", sheet = "ResponseCode")

codebook_Scoring <- codebook_Scoring[grepl("^SE", codebook_Scoring$Variable), ]
variables_with_partial_response <- codebook_Scoring$Variable[grep("Partially Correct Response", codebook_Scoring$`Value Scheme Detailed`)]

variables_with_partial_response <- gsub("SE", "S",
                                        variables_with_partial_response)

rm_items <- c() ## for saving removed items

for (b in 1:length(data)) {
  block <- data[[b]]
  partial_presence <- unique(names(block) %in% variables_with_partial_response)
  
  # Check if any partially scored items are present
  
  if (any(partial_presence)) {
    index <- which(names(block) %in% variables_with_partial_response) # Get indices of partially scored items
    
    itemname <- names(block)[index] # Names of partially scored items
    rm_items <- append(rm_items,itemname)
    # Remove partially scored items from the result list
    result[[b]] <- result[[b]][, -which(names(result[[b]]) %in% itemname)]
    
    print(paste(itemname, "in block", b, "is partially scored and has been removed from list"))
  } else {
    print(paste("No partially scored items present in block", b))
  }
}



### 👌⁉️
### to check whether items have been removed properly, run following :

### names(result[[blocknumber]])

#### output should not be of length 0


## All blocks gathered in one list
scored_blocks <- list()

## Create vector of block names
block_names <- unique(codebook_M7$Block)

#####################
### Function start ##
#####################

ITEMNAME <- 1
KEYCOLUMN <- 16

## changed names
# S01 = block
# S01_lon = block_lon
score_program <- function(){
  
  for (i in 1:length(block_names)){
    
    block <- result[[i]]
    
    

    n <- ncol(block)
    

    codebook <- codebook_M7[codebook_M7$Block==block_names[i],]
    itemscore <- codebook[,c(ITEMNAME,KEYCOLUMN)]
    
    itemscore$`Item ID` <- sub("SE", "S",itemscore$`Item ID`)  ## Change Item ID in CODEBOOKdf so that it matches that of item id in SE01_lon
    
    ## Change KEY from CODEBOOKdf to be easier to code
    itemscore <- itemscore %>%
      mutate(Key = recode(Key,
      "A" = 1,
      "B" = 2,
      "C" = 3,
      "D" = 4,
      .default = 1))
    
   


    ## Make block  long data
    items <- names(block[5:(n-1)]) ## grab only itemcolums

    block_lon <- block %>%
      pivot_longer(cols = items,
                   names_to = "Item ID")
    


    ## Merge key w long data
    block_lon <- merge(block_lon, itemscore)



    ## Score item answers according to key
    block_lon$value <- as.integer(block_lon$value)


    l <- nrow(block_lon)
    for (row in 1:l){
      if (block_lon$value[row] == 70){ #incorrect
        block_lon$value[row] = 0
      } else if (block_lon$value[row] == 79){ #incorrect
        block_lon$value[row] = 0
      } else if (block_lon$value[row] == block_lon$Key[row]){ # MC-question correct
        block_lon$value[row] = 1
      } else if (block_lon$value[row] == 10){ #correct
        block_lon$value[row] = 1
      } else if (block_lon$value[row] == 11){ #correct
        block_lon$value[row] = 1
      } else{
        block_lon$value[row] = 0 #incorrect
      }

    }


    block_wide <- block_lon %>%
      pivot_wider(id_cols = c(IDSTUD, ASBG01, Imm_Status, mode_adm, group),
                  names_from = 'Item ID',
                  values_from = value)
    
  



    ## Na-treatment, Omitted = 0, Not Reached = NA
    ### 2 NA in a row in the end, left untreated
    ### 1 NA anywhere = score 0

    exclude_columns <- c("IDSTUD", "ASBG01", "Imm_Status", "mode_adm", "group")
    items <- setdiff(names(block_wide), exclude_columns)


    ## Scoring function used
    block_wide[,items] <- set_last_2_NA(df=block_wide[,items]) #setting subsequent 0 values to NA
    block_wide <- as.data.frame(block_wide)
    


    scored_blocks[[i]] <- block_wide

  }
  # 
  # 
  # 
  return(scored_blocks)
  # 
  # 
  # 
  
  
}

scored_blocks <- score_program()

########################
##     update/save    ## 
##       objects      ##
########################

rem_items <- read_xlsx("Results/removed_items_mars.xlsx")
partiallyscored_items <- data.frame(item = rm_items, reason = rep("partially scored item", length(rm_items)))
rem_items <- rbind(rem_items,partiallyscored_items)

write.xlsx(rem_items, "Results/removed_items_mars.xlsx")

saveRDS(scored_blocks, file = "R-objects/Scored_blocks_ver3.rds")

# For sample size reporting -----------------------------------------------









