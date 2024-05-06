##################################################################
###                         Data wrangling                     ###
##################################################################


##############################
#Directory set to exam folder#
##############################



#Package to read in sas files
library("haven")
library("readxl")
library("tidyverse")
library(openxlsx)

#Functions
source("Functions/MT_functions_Wrangel.R")
source("Functions/MT_functions_statistics.R")

remove_obs <- function(df){
  df <- df[!(df$ASBG07==1 & df$ASBG06A==3 & df$ASBG06B==3),] # no information on parents
  df <- df[!(df$ASBG07==1 & df$ASBG06A==4 & df$ASBG06B==4),] # no information on parents
  df <- df[!(df$ASBG07==1 & is.na(df$ASBG06A) & is.na(df$ASBG06B)),] # no information on parents
  
  df <- df[!(is.na(df$ASBG07) & is.na(df$ASBG06A)),] # if we don't know the respondent's and on one of parents
  df <- df[!(is.na(df$ASBG07) & is.na(df$ASBG06B)),] # if we don't know the respondent's and on one of parents
  
  df <- df[!(is.na(df$ASBG07) & df$ASBG06A==3),]
  df <- df[!(is.na(df$ASBG07) & df$ASBG06A==4),]
  
  df <- df[!(is.na(df$ASBG07) & df$ASBG06B==3),]
  df <- df[!(is.na(df$ASBG07) & df$ASBG06B==4),]
  
  return(df)
}

Imm_evaluator <- function(df){
  df$Imm_Status <- ifelse(
    is.na(df$ASBG07) | is.na(df$ASBG06A+df$ASBG06B),
    1,
    ifelse(
      df$ASBG07 == 2 | sum(na.omit(df$ASBG06A+df$ASBG06B))== 4
      ,2,1
    )
  ) 
  return(df)
}

group_assingment <- function(df){
  for (i in 1:nrow(df)) {
    
    # Check if ASBG01 is 1 and Imm_Status is 1
    if (df[i, "ASBG01"] == 1 & df[i, "Imm_Status"] == 1) {
      df[i, "group"] <- 1
    } 
    # Check if ASBG01 is 2 and Imm_Status is 1
    else if (df[i, "ASBG01"] == 2 & df[i, "Imm_Status"] == 1) {
      df[i, "group"] <- 2
    } 
    # Check if ASBG01 is 1 and Imm_Status is 2
    else if (df[i, "ASBG01"] == 1 & df[i, "Imm_Status"] == 2) {
      df[i, "group"] <- 3
    } 
    # If none of the above conditions are met, assign group 4
    else {
      df[i, "group"] <- 4
    }
    
  }
  return(df)
}

remove_items <- function(df){
  removed_items <- c()
  for (item in 1:length(df)) {
    
    if (sum(is.na(df[, item])) == nrow(df)) {
      removed_items <- append(removed_items, colnames(df)[item])
    }
  }
  return(removed_items)
}


### Data (All paths checked)
## Achievement data
swe_achM7 <- read_sav("T19_G4_SWE_SPSS/asaswem7.sav") #seventh cycle
swe_achB7 <- read_sav("T19_G4_SWE_SPSS/asasweb7.sav") #Bridge data, not the same students, equivalent sample

## Student Questionnaire
swe_questionnaireM7 <- read_sav("T19_G4_SWE_SPSS/asgswem7.sav")
swe_questionnaireB7 <- read_sav("T19_G4_SWE_SPSS/asgsweb7.sav")

## Codebooks
codebook_M7 <- read_excel(path = "CODEBOOKS/Fanny_codebook.xlsx", sheet = "SCI_Etimss")
codebook_B7 <- read_excel(path = "CODEBOOKS/Fanny_codebook.xlsx", sheet = "SCI_B7")

## Derived items:
### Items scored based on responses on other items
### Ref Sample design in Timss 2019 ch 10, appF
### To be removed
derived_itemsEtimss <- paste0("SE",c("61083", "61142A", "51138Z", "61124", "61160", "71009",
                         "71106", "71254","51151","711063","71114","71017","71902",
                         "71041", "71046", "61116"))

derived_itemsPtimss <- paste0("SP",c("61083", "61142A", "51138Z","61124","61116"))

derived_itemrm <- c(derived_itemsEtimss,derived_itemsPtimss)




################################
##   Program                  ##
##   Immigration/Gender       ##
################################


##Block_name ^SE = etimss
M7_Swe <- scienceitems(data = swe_achM7, block_start = "^SE")
B7_Swe <- scienceitems(data = swe_achB7, block_start = "^SP")

#Removing derived items
M7_Swe <- M7_Swe[,!names(M7_Swe) %in% derived_itemsEtimss]
B7_Swe <- B7_Swe[,!names(B7_Swe) %in% derived_itemsPtimss]

#Subset answers on student questionnaire
GenIm_SweM7 <- genderimmigration(swe_questionnaireM7)
GenIm_SweB7 <- genderimmigration(swe_questionnaireB7)

#merge science items with context variables
B7_SWE <- merge(B7_Swe, GenIm_SweB7, by = "IDSTUD")
M7_SWE <- merge(M7_Swe, GenIm_SweM7, by = "IDSTUD")


## all who answered that they were born in the country but dont know about their parents are removed


B7_SWE <- remove_obs(df = B7_SWE)


M7_SWE <- remove_obs(df = M7_SWE)

## asbg01 gender == NA are removed.
B7_SWE <- B7_SWE[!is.na(B7_SWE$ASBG01),]

## asbg01 gender == NA are removed.
M7_SWE <- M7_SWE[!is.na(M7_SWE$ASBG01),]




## separate do diff df to create variables
B7contex_df <- B7_SWE[,c("IDSTUD","ASBG01","ASBG06A","ASBG06B","ASBG07")]
M7contex_df <- M7_SWE[,c("IDSTUD","ASBG01","ASBG06A","ASBG06B","ASBG07")]

## create empty imm_status vector
B7contex_df$Imm_Status <- rep(NA,nrow(B7_SWE))
M7contex_df$Imm_Status <- rep(NA,nrow(M7_SWE))

### Assign value to imm_status vector based on scoring rule (not born in country or both parents == 2)
B7contex_df <- Imm_evaluator(df = B7contex_df)
M7contex_df <- Imm_evaluator(df = M7contex_df)

## TEST:
length(!is.na(B7contex_df$Imm_Status)) == nrow(B7contex_df)
length(!is.na(M7contex_df$Imm_Status)) == nrow(M7contex_df)

### create empty groupvector
B7contex_df$group <- rep(NA, nrow(B7contex_df))
M7contex_df$group <- rep(NA, nrow(M7contex_df))

## Assign individuals to intersectional groups
B7contex_df <- group_assingment(B7contex_df)
M7contex_df <- group_assingment(M7contex_df)

## TEST
length(!is.na(B7contex_df$group)) == nrow(B7contex_df)
length(!is.na(M7contex_df$group)) == nrow(M7contex_df)


## Merge back together again
B7_SWE <- merge(B7_SWE, B7contex_df, by= c("IDSTUD","ASBG01","ASBG06A", "ASBG06B", "ASBG07"))
M7_SWE <- merge(M7_SWE, M7contex_df, by= c("IDSTUD","ASBG01","ASBG06A", "ASBG06B", "ASBG07"))

## TEST:
length(!is.na(B7_SWE$Imm_Status)) == nrow(B7contex_df)
length(!is.na(M7_SWE$Imm_Status)) == nrow(M7contex_df)


#### Remove items that were removed from the scale: ref user guide IEA
## etimss

block_rmE <- c()
reasonE <- c()


B7removed_items <- remove_items(df=B7_SWE)
M7removed_items <- remove_items(df=M7_SWE)

M7_SWE_rmItems <- M7_SWE[,!names(M7_SWE) %in% M7removed_items]
B7_SWE_rmItems <- B7_SWE[,!names(B7_SWE) %in% B7removed_items]
## test: Derived items removed
table(names(M7_SWE_rmItems) %in% derived_itemsEtimss) #Correct: FALSE
table(names(B7_SWE_rmItems) %in% derived_itemsPtimss) #Correct: FALSE


## create blocklist


BlockM7 <- block_creater_1(df = M7_SWE_rmItems, MODE = "E", removed_items = c(M7removed_items,derived_itemsEtimss))
BlockB7 <- block_creater_1(df = B7_SWE_rmItems, MODE = "P", removed_items = c(B7removed_items,derived_itemsPtimss))



## create a variable that codes for Etimss and Ptimss
for (i in 1:length(BlockM7)){
  BlockM7[[i]]$mode_adm <- rep("Etimss", length(BlockM7[[i]]$IDSTUD)) 
  
}

for (i in 1:length(BlockB7)){
  BlockB7[[i]]$mode_adm <- rep("Ptimss", length(BlockB7[[i]]$IDSTUD))
}

## TEST
for (i in 1:length(BlockM7)){
  print(length(BlockM7[[i]]$mode_adm) == nrow(BlockM7[[i]]))
}
for (i in 1:length(BlockB7)){
  print(length(BlockB7[[i]]$mode_adm) == nrow(BlockB7[[i]]))
}




###  Replace names of vector SE and SP

### So that items have the same name in both lists

# Etimss
for (i in 1:14){
  names(BlockM7[[i]]) <- sub("SE", "S",
                             names(BlockM7[[i]]))
}

names(BlockM7) <- sub("SE", "S",
                           names(BlockM7))
# PaperTimss
for (i in 1:8){
  names(BlockB7[[i]]) <- sub("SP", "S", 
                             names(BlockB7[[i]]))
}

names(BlockB7) <- sub("SP", "S", 
                      names(BlockB7))

## TEST
for (i in 1:14){
  print(names(BlockM7[[i]]))
}

for (i in 1:length(BlockB7)){
  print(names(BlockB7[[i]]))
}


# Create an empty list to store the combined data frames
result_1 <- list()

#Create empty vector for items that do not match across modes of administration
different_columns <- c()

# Iterate through the names in Block_ListM7

for (name in names(BlockM7)) {
  if (name %in% names(BlockB7)) {
    # Check if the data frames have the same number of columns
    if (ncol(BlockM7[[name]]) == ncol(BlockB7[[name]])) {
      # Combine data frames with the same number of columns
      combined_df <- rbind(BlockM7[[name]], BlockB7[[name]])
      result_1[[name]] <- combined_df
    } else {
      # Handle the case where data frames have different numbers of columns
      # You can choose to do something specific here, like displaying an error message
      cat("Data frames for", name, "have different numbers of columns\n")
      
      different_columns <- append(different_columns,setdiff(names(BlockM7[[name]]), names(BlockB7[[name]])))
      common_columns <- intersect(names(BlockM7[[name]]), names(BlockB7[[name]]))
      # Create new data frames with only the common columns
      combined_df <- rbind(BlockM7[[name]][common_columns], BlockB7[[name]][common_columns])
      result_1[[name]] <- combined_df
      # To handle it differently, you can assign one of the data frames or create an empty data frame, depending on your requirements.
      # result[[name]] <- Block_ListM7[[name]] # Or any other appropriate action
    }
  } else {
    result_1[[name]] <- BlockM7[[name]]
  }
}





###################
## Create objects #
###################

# rm_items <- data.frame(item = c(derived_itemrm,different_columns,M7removed_items,B7removed_items), reason = c(rep("derived items", length(derived_itemrm)),rep("unmatched E/P",length(different_columns)),rep("NA ref user guide", length(M7removed_items) + length(B7removed_items))))

###################
## Save objects   #
###################

#write.xlsx(rm_items, file = "Results/removed_items_mars.xlsx")
#saveRDS(result_1, file = "R-objects/All_blocks_ver3_mars.rds")

