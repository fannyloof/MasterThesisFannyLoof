

############################
## Number of observations ##
############################
#load data
dat <- readRDS("R-objects/Scored_blocks_ver3.rds")
mode.noninvar <- readRDS("R-objects/modenonivariant.rds")
#Functions
source("Functions/MT_functions_Wrangel.R")
source("Functions/analysis_functions.R")


fulldata <- blockmerger(scoredlist = dat, c(2:14))
dim(fulldata)

## Set NA to obervations of Ptimms that were non invariant
index <- match(mode.noninvar, names(fulldata))

for (i in 1:length(index)){
  print(length(fulldata[fulldata$mode_adm == "Ptimss", index[i]]))
  fulldata[fulldata$mode_adm == "Ptimss",index[i]] <- NA
}

View(fulldata)

samplesize <- function(df){
  for (i in 6:length(df)){
    etimms <- df[df[i]$mode_adm=="Etimss",]
    ptimms <- df[df[i]$mode_adm=="Ptimss",]
    print(paste(table(etimms$ASBG01,etimms$Imm_Status),i, "Etimms"))
    print(paste(table(ptimms$ASBG01,ptimms$Imm_Status),i, "Ptimms"))
  }
  
  for (i in 6:length(df)){
    scored_block <- na.omit(df[i])
    print(table(scored_block$ASBG01,scored_block$Imm_Status))
  }
  
  for (i in 1:length(df)){
    remove_cols <- c("IDSTUD", "ASBG01", "Imm_Status", "mode_adm", "group")
    print(length(df[i][, !names(df[i]) %in% remove_cols]))
  }
}

samplesize <- function(df){
  # Corrected indexing in the for loop to start from column 6
  for (i in 6:ncol(df)){
    etimms <- df[df$mode_adm=="Etimss",]
    ptimms <- df[df$mode_adm=="Ptimss",]
    # Corrected indexing of dataframe columns
    print(paste(table(etimms[,c("ASBG01","Imm_Status")]), i, "Etimms"))
    print(paste(table(ptimms[,c("ASBG01","Imm_Status")]), i, "Ptimms"))
  }
  
  # Corrected indexing in the for loop to start from column 6
  for (i in 6:ncol(df)){
    # Corrected subsetting and removing NAs
    scored_block <- na.omit(df[,i, drop = FALSE])
    print(table(scored_block$ASBG01, scored_block$Imm_Status))
  }
  
  # Corrected indexing in the for loop to iterate through column names
  for (col_name in names(df)){
    remove_cols <- c("IDSTUD", "ASBG01", "Imm_Status", "mode_adm", "group")
    # Corrected removal of specified columns
    remaining_cols <- df[, !names(df) %in% remove_cols]
    print(length(remaining_cols))
  }
}

# Assuming fulldata is your dataframe
samplesize(fulldata)

samplesize(fulldata)
