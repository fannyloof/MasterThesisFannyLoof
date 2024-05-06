

#######################################
### This script contains functions ####
### to get descriptives/statistics ####
#######################################

block_creater_1 <- function(df, MODE, removed_items){ # USED IN: 1,
  block_list <- list()
  if (MODE == "E"){
    blocks <- unique(codebook_M7$Block)
    
    block_list[["SE01"]] <- na.omit(df[,c("IDSTUD","Imm_Status", "ASBG01", "group",codebook_M7$`Item ID`[codebook_M7$Block == blocks[1]][!codebook_M7$`Item ID`[codebook_M7$Block == blocks[1]] %in% removed_items])])
    block_list[["SE02"]] <- na.omit(df[,c("IDSTUD","Imm_Status", "ASBG01", "group",codebook_M7$`Item ID`[codebook_M7$Block == blocks[2]][!codebook_M7$`Item ID`[codebook_M7$Block == blocks[2]] %in% removed_items])])
    block_list[["SE03"]] <- na.omit(df[,c("IDSTUD","Imm_Status", "ASBG01", "group",codebook_M7$`Item ID`[codebook_M7$Block == blocks[3]][!codebook_M7$`Item ID`[codebook_M7$Block == blocks[3]] %in% removed_items])])
    block_list[["SE04"]] <- na.omit(df[,c("IDSTUD","Imm_Status", "ASBG01", "group",codebook_M7$`Item ID`[codebook_M7$Block == blocks[4]][!codebook_M7$`Item ID`[codebook_M7$Block == blocks[4]] %in% removed_items])])
    block_list[["SE05"]] <- na.omit(df[,c("IDSTUD","Imm_Status", "ASBG01", "group",codebook_M7$`Item ID`[codebook_M7$Block == blocks[5]][!codebook_M7$`Item ID`[codebook_M7$Block == blocks[5]] %in% removed_items])])
    block_list[["SE06"]] <- na.omit(df[,c("IDSTUD","Imm_Status", "ASBG01", "group",codebook_M7$`Item ID`[codebook_M7$Block == blocks[6]][!codebook_M7$`Item ID`[codebook_M7$Block == blocks[6]] %in% removed_items])])
    block_list[["SE07"]] <- na.omit(df[,c("IDSTUD","Imm_Status", "ASBG01", "group",codebook_M7$`Item ID`[codebook_M7$Block == blocks[7]][!codebook_M7$`Item ID`[codebook_M7$Block == blocks[7]] %in% removed_items])])
    block_list[["SE08"]] <- na.omit(df[,c("IDSTUD","Imm_Status", "ASBG01", "group",codebook_M7$`Item ID`[codebook_M7$Block == blocks[8]][!codebook_M7$`Item ID`[codebook_M7$Block == blocks[8]] %in% removed_items])])
    block_list[["SE09"]] <- na.omit(df[,c("IDSTUD","Imm_Status", "ASBG01", "group",codebook_M7$`Item ID`[codebook_M7$Block == blocks[9]][!codebook_M7$`Item ID`[codebook_M7$Block == blocks[9]] %in% removed_items])])
    block_list[["SE10"]] <- na.omit(df[,c("IDSTUD","Imm_Status", "ASBG01", "group",codebook_M7$`Item ID`[codebook_M7$Block == blocks[10]][!codebook_M7$`Item ID`[codebook_M7$Block == blocks[10]] %in% removed_items])])
    block_list[["SE11"]] <- na.omit(df[,c("IDSTUD","Imm_Status", "ASBG01", "group",codebook_M7$`Item ID`[codebook_M7$Block == blocks[11]][!codebook_M7$`Item ID`[codebook_M7$Block == blocks[11]] %in% removed_items])])
    block_list[["SE12"]] <- na.omit(df[,c("IDSTUD","Imm_Status", "ASBG01", "group",codebook_M7$`Item ID`[codebook_M7$Block == blocks[12]][!codebook_M7$`Item ID`[codebook_M7$Block == blocks[12]] %in% removed_items])])
    block_list[["SE13"]] <- na.omit(df[,c("IDSTUD","Imm_Status", "ASBG01", "group",codebook_M7$`Item ID`[codebook_M7$Block == blocks[13]][!codebook_M7$`Item ID`[codebook_M7$Block == blocks[13]] %in% removed_items])])
    block_list[["SE14"]] <- na.omit(df[,c("IDSTUD","Imm_Status", "ASBG01", "group",codebook_M7$`Item ID`[codebook_M7$Block == blocks[14]][!codebook_M7$`Item ID`[codebook_M7$Block == blocks[14]] %in% removed_items])])
    
  } else{
    blocks <- unique(codebook_B7$Block)
    block_list[["SP01"]] <- na.omit(df[,c("IDSTUD","Imm_Status", "ASBG01", "group",codebook_B7$`Item ID`[codebook_B7$Block == blocks[1]][!codebook_B7$`Item ID`[codebook_B7$Block == blocks[1]] %in% removed_items])])
    block_list[["SP03"]] <- na.omit(df[,c("IDSTUD","Imm_Status", "ASBG01", "group",codebook_B7$`Item ID`[codebook_B7$Block == blocks[2]][!codebook_B7$`Item ID`[codebook_B7$Block == blocks[2]] %in% removed_items])])
    block_list[["SP05"]] <- na.omit(df[,c("IDSTUD","Imm_Status", "ASBG01", "group",codebook_B7$`Item ID`[codebook_B7$Block == blocks[3]][!codebook_B7$`Item ID`[codebook_B7$Block == blocks[3]] %in% removed_items])])
    block_list[["SP06"]] <- na.omit(df[,c("IDSTUD","Imm_Status", "ASBG01", "group",codebook_B7$`Item ID`[codebook_B7$Block == blocks[4]][!codebook_B7$`Item ID`[codebook_B7$Block == blocks[4]] %in% removed_items])])
    block_list[["SP07"]] <- na.omit(df[,c("IDSTUD","Imm_Status", "ASBG01", "group",codebook_B7$`Item ID`[codebook_B7$Block == blocks[5]][!codebook_B7$`Item ID`[codebook_B7$Block == blocks[5]] %in% removed_items])])
    block_list[["SP09"]] <- na.omit(df[,c("IDSTUD","Imm_Status", "ASBG01", "group",codebook_B7$`Item ID`[codebook_B7$Block == blocks[6]][!codebook_B7$`Item ID`[codebook_B7$Block == blocks[6]] %in% removed_items])])
    block_list[["SP11"]] <- na.omit(df[,c("IDSTUD","Imm_Status", "ASBG01", "group",codebook_B7$`Item ID`[codebook_B7$Block == blocks[7]][!codebook_B7$`Item ID`[codebook_B7$Block == blocks[7]] %in% removed_items])])
    block_list[["SP13"]] <- na.omit(df[,c("IDSTUD","Imm_Status", "ASBG01", "group",codebook_B7$`Item ID`[codebook_B7$Block == blocks[8]][!codebook_B7$`Item ID`[codebook_B7$Block == blocks[8]] %in% removed_items])])
    
  }
    
  return(block_list)
}

