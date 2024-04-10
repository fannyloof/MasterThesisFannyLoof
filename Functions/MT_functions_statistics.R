

#######################################
### This script contains functions ####
### to get descriptives/statistics ####
#######################################

block_creater_1 <- function(df, MODE, removed_items){
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

###################
## OLD FUNCTIONS ##
###################


addBlockName <- function(data_input, mode_adm = "Etimss") {
  ####Takes wide dataset and connects student responses to a blockname
  ####Compare to: https://timssandpirls.bc.edu/timss2019/frameworks/framework-chapters/assessment-design/student-booklet-design/#footnote
  N <- length(data_input$IDSTUD)
  
  if (mode_adm == "Etimss"){
    print("M7")
    data_input$Block_name1 <- NA
    data_input$Block_name2 <- NA
    
    
    
    for (i in 1:N) {
      if (data_input[i, "Booklet"] == 1) {
        data_input[i, "Block_name1"] <- "SE01"
        data_input[i, "Block_name2"] <- "SE02"
      } else if (data_input[i, "Booklet"] == 2) {
        data_input[i, "Block_name1"] <- "SE02"
        data_input[i, "Block_name2"] <- "SE02"
      } else if (data_input[i, "Booklet"] == 3) {
        data_input[i, "Block_name1"] <- "SE03"
        data_input[i, "Block_name2"] <- "SE04"
      } else if (data_input[i, "Booklet"] == 4) {
        data_input[i, "Block_name1"] <- "SE04"
        data_input[i, "Block_name2"] <- "SE05"
      } else if (data_input[i, "Booklet"] == 5) {
        data_input[i, "Block_name1"] <- "SE05"
        data_input[i, "Block_name2"] <- "SE06"
      } else if (data_input[i, "Booklet"] == 6) {
        data_input[i, "Block_name1"] <- "SE07"
        data_input[i, "Block_name2"] <- "SE07"
      } else if (data_input[i, "Booklet"] == 7) {
        data_input[i, "Block_name1"] <- "SE07"
        data_input[i, "Block_name2"] <- "SE08"
      } else if (data_input[i, "Booklet"] == 8) {
        data_input[i, "Block_name1"] <- "SE08"
        data_input[i, "Block_name2"] <- "SE09"
      } else if (data_input[i, "Booklet"] == 9) {
        data_input[i, "Block_name1"] <- "SE09"
        data_input[i, "Block_name2"] <- "SE10"
      } else if (data_input[i, "Booklet"] == 10) {
        data_input[i, "Block_name1"] <- "SE10"
        data_input[i, "Block_name2"] <- "SE11"
      } else if (data_input[i, "Booklet"] == 11) {
        data_input[i, "Block_name1"] <- "SE11"
        data_input[i, "Block_name2"] <- "SE12"
      } else if (data_input[i, "Booklet"] == 12) {
        data_input[i, "Block_name1"] <- "SE12"
        data_input[i, "Block_name2"] <- "SE13"
      } else if (data_input[i, "Booklet"] == 13) {
        data_input[i, "Block_name1"] <- "SE13"
        data_input[i, "Block_name2"] <- "SE14"
      } else {
        data_input[i, "Block_name1"] <- "SE14"
        data_input[i, "Block_name2"] <- "SE14"
      }
    }
  } else {
    data_input$Block_name1 <- NA
    data_input$Block_name2 <- NA
    
    codebook_B7 <- codebook_B7
    SP01_items <- codebook_B7$`Item ID`[codebook_B7$Block == "SP01"]
    SP03_items <- codebook_B7$`Item ID`[codebook_B7$Block == "SP03"]
    SP05_items <- codebook_B7$`Item ID`[codebook_B7$Block == "SP05"]
    SP06_items <- codebook_B7$`Item ID`[codebook_B7$Block == "SP06"]
    SP07_items <- codebook_B7$`Item ID`[codebook_B7$Block == "SP07"]
    SP09_items <- codebook_B7$`Item ID`[codebook_B7$Block == "SP09"]
    SP11_items <- codebook_B7$`Item ID`[codebook_B7$Block == "SP11"]
    SP13_items <- codebook_B7$`Item ID`[codebook_B7$Block == "SP13"]
    
   
    for (i in 1:N) {
      
      if (sum(data_input[i,SP01_items], na.rm = T) > 0){
        data_input[i, "Block_name1"] <- "SP01"
      } else if (sum(data_input[i,SP03_items], na.rm = T) > 0){
        data_input[i, "Block_name1"] <- "SP03"
      } else if (sum(data_input[i,SP05_items], na.rm = T) > 0){
        data_input[i, "Block_name1"] <- "SP05"
      } else if (sum(data_input[i,SP06_items], na.rm = T) > 0){
        data_input[i, "Block_name1"] <- "SP06"
      } else if (sum(data_input[i,SP07_items], na.rm = T) > 0){
        data_input[i, "Block_name1"] <- "SP07"
      } else if (sum(data_input[i,SP09_items], na.rm = T) > 0){
        data_input[i, "Block_name1"] <- "SP09"
      } else if (sum(data_input[i,SP11_items], na.rm = T) > 0){
        data_input[i, "Block_name1"] <- "SP11"
      } else {
        data_input[i, "Block_name1"] <- "SP13"
        }
    }
    }
  return(data_input)
}




addBookletNo <- function(data_input, block_start){
  ####Takes wide dataset and connects student responses to a booklet number
  ####Compare to: https://timssandpirls.bc.edu/timss2019/frameworks/framework-chapters/assessment-design/student-booklet-design/#footnote
  data_input$Booklet <- rep(NA,length(data_input$IDSTUD))
  N <- length(data_input$IDSTUD)
  
  SE01_items <- codebook_M7$`Item ID`[codebook_M7$Block == "SE01"]
  SE02_items <- codebook_M7$`Item ID`[codebook_M7$Block == "SE02"]
  SE03_items <- codebook_M7$`Item ID`[codebook_M7$Block == "SE03"]
  SE04_items <- codebook_M7$`Item ID`[codebook_M7$Block == "SE04"]
  SE05_items <- codebook_M7$`Item ID`[codebook_M7$Block == "SE05"]
  SE06_items <- codebook_M7$`Item ID`[codebook_M7$Block == "SE06"]
  SE07_items <- codebook_M7$`Item ID`[codebook_M7$Block == "SE07"]
  SE08_items <- codebook_M7$`Item ID`[codebook_M7$Block == "SE08"]
  SE09_items <- codebook_M7$`Item ID`[codebook_M7$Block == "SE09"]
  SE10_items <- codebook_M7$`Item ID`[codebook_M7$Block == "SE10"]
  SE11_items <- codebook_M7$`Item ID`[codebook_M7$Block == "SE11"]
  SE12_items <- codebook_M7$`Item ID`[codebook_M7$Block == "SE12"]
  SE13_items <- codebook_M7$`Item ID`[codebook_M7$Block == "SE13"]
  SE14_items <- codebook_M7$`Item ID`[codebook_M7$Block == "SE14"]
  
  
  
  
  for (i in 1:N){
    if (sum(is.na(data_input[i,SE01_items])) < length(SE01_items) & sum(is.na(data_input[i,SE02_items])) < length(SE02_items)){
      data_input$Booklet[i] <- 1
      
    } else if (sum(is.na(data_input[i,SE02_items])) < length(SE02_items)){
      data_input$Booklet[i] <- 2
      
    } else if (sum(is.na(data_input[i,SE03_items])) < length(SE03_items) & sum(is.na(data_input[i,SE04_items])) < length(SE04_items)){
      data_input$Booklet[i] <- 3
      
    } else if (sum(is.na(data_input[i,SE04_items])) < length(SE04_items) & sum(is.na(data_input[i,SE05_items])) < length(SE05_items)){
      data_input$Booklet[i] <- 4
      
    } else if (sum(is.na(data_input[i,SE05_items])) < length(SE05_items) & sum(is.na(data_input[i,SE06_items])) > length(SE06_items)){
      data_input$Booklet[i] <- 5
      
    } else if (sum(is.na(data_input[i,SE07_items])) < length(SE07_items) & sum(is.na(data_input[i,SE08_items])) < length(SE08_items)){
      data_input$Booklet[i] <- 7
      
    } else if (sum(is.na(data_input[i,SE07_items])) < length(SE07_items)){
      data_input$Booklet[i] <- 6
      
    } else if (sum(is.na(data_input[i,SE08_items])) < length(SE08_items) & sum(is.na(data_input[i,SE09_items])) < length(SE09_items)){
      data_input$Booklet[i] <- 8
      
    } else if (sum(is.na(data_input[i,SE09_items])) < length(SE09_items) & sum(is.na(data_input[i,SE10_items])) < length(SE10_items)){
      data_input$Booklet[i] <- 9
      
    } else if (sum(is.na(data_input[i,SE10_items])) < length(SE10_items) & sum(is.na(data_input[i,SE11_items])) < length(SE11_items)){
      data_input$Booklet[i] <- 10
      
    } else if (sum(is.na(data_input[i,SE11_items])) < length(SE11_items) & sum(is.na(data_input[i,SE12_items])) < length(SE12_items)){
      data_input$Booklet[i] <- 11
      
    } else if (sum(is.na(data_input[i,SE12_items])) < length(SE12_items) & sum(is.na(data_input[i,SE13_items])) < length(SE13_items)){
      data_input$Booklet[i] <- 12
      
    } else if (sum(is.na(data_input[i,SE13_items])) < length(SE13_items) & sum(is.na(data_input[i,SE14_items])) < length(SE14_items)){
      data_input$Booklet[i] <- 13
      
    } else {
      data_input$Booklet[i] <- 14
    }
    
  }
  return(data_input)
}



block_df <- function(data_input, df_type){
  blocknames_M7 <- unique(codebook_M7$Block)
  blocknames_B7 <- unique(codebook_B7$Block)
  Block_list <- list()
  if (df_type == "M7"){
    for (i in 1:length(blocknames_M7))
      Block_list[[i]] <- data_input[data_input$Block_name1 == blocknames_M7[i] | data_input$Block_name2 == blocknames_M7[i], c(
        "IDSTUD",
        "ASBG01", "group",
        "Imm_Stat",
        "Mode_adm",
        codebook_M7$`Item ID`[codebook_M7$Block==blocknames_M7[i]])]
  } else {
      
      for (i in 1:length(blocknames_B7)){
        Block_list[[i]] <- data_input[data_input$Block_name1 == blocknames_B7[i], c(
          "IDSTUD",
          "ASBG01", "group",
          "Imm_Stat",
          "Mode_adm",
          codebook_B7$`Item ID`[codebook_B7$Block==blocknames_B7[i]]
        )]
    }

  }
    return(Block_list)
 
}




#######################################
### NOT SURE WILL BE NEEDING THESE ####
### ------------------------------ ####
#######################################

block_creater <- function(data_input, block_start, sheet_name = "SCI_M7", path_name = "T19_G4_Codebooks/Fanny_codebook.xlsx"){
  #data_input = swe_achM7_SE.gen.imm
  #data_long = swe_achG4_lon
  #data_longNA = swe_achG4_lonNA
  codebook_M7 <- read_excel(path = path_name, sheet = sheet_name)
  
  data_long <- pivot_longer(data = data_input, cols = names(data_input[,grep(block_start, names(data_input), value = T)]), names_to = "Item ID")
  data_long <- merge(data_long, codebook_M7[c("Item ID", "Block")], by = "Item ID")
  
  data_long %>%
    drop_na(value) %>%
    group_by(Block) %>%
    summarise("respnse" = sum(value > 4)/sum(!is.na(value)))
  
  #Drop NA in value column to get block connected to student
  data_longNA <- data_long[!is.na(data_long$value),]
  
  return(data_longNA)
}

#tdf_long <- block_creater(data_input = tdf, block_start = "^SE", sheet_name = "SCI_M7", path_name = "T19_G4_Codebooks/Fanny_codebook.xlsx")



BlockSummary <- function(data, variable_name) {
  # Subset the data to select the relevant columns
  df <- data[c(variable_name, "IDSTUD", "Block")]
  
  # Get unique combinations of "ASBG01," "IDSTUD," and "Block"
  df_unique <- unique(df)
  
  # Group by "Block" and summarize the counts
  summary_block <- df_unique %>%
    group_by(Block) %>%
    summarise(n = n())
  
  # Group by "Block" and "ASBG01", "group" (as a factor) and summarize the counts
  summary_block_variable <- df_unique %>%
    group_by(Block, as.factor(!!sym(variable_name))) %>%
    summarise(n = n())
  
  return(list(BlockSummary = summary_block, BlockGenderSummary = summary_block_variable))
}


#Gender_summary <- BlockSummary(tdf_long, "ASBG01", "group")

#Immigration_summary <- BlockSummary(tdf_long, "Imm_Stat")




BlocksSummary <- function(data, variable_1,variable_2){
  
  GenderImm_block <- data[c("IDSTUD","Block", variable_1, variable_2)]
  GenderImmblock_unique <- unique(GenderImm_block)
  
  # Group by "Block" and "ASBG01", "group" (as a factor) and summarize the counts
  summary_blocks <- GenderImmblock_unique %>%
    group_by(Block, as.factor(!!sym(variable_1)), as.factor(!!sym(variable_1))) %>%
    summarise(n = n())
  
  write_csv(summary_blocks, file = "stats_byblock.csv")
  
  return(summary_blocks)
  
}
