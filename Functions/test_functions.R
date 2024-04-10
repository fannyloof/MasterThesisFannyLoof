

M7_GenIm_ImSwe_1 <- M7_GenIm_ImSwe
removed_items <- c()
for (item in 1:length(M7_GenIm_ImSwe_1)){
  if (sum(is.na(M7_GenIm_ImSwe_1[item])) == nrow(M7_GenIm_ImSwe_1)){
    removed_items <- append(removed_items, colnames(M7_GenIm_ImSwe_1[item]))
  }
}


length(removed_items)
length(M7_GenIm_ImSwe_1)
length(M7_GenIm_ImSwe)
M7_GenIm_ImSwe_1 <- M7_GenIm_ImSwe_1[,!names(M7_GenIm_ImSwe_1) %in% removed_items]
length(M7_GenIm_ImSwe_1)




block_creater_1 <- function(df){
  block_list <- list()
  blocks <- unique(codebook_M7$Block)
  
  block_list[["SE01"]] <- na.omit(M7_GenIm_ImSwe_1[,c("IDSTUD","Imm_Stat", "ASBG01",codebook_M7$`Item ID`[codebook_M7$Block == blocks[1]][!codebook_M7$`Item ID`[codebook_M7$Block == blocks[1]] %in% removed_items])])
  block_list[["SE02"]] <- na.omit(M7_GenIm_ImSwe_1[,c("IDSTUD","Imm_Stat", "ASBG01",codebook_M7$`Item ID`[codebook_M7$Block == blocks[2]][!codebook_M7$`Item ID`[codebook_M7$Block == blocks[2]] %in% removed_items])])
  block_list[["SE03"]] <- na.omit(M7_GenIm_ImSwe_1[,c("IDSTUD","Imm_Stat", "ASBG01",codebook_M7$`Item ID`[codebook_M7$Block == blocks[3]][!codebook_M7$`Item ID`[codebook_M7$Block == blocks[3]] %in% removed_items])])
  block_list[["SE04"]] <- na.omit(M7_GenIm_ImSwe_1[,c("IDSTUD","Imm_Stat", "ASBG01",codebook_M7$`Item ID`[codebook_M7$Block == blocks[4]][!codebook_M7$`Item ID`[codebook_M7$Block == blocks[4]] %in% removed_items])])
  block_list[["SE05"]] <- na.omit(M7_GenIm_ImSwe_1[,c("IDSTUD","Imm_Stat", "ASBG01",codebook_M7$`Item ID`[codebook_M7$Block == blocks[5]][!codebook_M7$`Item ID`[codebook_M7$Block == blocks[5]] %in% removed_items])])
  block_list[["SE06"]] <- na.omit(M7_GenIm_ImSwe_1[,c("IDSTUD","Imm_Stat", "ASBG01",codebook_M7$`Item ID`[codebook_M7$Block == blocks[6]][!codebook_M7$`Item ID`[codebook_M7$Block == blocks[6]] %in% removed_items])])
  block_list[["SE07"]] <- na.omit(M7_GenIm_ImSwe_1[,c("IDSTUD","Imm_Stat", "ASBG01",codebook_M7$`Item ID`[codebook_M7$Block == blocks[7]][!codebook_M7$`Item ID`[codebook_M7$Block == blocks[7]] %in% removed_items])])
  block_list[["SE08"]] <- na.omit(M7_GenIm_ImSwe_1[,c("IDSTUD","Imm_Stat", "ASBG01",codebook_M7$`Item ID`[codebook_M7$Block == blocks[8]][!codebook_M7$`Item ID`[codebook_M7$Block == blocks[8]] %in% removed_items])])
  block_list[["SE09"]] <- na.omit(M7_GenIm_ImSwe_1[,c("IDSTUD","Imm_Stat", "ASBG01",codebook_M7$`Item ID`[codebook_M7$Block == blocks[9]][!codebook_M7$`Item ID`[codebook_M7$Block == blocks[9]] %in% removed_items])])
  block_list[["SE10"]] <- na.omit(M7_GenIm_ImSwe_1[,c("IDSTUD","Imm_Stat", "ASBG01",codebook_M7$`Item ID`[codebook_M7$Block == blocks[10]][!codebook_M7$`Item ID`[codebook_M7$Block == blocks[10]] %in% removed_items])])
  block_list[["SE11"]] <- na.omit(M7_GenIm_ImSwe_1[,c("IDSTUD","Imm_Stat", "ASBG01",codebook_M7$`Item ID`[codebook_M7$Block == blocks[11]][!codebook_M7$`Item ID`[codebook_M7$Block == blocks[11]] %in% removed_items])])
  block_list[["SE12"]] <- na.omit(M7_GenIm_ImSwe_1[,c("IDSTUD","Imm_Stat", "ASBG01",codebook_M7$`Item ID`[codebook_M7$Block == blocks[12]][!codebook_M7$`Item ID`[codebook_M7$Block == blocks[12]] %in% removed_items])])
  block_list[["SE13"]] <- na.omit(M7_GenIm_ImSwe_1[,c("IDSTUD","Imm_Stat", "ASBG01",codebook_M7$`Item ID`[codebook_M7$Block == blocks[13]][!codebook_M7$`Item ID`[codebook_M7$Block == blocks[13]] %in% removed_items])])
  block_list[["SE14"]] <- na.omit(M7_GenIm_ImSwe_1[,c("IDSTUD","Imm_Stat", "ASBG01",codebook_M7$`Item ID`[codebook_M7$Block == blocks[14]][!codebook_M7$`Item ID`[codebook_M7$Block == blocks[14]] %in% removed_items])])
  
  return(block_list)
}

blockedi <- block_creater_1(M7_GenIm_ImSwe_1)

View(blockedi)


SE05_items_1 <- codebook_M7$`Item ID`[codebook_M7$Block == "SE05"]
SE05_items_1 <- codebook_M7$`Item ID`[codebook_M7$Block == "SE05"][!codebook_M7$`Item ID`[codebook_M7$Block == "SE05"] %in% removed_items]

SE05_1 <- na.omit(M7_GenIm_ImSwe_1[,c("IDSTUD","Imm_Stat", "ASBG01",codebook_M7$`Item ID`[codebook_M7$Block == "SE05"][!codebook_M7$`Item ID`[codebook_M7$Block == "SE05"] %in% removed_items])])

dim(SE05_1)
