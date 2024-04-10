########################
## analysis functions ##
########################

PE_separator <- function(LIST, block, mode_type){
  remove_cols <- c("IDSTUD","ASBG01","Imm_Status","mode_adm", "group")
  block_full <- LIST[[block]]
  
  if (mode_type == "P"){
    ptimms <- block_full[block_full$mode_adm == "Ptimss",]
    ptimms <- ptimms[, -which(names(ptimms) %in% remove_cols)]
    for (i in 1:length(ptimms)){
      ptimms[,i] <- as.integer(ptimms[,i])
    }
    return(ptimms)
  }else if (mode_type == "E"){
    etimms <- block_full[block_full$mode_adm == "Etimss",]
    etimms <- etimms[, -which(names(etimms) %in% remove_cols)]
    for (i in 1:length(etimms)){
      etimms[,i] <- as.integer(etimms[,i])
    }
    return(etimms)
    
  }else if (mode_type == "full"){
    l <- length(block_full[, -which(names(block_full) %in% remove_cols)])
    block <- block_full[, -which(names(block_full) %in% remove_cols)]
    idcol <- block_full[,remove_cols]
    for (i in 1:l){
      block[,i] <- as.integer(block[,i])
    }
    block_full <- cbind(idcol,block) 
    return(block_full)
    
  }else{
    block <- block_full[, -which(names(block_full) %in% remove_cols)]
    for (i in 2:length(block)){
      block[,i] <- as.integer(block[,i])
    }
    return(block)
  }
  
  
}


genim_add <- function(LIST, block_no){
  ## takes a list as location in list as arguments
  ## returns a df with a variable coding for gen and immigration status
  block <- LIST[[block_no]]
  
  block$GenIm <- rep(NA,length(nrow(block)))
  
  ## Coding interpretation
  
  for (i in 1:nrow(block)){
    if (block$ASBG01[i] == 1 & block$Imm_Stat[i] == 1){
      # Girl non-im
      block$GenIm[i] <- 1
    }else if (block$ASBG01[i] == 2 & block$Imm_Stat[i] == 1){
      # Boy non-im
      block$GenIm[i] <- 2
    }else if (block$ASBG01[i] == 1 & block$Imm_Stat[i] == 2){
      # Girl im
      block$GenIm[i] <- 3
    }else if (block$ASBG01[i] == 2 & block$Imm_Stat[i] == 2){
      # Boy im
      block$GenIm[i] <- 4
    } else{
      block$GenIm[i] <- NA
    }
  }
  
  print(paste(table(block$GenIm), block_no))
  LIST[[block_no]] <- block
  return(LIST)
}


###################
# model prep data #
###################








model_data <- function(block_no, remove_cols){ 
  groups <- list()
  
  
  
  ## all groups
  block_full <- dat[[block_no]]
  
  

  
  ## groups separated
  girl_nim <- block_full[block_full$group==1,] ## girl native
  girl_nim <- girl_nim[, -which(names(girl_nim) %in% remove_cols)]
  
  
  boy_nim <- block_full[block_full$group==2,] ## boy native
  boy_nim <- boy_nim[, -which(names(boy_nim) %in% remove_cols)]
  
  
  girl_im <- block_full[block_full$group==3,]  ## girl immigrant
  girl_im <- girl_im[, -which(names(girl_im) %in% remove_cols)]
  
  
  boy_im <- block_full[block_full$group==4,] ## boy immigrant
  boy_im <- boy_im[, -which(names(boy_im) %in% remove_cols)]
 
  ## do last
  block <- block_full[, -which(names(block_full) %in% remove_cols)]
  
  for (i in 1:length(girl_nim)){
    block[,i] <- as.integer(block[,i])
    girl_nim[,i] <- as.integer(girl_nim[,i])
    boy_nim[,i] <- as.integer(boy_nim[,i])
    girl_im[,i] <- as.integer(girl_im[,i])
    boy_im[,i] <- as.integer(boy_im[,i])
  }
  
  
  
  ## add to list
  groups[[paste0("full_",block_no)]] <- na.omit(block_full)
  groups[[paste0("block_",block_no)]] <- na.omit(block)
  
  groups[[paste0("girl_nim_",block_no)]] <- na.omit(girl_nim)
  groups[[paste0("boy_nim_", block_no)]] <- na.omit(boy_nim)
  groups[[paste0("girl_im_",block_no)]] <- na.omit(girl_im)
  groups[[paste0("boy_im_", block_no)]] <- na.omit(boy_im)
  
  print(table(groups[[paste0("full_",block_no)]]$group))

  return(groups)
}

## To gather items with full correct or incorrect responses
resp_category <- function(list){
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

remove_items <- function(list,block_no,itemvec){
  groups_itemrem <- list()
  
  girl_nim_ <- list[[paste0("girl_nim_", block_no)]]
  girl_nim_ <- girl_nim_[, -which(names(girl_nim_) %in% itemvec)]
    
  boy_nim_ <- list[[paste0("boy_nim_", block_no)]]
  boy_nim_ <- boy_nim_[, -which(names(boy_nim_) %in% itemvec)]
  
  girl_im_ <- list[[paste0("girl_im_", block_no)]]
  girl_im_ <- girl_im_[, -which(names(girl_im_) %in% itemvec)]
  
  boy_im_ <- list[[paste0("boy_im_", block_no)]]
  boy_im_ <- boy_im_[, -which(names(boy_im_) %in% itemvec)]
  
  block_ <- list[[paste0("block_", block_no)]]
  block_ <- block_[, -which(names(block_) %in% itemvec)]
  
  ## add to list
  
  groups_itemrem[[paste0("block_",block_no)]] <- block_
  
  groups_itemrem[[paste0("girl_nim_",block_no)]] <- girl_nim_
  groups_itemrem[[paste0("boy_nim_", block_no)]] <- boy_nim_
  groups_itemrem[[paste0("girl_im_",block_no)]] <- girl_im_
  groups_itemrem[[paste0("boy_im_", block_no)]] <- boy_im_
  
  return(groups_itemrem)
  
}

IRT_Confmodel <- function(list_name, block_no, itemtype){
  Group = as.factor(list_name[[paste0("full_", block_no)]]$group)
  
  girl_nimM <- mirt(list_name[[paste0("girl_nim_", block_no)]], 1, rep(itemtype,length(list_name[[paste0("girl_nim_", block_no)]])))
  boy_nimM <- mirt(list_name[[paste0("boy_nim_", block_no)]], 1, rep(itemtype,length(list_name[[paste0("boy_nim_", block_no)]])))
  girl_imM <- mirt(list_name[[paste0("girl_im_", block_no)]], 1, rep(itemtype,length(list_name[[paste0("boy_im_", block_no)]])))
  boy_imM <- mirt(list_name[[paste0("boy_im_", block_no)]], 1, rep(itemtype,length(list_name[[paste0("boy_im_", block_no)]])))
  fit_full <- mirt(list_name[[paste0("block_", block_no)]], 1, rep(itemtype,length(list_name[[paste0("block_", block_no)]])))
  
  fit <- rbind(girlnative = M2(girl_nimM, type = "M2*", na.rm = T),
               boynative = M2(boy_nimM, type = "M2*", na.rm = T),
               girlimmigrant = M2(girl_imM, type = "M2*", na.rm = T),
               boyimmigrant = M2(boy_imM, type = "M2*", na.rm = T),
               full = M2(fit_full, type = "M2*", na.rm = T))
  
  print(fit)
  return(fit)
}


#### BH - adjustment

pValueAdj <- function(df, NGroups){
  
  
  ## benjamini Hochberg adjustment procedure (i/m * Q)
  Q <- 0.05 #false discovery rate
  
  
  df$i <- rank(df$p) #ranking
  m <- NGroups * nrow(df) #number of statistical tests
  
  df$BH_val <- (df$i/m) * Q
  
  df$significance <- ifelse(df$p<df$BH_val | df$adj_p < Q,"DIF","NO_DIF")
  View(df)
  return(df)
}
