

#####################
### PlotData Prep ###
#####################
PlotDataInfo <- function(model_res, model_ures, Itm, blockName){
  
  THETA <- seq(-6,6,0.01)
  
  iteminformation <- as.data.frame(matrix(data=NA, nrow = length(THETA), ncol = length(Itm)))
  
  for (i in 1:length(Itm)){
    c <- iteminfo(extract.item(model_res, group = 1, i), Theta = THETA)
    iteminformation[,i] <- c
    
  }
  
  iteminformation1 <- as.data.frame(matrix(data=NA, nrow = length(THETA), ncol = length(Itm)))
  
  for (i in 1:length(Itm)){
    
    c <- iteminfo(extract.item(model_ures, group = 1, item = i), Theta = THETA)
    iteminformation1[,i] <- c
    
  }
  
  iteminformation2 <- as.data.frame(matrix(data=NA, nrow = length(THETA), ncol = length(Itm)))
  
  for (i in 1:length(Itm)){
    
    c <- iteminfo(extract.item(model_ures, group = 2, item = i), Theta = THETA)
    iteminformation2[,i] <- c
    
  }
  
  iteminformation3 <- as.data.frame(matrix(data=NA, nrow = length(THETA), ncol = length(Itm)))
  
  for (i in 1:length(Itm)){
    
    c <- iteminfo(extract.item(model_ures, group = 3, item = i), Theta = THETA)
    iteminformation3[,i] <- c
    
  }
  
  
  iteminformation4 <- as.data.frame(matrix(data=NA, nrow = length(THETA), ncol = length(Itm)))
  
  for (i in 1:length(Itm)){
    
    c <- iteminfo(extract.item(model_ures, group = 4, item = i), Theta = THETA)
    iteminformation4[,i] <- c
    
  }
  
  TInfo <- data.frame(Restricted = rowSums(iteminformation), Girl_native = rowSums(iteminformation1), Boy_native = rowSums(iteminformation2), Girl_immigrant = rowSums(iteminformation3), Boy_immigrant = rowSums(iteminformation4), Theta = THETA)
  TInfo_long <- pivot_longer(TInfo,
                             cols = c(Restricted,Girl_native,Boy_native, Girl_immigrant, Boy_immigrant) ,
                             names_to = "Group")
  
  # Renaming the Group column values
  TInfo_long$Group <- sub("Girl_native", "Girl native", TInfo_long$Group)
  TInfo_long$Group <- sub("Boy_native", "Boy native", TInfo_long$Group)
  TInfo_long$Group <- sub("Girl_immigrant", "Girl immigrant", TInfo_long$Group)
  TInfo_long$Group <- sub("Boy_immigrant", "Boy immigrant", TInfo_long$Group)
  
  #Add possibility to wrap
  TInfo_long$Block <- rep(blockName, nrow(TInfo_long))
  
  return(TInfo_long)
}

plotData <- function(model_res, model_ures, items, blockName) {
  
  
  Theta = seq(-6,6,0.01)
  itemprobabilities <- as.data.frame(matrix(data = NA, nrow = length(Theta), ncol = length(items)))
  for (i in 1:length(items)){
    
    x = as.data.frame(probtrace(extract.item(model_res, group = 1, item = items[i]), Theta = seq(-6,6,0.01)))
    x = x[2]
    itemprobabilities[,i] <- x
  }
  
  y = Theta
  
  
  itemprobabilities_ures1 <- as.data.frame(matrix(data = NA, nrow = length(Theta), ncol = length(items)))
  for (i in 1:length(items)){
    
    x = as.data.frame(probtrace(extract.item(model_ures, group = 1, item = items[i]), Theta = seq(-6,6,0.01)))
    x = x[2]
    itemprobabilities_ures1[,i] <- x
  }
  
  itemprobabilities_ures2 <- as.data.frame(matrix(data = NA, nrow = length(Theta), ncol = length(items)))
  for (i in 1:length(items)){
    
    x = as.data.frame(probtrace(extract.item(model_ures, group = 2, item = items[i]), Theta = seq(-6,6,0.01)))
    x = x[2]
    itemprobabilities_ures2[,i] <- x
  }
  
  itemprobabilities_ures3 <- as.data.frame(matrix(data = NA, nrow = length(Theta), ncol = length(items)))
  for (i in 1:length(items)){
    
    x = as.data.frame(probtrace(extract.item(model_ures, group = 3, item = items[i]), Theta = seq(-6,6,0.01)))
    x = x[2]
    itemprobabilities_ures3[,i] <- x
  }
  
  itemprobabilities_ures4 <- as.data.frame(matrix(data = NA, nrow = length(Theta), ncol = length(items)))
  for (i in 1:length(items)){
    
    x = as.data.frame(probtrace(extract.item(model_ures, group = 4, item = items[i]), Theta = seq(-6,6,0.01)))
    x = x[2]
    itemprobabilities_ures4[,i] <- x
  }
  
  TCC <- data.frame(Restricted = rowSums(itemprobabilities), Girl_native = rowSums(itemprobabilities_ures1), Boy_native = rowSums(itemprobabilities_ures2), Girl_immigrant = rowSums(itemprobabilities_ures3), Boy_immigrant = rowSums(itemprobabilities_ures4), Theta = Theta)
  TCC_long <- pivot_longer(TCC,
                           cols = c(Restricted,Girl_native,Boy_native, Girl_immigrant, Boy_immigrant) ,
                           names_to = "Group")
  
  # Renaming the Group column values
  TCC_long$Group <- sub("Girl_native", "Girl native", TCC_long$Group)
  TCC_long$Group <- sub("Boy_native", "Boy native", TCC_long$Group)
  TCC_long$Group <- sub("Girl_immigrant", "Girl immigrant", TCC_long$Group)
  TCC_long$Group <- sub("Boy_immigrant", "Boy immigrant", TCC_long$Group)
  
  #Add possibility to wrap
  TCC_long$Block <- rep(blockName, nrow(TCC_long))
  
  return(TCC_long)
}



DIFitemplotData <- function(mObj, itemindex, itemnnames){
  theta = seq(-6,6,0.01)
  resultpr = data.frame()
  for (it in 1:length(itemindex)){
    ite <- itemindex[it]
    
    for (group in 1:4){
      prob <- probtrace(extract.item(mObj, group = group, item = ite), Theta = theta)[,2]
      groupcol <- rep(group,length(prob))
      itemNam <- rep(itemnnames[it], length(prob))
      resultpr <- rbind(resultpr,cbind(prob,groupcol, itemNam, theta))
      resultpr$prob <- as.numeric(resultpr$prob)
      resultpr$itemNam <- as.factor(resultpr$itemNam)
      resultpr$groupcol <- as.factor(resultpr$groupcol)
      resultpr$theta <- as.numeric(resultpr$theta)
    }
  }
  resultpr$groupcol <- factor(resultpr$groupcol, levels = 1:4, labels = c("Girl Native", "Boy Native", "Girl Immigrant", "Boy Immigrant"))
  return(resultpr)
}

