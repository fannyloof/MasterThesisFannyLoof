
#################################
###       Computation         ###  
###   SE (Standard errors)    ###
#################################


library(mirt)
library(expss)
library(openxlsx)
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
## Load objects
modelres <-readRDS("R-objects/modelres.rds")
modelfull <- readRDS("R-objects/modelfull.rds")
modelfullBH <- readRDS("R-objects/modelfullBH.rds")


noninvariant <- readRDS("R-objects/namesnonInvariantMain.rds")
noninvariantBH <- readRDS("R-objects/namesnonInvariantBHMain.rds")
allitems <- readRDS("R-objects/namesInvariantMain.rds")
n.invariant <- which(allitems %in% noninvariant)
n.invariantBH <- which(allitems %in% noninvariantBH)
invariants <- allitems[-n.invariant]
invariantsBH <- allitems[-n.invariantBH]


DifRes <- readRDS("R-objects/Dif.ResultsMain.rds")

dim(DifRes) # 63 items


# FUNCTION ----------------------------------------------------------------
itms <- noninvariant

PARreport <- function(model, itms, DIFres, Ngroups = 4, indexover = allitems){
  PAR <- data.frame()
  iteration <- which(indexover %in% itms)
  #print(iteration)
  for (itm in iteration){
  print(itm)

    for (grp in 1:Ngroups){
      Group <- coef(model, printSE = T, IRT = T)[[grp]]
      ItName <- rep(names(Group[itm]),2)
      
      Item <- Group[[itm]]

      chisq <- rep(round(DIFres$X2[rownames(DIFres)==ItName[1]], digits = 3),2)
      df <- rep(round(DIFres$df[rownames(DIFres)==ItName[1]], digits = 3),2)
      p.val <- rep(round(DIFres$p[rownames(DIFres)==ItName[1]],digits = 3),2)
      pAdj.val <- rep(round(DIFres$adj_p[rownames(DIFres)==ItName[1]],digits = 3),2)
      Item <- cbind(rownames(Item),group = rep(grp,2), ItName, Item, df, chisq,p.val,pAdj.val)

      Item <- cbind(rownames(Item),ItName,Item)

      PAR <- rbind(PAR,Item)
      #View(PAR)
    }

  }

  SE <- PAR$b[PAR$V1 == "SE"]
  EST <- PAR[PAR$V1 == "par",]
  PAR <- cbind(EST,SE)

  col_toformat <- c("a","b", "SE","g","u", "df","chisq","p.val","pAdj.val")

  PAR[, col_toformat] <- lapply(PAR[, col_toformat], as.numeric)

  PAR[col_toformat] <- lapply(PAR[col_toformat], function(x) as.numeric(sprintf("%.3f", x)))

  if (Ngroups > 1){

    PAR$group <- sub(1, "Girl native", PAR$group)
    PAR$group <- sub(2, "Boy native", PAR$group)
    PAR$group <- sub(3, "Girl Immigrant", PAR$group)
    PAR$group <- sub(4, "Boy Immigrant", PAR$group)

    col_order <- c("V1","ItName", "group","a","b", "SE","g","u", "df","chisq","p.val","pAdj.val")

  } else {

    PAR <- PAR[,!names(PAR) %in% "group"]
    col_order <- c("V1","ItName","a","b", "SE","g","u", "df","chisq","p.val","pAdj.val")
  }


  PAR <- PAR[,col_order]
  PAR$SE <- paste0("[", PAR$SE, "]")

  return(PAR)
}


# PARAMETER REPORTS -------------------------------------------------------
itemparameters.ures <- PARreport(model = modelfull, itms = noninvariant, DIFres = DifRes)
itemparameters.uresBH <- PARreport(model = modelfullBH, itms = noninvariantBH, DIFres = DifRes)

itemparameters.rest <- PARreport(model = modelfull, itms = invariants, DIFres = DifRes, Ngroups = 1)
itemparameters.restBH <- PARreport(model = modelfullBH, itms = invariantsBH, DIFres = DifRes, Ngroups = 1)


# Create object for plot --------------------------------------------------




deltaDiff <- function(item.names, modelPAR){
  difSize <- data.frame(ItName = character(),
                        minDiff = numeric(),
                        maxDiff = numeric())
  for (item in item.names){
    #print(item)
    modelPAR$b[modelPAR$ItName == item]
    order(modelPAR$b[modelPAR$ItName == item])
    ordered <- modelPAR$b[modelPAR$ItName == item][order(modelPAR$b[modelPAR$ItName == item])]
    difSize[item,"ItName"] <- item
    difSize[item,"minDiff"] <- min(abs(diff(ordered)))
    difSize[item, "maxDiff"] <- max(abs(diff(ordered)))
    
  }
  return(difSize)
}

plotdfdiff <- deltaDiff(item.names = noninvariant, modelPAR = itemparameters.ures)
plotdfdiffBH <- deltaDiff(item.names = noninvariantBH, modelPAR = itemparameters.uresBH)

View(plotdfdiff)

plotdiff_lon <- plotdfdiff %>% 
  pivot_longer(cols = c(maxDiff, minDiff),
               names_to = "Detected difference",
               values_to = "parDiff") 
plotdiff_lon$`Detected difference` <- gsub("minDiff","Minumum", plotdiff_lon$`Detected difference`)
plotdiff_lon$`Detected difference` <- gsub("maxDiff", "Maximum", plotdiff_lon$`Detected difference`)
plotdiff_lon$Significance <- rep("w/o adjusted p-value", nrow(plotdiff_lon))

plotdiffBH_lon <- plotdfdiffBH %>%
  pivot_longer(cols = c(maxDiff, minDiff),
               names_to = "Detected difference",
               values_to = "parDiff")

plotdiffBH_lon$`Detected difference` <- gsub("minDiff","Minumum", plotdiffBH_lon$`Detected difference`)
plotdiffBH_lon$`Detected difference` <- gsub("maxDiff", "Maximum", plotdiffBH_lon$`Detected difference`)
plotdiffBH_lon$Significance<- rep("BH adjusted p-value", nrow(plotdiffBH_lon))

plotdiff_all <- rbind(plotdiff_lon,plotdiffBH_lon)
# plots  -------------------------------------------------


ggplot(data = plotdiff_all, mapping = aes( y = parDiff, color = `Detected difference`)) +
  geom_boxplot() +
  facet_wrap(~Significance) +
  # ggtitle(label = bquote("Model estimated Differences in" ~ delta ~ "- parameter"),
  #       subtitle = "Model adjusted for DIF w/o BH adjustment") +
  ylab(expression(delta))
  

# Workbook Create ---------------------------------------------------------

wb <- createWorkbook()

## ADD sheets
itemparameters.ures$ItName <- gsub("S", "SE", itemparameters.ures$ItName)
addWorksheet(wb, "DIF items")
writeData(wb, "DIF items", itemparameters.ures, rowNames = FALSE)

itemparameters.uresBH$ItName <- gsub("S", "SE", itemparameters.uresBH$ItName)
addWorksheet(wb, "DIF items BH adj")
writeData(wb, "DIF items BH adj", itemparameters.uresBH, rowNames = FALSE)

itemparameters.rest$ItName <- gsub("S", "SE", itemparameters.rest$ItName)
addWorksheet(wb, "Invariant items")
writeData(wb, "Invariant items", itemparameters.rest, rowNames = FALSE)

addWorksheet(wb, "Invariant items BH adj")
writeData(wb, "Invariant items BH adj", itemparameters.restBH, rowNames = FALSE)

## Save workbook

saveWorkbook(wb, "Results/MGParameters.xlsx", overwrite = TRUE)
