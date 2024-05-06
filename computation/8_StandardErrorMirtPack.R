
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

## DATA 
# names of items evaluated for DIF 
allitems <- readRDS("R-objects/itemsevaluated.rds") # checked

### Model objects
# Main analysis:
modelres <- readRDS("R-objects/scalarmain.rds") #checked
modelfull <- readRDS("R-objects/partialmain.rds") #checked
modelfullBH <- readRDS("R-objects/partialmainBH.rds") #checked
noninvariant <- readRDS("R-objects/namesnonInvariantMain.rds") #checked
noninvariantBH <- readRDS("R-objects/namesnonInvariantBHMain.rds") #checked

## DIF evaluation results
DifRes <- readRDS("R-objects/Dif.ResultsMain.rds") # checked
# rownames(DifRes) <- gsub("S", "SE", rownames(DifRes))
Difrespur <- readRDS("R-objects/Step2purResults") # checked
# rownames(Difrespur) <- gsub("S", "SE", rownames(Difrespur))

# MODE ANALYSIS:
partial.mode <- readRDS("R-objects/partialmgMode.rds")
modenoninvar <- readRDS("R-objects/modenonivariant.rds")
DifResmode <- readRDS("R-objects/DIFresmode.rds")
modeevaluateditems <- rownames(DifResmode)[!is.na(DifResmode$p)]
nonivariantmode <- modeevaluateditems[modeevaluateditems %in% rownames(DifResmode[DifResmode$p < 0.05,])]
## Create objects
n.invariant <- match(noninvariant, allitems) #index
n.invariantBH <- match(noninvariantBH, allitems) #index
invariants <- allitems[-n.invariant]
invariantsBH <- allitems[-n.invariantBH]
itms <- noninvariant #?



# FUNCTIONS ----------------------------------------------------------------

# Creates a table with estimated model parameters
PARreport <- function(model, itms, DIFres, Ngroups = 4, indexover = allitems){
  PAR <- data.frame()
  iteration <- which(indexover %in% itms)
  print(iteration)
  for (itm in iteration){
    for (grp in 1:Ngroups){
      Group <- coef(model, printSE = T, IRT = T)[[grp]]
      ItName <- rep(names(Group[itm]),2)

      Item <- Group[[itm]]

      chisq <- rep(round(DIFres$X2[rownames(DIFres)==ItName[1]], digits = 3),2)
      df <- rep(round(DIFres$df[rownames(DIFres)==ItName[1]], digits = 3),2)
      p.val <- rep(round(DIFres$p[rownames(DIFres)==ItName[1]],digits = 3),2)

      pAdj.val <- rep(round(DIFres$adj_p[rownames(DIFres)==ItName[1]],digits = 3),2)
      # 
      Item <- cbind(rownames(Item),group = rep(grp,2), ItName, Item, df, chisq,p.val,pAdj.val)

      Item <- cbind(rownames(Item),ItName,Item)
      # print(Item)
      # 
      PAR <- rbind(PAR,Item)
      # View(PAR)
    }

  }

  SE <- PAR$b[PAR$V1 == "SE"]
  EST <- PAR[PAR$V1 == "par",]
  PAR <- cbind(EST,SE)
  #

  col_toformat <- c("a","b", "SE","g","u", "df","chisq","p.val","pAdj.val")

  PAR[, col_toformat] <- lapply(PAR[, col_toformat], as.numeric)

  PAR[col_toformat] <- lapply(PAR[col_toformat], function(x) as.numeric(sprintf("%.3f", x)))
  # View(PAR)

  if (Ngroups == 4){

    PAR$group <- sub(1, "Girl native", PAR$group)
    PAR$group <- sub(2, "Boy native", PAR$group)
    PAR$group <- sub(3, "Girl Immigrant", PAR$group)
    PAR$group <- sub(4, "Boy Immigrant", PAR$group)

    col_order <- c("V1","ItName", "group","a","b", "SE","g","u", "df","chisq","p.val","pAdj.val")

  } else if (Ngroups == 2){
    PAR$group <- sub(1, "eTIMSS", PAR$group)
    PAR$group <- sub(2, "pTIMSS", PAR$group)

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

PAR_wide <- pivot_wider(data = itemparameters.ures,
                        names_from = "group",
                        values_from = c("a"     ,   "b"    ,    "SE"   ,    "g"   ,     "u"    ,    "df"   ,    "chisq" ,  "p.val",    "pAdj.val"))

PAR_wide$`b_Girl native` <- paste(PAR_wide$`b_Girl native`,PAR_wide$`SE_Girl native`)
PAR_wide$`b_Boy native`<- paste(PAR_wide$`b_Boy native`,PAR_wide$`SE_Boy native`)
PAR_wide$`b_Girl Immigrant`<- paste(PAR_wide$`b_Girl Immigrant`,PAR_wide$`SE_Girl Immigrant`)
PAR_wide$`b_Boy Immigrant`<- paste(PAR_wide$`b_Boy Immigrant`, PAR_wide$`SE_Boy Immigrant`)

itemparTable <- PAR_wide[,c("ItName" ,"b_Girl native", "b_Boy native", "b_Girl Immigrant" , "b_Boy Immigrant", "df_Girl Immigrant", "chisq_Girl native","p.val_Girl native", "pAdj.val_Girl native" )]

colnames(itemparTable)[1] <- c("Item")
colnames(itemparTable)[6:length(itemparTable)] <- c("DF", "chisq","p","p.adj")

itemparameters.uresBH <- PARreport(model = modelfullBH, itms = noninvariantBH, DIFres = DifRes)


#Purified results
### NOTE: can only collect parameters for the parameters run for DIF, i.e. the non-anchors. 
### NOTE: Therefore only parameters for 18 items are reported
itemparametersPur.ures <- PARreport(model = modelfull, itms = noninvariant, DIFres = Difrespur)

PAR_widePur <- pivot_wider(data = itemparametersPur.ures,
                        names_from = "group",
                        values_from = c("a"     ,   "b"    ,    "SE"   ,    "g"   ,     "u"    ,    "df"   ,    "chisq" ,  "p.val",    "pAdj.val"))

PAR_widePur$`b_Girl native` <- paste(PAR_widePur$`b_Girl native`,PAR_widePur$`SE_Girl native`)
PAR_widePur$`b_Boy native`<- paste(PAR_widePur$`b_Boy native`,PAR_widePur$`SE_Boy native`)
PAR_widePur$`b_Girl Immigrant`<- paste(PAR_widePur$`b_Girl Immigrant`,PAR_widePur$`SE_Girl Immigrant`)
PAR_widePur$`b_Boy Immigrant`<- paste(PAR_widePur$`b_Boy Immigrant`, PAR_widePur$`SE_Boy Immigrant`)

itemparTablePur <- PAR_widePur[,c("ItName" ,"b_Girl native", "b_Boy native", "b_Girl Immigrant" , "b_Boy Immigrant", "df_Girl Immigrant", "chisq_Girl native","p.val_Girl native", "pAdj.val_Girl native" )]

colnames(itemparTablePur)[1] <- c("Item")
colnames(itemparTablePur)[6:length(itemparTablePur)] <- c("DF", "chisq","p","p.adj")

itemparameters.rest <- PARreport(model = modelfull, itms = invariants, DIFres = DifRes, Ngroups = 1)
itemparameters.restBH <- PARreport(model = modelfullBH, itms = invariantsBH, DIFres = DifRes, Ngroups = 1)

#Mode analysis parameters
itemparameters.partial <- PARreport(model = partial.mode, itms = nonivariantmode, DIFres = DifResmode, Ngroups = 2, indexover = rownames(DifResmode))

PAR_wideMode <- pivot_wider(data = itemparameters.partial,
                           names_from = "group",
                           values_from = c("a"     ,   "b"    ,    "SE"   ,    "g"   ,     "u"    ,    "df"   ,    "chisq" ,  "p.val",    "pAdj.val"))

PAR_wideMode$b_eTIMSS <- paste(PAR_wideMode$b_eTIMSS,PAR_wideMode$SE_eTIMSS)
PAR_wideMode$b_pTIMSS <- paste(PAR_wideMode$b_pTIMSS,PAR_wideMode$SE_pTIMSS)

itemparTableMode <- PAR_wideMode[,c("ItName" ,"b_eTIMSS", "b_pTIMSS",  "df_eTIMSS", "chisq_eTIMSS","p.val_eTIMSS", "pAdj.val_eTIMSS" )]
colnames(itemparTableMode)[1] <- c("Item")
colnames(itemparTableMode)[4:length(itemparTableMode)] <- c("DF", "chisq","p","p.adj")

# Workbook Create ---------------------------------------------------------

write.xlsx(itemparTable, "itempar_wide.xlsx")
write.xlsx(itemparTablePur, "Results/itemparTablepur.xlsx")
write.xlsx(itemparTableMode, "Results/itemparTableMode.xlsx")

wb <- createWorkbook()
wd <- createWorkbook()
wc <- createWorkbook()
## ADD sheets wb workbook
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

## ADD sheet wd workbook
itemparametersPur.ures$ItName <- gsub("S", "SE", itemparametersPur.ures$ItName)
addWorksheet(wd, "DIF items")
writeData(wd, "DIF items", itemparametersPur.ures, rowNames = FALSE)

## ADD sheet wc workbook
itemparameters.partial$ItName[itemparameters.partial$group == "eTIMSS"] <- gsub("S", "SE", itemparameters.partial$ItName[itemparameters.partial$group == "eTIMSS"])
itemparameters.partial$ItName[itemparameters.partial$group == "pTIMSS"] <- gsub("S", "SP", itemparameters.partial$ItName[itemparameters.partial$group == "pTIMSS"])
itemparameters.partial[itemparameters.partial$group == "pTIMSS", c("df","chisq","p.val","pAdj.val")] <- NA

addWorksheet(wc, "Itemparameters mode DIF")
writeData(wc, "Itemparameters mode DIF", itemparameters.partial, rowNames = F)

## Save workbook

saveWorkbook(wb, "Results/MGParameters.xlsx", overwrite = TRUE)
saveWorkbook(wd,"Results/MGParametersPur.xlsx", overwrite = T)
saveWorkbook(wc, "Results/MGParameters_mode.xlsx", overwrite = T)

