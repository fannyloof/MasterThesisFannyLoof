

##############################
### item stats for meeting ###
##############################


#libraries
library(mirt)
library(readxl)
library("haven")
library("readxl")
library(tidyr)
library(dplyr)
library(openxlsx)
library(expss)
library(ggplot2)

#Data
dat <- readRDS("R-objects/Scored_blocks_ver3.rds") # checked
mode.noninvar <- readRDS("R-objects/modenonivariant.rds") # checked

swe_achB7 <- read_sav("T19_G4_SWE_SPSS/asasweb7.sav") #Bridge data, not the same students, equivalent sample, checked
swe_achM7 <- read_sav("T19_G4_SWE_SPSS/asaswem7.sav") #seventh cycle, checked
codebook <- read_excel("CODEBOOKS/Fanny_codebook.xlsx", sheet = "SCI_Etimss") # checked

difitems <- read_excel(path = "Results/MGParameters.xlsx", sheet = "DIF items") # checked ?
difitemsBH <- read_excel(path = "Results/MGParameters.xlsx", sheet = "DIF items BH adj") # checked ?

#Functions
source("Functions/MT_functions_Wrangel.R")

# program -----------------------------------------------------------------

fulldata <- blockmerger(scoredlist = dat, c(2:14))


## Set NA to obervations of Ptimms that were non invariant
index <- match(mode.noninvar, names(fulldata))

for (i in 1:length(index)){
  #print(length(fulldata[fulldata$mode_adm == "Ptimss", index[i]]))
  fulldata[fulldata$mode_adm == "Ptimss",index[i]] <- NA
}



responsedf <- data.frame()

for (itm in 6:length(fulldata)){
  
  for (grp in 1:4){
    
    no.resp <- sum(!is.na(fulldata[,itm][fulldata$group == grp]))
    item.name <- colnames(fulldata[itm])
    # print(item.name)
    # grp <- as.factor(grp)
    responsedf <- rbind(responsedf,cbind(no.resp,item.name, grp))
  }

    
}

responsedf$grp <- factor(responsedf$gr, levels = 1:4, labels = c("Girl Native", "Boy Native", "Girl Immigrant", "Boy Immigrant"))
responsedf$no.resp <- as.numeric(responsedf$no.resp)





responsedf_wide <- pivot_wider(responsedf,
            values_from = "no.resp",
            names_from = "grp"
            )

responsedf_wide$item.name <- gsub("S","SE", responsedf_wide$item.name)


  
# create excel ------------------------------------------------------------

# wb <- createWorkbook()
# 
# addWorksheet(wb,"No of responses")
# writeData(wb,"No of responses",responsedf_wide, rowNames = FALSE)
# 
# addWorksheet(wb, "DIF items")
# writeData(wb, "DIF items", difitems, rowNames = FALSE)
# 
# addWorksheet(wb, "DIF BH adj items")
# writeData(wb, "DIF BH adj items", difitemsBH, rowNames = FALSE)
# 
# addWorksheet(wb, "Etimss science items")
# writeData(wb, "Etimss science items", codebook, rowNames = FALSE)
# 
# saveWorkbook(wb, "NobsPrItem.xlsx" ,overwrite = TRUE)




