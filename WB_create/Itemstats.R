

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

#load data
dat <- readRDS("R-objects/Scored_blocks_ver3.rds")

swe_achB7 <- read_sav("T19_G4_SWE_SPSS/asasweb7.sav") #Bridge data, not the same students, equivalent sample
swe_achM7 <- read_sav("T19_G4_SWE_SPSS/asaswem7.sav") #seventh cycle
codebook <- read_excel("CODEBOOKS/Fanny_codebook.xlsx", sheet = "SCI_Etimss")

difitems <- read_excel(path = "Results/MGParameters.xlsx", sheet = "DIF items")
difitemsBH <- read_excel(path = "Results/MGParameters.xlsx", sheet = "DIF items BH adj")

#Functions
source("Functions/PlotUtilities.R")
source("Functions/MT_functions_Wrangel.R")
# program -----------------------------------------------------------------


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


fulldata_ch <- readRDS("R-objects/Data_Use4ModelEstMain.rds")

responsedf <- data.frame()

for (itm in 3:length(fulldata_ch)){
  
  for (grp in 1:4){
    
    no.resp <- sum(!is.na(fulldata_ch[,itm][fulldata_ch$group == grp]))
    item.name <- colnames(fulldata_ch[itm])
    # grp <- as.factor(grp)
    responsedf <- rbind(responsedf,cbind(no.resp,item.name, grp))
  }

    
}

responsedf$grp <- factor(responsedf$gr, levels = 1:4, labels = c("Girl Native", "Boy Native", "Girl Immigrant", "Boy Immigrant"))
responsedf$no.resp <- as.numeric(responsedf$no.resp)


#View(responsedf)
View(responsedf)

responsedf_wide <- pivot_wider(responsedf,
            values_from = "no.resp",
            names_from = "grp"
            )

responsedf_wide$item.name <- gsub("S","SE", responsedf_wide$item.name)
View(responsedf_wide)


# Fig create --------------------------------------------------------------
names(responsedf)[3] <- c("Intersectional Groups")
ggplot(data = responsedf, aes(x = no.resp, fill = `Intersectional Groups`)) +
  geom_histogram(color = "black") +
  facet_wrap(~`Intersectional Groups`) +
  xlab("Number of responses") +
  ylab("Number of items") +
  scale_x_continuous(breaks = c(seq(0,50,25),seq(50,400,100))) +
  theme(legend.position = "none")

ggplot(data = responsedf, aes(y = no.resp, x = `Intersectional Groups`)) +
  geom_boxplot() +
  ylab("Number of responses") +
  scale_y_continuous(breaks = c(seq(0,50,25),seq(50,400,100))) +
  theme(axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_text())
  
# create excel ------------------------------------------------------------

wb <- createWorkbook()

addWorksheet(wb,"No of responses")
writeData(wb,"No of responses",responsedf_wide, rowNames = FALSE)

addWorksheet(wb, "DIF items")
writeData(wb, "DIF items", difitems, rowNames = FALSE)

addWorksheet(wb, "DIF BH adj items")
writeData(wb, "DIF BH adj items", difitemsBH, rowNames = FALSE)

addWorksheet(wb, "Etimss science items")
writeData(wb, "Etimss science items", codebook, rowNames = FALSE)

saveWorkbook(wb, "NobsPrItem.xlsx" ,overwrite = TRUE)




