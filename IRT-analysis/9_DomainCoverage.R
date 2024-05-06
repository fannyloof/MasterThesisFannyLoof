

############################
###   Item content rep   ###
############################

library(mirt)
library(expss)
library(openxlsx)
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)

## Load data/objects

allitems <- readRDS("R-objects/itemsEvaluated.rds") # checked
noninvariant <- readRDS("R-objects/namesnonInvariantMain.rds") # checked
codebook_M7 <- read_excel(path = "CODEBOOKS/Fanny_codebook.xlsx", sheet = "SCI_Etimss") # checked

# names(codebook_M7)
# table(codebook_M7$`Content Domain`) # earth science; life science; physical science
# table(codebook_M7$`Topic Area`[codebook_M7$`Content Domain`=="Earth Science"]) # 3: Earth in the solar system; Earth's Physical Characteristics..; Earth's weather and climates
# table(codebook_M7$`Topic Area`[codebook_M7$`Content Domain`=="Life Science"]) # 6: Characteristics of life and processes of organisms, human health, organisms
# table(codebook_M7$`Topic Area`[codebook_M7$`Content Domain`=="Physical Science"]) # 3: Classificatins of properties of matter


# Functions 
contentcoverage <- function(ContentDomain, indexcol){
  df <- data.frame(`N items` = numeric(),
                   `Content Domain` = character(),
                   `Booklet Number` = numeric())
  for (booklet in 1:14){
    if(booklet< 14){
      df[booklet,1] <- table(codebook_M7$Block[codebook_M7[,indexcol]==ContentDomain])[[booklet]] + table(codebook_M7$Block[codebook_M7[,indexcol]==ContentDomain])[[booklet + 1]]
      
    } else {
      df[booklet,1] <-  table(codebook_M7$Block[codebook_M7[,indexcol]==ContentDomain])[[booklet]] + table(codebook_M7$Block[codebook_M7[,indexcol]==ContentDomain])[[booklet - booklet + 1]]
    }
    df[booklet,2] <- ContentDomain
    df[booklet,3] <- booklet
  }
  return(df)
}

# Program: 
EarthDom <- contentcoverage(ContentDomain = "Earth Science", indexcol = which(names(codebook_M7) == "Content Domain"))
SciDom <- contentcoverage(ContentDomain = "Life Science", indexcol = which(names(codebook_M7) == "Content Domain"))
PhyDom <- contentcoverage(ContentDomain = "Physical Science", indexcol = which(names(codebook_M7) == "Content Domain"))

AllDom <- rbind(EarthDom, SciDom, PhyDom)

AllDom_wide <- AllDom %>%
  pivot_wider(names_from = "Content.Domain",
              values_from = "N.items")

AllDom_wide$EarthSciPerc <-round((AllDom_wide$`Earth Science`/ rowSums(AllDom_wide[2:4])*100),2)
AllDom_wide$LifeSciPerc <-round((AllDom_wide$`Life Science`/ rowSums(AllDom_wide[2:4]))*100,2)
AllDom_wide$PhysSciPerc <-round((AllDom_wide$`Physical Science`/ rowSums(AllDom_wide[2:4])*100),2)

ApplDom <- contentcoverage(ContentDomain = "Applying", indexcol = which(names(codebook_M7) == "Cognitive Domain"))
KnoDom <- contentcoverage(ContentDomain = "Knowing", indexcol = which(names(codebook_M7) == "Cognitive Domain"))
ReasDom <- contentcoverage(ContentDomain = "Reasoning", indexcol = which(names(codebook_M7) == "Cognitive Domain"))

CogDom <- rbind(ApplDom, KnoDom, ReasDom)

Cogdom_wide <- CogDom %>%
  pivot_wider(names_from = "Content.Domain",
              values_from = "N.items")

Cogdom_wide$ApplyingPerc <-round((Cogdom_wide$Applying/ rowSums(Cogdom_wide[2:4])*100),2)
Cogdom_wide$KnowingPerc <- round((Cogdom_wide$Knowing/ rowSums(Cogdom_wide[2:4])*100),2)
Cogdom_wide$ReasoningPerc <- round((Cogdom_wide$Reasoning/ rowSums(Cogdom_wide[2:4])*100),2)

Alldomains <- merge(Cogdom_wide, AllDom_wide, by = "Booklet.Number")



# My data coverage --------------------------------------------------------

noninvariant <- gsub("S", "SE", noninvariant)


ContentDom <- c()
for (item in 1:length(noninvariant)){
  ContentDom[item] <- codebook_M7$`Content Domain`[codebook_M7$`Item ID`== noninvariant[item]][1]
}

DontDomdf <- as.data.frame(table(ContentDom))
DontDomdf$Perc <- round((DontDomdf$Freq / sum(DontDomdf$Freq))*100,2)
names(DontDomdf)[1] <- "Domain"

CognitiveDom <- c()
for (item in 1:length(noninvariant)){
  CognitiveDom[item] <- codebook_M7$`Cognitive Domain`[codebook_M7$`Item ID`== noninvariant[item]][1]
}

CogDomdf <- as.data.frame(table(CognitiveDom))
CogDomdf$Perc <-round( (CogDomdf$Freq / sum(CogDomdf$Freq))*100,2)
names(CogDomdf)[1] <- "Domain"

MyContCoverage <- rbind(DontDomdf, CogDomdf) 

CognitiveDomNin <-  data.frame(item = character(),
                               Domain = character())

for (item in 1:length(noninvariant)){
  CognitiveDomNin[item,"Domain"] <- codebook_M7$`Cognitive Domain`[codebook_M7$`Item ID`== noninvariant[item]][1]
  CognitiveDomNin[item, "item"] <- noninvariant[item]

}

ContDomNin <- data.frame(item = character(),
                         Domain = character())

for (item in 1:length(noninvariant)){
  ContDomNin[item, "Domain"] <- codebook_M7$`Content Domain`[codebook_M7$`Item ID`== noninvariant[item]][1]
  ContDomNin[item, "item"] <- noninvariant[item]
}

DIFdomain <- merge(CognitiveDomNin, ContDomNin, by= "item")
names(DIFdomain) <- c("Item", "Cognitive", "Content")
DIFdomain <- DIFdomain[order(DIFdomain$Cognitive,DIFdomain$Content,DIFdomain$Item),]
View(DIFdomain)



##### item.type
df.type <- data.frame(`Item ID` = allitems, "DIF" = rep("No eval",70) )
df.type$DIF <- ifelse(df.type$Item.ID %in% DIFdomain$Item, "DIF", "NO DIF")
colnames(df.type)[1] <- "Item ID"
item.type <- merge(df.type, codebook_M7, by = "Item ID", all.x = T)


subset_data <- item.type[item.type$`Content Domain` == "Physical Science" & item.type$DIF == "DIF", ]


# export file -------------------------------------------------------------
write.xlsx(Alldomains, "Results/domainCoverage.xlsx")
write.xlsx(DIFdomain, "Results/DIFCoverage.xlsx")
