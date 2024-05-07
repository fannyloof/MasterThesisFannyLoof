
###################################
##### DIF analysis GenIm        ###
###################################

#libraries
library(mirt)
library(readxl)
library("haven")
library("readxl")
library(tidyr)
library(dplyr)

library(ggplot2)
## DATA 
# scored data
dat <- readRDS("R-objects/Scored_blocks_ver3.rds") # checked

# Items deemed non-invariant across etimss and papertimss
mode.noninvar <- readRDS("R-objects/modenonivariant.rds") # checked
#Functions
source("Functions/PlotUtilities.R")
source("Functions/MT_functions_Wrangel.R")


# program -----------------------------------------------------------------


fulldata <- blockmerger(scoredlist = dat, c(2:14))
dim(fulldata)
dim(fulldata[fulldata$mode_adm == "Etimss",])
dim(fulldata[fulldata$mode_adm == "Ptimss",])
# previous results --------------------------------------------------------


# Data preparation -------------------------------------------------------

## Set NA to obervations of Ptimms that were non invariant
index <- match(mode.noninvar, names(fulldata))

for (i in 1:length(index)){
  print(length(fulldata[fulldata$mode_adm == "Ptimss", index[i]]))
  fulldata[fulldata$mode_adm == "Ptimss",index[i]] <- NA
}

## Choosing items with atleast 25 observations
item4eval <- c()


for (item in 6:length(fulldata)) {
  evaluated <- c()
  
  # Check if each group has at least 25 non-NA responses for the current item
  for (grp in 1:4) {
    evaluated <- append(evaluated, sum(!is.na(fulldata[, item][fulldata$group == grp])) >= 25)
  }
  
  # If all groups meet the criteria, add data to new_fulldata
  if (all(evaluated)) {
    new_fulldata <- names(fulldata[item])
    item4eval <- append(item4eval,new_fulldata)
  }
}

remove_item <- c()
remove_item <- names(fulldata[6:length(fulldata)])[!(names(fulldata[6:length(fulldata)]) %in% item4eval)]



new_fulldata <- fulldata[,c("IDSTUD","group",item4eval)]
 #: 4491 obs, 71 items

## Remove all with NA
rows_all_na <- apply(new_fulldata[,3:length(new_fulldata)], 1, function(x) all(is.na(x)))

new_fulldata.noNA <- new_fulldata[!rows_all_na,]
 #: 3840 obs, 71 items


# 
## remove items that have only one response category

onerespit <- c()
for (grp in 1:4) {
  for (i in 3:ncol(new_fulldata.noNA)) {
    group_data <- new_fulldata.noNA[new_fulldata.noNA$group == grp, i]
    mean_score <- sum(group_data, na.rm = TRUE) / sum(!is.na(group_data))

    if (all(mean_score %in% c(0, 1))) {
      onerespit <- c(onerespit, names(new_fulldata.noNA)[i])
    }
  }
}


#   
new_fulldata.noNA <- new_fulldata.noNA[,!colnames(new_fulldata.noNA)%in%onerespit]
 #: 3840 obs, 70 items


## UPDATE:  Remove all with NA
rows_all_na <- apply(new_fulldata.noNA[,3:length(new_fulldata.noNA)], 1, function(x) all(is.na(x)))
new_fulldata.noNA <- new_fulldata.noNA[!rows_all_na,]
#  3840 obs, 70 items


# DIF ANALYSIS ------------------------------------------------------------


#### fit multigroup
modeldata <- new_fulldata.noNA[,-c(1:2)]
# saveRDS(names(modeldata),"R-objects/itemsevaluated.rds")

scalar.main <- multipleGroup(data = modeldata, itemtype = "Rasch",
                              group = as.factor(new_fulldata.noNA$group),
                              invariance = c("free_means", "free_var", names(modeldata)),
                              SE = TRUE,
                              verbose = FALSE,
                              TOL = 0.000001)


### DIF analysis
Dif.results <- DIF(scalar.main, which.par = c("d"), scheme = "drop", items2test = 1:length(modeldata), p.adjust = "BH", TOL = 0.000001)
# saveRDS(Dif.results, "R-objects/Dif.ResultsMain.rds")
dif.items <- rownames(Dif.results[Dif.results$p<=0.05,])
dif.itemsBH <- Dif.results[Dif.results$adj_p <=0.05,]

noninvariant <- colnames(modeldata) %in% dif.items
noninvariantBH <- colnames(modeldata) %in% dif.itemsBH

partial.main <- multipleGroup(data = modeldata, itemtype = "Rasch",
                              group = as.factor(new_fulldata.noNA$group),
                              invariance = c("free_means", "free_var", names(modeldata[,!noninvariant])),
                              SE = TRUE,
                              verbose = FALSE,
                              TOL = 0.000001)


partial.mainBH <- multipleGroup(data = modeldata, itemtype = "Rasch",
                           group = as.factor(new_fulldata.noNA$group),
                           invariance = c("free_means", "free_var", names(modeldata[,!noninvariantBH])),
                           SE = TRUE,
                           verbose = FALSE,
                           TOL = 0.000001)

##Mean confidence interval
coef(partial.mainBH, SEpar=TRUE)['1']
coef(partial.mainBH, SEpar=TRUE)['2']
coef(partial.mainBH, SEpar=TRUE)['3']
coef(partial.mainBH, SEpar=TRUE)['4']

## save objects
saveRDS(scalar.main, "R-objects/scalarmain.rds") #checked
saveRDS(partial.main,"R-objects/partialmain.rds") #checked
saveRDS(partial.mainBH, "R-objects/partialmainBH.rds") #checked


# model comparison --------------------------------------------------------


configural <- readRDS("R-objects/Configural.rds") #checked
partialinvar <- readRDS("R-objects/partialmain.rds") #checked
partialinvarBH <- readRDS("R-objects/partialmainBH.rds") #checked

anova(partialinvar, configural)
#Results:
#                   AIC    SABIC       HQ      BIC    logLik      X2  df     p
# partialinvar 49641.60 50044.51 49932.58 50460.77 -24689.80                  
# configural   49772.25 50645.75 50403.07 51548.17 -24602.13 175.348 153 0.104

anova(partialinvarBH, configural)
#Results:
#                     AIC    SABIC       HQ      BIC    logLik      X2  df p
# partialinvarBH 49680.18 49990.82 49904.52 50311.75 -24739.09              
# configural     49772.25 50645.75 50403.07 51548.17 -24602.13 273.926 183 0


# Create objects ----------------------------------------------------------

## Non invariant items w/o BH adjustment
itname.noninvar <- names(modeldata[,noninvariant])
# saveRDS(itname.noninvar,"R-objects/namesnonInvariantMain.rds")
indexitems <- match(itname.noninvar, names(modeldata))

## Non invariant items w BH adjustment
itname.noninvarBH <- names(modeldata[,noninvariantBH])
# saveRDS(itname.noninvarBH, "R-objects/namesnonInvariantBHMain.rds")
indexitemsBH <- match(itname.noninvarBH, names(modeldata))


# To not have to run entire program ---------------------------------------
#Functions
source("Functions/PlotUtilities.R")
## Data 
# used for DIF evaluation
dat <- readRDS("R-objects/Data_Use4ModelEstMain.rds") #checked

# non-invariant item names
itname.noninvar <- readRDS("R-objects/namesnonInvariantMain.rds") #checked
itname.noninvarBH <- readRDS("R-objects/namesnonInvariantBHMain.rds") #checked

## Model objects
scalar.main <- readRDS("R-objects/scalarmain.rds") #checked
partial.mainBH <- readRDS("R-objects/partialmainBH.rds") #checked
partial.main <- readRDS("R-objects/partialmain.rds") #checked



modeldata <- dat[,-c(1:2)]
indexitemsBH <- match(itname.noninvarBH, names(modeldata))
indexitems <- match(itname.noninvar, names(modeldata))
## W/o & W BH adjustment
TCCdataBH <- plotData(model_res = scalar.main, model_ures = partial.mainBH, items = names(modeldata),blockName = "With BH adjustment")
TCCdata <- plotData(model_res = scalar.main, model_ures =  partial.mainBH, items = names(modeldata),blockName = "Without BH adjustment")
TCCdata <- rbind(TCCdataBH,TCCdata)

infoBH <- PlotDataInfo(model_res = scalar.main, model_ures = partial.mainBH, Itm = names(modeldata) ,blockName = "BH adjusted p-values")
info <- PlotDataInfo(model_res = scalar.main, model_ures = partial.main, Itm = names(modeldata) ,blockName = "not adjusted p-values")

info <- rbind(info,infoBH)


ICCwoW <- DIFitemplotData(mObj = partial.main, itemindex = indexitems, itemnnames = itname.noninvar)
ICCwoW$itemNam <- gsub("S", "SE", ICCwoW$itemNam)
ICCwoWBH <- DIFitemplotData(mObj = partial.mainBH, itemindex = indexitemsBH, itemnnames = itname.noninvarBH)
ICCwoWBH$itemNam <- gsub("S", "SE", ICCwoWBH$itemNam)

itname.noninvar <- gsub("S", "SE", itname.noninvar)
itname.noninvarBH <- gsub("S", "SE", itname.noninvarBH)

# Create figures ----------------------------------------------------------

## TCC
mycolors = c("#0bdd9b", "#2e4052", "#ffc857", "#e5323b")
## TCC
ggplot(data = TCCdata[!TCCdata$Group == "Restricted",], aes(x = Theta, y = value, color = Group, linetype = Group)) +
  geom_line(linewidth = 2) +
  facet_wrap(~Block) +
  # ggtitle("Test Characteristic Curves", subtitle = "Adjusted for DIF") +
  ylab("Expected Test score") +
  xlab(expression(Theta)) +
  scale_linetype_manual(values =c("solid", "dashed", "dotted", "dotdash"))+
  scale_color_manual(values = mycolors)+
  theme(
    axis.title.y = element_text(size = 18), # Increase y-axis label size
    axis.title.x = element_text(size = 18),
    strip.text = element_text(size = 18),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.position = "bottom" # Increase legend text size
  )

colnames(ICCwoW)[2] <- "Group"
ggplot(data = ICCwoW[ICCwoW$itemNam %in% itname.noninvar[1:4],], aes(x = theta, y = prob, color = Group, linetype = Group)) +
  geom_line(size = 2) +
  facet_wrap(~itemNam) +
  scale_linetype_manual(values =c("solid", "dashed", "dotted", "dotdash"))+
  scale_color_manual(values = mycolors)+
  #ggtitle("Item Characteristic Curves", subtitle = "Adjusted for DIF without BH-adjustment without Student weights") +
  ylab("Expected Item probabilities") +
  xlab(expression(Theta)) +
  theme(axis.title.y = element_text(size = 18), # Increase y-axis label size
        axis.title.x = element_text(size = 18),
        strip.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.position = "bottom" 
        )

ggplot(data = ICCwoW[ICCwoW$itemNam %in% itname.noninvar[5:8],], aes(x = theta, y = prob, color = Group, linetype = Group)) +
  geom_line(size = 2) +
  facet_wrap(~itemNam) +
  scale_linetype_manual(values =c("solid", "dashed", "dotted", "dotdash"))+
  scale_color_manual(values = mycolors)+
  #ggtitle("Item Characteristic Curves", subtitle = "Adjusted for DIF without BH-adjustment without Student weights") +
  ylab("Expected Item probabilities") +
  xlab(expression(Theta))  +
  theme(axis.title.y = element_text(size = 18), # Increase y-axis label size
        axis.title.x = element_text(size = 18),
        strip.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.position = "bottom" 
  )

ggplot(data = ICCwoW[ICCwoW$itemNam %in% itname.noninvar[9:12],], aes(x = theta, y = prob, color = Group, linetype = Group)) +
  geom_line(size = 2) +
  facet_wrap(~itemNam) +
  scale_linetype_manual(values =c("solid", "dashed", "dotted", "dotdash"))+
  scale_color_manual(values = mycolors)+
  #ggtitle("Item Characteristic Curves", subtitle = "Adjusted for DIF without BH-adjustment without Student weights") +
  ylab("Expected Item probabilities") +
  xlab(expression(Theta))  +
  theme(axis.title.y = element_text(size = 18), # Increase y-axis label size
        axis.title.x = element_text(size = 18),
        strip.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.position = "bottom" 
  )

ggplot(data = ICCwoW[ICCwoW$itemNam %in% itname.noninvar[13:16],], aes(x = theta, y = prob, color = Group, linetype = Group)) +
  geom_line(size = 2) +
  facet_wrap(~itemNam) +
  scale_linetype_manual(values =c("solid", "dashed", "dotted", "dotdash"))+
  scale_color_manual(values = mycolors)+
  #ggtitle("Item Characteristic Curves", subtitle = "Adjusted for DIF without BH-adjustment without Student weights") +
  ylab("Expected Item probabilities") +
  xlab(expression(Theta))  +
  theme(axis.title.y = element_text(size = 18), # Increase y-axis label size
        axis.title.x = element_text(size = 18),
        strip.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.position = "bottom" 
  )

ggplot(data = ICCwoW[ICCwoW$itemNam %in% itname.noninvar[17:18],], aes(x = theta, y = prob, color = Group, linetype = Group)) +
  geom_line(size = 2) +
  facet_wrap(~itemNam) +
  scale_linetype_manual(values =c("solid", "dashed", "dotted", "dotdash"))+
  scale_color_manual(values = mycolors)+
  #ggtitle("Item Characteristic Curves", subtitle = "Adjusted for DIF without BH-adjustment without Student weights") +
  ylab("Expected Item probabilities") +
  xlab(expression(Theta))  +
  theme(axis.title.y = element_text(size = 18), # Increase y-axis label size
        axis.title.x = element_text(size = 18),
        strip.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.position = "bottom" 
  )


ggplot(data = ICCwoWBH[ICCwoWBH$itemNam %in% itname.noninvarBH[1:4],], aes(x = theta, y = prob, color = groupcol)) +
  geom_line() +
  facet_wrap(~itemNam) +
  #ggtitle("Item Characteristic Curves", subtitle = "Adjusted for DIF with BH-adjustment without Student weights") +
  ylab("Expected Item probabilities") +
  xlab(expression(Theta))

ggplot(data = ICCwoWBH[ICCwoWBH$itemNam %in% itname.noninvarBH[5:8],], aes(x = theta, y = prob, color = groupcol)) +
  geom_line() +
  facet_wrap(~itemNam) +
  #ggtitle("Item Characteristic Curves", subtitle = "Adjusted for DIF with BH-adjustment without Student weights") +
  ylab("Expected Item probabilities") +
  xlab(expression(Theta))

### Test information

ggplot(data = info[!info$Group=="Restricted",], aes(x = Theta, y = value, color = Group, linetype = Group)) +
  geom_line(linewidth = 2) +
  facet_wrap(~Block) +
  # ggtitle("Test Characteristic Curves", subtitle = "Adjusted for DIF") +
  ylab("Expected Test score") +
  xlab(expression(Theta)) +
  scale_linetype_manual(values =c("solid", "dashed", "dotted", "dotdash"))+
  scale_color_manual(values = mycolors)+
  # ggtitle("Test Information Curves", subtitle = "Adjusted for DIF") +
  theme(
    axis.title.y = element_text(size = 18), # Increase y-axis label size
    axis.title.x = element_text(size = 18),
    strip.text = element_text(size = 18),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.position = "bottom"# Increase legend text size
  )


########################
## update r-objects    #
########################
# rem_items <- read_xlsx("Results/removed_items_mars.xlsx") #checked
# 
# reason <- data.frame(item = c(remove_item,onerespit), reason = c(rep("Less than 25 observations", length(remove_item)),c("One response category")))
# reason$item <- gsub("S", "SE", reason$item)
# rem_items <- rbind(reason,rem_items)
# 
# write.xlsx(rem_items, "Results/removed_items_mars.xlsx")


