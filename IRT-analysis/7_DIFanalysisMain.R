
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
#load data
dat <- readRDS("R-objects/Scored_blocks_ver3.rds")
mode.noninvar <- readRDS("R-objects/modenonivariant.rds")
#Functions
source("Functions/PlotUtilities.R")
source("Functions/MT_functions_Wrangel.R")
source("Functions/analysis_functions.R")

# program -----------------------------------------------------------------


fulldata <- blockmerger(scoredlist = dat, c(2:14))
dim(fulldata)


# previous results --------------------------------------------------------


# difitems <- read_excel(path = "Results/MGParameters.xlsx", sheet = "DIF items") ## items from former results
# difitemsBH <- read_excel(path = "Results/MGParameters.xlsx", sheet = "DIF items BH adj") ## items from former results
# # View(difitems)
# invariantitems <- unique(difitems$ItName)
# invariantitemsBH <- unique(difitemsBH$ItName)
# length(difitems$ItName)
# length(invariantitems)



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
length(remove_item)

item4eval


new_fulldata <- fulldata[,c("IDSTUD","group",item4eval)]
dim(new_fulldata) #: 4491 obs, 65 items

## Remove all with NA
rows_all_na <- apply(new_fulldata[,3:length(new_fulldata)], 1, function(x) all(is.na(x)))

new_fulldata.noNA <- new_fulldata[!rows_all_na,]
dim(new_fulldata.noNA) #: 3840 obs, 65 items



# ## Remove MODE non-invariant items
# new_fulldata.noNA <- new_fulldata.noNA[, !colnames(new_fulldata.noNA) %in% mode_invariant]
# dim(new_fulldata.noNA) #: 3840 obs, 64 items
# 
# ## UPDATE:  Remove all with NA
# rows_all_na <- apply(new_fulldata.noNA[,3:length(new_fulldata.noNA)], 1, function(x) all(is.na(x)))
# new_fulldata.noNA <- new_fulldata.noNA[!rows_all_na,]
# dim(new_fulldata.noNA) #: 3838 obs, 64 items
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

onerespit
#   
new_fulldata.noNA <- new_fulldata.noNA[,!colnames(new_fulldata.noNA)%in%onerespit]
dim(new_fulldata.noNA) #: 3840 obs, 64 items


## UPDATE:  Remove all with NA
rows_all_na <- apply(new_fulldata.noNA[,3:length(new_fulldata.noNA)], 1, function(x) all(is.na(x)))
new_fulldata.noNA <- new_fulldata.noNA[!rows_all_na,]
# dim(new_fulldata.noNA)# 3840 obs, 64 items

# saveRDS(new_fulldata.noNA, "R-objects/Data_Use4ModelEstMain.rds")
# DIF ANALYSIS ------------------------------------------------------------


#### fit multigroup
modeldata <- new_fulldata.noNA[,-c(1:2)]

modelreswoWe <- multipleGroup(data = modeldata, itemtype = "Rasch",
                              group = as.factor(new_fulldata.noNA$group),
                              invariance = c("free_means", "free_var", names(modeldata)),
                              SE = TRUE,
                              verbose = FALSE,
                              TOL = 0.000001)

### DIF analysis
Dif.results <- DIF(modelreswoWe, which.par = c("d"), scheme = "drop", items2test = 1:length(modeldata), p.adjust = "BH", TOL = 0.000001)
# saveRDS(Dif.results, "R-objects/Dif.ResultsMain.rds")
dif.items <- rownames(Dif.results[Dif.results$p<=0.05,])
dif.itemsBH <- rownames(Dif.results[Dif.results$adj_p<=0.05,])
noninvariant <- colnames(modeldata) %in% dif.items
noninvariantBH <- colnames(modeldata) %in% dif.itemsBH
### correct models
modelfull <- multipleGroup(data = modeldata, itemtype = "Rasch",
                           group = as.factor(new_fulldata.noNA$group),
                           invariance = c("free_means", "free_var", names(modeldata[,!noninvariant])),
                           SE = TRUE,
                           verbose = FALSE,
                           TOL = 0.000001)

modelfullBH <- multipleGroup(data = modeldata, itemtype = "Rasch",
                           group = as.factor(new_fulldata.noNA$group),
                           invariance = c("free_means", "free_var", names(modeldata[,!noninvariantBH])),
                           SE = TRUE,
                           verbose = FALSE,
                           TOL = 0.000001)

##Mean confidence interval
coef(modelreswoWe, SEpar=TRUE)['1']
coef(modelreswoWe, SEpar=TRUE)['2']
coef(modelreswoWe, SEpar=TRUE)['3']
coef(modelreswoWe, SEpar=TRUE)['4']

## save objects
saveRDS(modelreswoWe, "R-objects/modelres.rds")
saveRDS(modelfull,"R-objects/modelfull.rds")
saveRDS(modelfullBH, "R-objects/modelfullBH.rds")


# model comparison --------------------------------------------------------


configural <- readRDS("R-objects/configuralmodel.rds")
partialinvar <- readRDS("R-objects/modelfull.rds")
partialinvarBH <- readRDS("R-objects/modelfullBH.rds")

anova(partialinvar, configural)
#Results:
#                   AIC    SABIC       HQ      BIC    logLik      X2  df     p
# partialinvar 44920.61 45305.07 45198.26 45702.26 -22335.30                  
# configural   45032.10 45831.78 45609.62 46657.94 -22256.05 158.507 135 0.081

anova(partialinvarBH, configural)
#Results:
#                     AIC    SABIC       HQ      BIC    logLik      X2  df p
# partialinvarBH 44955.07 45247.26 45166.08 45549.13 -22382.53              
# configural     45032.10 45831.78 45609.62 46657.94 -22256.05 252.967 165 0


# Create objects ----------------------------------------------------------

# saveRDS(names(modeldata),"R-objects/namesInvariantMain.rds")
itname.noninvar <- names(modeldata[,noninvariant])
# saveRDS(itname.noninvar,"R-objects/namesnonInvariantMain.rds")
indexitems <- match(itname.noninvar, names(modeldata))

itname.noninvarBH <- names(modeldata[,noninvariantBH])
# saveRDS(itname.noninvarBH, "R-objects/namesnonInvariantBHMain.rds")
indexitemsBH <- match(itname.noninvarBH, names(modeldata))


# To not have to run entire program ---------------------------------------
#Functions
source("Functions/PlotUtilities.R")
#Data used for DIF evaluation
dat <- readRDS("R-objects/Data_Use4ModelEstMain.rds")

# dim(dat)
modelreswoWe <- readRDS("R-objects/modelres.rds")
modelfullBH <- readRDS("R-objects/modelfullBH.rds")
modelfull <- readRDS("R-objects/modelfull.rds")

itname.noninvar <- readRDS("R-objects/namesnonInvariantMain.rds")

itname.noninvarBH <- readRDS("R-objects/namesnonInvariantBHMain.rds")

modeldata <- dat[,-c(1:2)]
indexitemsBH <- match(itname.noninvarBH, names(modeldata))
indexitems <- match(itname.noninvar, names(modeldata))
## W/o & W BH adjustment
TCCdataBH <- plotData(model_res = modelreswoWe, model_ures = modelfullBH, items = names(modeldata),blockName = "With BH adjustment")
TCCdata <- plotData(model_res = modelreswoWe, model_ures = modelfull, items = names(modeldata),blockName = "Without BH adjustment")
TCCdata <- rbind(TCCdataBH,TCCdata)

infoBH <- PlotDataInfo(model_res = modelreswoWe, model_ures = modelfullBH, Itm = names(modeldata) ,blockName = "BH adjusted p-values")
info <- PlotDataInfo(model_res = modelreswoWe, model_ures = modelfull, Itm = names(modeldata) ,blockName = "not adjusted p-values")

info <- rbind(info,infoBH)


ICCwoW <- DIFitemplotData(mObj = modelfull, itemindex = indexitems, itemnnames = itname.noninvar)
ICCwoW$itemNam <- gsub("S", "SE", ICCwoW$itemNam)
ICCwoWBH <- DIFitemplotData(mObj = modelfull, itemindex = indexitemsBH, itemnnames = itname.noninvarBH)
ICCwoWBH$itemNam <- gsub("S", "SE", ICCwoWBH$itemNam)

itname.noninvar <- gsub("S", "SE", itname.noninvar)
itname.noninvarBH <- gsub("S", "SE", itname.noninvarBH)

# Create figures ----------------------------------------------------------

## TCC
ggplot(data = TCCdata, aes(x = Theta, y = value, color = Group)) +
  geom_line() +
  facet_wrap(~Block) +
  ggtitle("Test Characteristic Curves", subtitle = "Adjusted for DIF") +
  ylab("Expected Test score") +
  xlab(expression(Theta))


ggplot(data = ICCwoW[ICCwoW$itemNam %in% itname.noninvar[1:4],], aes(x = theta, y = prob, color = groupcol)) +
  geom_line() +
  facet_wrap(~itemNam) +
  ggtitle("Item Characteristic Curves", subtitle = "Adjusted for DIF without BH-adjustment without Student weights") +
  ylab("Expected Item probabilities") +
  xlab(expression(Theta))

ggplot(data = ICCwoW[ICCwoW$itemNam %in% itname.noninvar[5:8],], aes(x = theta, y = prob, color = groupcol)) +
  geom_line() +
  facet_wrap(~itemNam) +
  ggtitle("Item Characteristic Curves", subtitle = "Adjusted for DIF without BH-adjustment without Student weights") +
  ylab("Expected Item probabilities") +
  xlab(expression(Theta))

ggplot(data = ICCwoW[ICCwoW$itemNam %in% itname.noninvar[9:12],], aes(x = theta, y = prob, color = groupcol)) +
  geom_line() +
  facet_wrap(~itemNam) +
  ggtitle("Item Characteristic Curves", subtitle = "Adjusted for DIF without BH-adjustment without Student weights") +
  ylab("Expected Item probabilities") +
  xlab(expression(Theta))

ggplot(data = ICCwoW[ICCwoW$itemNam %in% itname.noninvar[13:16],], aes(x = theta, y = prob, color = groupcol)) +
  geom_line() +
  facet_wrap(~itemNam) +
  ggtitle("Item Characteristic Curves", subtitle = "Adjusted for DIF without BH-adjustment without Student weights") +
  ylab("Expected Item probabilities") +
  xlab(expression(Theta))

ggplot(data = ICCwoW[ICCwoW$itemNam %in% itname.noninvar[17:18],], aes(x = theta, y = prob, color = groupcol)) +
  geom_line() +
  facet_wrap(~itemNam) +
  ggtitle("Item Characteristic Curves", subtitle = "Adjusted for DIF without BH-adjustment without Student weights") +
  ylab("Expected Item probabilities") +
  xlab(expression(Theta))


ggplot(data = ICCwoWBH[ICCwoWBH$itemNam %in% itname.noninvarBH[1:4],], aes(x = theta, y = prob, color = groupcol)) +
  geom_line() +
  facet_wrap(~itemNam) +
  ggtitle("Item Characteristic Curves", subtitle = "Adjusted for DIF with BH-adjustment without Student weights") +
  ylab("Expected Item probabilities") +
  xlab(expression(Theta))

ggplot(data = ICCwoWBH[ICCwoWBH$itemNam %in% itname.noninvarBH[5:8],], aes(x = theta, y = prob, color = groupcol)) +
  geom_line() +
  facet_wrap(~itemNam) +
  ggtitle("Item Characteristic Curves", subtitle = "Adjusted for DIF with BH-adjustment without Student weights") +
  ylab("Expected Item probabilities") +
  xlab(expression(Theta))

### Test information

ggplot(data = info, aes(x = Theta, y = value, color = Group)) +
  geom_line() +
  facet_wrap(~Block) +
  ggtitle("Test Information Curves", subtitle = "Adjusted for DIF") +
  ylab(expression(I(Theta))) +
  xlab(expression(Theta))


########################
## update r-objects    #
########################
rem_items <- read_xlsx("Results/removed_items_mars.xlsx")

reason <- data.frame(item = c(remove_item,onerespit), reason = c(rep("Less than 25 observations", length(remove_item)),c("One response category")))
reason$item <- gsub("S", "SE", reason$item)
rem_items <- rbind(reason,rem_items)

write.xlsx(rem_items, "Results/removed_items_mars.xlsx")


