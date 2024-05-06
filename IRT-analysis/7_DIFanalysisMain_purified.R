
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
source("Functions/MT_functions_Wrangel.R")


# program -----------------------------------------------------------------


fulldata <- blockmerger(scoredlist = dat, c(2:14))



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
# 3840 obs, 70 items


# DIF ANALYSIS ------------------------------------------------------------


#### fit multigroup
modeldata <- new_fulldata.noNA[,-c(1:2)]


scalar.main <- multipleGroup(data = modeldata, itemtype = "Rasch",
                             group = as.factor(new_fulldata.noNA$group),
                             invariance = c("free_means", "free_var", names(modeldata)),
                             SE = TRUE,
                             verbose = FALSE,
                             TOL = 0.000001)


### DIF analysis
# Two step procedure
step1result <- data.frame(matrix(NA,1,9))
colnames(step1result) <- c("groups","AIC","AICc","SABIC","HQ","BIC","X2","df","p")

for (i in 1:length(modeldata)){
  test.model <- multipleGroup(data = modeldata, itemtype = "Rasch",
                               group = as.factor(new_fulldata.noNA$group),
                               invariance = c("free_means", "free_var", names(modeldata[,-i])),
                               SE = TRUE,
                               verbose = FALSE,
                               TOL = 0.000001)
  print(i)
  step1temp <- DIF(test.model, c('d'), items2test = i, TOL = 0.000001)
  step1result[i,] <- step1temp
  
}

Anchors <- rownames(step1result[step1result[,"p"] > 0.05,])
test.model2 <- multipleGroup(data = modeldata, itemtype = "Rasch",
                            group = as.factor(new_fulldata.noNA$group),
                            invariance = c("free_means", "free_var", names(modeldata[,Anchors])),
                            SE = TRUE,
                            verbose = FALSE,
                            TOL = 0.000001)

nonInvar <- colnames(modeldata)[!colnames(modeldata) %in% Anchors]
tooTest <- match(nonInvar, names(modeldata))
step2result <- DIF(test.model2, c('d'), items2test = tooTest, TOL = 0.000001, p.adjust = "BH")


NoninvarPur <- rownames(step2result[step2result[,"p"] < 0.05,])
NoninvarPurBH <- rownames(step2result[step2result[,"adj_p"] < 0.05,])

coef(test.model2, SEpar=TRUE)['1']
coef(test.model2, SEpar=TRUE)['2']
coef(test.model2, SEpar=TRUE)['3']
coef(test.model2, SEpar=TRUE)['4']


# save objects ------------------------------------------------------------


saveRDS(step2result, "R-objects/Step2purResults")