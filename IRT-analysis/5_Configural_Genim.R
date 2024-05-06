#################################
###       IRT-analysis        ###  
###   Configural invariance   ###
###     Gender/immigration    ###
#################################


# load packages
library(mirt)
library(openxlsx)
library(expss)
library(haven)
## data
#Scored blocks
dat <- readRDS("R-objects/Scored_blocks_ver3.rds") #checked
#Items deemed non-invariant across etimss and papertimss
mode.noninvar <- readRDS("R-objects/modenonivariant.rds") #checked


# load functions
source("Functions/MT_functions_Wrangel.R")
source("Functions/analysis_functions.R")
# create object
fulldata <- blockmerger(scoredlist = dat, c(2:14))

remove_cols <- c("IDSTUD","ASBG01","Imm_Status","mode_adm", "group")
rm_items <- c()

#### Set mode noninvariant to NA
## For list
dat[[7]]$S51194[dat[[7]]$mode_adm == "Ptimss"] <- NA

dat[[11]]$S61161[dat[[11]]$mode_adm == "Ptimss"] <- NA


## For fulldata

index <- match(mode.noninvar, names(fulldata))

for (i in 1:length(index)){
  print(length(fulldata[fulldata$mode_adm == "Ptimss", index[i]]))
  fulldata[fulldata$mode_adm == "Ptimss",index[i]] <- NA
}

########################
##### BLOCK SE/SP01 ####
########################

## creating list w data separated
groups <- model_data(block_no = 1, remove_cols = remove_cols)



### Fit statistics Rasch model in all groups
modelfits1 <- IRT_Confmodel(list_name =  groups, block_no = 1, itemtype = "Rasch")

#### Configural and scalar
modeldat <- groups[["full_1"]][,!names(groups[["full_1"]]) %in% remove_cols]

### Fit a configural model
configural <- multipleGroup(data = modeldat, 1, itemtype = "Rasch", group = as.factor(groups[["full_1"]]$group))

#### Fit statistics configural model
fitcon1 <- M2(configural, type = "M2*", na.rm = T)
configural #to get BIC and AIC

### Fit a scalar model to block to get fit statistics
scalar <- multipleGroup(data = modeldat, 1, itemtype = "Rasch",
                        group = as.factor(groups[["full_1"]]$group),
                        invariance = c("free_means", "free_var", names(modeldat)),
                        SE = TRUE,
                        verbose = FALSE)

### Fit statistics scalar model
fitscal1 <- M2(scalar, type = "M2*", na.rm = T) #
scalar

conscal <- rbind(con1 = fitcon1, scal1 = fitscal1) #

########################
##### BLOCK SE/SP02 
##### NOT ESTIMATED
########################

### does not meet requirement no.obs > no.items

########################
##### BLOCK SE/SP03 ####
########################

## creating list w data separated
groups <- model_data(block_no = 3, remove_cols = remove_cols)

# modelfits3 <- IRT_Confmodel(list_name = groups, block_no = 3, itemtype = "Rasch")

#!!!!! too few resp categories !!!!!!#
remItem <- resp_category(list = groups)

#!!!! solution !!!!!#
groups_notfull <- remove_items(list = groups, itemvec = remItem,block_no = 3)

### Fit statistics Rasch model in all groups
modelfits3 <- IRT_Confmodel(list_name = groups_notfull, block_no = 3, itemtype = "Rasch")

#### Configural and scalar
modeldat <- groups_notfull[["block_3"]]

### Fit a configural model

configural <- multipleGroup(data = modeldat, 1, itemtype = "Rasch", group = as.factor(groups[["full_3"]]$group))

#### Fit statistics configural model
fitcon3 <- M2(configural, type = "M2*", na.rm = T)
configural
#### Fit scalar model

scalar <- multipleGroup(data = modeldat, 1, itemtype = "Rasch",
                        group = as.factor(groups[["full_3"]]$group),
                        invariance = c("free_means", "free_var", names(modeldat)),
                        SE = TRUE,
                        verbose = FALSE)

### Fit statistics scalar model
fitscal3 <- M2(scalar, type = "M2*", na.rm = T) #?
scalar


conscal3 <- rbind(con3 = fitcon3, scal3 = fitscal3) #?
conscal <- rbind(conscal, conscal3) #?

# # save removed items
# rm_items <- append(rm_items, remItem)
# 
# # update df 
# dat[[3]] <- dat[[3]][, !names(dat[[3]]) %in% remItem]

########################
##### BLOCK SE/SP04 ####
########################

### does not meet requirement no.obs > no.items

########################
##### BLOCK SE/SP05 ####
########################

## creating list w data separated
groups <- model_data(block_no = 5, remove_cols = remove_cols)

### ### Fit statistics Rasch model in all groups
modelfits5 <- IRT_Confmodel(list_name = groups, block_no = 5, itemtype = "Rasch")

# #!!!!! too few resp categories !!!!!!#
# remItem <- resp_category(list = groups)
# 
# #!!!! solution !!!!!#
# groups_notfull <- remove_items(list = groups, itemvec = remItem,block_no = 5)
# 
# ### Fit statistics Rasch model in all groups
# modelfits5 <- IRT_Confmodel(list_name = groups_notfull, block_no = 5, itemtype = "Rasch")

#### Configural and scalar
modeldat <- groups[["full_5"]]

### Fit a configural model
configural <- multipleGroup(data = modeldat[,c(6:length(modeldat))], 1, itemtype = "Rasch", group = as.factor(groups[["full_5"]]$group))

#### Fit statistics configural model
fitcon5 <- M2(configural, type = "M2*", na.rm = T)
configural

#### Fit scalar model

scalar <- multipleGroup(data = modeldat[,6:length(modeldat)], 1, itemtype = "Rasch",
                        group = as.factor(groups[["full_5"]]$group),
                        invariance = c("free_means", "free_var", names(modeldat)),
                        SE = TRUE,
                        verbose = FALSE)

### Fit statistics scalar model
fitscal5 <- M2(scalar, type = "M2*", na.rm = T)
scalar

conscal5 <- rbind(con5 = fitcon5, scal5 = fitscal5)
conscal <- rbind(conscal, conscal5)
########################
##### BLOCK SE/SP06. 
##### only model on full
########################

### does not meet requirement no.obs > no.items

########################
##### BLOCK SE/SP07 ####
########################

### does not meet requirement no.obs > no.items

########################
##### BLOCK SE/SP08 ####
########################

### does not meet requirement no.obs > no.items

########################
##### BLOCK SE/SP09 ####
########################

## creating list w data separated
groups <- model_data(block_no = 9, remove_cols = remove_cols)

### Fit statistics for Configural model in all groups
modelfits9 <- IRT_Confmodel(list_name = groups, block_no = 9, itemtype = "Rasch")

#### Configural and scalar
modeldat <- groups[["full_9"]][,!names(groups[["full_9"]]) %in% remove_cols]

### Fit a configural model
configural <- multipleGroup(data = modeldat, 1, itemtype = "Rasch", group = as.factor(groups[["full_9"]]$group))

#### Fit statistics configural model
fitcon9 <- M2(configural, type = "M2*", na.rm = T)
configural
#### Fit scalar model
scalar <- multipleGroup(data = modeldat, 1, itemtype = "Rasch",
                        group = as.factor(groups[["full_9"]]$group),
                        invariance = c("free_means", "free_var", names(modeldat)),
                        SE = TRUE,
                        verbose = FALSE)

### Fit statistics scalar model
fitscal9 <- M2(scalar, type = "M2*", na.rm = T)
scalar

conscal9 <- rbind(con9 = fitcon9, scal9 = fitscal9)
conscal <- rbind(conscal, conscal9)
########################
##### BLOCK SE/SP10 ####
########################

### does not meet requirement no.obs > no.items

########################
##### BLOCK SE/SP11 ####
########################

## creating list w data separated
groups <- model_data(block_no = 11, remove_cols = remove_cols)

### Fit evaluation Rasch model in all groups
modelfits11 <- IRT_Confmodel(list_name = groups, block_no = 11, itemtype = "Rasch")

#### Configural and scalar 
modeldat <- groups[["full_11"]][,!names(groups[["full_11"]]) %in% remove_cols]

### Fit a configural model
configural <- multipleGroup(data = modeldat, 1, itemtype = "Rasch", group = as.factor(groups[["full_11"]]$group))

#### Fit statistics configural model
fitcon11 <- M2(configural, type = "M2*", na.rm = T)
configural
#### Fit scalar model
scalar <- multipleGroup(data = modeldat, 1, itemtype = "Rasch",
                        group = as.factor(groups[["full_11"]]$group),
                        invariance = c("free_means", "free_var", names(modeldat)),
                        SE = TRUE,
                        verbose = FALSE)

### Fit statistics scalar model
fitscal11 <- M2(scalar, type = "M2*", na.rm = T)
scalar

conscal11 <- rbind(con11 = fitcon11, scal11 = fitscal11)
conscal <- rbind(conscal, conscal11)
########################
##### BLOCK SE/SP12
########################

### does not meet requirement no.obs > no.items

########################
##### BLOCK SE/SP13 ####
########################

## creating list w data separated
groups <- model_data(block_no = 13, remove_cols = remove_cols)

### Fit evaluation Rasch model in all groups
modelfits13 <- IRT_Confmodel(list_name = groups, block_no = 13, itemtype = "Rasch")

#### Configural and Scalar
modeldat <- groups[["full_13"]][,!names(groups[["full_13"]]) %in% remove_cols]

### Fit a configural model
configural <- multipleGroup(data = modeldat, 1, itemtype = "Rasch", group = as.factor(groups[["full_13"]]$group))

#### Fit statistics configural model
fitcon13 <- M2(configural, type = "M2*", na.rm = T)
configural
#### Fit scalar model
scalar <- multipleGroup(data = modeldat, 1, itemtype = "Rasch",
                        group = as.factor(groups[["full_13"]]$group),
                        invariance = c("free_means", "free_var", names(modeldat)),
                        SE = TRUE,
                        verbose = FALSE)

### Fit statistics scalar model
fitscal13 <- M2(scalar, type = "M2*", na.rm = T)
scalar


conscal13 <- rbind(con13 = fitcon13, scal13 = fitscal13)
conscal <- rbind(conscal, conscal13)
########################
##### BLOCK SE/SP14 
########################

### does not meet requirement no.obs > no.items


###############################
# Compare configural & Scalar #
###############################

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






new_fulldata <- fulldata[,c("IDSTUD","group",item4eval)]


## Remove all with NA
rows_all_na <- apply(new_fulldata[,3:length(new_fulldata)], 1, function(x) all(is.na(x)))

new_fulldata.noNA <- new_fulldata[!rows_all_na,]



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



## UPDATE:  Remove all with NA
rows_all_na <- apply(new_fulldata.noNA[,3:length(new_fulldata.noNA)], 1, function(x) all(is.na(x)))
new_fulldata.noNA <- new_fulldata.noNA[!rows_all_na,]


# saveRDS(new_fulldata.noNA, "R-objects/Data_Use4ModelEstMain.rds")
# DIF ANALYSIS ------------------------------------------------------------


#### fit multigroup
modeldata <- new_fulldata.noNA[,-c(1:2)]

configural_alldata <- multipleGroup(data = modeldata, 1, group = as.factor(new_fulldata.noNA$group), itemtype = "Rasch", TOL = 0.000001) 


scalar_alldata <- multipleGroup(data = modeldata, 1, group = as.factor(new_fulldata.noNA$group), itemtype = "Rasch",
                                invariance = c("free_means", "free_var", names(modeldata)),
                                SE = TRUE,
                                verbose = FALSE,
                                TOL = 0.000001)

anova(scalar_alldata,configural_alldata)

#### Results:
#                         AIC    SABIC       HQ      BIC    logLik      X2  df p
# scalar_alldata     49781.14 50017.96 49952.17 50262.63 -24813.57              
# configural_alldata 49772.25 50645.75 50403.07 51548.17 -24602.13 422.884 207 0

#conclusion: the configural model fits the data significantly better, it's therefore appropriate to evaluate a partial invariant model

##########################
##     save results.    ##
##########################
# wb = createWorkbook() #create an empty workbook
# 
# sh = addWorksheet(wb, "Block1")
# xl_write(modelfits1, wb, sh, rownames = T)
# addWorksheet(wb,"Block3")
# writeData(wb,"Block3",modelfits3, rowNames = TRUE)
# addWorksheet(wb,"Block5")
# writeData(wb,"Block5",modelfits5, rowNames = TRUE)
# addWorksheet(wb,"Block9")
# writeData(wb,"Block9",modelfits9, rowNames = TRUE)
# addWorksheet(wb,"Block11")
# writeData(wb,"Block11",modelfits11, rowNames = TRUE)
# addWorksheet(wb,"Block13")
# writeData(wb,"Block13",modelfits13, rowNames = TRUE)
# 
# addWorksheet(wb, "Conscal")
# writeData(wb,"Conscal", conscal, rowNames = TRUE)
# 
# saveWorkbook(wb, "Results/Config_MainGroup_Rasch.xlsx", overwrite = TRUE)
# 
# # Save object
# saveRDS(configural_alldata, "R-objects/Configural.rds")

# rem_items <- read_xlsx("Results/removed_items_mars.xlsx")
# oneCat_items <- data.frame(item = rm_items, reason = rep("One Resp Category", length(rm_items)))
# rem_items <- rbind(oneCat_items,rem_items)
# write.xlsx(rem_items, "Results/removed_items_mars.xlsx")




