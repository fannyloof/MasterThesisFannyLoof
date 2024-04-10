#################################
###      DIF - analysis       ###
###       Paper/Etimss        ###
#################################

# load packages
library(mirt)
library(openxlsx)
library(expss)
library(openxlsx)

# load data
dat <- readRDS("R-objects/Scored_blocks_ver3.rds")


#Functions
source("Functions/PlotUtilities.R")
source("Functions/MT_functions_Wrangel.R")
source("Functions/analysis_functions.R")
source("Functions/analysis_functions.R")

# empty object
rm_items <- c()

# columns to remoce
remove_cols <- c("IDSTUD","ASBG01","Imm_Status","mode_adm", "group")

fulldata <- blockmerger(scoredlist = dat,blocks2merge = c(2:14))
dim(fulldata)
#View(fulldata)


## check to see that group size is big enough

item4eval <- c()
for (item in 6:length(fulldata)) {
  evaluated <- c()
  
  # Check if each group has at least 25 non-NA responses for the current item
  for (grp in c("Etimss", "Ptimss")) {
    evaluated <- append(evaluated, sum(!is.na(fulldata[, item][fulldata$mode_adm == grp])) >= 25)
  }
  
  # If all groups meet the criteria, add data to new_fulldata
  if (all(evaluated)) {
    new_fulldata <- names(fulldata[item])
    item4eval <- append(item4eval,new_fulldata)
  }
}


item4eval 
##length(item4eval) atleast 25 in both modes!



## Remove all items with full NA for Ptimms
Etimnams <- which(apply(X = fulldata[fulldata$mode_adm == "Ptimss",-c(1:5)], MARGIN = 2, FUN = function(x) sum(is.na(x)) == nrow(fulldata[fulldata$mode_adm == "Ptimss",-c(1:5)])))

fulldataNoNA <- fulldata[,!names(fulldata) %in% names(Etimnams)]

## remove rows with full NA
rowsNa <- which(apply(X = fulldataNoNA, 1, FUN = function (x) sum(is.na(x)) == length(fulldataNoNA) - 5))

fulldataNoNA <- fulldataNoNA[-rowsNa,]
# dim(fulldataNoNA)

## how many responses per group and item?
responsedf <- data.frame()

for (itm in 6:length(fulldataNoNA)){
  
  for (grp in c("Etimss", "Ptimss")){
    
    no.resp <- sum(!is.na(fulldataNoNA[,itm][fulldataNoNA$mode_adm == grp]))
    item.name <- colnames(fulldataNoNA[itm])
    # grp <- as.factor(grp)
    responsedf <- rbind(responsedf,cbind(no.resp,item.name, grp))
  }
  
  
}

View(responsedf)

responsedf_wide <- pivot_wider(responsedf,
                               values_from = "no.resp",
                               names_from = "grp"
)

View(responsedf_wide)
responsedf_wide$Etimss <- as.numeric(responsedf_wide$Etimss)
responsedf_wide$Ptimss <- as.numeric(responsedf_wide$Ptimss)
summary(responsedf_wide) ##no group as small as 25. so BH adjustment is warranted.

dim(fulldata)# 3952 
dim(fulldataNoNA) #3944

###Estimating models
modeldata <- fulldataNoNA[,-c(1:5)]

#### Configural model:
configural.mode <- multipleGroup(data = modeldata, itemtype = "Rasch",
                              group = as.factor(fulldataNoNA$mode_adm),
                              SE = TRUE,
                              verbose = FALSE,
                              TOL = 0.000001)

#### Scalar model: fit multigroup
scalar.mode <- multipleGroup(data = modeldata, itemtype = "Rasch",
                              group = as.factor(fulldataNoNA$mode_adm),
                              invariance = c("free_means", "free_var", names(modeldata)),
                              SE = TRUE,
                              verbose = FALSE,
                              TOL = 0.000001)

#### Model comparison:
anova(scalar.mode,configural.mode)
#                      AIC    SABIC       HQ      BIC    logLik      X2  df p
# scalar.mode     65399.89 65722.54 65631.57 66053.00 -32595.94              
# configural.mode 65430.38 66063.27 65884.82 66711.49 -32511.19 169.511 100 0

Dif.results <- DIF(scalar.mode, which.par = c("d"), scheme = "drop", items2test = 1:length(modeldata), p.adjust = "BH")
# saveRDS(Dif.results,"R-objects/DIFresmode.rds")
dif.items <- rownames(Dif.results[Dif.results$p<=0.05 & Dif.results$X2 >= 0,])
# saveRDS(dif.items,"R-objects/modenonivariant.rds")
#Dif.results[which(Dif.results$p < 0.05),]
dif.itemsBH <- rownames(Dif.results[Dif.results$adj_p<=0.05 & Dif.results$X2>=0,])

length(dif.items)
length(dif.itemsBH)


#### Partial invariant model
partial.mode <- multipleGroup(modeldata, 1, itemtype = "Rasch",
                              group = as.factor(fulldataNoNA$mode_adm),
                              invariance = c("free_means", "free_var", names(modeldata[,!names(modeldata) %in% dif.items])),
                              SE = T,
                              verbose = F,
                              TOL = 0.000001)

#saveRDS(partial.mode, "R-objects/partialmgMode.rds")

########################
## update r-objects    #
########################
# rem_items <- read_xlsx("Results/removed_items_mars.xlsx")
# dif_items <- data.frame(item = dif.itemsBH, reason = rep("DIF mode", length(dif.itemsBH)))
# rem_items <- rbind(dif_items,rem_items)
# 
# 
# write.xlsx(rem_items, "Results/removed_items_mars.xlsx")
# saveRDS(object = dif.itemsBH, "R-objects/dif.itemsBHmodeEvalResult.rds" )


