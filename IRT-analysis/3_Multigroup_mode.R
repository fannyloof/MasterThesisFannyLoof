
#################################
###       IRT-analysis        ###  
### Multigroup - restricted   ###
###       Paper/Etimss        ###
#################################

# load packages
library(mirt)
library(openxlsx)
library(expss)
# load data
dat <- readRDS("R-objects/Scored_blocks_ver3.rds") #checked
# load functions
source("Functions/analysis_functions.R")
# create objects
remove_cols <- c("IDSTUD",     "ASBG01",     "Imm_Status", "mode_adm",   "group")

########################
##### BLOCK SE/SP01 ####
########################

block1_full <- PE_separator(LIST = dat, 1,"full")
block1 <- PE_separator(LIST = dat,1, "block")
ptimms1 <- PE_separator(LIST = dat, 1, "P")
etimms1 <- PE_separator(LIST = dat, 1,"E")

nrow(ptimms1)
nrow(na.omit(ptimms1)) ## basis for computation
nrow(etimms1)
nrow(na.omit(etimms1)) ## basis for computation
# fit Rasch comparing modes
pTimRasch <- mirt(ptimms1, 1, rep("Rasch",length(ptimms1)))
eTimRasch <- mirt(etimms1, 1, rep("Rasch",length(etimms1)))

# model evaluation
M2(pTimRasch, type = "M2*", na.rm = T)
M2(eTimRasch, type = "M2*", na.rm = T)

#### Configural and scalar
modeldat <- block1
### Fit a configural model
configural <- multipleGroup(data = modeldat, 1, itemtype = "Rasch", group = as.factor(block1_full$mode_adm))

#### Fit statistics configural model
fitcon1 <- M2(configural, type = "M2*", na.rm = T)

#### Fit scalar model

scalar <- multipleGroup(data = modeldat, 1, itemtype = "Rasch",
                        group = as.factor(block1_full$mode_adm),
                        invariance = c("free_means", "free_var", names(modeldat)),
                        SE = TRUE,
                        verbose = FALSE) #?

### Fit statistics scalar model
fitscal1 <- M2(scalar, type = "M2*", na.rm = T) #?

conscal1 <- rbind(con1 = fitcon1, scal1 = fitscal1) #?
conscal <- conscal1 #?

# conclusion: can use them as one



########################
##### BLOCK SE/SP03 ####
########################

block3_full <- PE_separator(LIST= dat, 3,"full")
block3 <- PE_separator(LIST= dat, 3, "block")
ptimms3 <- PE_separator(LIST= dat,3, "P")
etimms3 <- PE_separator(LIST= dat,3,"E")

nrow(na.omit(ptimms3))
nrow(na.omit(etimms3))

pTimRasch <- mirt(ptimms3, 1, rep("Rasch",length(ptimms3)))
eTimRasch <- mirt(etimms3, 1, rep("Rasch",length(etimms3)))

M2(pTimRasch, type = "M2*", na.rm = T)
M2(eTimRasch, type = "M2*", na.rm = T)

#### Configural and scalar
modeldat <- block3
### Fit a configural model
configural <- multipleGroup(data = modeldat, 1, itemtype = "Rasch", group = as.factor(block3_full$mode_adm))

#### Fit statistics configural model
fitcon3 <- M2(configural, type = "M2*", na.rm = T)

#### Fit scalar model

scalar <- multipleGroup(data = modeldat, 1, itemtype = "Rasch",
                        group = as.factor(block3_full$mode_adm),
                        invariance = c("free_means", "free_var", names(modeldat)),
                        SE = TRUE,
                        verbose = FALSE) #?

### Fit statistics scalar model
fitscal3 <- M2(scalar, type = "M2*", na.rm = T) #?

conscal3 <- rbind(con3 = fitcon3, scal3 = fitscal3) #?
conscal <- rbind(conscal, conscal3) #?




########################
##### BLOCK SE/SP05 ####
########################

block5_full <- PE_separator(LIST= dat,5,"full")
block5 <- PE_separator(LIST= dat,5, "block")
ptimms5 <- PE_separator(LIST= dat,5, "P")
etimms5 <- PE_separator(LIST= dat,5,"E")


pTimRasch <- mirt(ptimms5, 1, rep("Rasch",length(ptimms5)))
eTimRasch <- mirt(etimms5, 1, rep("Rasch",length(etimms5)))

M2(pTimRasch, type = "M2*", na.rm = T)
M2(eTimRasch, type = "M2*", na.rm = T)

modeldat <- block5
### Fit a configural model
configural <- multipleGroup(data = modeldat, 1, itemtype = "Rasch", group = as.factor(block5_full$mode_adm))

#### Fit statistics configural model
fitcon5 <- M2(configural, type = "M2*", na.rm = T)

#### Fit scalar model

scalar <- multipleGroup(data = modeldat, 1, itemtype = "Rasch",
                        group = as.factor(block5_full$mode_adm),
                        invariance = c("free_means", "free_var", names(modeldat)),
                        SE = TRUE,
                        verbose = FALSE) #?

### Fit statistics scalar model
fitscal5 <- M2(scalar, type = "M2*", na.rm = T) #?

conscal5 <- rbind(con5 = fitcon5, scal5 = fitscal5) #?
conscal <- rbind(conscal, conscal5) #?

########################
##### BLOCK SE/SP06 ####
########################

block6_full <- PE_separator(LIST= dat,6,"full")
block6 <- PE_separator(LIST= dat,6, "block")
ptimms6 <- PE_separator(LIST= dat,6, "P")
etimms6 <- PE_separator(LIST= dat,6,"E")

pTimRasch <- mirt(ptimms6, 1, rep("Rasch",length(ptimms6)))
eTimRasch <- mirt(etimms6, 1, rep("Rasch",length(etimms6)))

M2(pTimRasch, type = "M2*", na.rm = T)
M2(eTimRasch, type = "M2*", na.rm = T)


modeldat <- block6
### Fit a configural model
configural <- multipleGroup(data = modeldat, 1, itemtype = "Rasch", group = as.factor(block6_full$mode_adm))

#### Fit statistics configural model
fitcon6 <- M2(configural, type = "M2*", na.rm = T)

#### Fit scalar model

scalar <- multipleGroup(data = modeldat, 1, itemtype = "Rasch",
                        group = as.factor(block6_full$mode_adm),
                        invariance = c("free_means", "free_var", names(modeldat)),
                        SE = TRUE,
                        verbose = FALSE) #?

### Fit statistics scalar model
fitscal6 <- M2(scalar, type = "M2*", na.rm = T) #?

conscal6 <- rbind(con6 = fitcon6, scal6 = fitscal6) #?
conscal <- rbind(conscal, conscal6) #?




########################
##### BLOCK SE/SP07 ####
########################

block7_full <- PE_separator(LIST= dat,7,"full")
block7 <- PE_separator(LIST= dat,7, "block")
ptimms7 <- PE_separator(LIST= dat,7, "P")
etimms7 <- PE_separator(LIST= dat,7,"E")

pTimRasch <- mirt(ptimms7, 1, rep("Rasch",length(ptimms7)))
eTimRasch <- mirt(etimms7, 1, rep("Rasch",length(etimms7)))

M2(pTimRasch, type = "M2*", na.rm = T)
M2(eTimRasch, type = "M2*", na.rm = T)

modeldat <- block7
### Fit a configural model
configural <- multipleGroup(data = modeldat, 1, itemtype = "Rasch", group = as.factor(block7_full$mode_adm))

#### Fit statistics configural model
fitcon7 <- M2(configural, type = "M2*", na.rm = T)

#### Fit scalar model

scalar <- multipleGroup(data = modeldat, 1, itemtype = "Rasch",
                        group = as.factor(block7_full$mode_adm),
                        invariance = c("free_means", "free_var", names(modeldat)),
                        SE = TRUE,
                        verbose = FALSE) #?

### Fit statistics scalar model
fitscal7 <- M2(scalar, type = "M2*", na.rm = T) #?

conscal7 <- rbind(con7 = fitcon7, scal7 = fitscal7) #?
conscal <- rbind(conscal, conscal7) #?






########################
##### BLOCK SE/SP09 ####
########################

block9_full <- PE_separator(LIST= dat,9,"full")
block9 <- PE_separator(LIST= dat,9, "block")
ptimms9 <- PE_separator(LIST= dat,9, "P")
etimms9 <- PE_separator(LIST= dat,9,"E")

pTimRasch <- mirt(ptimms9, 1, rep("Rasch",length(ptimms9)))
eTimRasch <- mirt(etimms9, 1, rep("Rasch",length(etimms9)))

M2(pTimRasch, type = "M2*", na.rm = T)
M2(eTimRasch, type = "M2*", na.rm = T)

modeldat <- block9
### Fit a configural model
configural <- multipleGroup(data = modeldat, 1, itemtype = "Rasch", group = as.factor(block9_full$mode_adm))

#### Fit statistics configural model
fitcon9 <- M2(configural, type = "M2*", na.rm = T)

#### Fit scalar model

scalar <- multipleGroup(data = modeldat, 1, itemtype = "Rasch",
                        group = as.factor(block9_full$mode_adm),
                        invariance = c("free_means", "free_var", names(modeldat)),
                        SE = TRUE,
                        verbose = FALSE) #?

### Fit statistics scalar model
fitscal9 <- M2(scalar, type = "M2*", na.rm = T) #?

conscal9 <- rbind(con9 = fitcon9, scal9 = fitscal9) #?
conscal <- rbind(conscal, conscal9) #?




########################
##### BLOCK SE/SP11 ####
########################


block11_full <- PE_separator(LIST= dat,11,"full")
block11 <- PE_separator(LIST= dat,11, "block")
ptimms11 <- PE_separator(LIST= dat,11, "P")
etimms11 <- PE_separator(LIST= dat,11,"E")

pTimRasch <- mirt(ptimms11, 1, rep("Rasch",length(ptimms11)))
eTimRasch <- mirt(etimms11, 1, rep("Rasch",length(etimms11)))

M2(pTimRasch, type = "M2*", na.rm = T)
M2(eTimRasch, type = "M2*", na.rm = T)

modeldat <- block11
### Fit a configural model
configural <- multipleGroup(data = modeldat, 1, itemtype = "Rasch", group = as.factor(block11_full$mode_adm))

#### Fit statistics configural model
fitcon11 <- M2(configural, type = "M2*", na.rm = T)

#### Fit scalar model

scalar <- multipleGroup(data = modeldat, 1, itemtype = "Rasch",
                        group = as.factor(block11_full$mode_adm),
                        invariance = c("free_means", "free_var", names(modeldat)),
                        SE = TRUE,
                        verbose = FALSE) #?

### Fit statistics scalar model
fitscal11 <- M2(scalar, type = "M2*", na.rm = T) #?

conscal11 <- rbind(con11 = fitcon11, scal11 = fitscal11) #?
conscal <- rbind(conscal, conscal11) #?





########################
##### BLOCK SE/SP13 ####
########################

block13_full <- PE_separator(LIST= dat,13,"full")
block13 <- PE_separator(LIST= dat,13, "block")
ptimms13 <- PE_separator(LIST= dat,13, "P")
etimms13 <- PE_separator(LIST= dat,13,"E")

pTimRasch <- mirt(ptimms13, 1, rep("Rasch",length(ptimms13)))
eTimRasch <- mirt(etimms13, 1, rep("Rasch",length(etimms13)))

M2(pTimRasch, type = "M2*", na.rm = T)
M2(eTimRasch, type = "M2*", na.rm = T)

modeldat <- block13
### Fit a configural model
configural <- multipleGroup(data = modeldat, 1, itemtype = "Rasch", group = as.factor(block13_full$mode_adm))

#### Fit statistics configural model
fitcon13 <- M2(configural, type = "M2*", na.rm = T)

#### Fit scalar model

scalar <- multipleGroup(data = modeldat, 1, itemtype = "Rasch",
                        group = as.factor(block13_full$mode_adm),
                        invariance = c("free_means", "free_var", names(modeldat)),
                        SE = TRUE,
                        verbose = FALSE) #?

### Fit statistics scalar model
fitscal13 <- M2(scalar, type = "M2*", na.rm = T) #?

conscal13 <- rbind(con13 = fitcon13, scal13 = fitscal13) #?
conscal <- rbind(conscal, conscal13) #?




# write.xlsx(conscal, "Results/ModelfitMode.xlsx")

