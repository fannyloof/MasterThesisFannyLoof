
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
dat <- readRDS("R-objects/Scored_blocks_ver3.rds")
# load functions
source("Functions/analysis_functions.R")

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

# conclusion: can use them as one
# go on to fit multiplegroup

nrow(block1_full)
block1_full <- na.omit(block1_full) #basis for computation
block1 <- na.omit(block1) # to get ptimmss/etimms vector matching df for computation


mode <- block1_full$mode_adm


## estimate multigroup
# fit a multiple group model
timssSciMode_res <- multipleGroup(block1, 1,itemtype = "Rasch", group = mode,
                            invariance = c("free_means", "free_var",
                                           colnames(block1)),
                            SE = T, verbose = F)
coef(timssSciMode_res, IRT = T)
saveRDS(timssSciMode_res, "R-objects/B1_MGMmode_RASCH_r.rds")


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

# conclusion: good fit of Rasch in both modes
# go on to fit multiplegroup

block3_full <- na.omit(block3_full)
block3 <- na.omit(block3)
mode <- block3_full$mode_adm

## estimate multigroup
# fit a multiple group model
timssSciMode_res <- multipleGroup(block3, 1, itemtype = "Rasch", group = mode,
                                  invariance = c("free_means", "free_var",
                                                 colnames(block3)),
                                  SE = T, verbose = F)
coef(timssSciMode_res, IRT = TRUE)
coef(timssSciMode_res, simplify = T)[["Etimss"]]$means
saveRDS(timssSciMode_res, "R-objects/B3_MGMmode_Rasch_r.rds")



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

## Rasch appropriate in both, fit multigroup
block5_full <- na.omit(block5_full)
block5 <- na.omit(block5)
mode <- block5_full$mode_adm



## estimate multigroup
# fit a multiple group model
timssSciMode_res <- multipleGroup(block5, 1, itemtype = "Rasch", group = mode,
                                    invariance = c("free_means", "free_variance",
                                                   colnames(block5)),
                                    SE = T, verbose = F)
coef(timssSciMode_res, IRT=T)
coef(timssSciMode_res, simplify = T)[["Etimss"]]$cov

saveRDS(timssSciMode_res, "R-objects/B5_MGMmode_Rasch_r.rds")

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

# pTimRasch <- mirt(ptimms6, 1, rep("3PL",length(ptimms6)))
# eTimRasch <- mirt(etimms6, 1, rep("3PL",length(etimms6)))
# 
# M2(pTimRasch, type = "M2*", na.rm = T)
# M2(eTimRasch, type = "M2*", na.rm = T)


# conclusion: better fit of the 3Pl in both modes
# go on to fit multiplegroup

block6_full <- na.omit(block6_full)
block6 <- na.omit(block6)
mode <- block6_full$mode_adm

## estimate multigroup
# fit a multiple group model
timssSciMode_res <- multipleGroup(block6, 1, itemtype = "Rasch", group = mode,
                                    invariance = c("free_means", "free_var",
                                                   colnames(block6)),
                                    SE = T, verbose = F)

coef(timssSciMode_res, IRT=T)
saveRDS(timssSciMode_res, "R-objects/B6_MGMmode_Rasch_r.rds")



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

# conclusion: not good fit of Rasch in paper mode
# go on to fit multiplegroup

block7_full <- na.omit(block7_full)
block7 <- na.omit(block7)
mode <- block7_full$mode_adm

## estimate multigroup
# fit a multiple group model
timssSciMode_res <- multipleGroup(block7, 1, itemtype = "Rasch", group = mode,
                                    invariance = c("free_means", "free_var",
                                                   colnames(block7)),
                                    SE = T, verbose = F)
coef(timssSciMode_res, IRT=T)
saveRDS(timssSciMode_res, "R-objects/B7_MGMmode_Rasch_r.rds")



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

# conclusion: good fit of Rasch in both mode
# go on to fit multiplegroup

block9_full <- na.omit(block9_full)
block9 <- na.omit(block9)
mode <- block9_full$mode_adm

## estimate multigroup
# fit a multiple group model
timssSciMode_res <- multipleGroup(block9, 1, itemtype = "Rasch", group = mode,
                                    invariance = c("free_means", "free_var",
                                                   colnames(block9)),
                                    SE = T, verbose = F)
coef(timssSciMode_res, IRT=T)
saveRDS(timssSciMode_res, "R-objects/B9_MGMmode_Rasch_r.rds")

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

# conclusion: good fit of Rasch in both mode
# go on to fit multiplegroup

block11_full <- na.omit(block11_full)
block11 <- na.omit(block11)
mode <- block11_full$mode_adm

## estimate multigroup
# fit a multiple group model
timssSciMode_res <- multipleGroup(block11, 1, itemtype= "Rasch", group = mode,
                                    invariance = c("free_means", "free_var",
                                                   colnames(block11)),
                                    SE = T, verbose = F)
coef(timssSciMode_res, IRT=T)

saveRDS(timssSciMode_res, "R-objects/B11_MGMmode_Rasch_r.rds")


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

# conclusion: good fit of Rasch in both mode
# go on to fit multiplegroup

block13_full <- na.omit(block13_full)
block13 <- na.omit(block13)
mode <- block13_full$mode_adm

## estimate multigroup
# fit a multiple group model
timssSciMode_res <- multipleGroup(block13, 1, itemtype = "Rasch", group = mode,
                                  invariance = c("free_means", "free_var",
                                                 colnames(block13)),
                                  SE = T, verbose = F)
coef(timssSciMode_res, IRT=T)

saveRDS(timssSciMode_res, "R-objects/B13_MGMmode_Rasch_r.rds")






       