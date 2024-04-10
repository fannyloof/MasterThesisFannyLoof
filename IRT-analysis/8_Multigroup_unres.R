
#############################
#             IRT           #
#Multigroup - Unconstrained #
#     MODEL adj for DIF     #
#############################

# packages
library(mirt)

# data
dat <- readRDS("R-objects/Scored_blocks_Configural_Rasch.rds")

# functions
source("Functions/analysis_functions.R")
remove_cols <- c("IDSTUD","ASBG01","Imm_Status","mode_adm", "group")

# create objects
invariant <- list()
invariant_BH <- list()
noninvariant <- list()
noninvariant_BH <- list()
########################
##### BLOCK SE/SP01 ####
########################
### W.O BH-ADJUSTMENT

block <- model_data(block_no = 1, remove_cols = remove_cols)
dif <- readRDS("R-objects/B1_DIFGenIm_Rasch.rda")
dif$itemno <- 1:nrow(dif)

noninvariant_items <- dif$itemno[dif$p < 0.05]
inv_items <- dif$itemno[dif$p >= 0.05]
inv_itemsBH <- dif$itemno[dif$adj_p >= 0.05]

invariant_items <- colnames(block[[2]])
invariant_items <- setdiff(invariant_items, noninvariant_items)

timssSciGenIm_res <- multipleGroup(block[[2]], 1, itemtype = "Rasch", group = as.factor(block[[1]]$group),
                                   invariance = c("free_means","free_var",
                                                  invariant_items),
                                   SE = T, verbose = F)


coef(timssSciGenIm_res, IRT = T)
saveRDS(timssSciGenIm_res,"R-objects/B1_MGM_u_Rasch.rds")

noninvariant[["Block1"]] <- noninvariant_items
invariant[["Block1"]] <- inv_items
invariant_BH[["Block1"]] <- inv_itemsBH
########################
##### BLOCK SE/SP03 ####
########################
### W.O BH-ADJUSTMENT

block <- model_data(block_no = 3, remove_cols = remove_cols)
dif <- readRDS("R-objects/B3_DIFGenIm_Rasch.rda")
dif$itemno <- 1:nrow(dif)

noninvariant_items <- dif$itemno[dif$p < 0.05]
inv_items <- dif$itemno[dif$p >= 0.05]
inv_itemsBH <- dif$itemno[dif$adj_p >= 0.05]

invariant_items <- colnames(block[[2]])
invariant_items <- setdiff(invariant_items, noninvariant_items)

timssSciGenIm_res <- multipleGroup(block[[2]], 1, itemtype = "Rasch", group = as.factor(block[[1]]$group),
                                   invariance = c("free_means","free_var",
                                                  invariant_items),
                                   SE = T, verbose = F)


coef(timssSciGenIm_res, IRT = T)
saveRDS(timssSciGenIm_res,"R-objects/B3_MGM_u_Rasch.rds")

noninvariant[["Block3"]] <- noninvariant_items
invariant[["Block3"]] <- inv_items
invariant_BH[["Block3"]] <- inv_itemsBH
########################
##### BLOCK SE/SP05 ####
########################
## W BH ADJUSTMENT
block <- model_data(block_no = 5, remove_cols = remove_cols)
dif <- readRDS("R-objects/B5_DIFGenIm_Rasch.rda")
dif$itemno <- 1:nrow(dif)

noninvariant_items <- dif$itemno[dif$adj_p < 0.05]
inv_items <- dif$itemno[dif$adj_p >= 0.05]

noninvariant_items <- colnames(block[[2]][dif$significance=="DIF"])
all_items <- colnames(block[[2]])
invariant_items <- setdiff(all_items, noninvariant_items)


timssSciGenIm_res <- multipleGroup(block[[2]], 1, itemtype = "Rasch", group = as.factor(block[[1]]$group),
                                   invariance = c("free_means","free_var",
                                                  invariant_items),
                                   SE = T, verbose = F)

coef(timssSciGenIm_res, IRT = T)

saveRDS(timssSciGenIm_res,"R-objects/B5_MGM_u_Rasch_BH.rds")

noninvariant_BH[["Block5"]] <- noninvariant_items
invariant_BH[["Block5"]] <- inv_items
## W.O BH ADJUSTMENT
block <- model_data(block_no = 5, remove_cols = remove_cols)
dif <- readRDS("R-objects/B5_DIFGenIm_Rasch.rda")
dif$itemno <- 1:nrow(dif)

noninvariant_items <- dif$itemno[dif$p < 0.05]
inv_items <- dif$itemno[dif$p >= 0.05]

all_items <- colnames(block[[2]])
invariant_items <- setdiff(all_items, noninvariant_items)


timssSciGenIm_res <- multipleGroup(block[[2]], 1, itemtype = "Rasch", group = as.factor(block[[1]]$group),
                                   invariance = c("free_means","free_var",
                                                  invariant_items),
                                   SE = T, verbose = F)

coef(timssSciGenIm_res, IRT = T)

saveRDS(timssSciGenIm_res,"R-objects/B5_MGM_u_Rasch.rds")

noninvariant[["Block5"]] <- noninvariant_items
invariant[["Block5"]] <- inv_items
########################
##### BLOCK SE/SP09 ####
########################
## W.O BH ADJUSTMENT
block <- model_data(block_no = 9, remove_cols = remove_cols)
dif <- readRDS("R-objects/B9_DIFGenIm_Rasch.rda")
dif$itemno <- 1:nrow(dif)

noninvariant_items <- dif$itemno[dif$p < 0.05]
inv_items <- dif$itemno[dif$p >= 0.05]
inv_itemsBH <- dif$itemno[dif$adj_p >= 0.05]

all_items <- colnames(block[[2]])
invariant_items <- setdiff(all_items, noninvariant_items)


timssSciGenIm_res <- multipleGroup(block[[2]], 1, itemtype = "Rasch", group = as.factor(block[[1]]$group),
                                   invariance = c("free_means","free_var",
                                                  invariant_items),
                                   SE = T, verbose = F)

coef(timssSciGenIm_res, IRT = T)

saveRDS(timssSciGenIm_res,"R-objects/B9_MGM_u_Rasch.rds")


noninvariant[["Block9"]] <- noninvariant_items
invariant[["Block9"]] <- inv_items
invariant_BH[["Block9"]] <- inv_itemsBH
########################
##### BLOCK SE/SP11 ####
########################
## W.O BH ADJUSTMENT
block <- model_data(block_no = 11, remove_cols = remove_cols)
dif <- readRDS("R-objects/B11_DIFGenIm_Rasch.rda")
dif$itemno <- 1:nrow(dif)

noninvariant_items <- dif$itemno[dif$p < 0.05]
inv_items <- dif$itemno[dif$p >= 0.05]
inv_itemsBH <- dif$itemno[dif$adj_p >= 0.05]

all_items <- colnames(block[[2]])
invariant_items <- setdiff(all_items, noninvariant_items)


timssSciGenIm_res <- multipleGroup(block[[2]], 1, itemtype = "Rasch", group = as.factor(block[[1]]$group),
                                   invariance = c("free_means","free_var",
                                                  invariant_items),
                                   SE = T, verbose = F)

coef(timssSciGenIm_res, IRT = T)

saveRDS(timssSciGenIm_res,"R-objects/B11_MGM_u_Rasch.rds")

noninvariant[["Block11"]] <- noninvariant_items
invariant[["Block11"]] <- inv_items
invariant_BH[["Block11"]] <- inv_itemsBH
########################
##### BLOCK SE/SP13 ####
########################
## W. BH ADJUSTMENT
block <- model_data(block_no = 13, remove_cols = remove_cols)
dif <- readRDS("R-objects/B13_DIFGenIm_Rasch.rda")
dif$itemno <- 1:nrow(dif)

noninvariant_items <- dif$itemno[dif$adj_p < 0.05]
inv_items <- dif$itemno[dif$adj_p >= 0.05]

noninvariant_items <- colnames(block[[2]][dif$significance=="DIF"])
all_items <- colnames(block[[2]])
invariant_items <- setdiff(all_items, noninvariant_items)


timssSciGenIm_res <- multipleGroup(block[[2]], 1, itemtype = "Rasch", group = as.factor(block[[1]]$group),
                                   invariance = c("free_means","free_var",
                                                  invariant_items),
                                   SE = T, verbose = F)

coef(timssSciGenIm_res, IRT = T)

saveRDS(timssSciGenIm_res,"R-objects/B13_MGM_u_Rasch_BH.rds")

noninvariant_BH[["Block13"]] <- noninvariant_items
invariant_BH[["Block13"]] <- inv_items

## W.O BH ADJUSTMENT
block <- model_data(block_no = 13, remove_cols = remove_cols)
dif <- readRDS("R-objects/B13_DIFGenIm_Rasch.rda")
dif$itemno <- 1:nrow(dif)

noninvariant_items <- dif$itemno[dif$p < 0.05]
inv_items <- dif$itemno[dif$p >= 0.05]

all_items <- colnames(block[[2]])
invariant_items <- setdiff(all_items, noninvariant_items)


timssSciGenIm_res <- multipleGroup(block[[2]], 1, itemtype = "Rasch", group = as.factor(block[[1]]$group),
                                   invariance = c("free_means","free_var",
                                                  invariant_items),
                                   SE = T, verbose = F)

coef(timssSciGenIm_res, IRT = T)

saveRDS(timssSciGenIm_res,"R-objects/B13_MGM_u_Rasch.rds")

noninvariant[["Block13"]] <- noninvariant_items
invariant[["Block13"]] <- inv_items

### SAVE OBJECTS
saveRDS(noninvariant, "R-objects/NoninvariantItemNames.rds")
saveRDS(noninvariant_BH, "R-objects/NoninvariantItemNamesBH.rds")

saveRDS(invariant, "R-objects/InvariantItemNames.rds")
saveRDS(invariant_BH, "R-objects/InvariantItemNamesBH.rds")

