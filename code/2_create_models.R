# tree (baseline)    0.65 +- 0.05   0.60 +- 0.06   0.85 +- 0.03 
# tree (tuned)       0.62 +- 0.05   0.63 +- 0.09   0.86 +- 0.04
# forest (baseline)  0.75 +- 0.06   0.71 +- 0.05   0.91 +- 0.03 
# forest (tuned)     0.76 +- 0.05   0.72 +- 0.05   0.91 +- 0.02

SEED <- 123

source("0_utils.R")
df <- readRDS(paste0("../data/mpilot_accidents_dataset_preprocessed.rds"))

mean(as.numeric(as.character(df$target)))
# df$target <- 1 - df$target

set.seed(SEED, kind = "L'Ecuyer-CMRG")
evaluate_tree(df)
set.seed(SEED, kind = "L'Ecuyer-CMRG")
evaluate_forest(df)


# -- grid search forest

G_forest <- list(
  n_trees = c(100, 200, 300),
  row_frac = c(0.7, 0.8),
  col_frac = c(0.6, 0.7, 0.8),
  maxdepth = c(3, 5, 7),
  minsplit = c(10, 15, 20),
  cp = c(0.05, 0.01, 0.005)
)

result <- grid_search_forest(df, random_seed = SEED, grid_list = G_forest)

saveRDS(result, paste0("../results/forest_grid.rds"))

result <- readRDS(paste0("../results/forest_grid.rds"))
scores <- do.call(rbind,
                  sapply(result[result[,1] != " failed ", 1],
                         function(x) as.data.frame(x),
                         simplify = F))
scores <- round(scores, 3)

head(scores[order(scores$auprc_m, decreasing = T),])
head(scores[order(scores$auc_m, decreasing = T),])

set.seed(SEED, kind = "L'Ecuyer-CMRG")
ret <- evaluate_forest(df,
                n_trees = 200,
                row_frac = 0.7,
                col_frac = 0.6,
                control = rpart.control(
                  maxdepth = 7,
                  minsplit = 10,
                  cp = 0.01
                ))
ret$scores
ret$importance_split


# -- grid search tree

G_tree <- list(
  maxdepth = c(3, 4, 5, 6, 7, 8),
  minsplit = c(8, 10, 13, 15, 17, 20, 24),
  cp = c(0.1, 0.05, 0.03, 0.01, 0.005, 0.001)
)

result_tree <- grid_search_tree(df, random_seed = SEED, grid_list = G_tree)

saveRDS(result_tree, paste0("../results/tree_grid.rds"))

result_tree <- readRDS(paste0("../results/tree_grid.rds"))
scores <- do.call(rbind,
                  sapply(result_tree[,1],
                         function(x) as.data.frame(x), simplify = F))
scores <- round(scores, 3)

head(scores[order(scores$auprc_m, decreasing = T),])
head(scores[order(scores$auc_m, decreasing = T),])

set.seed(SEED, kind = "L'Ecuyer-CMRG")
ret <- evaluate_tree(df,
                     control = rpart.control(
                       maxdepth = 4,
                       minsplit = 13,
                       cp = 0.001
                     ))
ret$scores
ret$importance_split
