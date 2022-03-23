suppressWarnings(
  suppressPackageStartupMessages({
    library(rpart)
    library(dplyr)
    library(DALEX)
    library(PRROC)
    library(caret)
  })
)


plot_auprc <- function(explainer) {
  fg <- explainer$y_hat[explainer$y == 1]
  bg <- explainer$y_hat[explainer$y == 0]
  pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
  plot(pr)
}


score_auprc <- function(explainer) {
  fg <- explainer$y_hat[explainer$y == 1]
  bg <- explainer$y_hat[explainer$y == 0]
  pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
  pr$auc.davis.goadrich
}


plot_y_hat <- function(explainer, class = 0) {
  hist(
    explainer$y_hat[explainer$y == class],
    main = paste0("Distribution of model predictions where
                  target variable (class) was ", class),
    xlab = paste0("predictions (class ", class,")")
  )  
}


create_weights <- function(x) {
  class <- 1 - floor(mean(x) + 0.5)
  x <- as.numeric(as.character(x))
  ifelse(x == class, 1 / abs((1 - class) - mean(x)), 1)
}


create_weights2 <- function(x) {
  x <- as.numeric(as.character(x))
  n1 <- sum(x == 1)
  n0 <- sum(x == 0)
  ifelse(x == 1, length(x) / n1, length(x) / n0)
}


create_forest <- function(data,
                          weights = rep(1, nrow(data)),
                          n_trees = 100,
                          row_frac = 0.7,
                          col_frac = 0.6,
                          random_seed = 123,
                          ...) {
  set.seed(random_seed, kind = "L'Ecuyer-CMRG")

  trees <- list()
  importance <- list()
  for (i in 1:n_trees) {
    
    row_index <- sample(1:dim(data)[1],
                        row_frac*dim(data)[1],
                        replace = F)
    col_index <- sample(2:dim(data)[2],
                        col_frac*dim(data)[2],
                        replace = F)
    
    d <- data[row_index, c(1, col_index)]
    w <- weights[row_index]
    m <- rpart(target~., d, weights = w, ...)
    
    trees[[i]] <- m
    importance[[i]] <- m$variable.importance
  }
  
  importance <- do.call(c, importance)
  importance <- sapply(split(importance, names(importance)), mean)
  
  ret <- list(
    trees = trees,
    importance = importance,
    params = list(
      n_trees = n_trees,
      row_frac = row_frac,
      col_frac = col_frac
    )
  )
  class(ret) <- "forest"
  ret
}


reduceApplyListOfArrays <- function(x) {
  # https://stackoverflow.com/a/67991665
  y<-apply(array(unlist(x), c(dim(x[[1]]), dim(x[[2]]), length(x))), 
           c(1,2), mean)
  colnames(y) <- colnames(x[[1]])
  rownames(y) <- rownames(x[[1]])
  return(y)
}


predict.forest <- function(model, data) {
  p <- list()
  for (i in 1:model$params$n_trees) {
    p <- c(p, list(predict(model$trees[[i]], data)))
  }
  Reduce(`+`, p) / length(p)
}


pf_forest <- function(m, d) predict(m, d)[, 2]


explainers_forest <- function(data, ...) {
  train_test_split <- createDataPartition(data$target, p = 0.8, times = 5)
  elist <- list()
  
  params <- list(...)
  params_names <- names(list(...))
  
  for (j in 1:length(train_test_split)) {
    train <- data[train_test_split[[j]],]
    test  <- data[-train_test_split[[j]],]
    train_weights <- create_weights2(train$target)
    train$target <- as.factor(train$target)
    if (length(params) == 0) {
      m <- create_forest(train,
                         train_weights)
    } else {
      m <- create_forest(train,
                         train_weights,
                         n_trees = params$n_trees,
                         row_frac = params$row_frac,
                         col_frac = params$col_frac,
                         control = params$control)   
    }
    elist[[j]] <- DALEX::explain(m,
                        test[,-1],
                        as.numeric(as.character(test$target)),
                        pf_forest,
                        type = "classification",
                        verbose = FALSE)
    
  }
  elist
}


evaluate_forest <- function(data, importance = FALSE, ...) {
  train_test_split <- createDataPartition(data$target, p = 0.8, times = 5)
  mp <- data.frame()
  imp_split <- c()
  imp_perm <- importance_perm <- list()
  
  params <- list(...)
  params_names <- names(list(...))
  
  for (j in 1:length(train_test_split)) {
    train <- data[train_test_split[[j]],]
    test  <- data[-train_test_split[[j]],]
    train_weights <- create_weights2(train$target)
    train$target <- as.factor(train$target)
    if (length(params) == 0) {
      m <- create_forest(train,
                         train_weights)
    } else {
      m <- create_forest(train,
                         train_weights,
                         n_trees = params$n_trees,
                         row_frac = params$row_frac,
                         col_frac = params$col_frac,
                         control = params$control)   
    }
    e <- DALEX::explain(m,
                        test[,-1],
                        as.numeric(as.character(test$target)),
                        pf_forest,
                        type = "classification",
                        verbose = FALSE)
    s <- c(model_performance(e)$measures,
           auprc = score_auprc(e))
    
    mp <- rbind(mp, as.data.frame(s))
    imp_split <- c(imp_split, m$importance)
    
    if (importance) {
      temp <- model_parts(e)
      temp <- cbind(temp[temp$permutation == 0, c(1, 3)], split = j)
      imp_perm[[j]] <- temp      
    }
  }
  
  imp_split_agg <- sapply(split(imp_split, names(imp_split)), mean)
  
  scores <- mp %>%
    summarise(acc_m = mean(accuracy),
              acc_sd = sd(accuracy),
              auc_m = mean(auc),
              auc_sd = sd(auc),
              auprc_m = mean(auprc),
              auprc_sd = sd(auprc))
  
  importance_split <- data.frame(
    variable = names(imp_split_agg),
    importance = imp_split_agg
  ) %>% 
    arrange(importance) %>%
    mutate(variable = factor(variable, levels = variable))
  rownames(importance_split) <- NULL
  
  if (importance) {
    imp_perm_agg <- do.call(rbind, imp_perm) %>%
      group_by(variable) %>%
      summarise(importance = mean(dropout_loss))
    importance_perm <- as.data.frame(imp_perm_agg[c(-1, -2),]) %>%
      arrange(importance) %>%
      mutate(variable = factor(variable, levels = variable))  
  }
  
  list(
    scores=scores,
    importance_split=importance_split,
    importance_perm=importance_perm
  )
}


grid_search_forest <- function(data, grid_list, random_seed = 123, verbose = TRUE) {
  grid_matrix <- expand.grid(grid_list)
  grid_results <- list()
  
  for (i in 1:dim(grid_matrix)[1]) {
    if (verbose) cat("Grid:", i, "out of", dim(grid_matrix)[1], "\n")
    
    params <- grid_matrix[i,]
    set.seed(random_seed, kind = "L'Ecuyer-CMRG")
    
    grid_results[[i]] <- tryCatch(evaluate_forest(
      data,
      n_trees = params$n_trees,
      row_frac = params$row_frac,
      col_frac = params$col_frac,
      control = rpart.control(
        maxdepth = params$maxdepth,
        minsplit = params$minsplit,
        cp = params$cp
      )
    ), 
    error = function(e) " failed ")
  }
  
  results <- do.call(rbind, grid_results)
  rownames(results) <- apply(grid_matrix, 1, function(x) {
    x <- unlist(x)
    gsub(" ", ":", paste(names(x), x, collapse = "-"))
  })
  
  results
}


explainers_tree <- function(data, ...) {
  train_test_split <- createDataPartition(data$target, p = 0.8, times = 5)
  elist <- list()
  for (j in 1:length(train_test_split)) {
    train <- data[train_test_split[[j]],]
    test  <- data[-train_test_split[[j]],]
    train_weights <- create_weights2(train$target)
    train$target <- as.factor(train$target)
    
    m <- rpart(
      target~.,
      train,
      weights = train_weights,
      ...
    )
    
    elist[[j]] <- DALEX::explain(m,
                        test[,-1],
                        as.numeric(as.character(test$target)),
                        type = "classification",
                        verbose = FALSE)
  }
  elist
}

evaluate_tree <- function(data, importance = FALSE, ...) {
  train_test_split <- createDataPartition(data$target, p = 0.8, times = 5)
  mp <- data.frame()
  imp_split <- c()
  imp_perm <- importance_perm <- list()
  
  for (j in 1:length(train_test_split)) {
    train <- data[train_test_split[[j]],]
    test  <- data[-train_test_split[[j]],]
    train_weights <- create_weights2(train$target)
    train$target <- as.factor(train$target)
    
    m <- rpart(
      target~.,
      train,
      weights = train_weights,
      ...
    )
    
    e <- DALEX::explain(m,
                        test[,-1],
                        as.numeric(as.character(test$target)),
                        type = "classification",
                        verbose = FALSE)
    s <- c(model_performance(e)$measures,
           auprc = score_auprc(e))
    
    mp <- rbind(mp, as.data.frame(s))
    imp_split <- c(imp_split, m$variable.importance)
    
    if (importance) {
      temp <- model_parts(e)
      temp <- cbind(temp[temp$permutation == 0, c(1, 3)], split = j)
      imp_perm[[j]] <- temp      
    }
  }
  
  imp_split_agg <- sapply(split(imp_split, names(imp_split)), mean)

  scores <- mp %>%
    summarise(acc_m = mean(accuracy),
              acc_sd = sd(accuracy),
              auc_m = mean(auc),
              auc_sd = sd(auc),
              auprc_m = mean(auprc),
              auprc_sd = sd(auprc))
  
  importance_split <- data.frame(
    variable = names(imp_split_agg),
    importance = imp_split_agg
  ) %>% 
    arrange(importance) %>%
    mutate(variable = factor(variable, levels = variable))
  rownames(importance_split) <- NULL
  
  if (importance) {
    imp_perm_agg <- do.call(rbind, imp_perm) %>%
      group_by(variable) %>%
      summarise(importance = mean(dropout_loss))
    importance_perm <- as.data.frame(imp_perm_agg[c(-1, -2),]) %>%
      arrange(importance) %>%
      mutate(variable = factor(variable, levels = variable))  
  }
  
  list(
    scores=scores,
    importance_split=importance_split,
    importance_perm=importance_perm
  )
}

grid_search_tree <- function(data, grid_list, random_seed = 123, verbose = TRUE) {
  grid_matrix <- expand.grid(grid_list)
  grid_results <- list()
  
  for (i in 1:dim(grid_matrix)[1]) {
    if (verbose) cat("Grid:", i, "out of", dim(grid_matrix)[1], "\n")
    
    params <- grid_matrix[i,]
    set.seed(random_seed, kind = "L'Ecuyer-CMRG")
    
    grid_results[[i]] <- evaluate_tree(
      data,
      control = rpart.control(
        maxdepth = params$maxdepth,
        minsplit = params$minsplit,
        cp = params$cp
      )
    )
  }
  
  results <- do.call(rbind, grid_results)
  rownames(results) <- apply(grid_matrix, 1, function(x) {
    x <- unlist(x)
    gsub(" ", ":", paste(names(x), x, collapse = "-"))
  })
  
  results
}


plot_split_labels <- function(x, labs, digits, varlen, faclen) {
  root <- labs[1]
  temp <- sapply(labs[-1], function(x) {
    length <- 0
    maxrow <- 20
    rows <- 1
    first <- TRUE
    label <- ""
    values <- strsplit(x, ",")[[1]]
    additional_iter <- 1
    if (length(values) == 1) {
      
      # special case for the `visibility` variable
      if (values == " >= 2") values <- "Moderate, Poor"
      if (values == " < 2") values <- "Good"
      
      return(values)
    }
    for (i in 1:length(values)) {
      value <- values[i]
      length <- length + nchar(value)
      if (length > maxrow * rows || additional_iter == 4) {
        label <- paste0(label, "\n")
        rows <- rows + 1
        additional_iter <- 1
      } 
      if (first) {
        label <- paste0(value, ", ")
        first <- FALSE
      } else {
        if (i == length(values)) {
          label <- paste0(label, value) 
        } else {
          label <- paste0(label, value, ", ")   
        }
      }
      additional_iter <- additional_iter + 1
    }
    label
  })
  c(root, temp)
}


# receiver operating characteristic
plot_roc_multiple <- function(...) {
  # based on https://github.com/ModelOriented/DALEX/blob/master/R/plot_model_performance.R
  if (length(list(...)) == 0) {
    # if single explainer
    dfl <- list(x$residuals)
    df <- x$residuals
  } else {
    # if multiple explainers
    dfl <- lapply(list(...),
                   function(tmp) tmp$residuals)
    df <- do.call(rbind, rev(dfl))
  }
  
  fpr_domain <- seq(0, 1, length.out=21)
  
  rocdfl <- lapply(dfl, function(df) {
    # assuming that y = 0/1 where 1 is the positive
    tpr_tmp <- tapply(df$observed, df$predicted, sum)
    tpr <- c(0, cumsum(rev(tpr_tmp))) / sum(df$observed)
    fpr_tmp <- tapply(1 - df$observed, df$predicted, sum)
    fpr <- c(0, cumsum(rev(fpr_tmp))) / sum(1 - df$observed)
    
    tpr_interpolated <- pracma::interp1(fpr, tpr, fpr_domain)
    tpr_interpolated[1] <- 0
    tpr_interpolated[length(fpr_domain)] <- 1
    
    data.frame(tpr = tpr_interpolated, fpr = fpr_domain, 
               label = df$label[1], iter = df$iter[1])
  })
  rocdf <- do.call(rbind, rocdfl)
  
  rocdf_aggregated <- rocdf %>%
    group_by(label, fpr) %>% 
    summarise(tpr_m = mean(tpr), tpr_sd = sd(tpr))
  
  fpr <- tpr_m <- tpr_sd <- label <- NULL
  ggplot(rocdf_aggregated, aes(x = fpr, y = tpr_m)) +
    geom_abline(slope = 1, intercept = 0, color = "darkgrey", lty = 2) +
    geom_line(aes(color = label), size = 1) +
    geom_ribbon(aes(ymin = tpr_m - tpr_sd, ymax = tpr_m + tpr_sd, fill = label), 
                alpha = 0.5) +
    theme_drwhy() +
    scale_fill_manual(name = "Model", values = colors_discrete_drwhy(3)) +
    scale_color_manual(name = "Model", values = colors_discrete_drwhy(3)) +
    scale_x_continuous("False positive rate", limits = c(0, 1), expand = c(0, 0)) +
    scale_y_continuous("True positive rate", limits = c(0, 1), expand = c(0, 0)) +
    coord_fixed() +
    labs(title = "ROC curve",
         subtitle = "mean +- sd for five train:test splits") +
    theme(
      panel.grid.major.y = element_line(color = "grey90", size = 0.5, linetype = 1),
      panel.grid.minor.y = element_line(color = "grey90", size = 0.5,  linetype = 1),
      panel.grid.major.x = element_line(color = "grey90", size = 0.5, linetype = 1),
      panel.grid.minor.x = element_line(color = "grey90", size = 0.5,  linetype = 1),
      legend.text = element_text(color = "#371ea3"),
      legend.title = element_text(color = "#371ea3")
    )
}


plot_prc_multiple <- function(...) {
  expl <- list(...)
  
  recall_domain <- seq(0, 1, length.out=21)
  
  prdfl <- lapply(expl, function(exp) {
    fg <- exp$y_hat[exp$y == 1]
    bg <- exp$y_hat[exp$y == 0]
    pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
    df <- pr$curve
    
    precision_interpolated <- pracma::interp1(rev(df[,1]), rev(df[,2]), recall_domain)
    
    data.frame(precision = precision_interpolated,
               recall = recall_domain,
               label = exp$label,
               iter = exp$iter)
  })
  prdf <- do.call(rbind, prdfl)

  prdf_aggregated <- prdf %>%
    group_by(label, recall) %>% 
    summarise(precision_m = mean(precision), precision_sd = sd(precision))
  
  ggplot(prdf_aggregated, aes(x = recall, y = precision_m)) +
    geom_hline(yintercept = 0.77, color = "darkgrey", lty = 2) +
    geom_step(aes(color = label), size = 1) +
    geom_ribbon(aes(ymin = precision_m - precision_sd,
                    ymax = precision_m + precision_sd, 
                    fill = label), 
                alpha = 0.5) +
    theme_drwhy() +
    scale_fill_manual(name = "Model", values = colors_discrete_drwhy(3)) +
    scale_color_manual(name = "Model", values = colors_discrete_drwhy(3)) +
    scale_x_continuous("Recall", limits = c(0, 1), expand = c(0, 0)) +
    scale_y_continuous("Precision", limits = c(0.75, 1), expand = c(0, 0)) +
    labs(title = "PR curve",
         subtitle = "mean +- sd for five train:test splits") +
    theme(
      panel.grid.major.y = element_line(color = "grey90", size = 0.5, linetype = 1),
      panel.grid.minor.y = element_line(color = "grey90", size = 0.5,  linetype = 1),
      panel.grid.major.x = element_line(color = "grey90", size = 0.5, linetype = 1),
      panel.grid.minor.x = element_line(color = "grey90", size = 0.5,  linetype = 1),
      legend.text = element_text(color = "#371ea3"),
      legend.title = element_text(color = "#371ea3")
    )
}
