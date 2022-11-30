library(tidyverse)

CVmaster = function(training_data, training_labels, classifier, 
                    K = 5, loss = "misclassification", 
                    split = c("image", "block", "done"), 
                    type = c("class", "prob", "response"),
                    thresh = 0.5, formula = TRUE, 
                    test_data = NULL, test_labels = NULL, ...) {
  ags = list(...)
  
  if (split == "done") {
    snum = 0
    training_data$block2 = rep(0, nrow(training_data))
    training_data = training_data %>%
      mutate(tl = training_labels) %>%
      arrange(block)
    bk = training_data %>% pull(block) %>% unique()
    for (i in 1:length(bk)) {
      snum = snum + 1
      training_data = training_data %>%
        mutate(block2 = ifelse(block == bk[i], snum, block2))
    }
    training_labels = training_data %>%
      pull(tl)
    training_data = training_data %>%
      mutate(block = block2) %>%
      dplyr::select(!c(tl, block2))
    labels = training_data %>%
      pull(block)
  } else if (split == "block"){
    output = block_split(training_data, training_labels, K)
    training_data = output$training_data
    labels = output$labels
    snum = output$snum
    test_data = NULL
    test_labels = NULL
  } else if (split == "image") {
    output = image_split(training_data, training_labels, K)
    training_data = output$training_data
    labels = output$labels
    training_labels = output$train_labels
    snum = output$snum
    test_data = output$test_data
    test_labels = output$test_labels
  } else {
    stop("Improper split method provided")
  }
  
  #Splitting the blocks
  bks = seq.int(from = 1, to = snum, by = 1)
  folds = createFolds(bks, k = K)
  
  error = numeric()
  rows = character()
  iter = 0
  for (f in folds){
    iter = iter + 1
    #print(paste0("Fold ", iter))
    #print(f)
    rows = c(rows,
             paste0("Fold ", iter, " CV-loss"))
    X = training_data %>%
      filter(block %in% f) %>%
      dplyr::select(!c(X, Y, image, block))
    y = training_labels[labels %in% f]
    
    trainy = training_labels[!(labels %in% f)]
    train = training_data %>%
        filter(!(block %in% f)) %>%
        dplyr::select(!c(X, Y, image, block)) %>%
        mutate(trainy = trainy)
    
    if (formula == TRUE) {
    formula_written = as.formula(paste("trainy ~ ", paste(colnames(train)
                         [-length(colnames(train))], 
                         collapse = "+ ")))
    model = do.call(classifier,
                    append(list(
                      formula_written,
                      data = as_tibble(train)
                 ), ags)
                )
    } else {
      Xv = train %>%
        dplyr::select(!trainy) %>%
        as.matrix()
      model = do.call(classifier,
                    append(list(
                      Xv,
                      trainy
                    ), ags)
    )
    }
    preds = predict(model, X, type=type)
    if (is.list(preds)) {
      preds = preds$class
    }
    if(type == "prob") {
      preds = apply(preds, MARGIN = 1, FUN = which.max) - 2
    }
    # logistic regression
    if (type == "response") {
      preds <- ifelse(preds > thresh, 1, 0)
    }
    err = 1 - mean(preds == y)
    error = c(error, err)
    #print(paste0("CV-loss for fold ", iter, ": ", err))
  }
  # average loss
  e = mean(error)
  # train on all data
  train = training_data %>%
    dplyr::select(!c(X, Y, image, block)) %>%
    mutate(trainy = training_labels)
  if (formula == TRUE) {
    formula_written = as.formula(paste("trainy ~ ", paste(colnames(train)
                                                          [-length(colnames(train))], 
                                                          collapse = "+ ")))
    model = do.call(classifier,
                    append(list(
                      formula_written,
                      data = as_tibble(train)
                    ), ags)
    )
  } else {
    Xv = train %>%
      dplyr::select(!trainy) %>%
      as.matrix()
    model = do.call(classifier,
                    append(list(
                      Xv,
                      trainy
                    ), ags)
    )
  }
  # test loss
  if (!is.null(test_data) & !is.null(test_labels)) {
    cols = colnames(train)[-length(colnames(train))]
    test_data = test_data[, cols]
    preds = predict(model, test_data, type=type)
    # test ROC
    if (classifier == "glm") {
      roc_obj <- roc(test_labels, preds)
    }
    else if (classifier == "lda" | classifier == "qda") {
      roc_obj <- roc(test_labels, preds$posterior[, "1"])
    }
    if (is.list(preds)) {
      preds = preds$class
    }
    if (type == "prob") {
      preds = apply(preds, MARGIN = 1, FUN = which.max) - 2
    }
    if (type == "response") {
      preds <- ifelse(preds > thresh, 1, 0)
    }
    test_error = 1 - mean(preds == test_labels)
  }
  else {
    test_error = 1
    roc_obj = NULL
  }
  df = data.frame(
    c(error, e, test_error),
    row.names = c(rows,
                 "Average CV-loss",
                 "Test CV-loss")
  )
  #print(paste0("The ", K, "-fold CV-loss is: ", e))
  return(list(
    "CV_loss" = df,
    "roc_obj" = roc_obj
  ))
}