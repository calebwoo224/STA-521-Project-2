library(tidyverse)

CVmaster = function(training_data, training_labels, classifier, 
                    K = 5, loss = "misclassification", 
                    split = c("image", "block"), 
                    type = c("class", "prob", "response"),
                    thresh = 0.5, formula = TRUE, seed = 521, ...) {
  ags = list(...)
  
  set.seed(seed)
  
  if (split == "block"){
    output = block_split(training_data, training_labels, K, seed=seed)
    training_data = output$training_data
    labels = output$labels
    snum = output$snum
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
  if (split == "block") {
    bks = sample(bks)
    tnum = floor(length(bks)/6) #Set ~1/6 for testing
    test_data = training_data %>%
      filter(block %in% bks[((length(bks)-tnum)+1):length(bks)])
    test_labels = training_labels[(labels %in% bks[((length(bks)-tnum)+1):length(bks)])]
    bks = bks[1:(length(bks)-tnum)]
    training_data = training_data %>%
      filter(block %in% bks)
    training_labels = training_labels[labels %in% bks]
    labels = labels[labels %in% bks]
  }
  folds = createFolds(bks, k = K)
  
  cv_preds = list()
  cv_response_preds = list()
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
    if (classifier == "gbm") {
      train[, "trainy"] = as.numeric(as.character(train[, "trainy"]))
    }
    if (formula == TRUE) {
      formula_written = as.formula(paste("trainy ~ ", paste(colnames(train)
                           [-length(colnames(train))], 
                           collapse = "+ ")))
      model = do.call(classifier,
                      append(list(
                        formula_written,
                        as_tibble(train)
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
    preds = predict(model, X, type = type)
    if (is.list(preds)) {
      preds = preds$class
    }
    if(type == "prob" | type == "vector") {
      preds = apply(preds, MARGIN = 1, FUN = which.max) - 1
    }
    if (type == "response") {
      cv_response_preds[[paste0("Fold", iter)]] = preds
      preds <- ifelse(preds > thresh, 1, 0)
    }
    cv_preds[[paste0("Fold", iter)]] = preds
    err = 1 - mean(preds == y)
    error = c(error, err)
    #print(paste0("CV-loss for fold ", iter, ": ", err))
  }
  # average loss
  e = mean(error)
  #print(e)
  # train on all data
  train = training_data %>%
    dplyr::select(!c(X, Y, image, block)) %>%
    mutate(trainy = training_labels)
  if (classifier == "gbm") {
    train[, "trainy"] = as.numeric(as.character(train[, "trainy"]))
  }
  if (formula == TRUE) {
    formula_written = as.formula(paste("trainy ~ ", paste(colnames(train)
                                                          [-length(colnames(train))], 
                                                          collapse = "+ ")))
    model = do.call(classifier,
                    append(list(
                      formula_written,
                      as_tibble(train)
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
    if (classifier == "glm" | classifier == "gbm") {
      roc_obj <- roc(test_labels, preds)
    }
    else if (classifier == "lda" | classifier == "qda") {
      roc_obj <- roc(test_labels, preds$posterior[, "1"])
    }
    else if (classifier %in% c("tree", "knn3", "randomForest")) {
      roc_obj <- roc(test_labels, preds[, "1"])
    }
    
    test_response_preds = NULL
    if (is.list(preds)) {
      preds = preds$class
    }
    if (type == "prob" | type == "vector") {
      preds = apply(preds, MARGIN = 1, FUN = which.max) - 1
    }
    if (type == "response") {
      test_response_preds = preds
      preds <- ifelse(preds > thresh, 1, 0)
    }
    test_preds = preds
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
    "roc_obj" = roc_obj,
    "cv_preds" = cv_preds,
    "cv_response_preds" = cv_response_preds,
    "test_preds" = test_preds,
    "test_response_preds" = test_response_preds,
    "test_labels" = test_labels
  ))
}