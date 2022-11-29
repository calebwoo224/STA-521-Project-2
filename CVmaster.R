library(tidyverse)

CVmaster = function(training_data, training_labels, classifier, 
                    K = 5, loss = "misclassification", 
                    split = c("image", "block"), 
                    type = c("class", "prob", "response"),
                    formula = TRUE, ...) {
  ags = list(...)
  
  if (split == "block"){
    output = block_split(training_data, training_labels, K)
    training_data = output$training_data
    labels = output$labels
    snum = output$snum
  } else if (split == "image") {
    output = image_split(training_data, training_labels, K)
    training_data = output$training_data
    labels = output$labels
    snum = output$snum
  } else {
    stop("Improper split method provided")
  }
  
  #Splitting the blocks
  bks = seq.int(from = 1, to = snum, by = 1)
  folds = createFolds(bks, k = K)
  
  error = numeric()
  iter = 0
  for (f in folds){
    iter = iter + 1
    print(paste0("Fold ", iter))
    print(f)
    X = training_data %>%
      filter(block %in% f) %>%
      dplyr::select(!c(X, Y, image, block))
    y = training_labels[labels %in% f]
    # debug print
    print(length(y))
    print(nrow(X))
    
    trainy = training_labels[!(labels %in% f)]
    # debug print
    print(length(trainy))
    print(training_data %>%
            filter(!(block %in% f)) %>%
            nrow())
    train = training_data %>%
        filter(!(block %in% f)) %>%
        dplyr::select(!c(X, Y, image, block)) %>%
        mutate(trainy = trainy)
    
    if (formula == TRUE) {
    formula = as.formula(paste("trainy ~ ", paste(colnames(train)
                         [-length(colnames(train))], 
                         collapse = "+ ")))
    model = do.call(classifier,
                    append(list(
                      formula,
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
    err = 1 - mean(preds == y)
    error = c(error, err)
    print(paste0("CV-loss for fold ", iter, ": ", err))
  }
  e = mean(error)
  print(paste0("The ", K, "-fold CV-loss is: ", e))
  return(e)
}