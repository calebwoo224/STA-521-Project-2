library(tidyverse)

CVmaster = function(training_data, training_labels, classifier, 
                    K = 5, loss = "misclassification", 
                    split = c("image", "block"), ...) {
  ags = list(...)
  
  if (split == "block"){
    output = block_split(training_data, training_labels)
    training_data = output$training_data
    labels = output$labels
    snum = output$snum
  } else if (split == "image") {
    output = image_split()
  } else {
    stop("Improper split method provided")
  }
  
  #Splitting the blocks
  bks = seq.int(from = 1, to = snum, by = 1)
  folds = createFolds(bks, k = K)
  
  error = numeric()
  for (f in folds){
    print(f)
    X = training_data %>%
      filter(block %in% f) %>%
      select(!c(X, Y, image, block))
    y = training_labels[labels %in% f]
    
    trainy = training_labels[!(labels %in% f)]
    train = training_data %>%
        filter(!(block %in% f)) %>%
        select(!c(X, Y, image, block)) %>%
        mutate(trainy = trainy)
    
    formula = as.formula(paste("trainy ~ ", paste(colnames(train)
                         [-length(colnames(train))], 
                         collapse = "+ ")))
    
    model = do.call(classifier,
                    append(list(
                      formula,
                      data = as_tibble(train)
                 ), ags)
                )
    preds = predict(model, X, type = "class")
    err = 1 - mean(preds == y)
    error = c(error, err)
  }
  e = mean(error)
  print(paste0("The ", K, "-fold CV-loss is: ", e))
  return(e)
}