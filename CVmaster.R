library(tidyverse)

CVmaster = function(training_data, training_labels, classifier, 
                    K = 5, loss = "misclassification", ...) {
  ags = list(...)
  
  #Building the blocks
  training_data$block = rep(0, nrow(training_data))
  training_labels_block = numeric()
  ims = distinct(training_data %>% select(image))
  snum = 0
  
  for (im in ims){
    xvals = training_data %>%
      filter(image == im) %>%
      pull(X) %>%
      unique()
    yvals = training_data %>%
      filter(image == im) %>%
      pull(Y) %>%
      unique()
    
    xsplit = split(xvals,
                   cut(seq_along(xvals),
                       K,
                       labels = FALSE))
    
    ysplit = split(yvals,
                   cut(seq_along(yvals),
                       K,
                       labels = FALSE))

    for (xs in xsplit){
      for (ys in ysplit){
        snum = snum + 1
        training_data = training_data %>%
          mutate(block = ifelse(
            ((image == im) & (X %in% xs) & (Y %in% ys)), 
            snum, block))
        training_labels_block = c(training_labels_block, 
                                  rep(snum, 
                                      nrow(training_data %>%
                                                   filter((image == im) & 
                                                            (X %in% xs) & 
                                                            (Y %in% ys)))))
      }
    }
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
    y = training_labels[training_labels_block %in% f]
    
    trainy = training_labels[!(training_labels_block %in% f)]
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