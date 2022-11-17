library(tidyverse)

CVmaster = function(training_data, training_labels, classifier, 
                    K = 5, loss = "misclassification") {
  
  #Building the blocks
  training_data$block = rep(0, ncol(training_data))
  ims = distinct(training_data %>% select(image))
  snum = 0
  
  for (im in ims){
    xvals = training_data %>%
      filter(image = im) %>%
      pull(X) %>%
      unique()
    yvals = training_data %>%
      filter(image = im) %>%
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
      }
    }
  }
  
  #Splitting the blocks
  bks = seq.int(from = 1, to = snum, by = 1)
  
}