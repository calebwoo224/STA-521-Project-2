block_split = function(training_data, training_labels) {
  
  training_data$block = rep(0, nrow(training_data))
  training_labels_block = numeric()
  ims = distinct(training_data %>% select(image)) %>% pull(image)
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
  
  return(list("training_data" = training_data, 
              "labels" = training_labels_block, 
              "snum" = snum))
}