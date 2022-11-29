image_split <- function(data, labels, K) {
  
  data$block = rep(0, nrow(data))
  training_labels_block = numeric()
  ims = distinct(data %>% dplyr::select(image)) %>% pull(image)
  snum = 0
  for (im in ims) {
    if (im == 2) {
      data$label <- labels
      val_test <- data %>%
        filter(image == 2)
      n <- nrow(val_test)
      half_i <- floor(nrow(val_test)/2)
      
      val <- val_test[1:half_i, -ncol(val_test)]
      val_labels <- val_test[1:half_i, ncol(val_test)]
      test <- val_test[(half_i+1):n, -ncol(val_test)]
      test_labels <- val_test[(half_i+1):n, ncol(val_test)]
      
      xvals = val %>%
        filter(image == im) %>%
        pull(X) %>%
        unique()
      yvals = val %>%
        filter(image == im) %>%
        pull(Y) %>%
        unique()
    }
    else {
      xvals = data %>%
        filter(image == im) %>%
        pull(X) %>%
        unique()
      yvals = data %>%
        filter(image == im) %>%
        pull(Y) %>%
        unique()
    }
    
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
        data = data %>%
          mutate(block = ifelse(
            ((image == im) & (X %in% xs) & (Y %in% ys)), 
            snum, block))
        training_labels_block = c(training_labels_block, 
                                  rep(snum, 
                                      nrow(data %>%
                                             filter((image == im) & 
                                                      (X %in% xs) & 
                                                      (Y %in% ys)))))
      }
    }
  }
  return(list("training_data" = data %>% filter(block != 0), 
              "labels" = training_labels_block,
              "val_data" = val,
              "val_labels" = val_labels,
              "test_data" = test,
              "test_labels" = test_labels,
              "snum" = snum))
}