image_split <- function(data, labels, K) {
  
  data$block = rep(0, nrow(data))
  ims = distinct(data %>% dplyr::select(image)) %>% pull(image)
  snum = 0
  train_blocks <- floor(K/2.5)
  if (K-2*train_blocks >= train_blocks) {
    train_blocks <- train_blocks + 1
  }
  val_blocks <- K-2*train_blocks
  
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
      if (val_blocks > 1) {
        xsplit = split(xvals,
                       cut(xvals,
                           val_blocks,
                           labels = FALSE))
      }
      else {
        xsplit = xvals
        ysplit = yvals
      }
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
      xsplit = split(xvals,
                     cut(xvals,
                         train_blocks,
                         labels = FALSE))
    }
    
    if (typeof(xsplit) == "list") {
      for (i in 1:length(xsplit)) {
          xs = xsplit[[i]]
          snum = snum + 1
          data = data %>%
            mutate(block = ifelse(
              ((image == im) & (X %in% xs) & (Y %in% yvals)), 
              snum, block))
      }
    }
    else {
      snum = snum + 1
      data = data %>%
        mutate(block = ifelse(
          ((image == im) & (X %in% xsplit) & (Y %in% ysplit)), 
          snum, block))
    }
  }
  
  return(list("training_data" = data %>%
                filter(block != 0) %>%
                dplyr::select(-label), 
              "labels" = data %>%
                filter(block != 0) %>%
                pull(block),
              "train_labels"= data %>%
                filter(block != 0) %>%
                pull(label),
              "val_data" = val,
              "val_labels" = val_labels,
              "test_data" = test,
              "test_labels" = test_labels,
              "snum" = snum))
}