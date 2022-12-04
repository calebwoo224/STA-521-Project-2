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
      
      x_range = val_test %>%
        arrange(X) %>%
        pull(X) %>%
        unique()
      half_i = floor(length(x_range)/2)
      
      test = val_test %>%
        filter((image == im) & (X %in% x_range[1:half_i]))
      test_labels = test[, ncol(test)]
      test = test[, -ncol(test)]
      
      val = val_test %>%
        filter((image == im) & (X %in% x_range[(half_i+1):length(x_range)]))
      val_labels = val[, ncol(val)]
      val = val[, -ncol(val)]
      
      xvals = val %>%
        filter(image == im) %>%
        pull(X) %>%
        unique()
      yvals = val %>%
        filter(image == im) %>%
        pull(Y) %>%
        unique()
      if (val_blocks > 1) {
        ysplit = split(yvals,
                       cut(seq_along(yvals),
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
      ysplit = split(yvals,
                     cut(seq_along(yvals),
                         train_blocks,
                         labels = FALSE))
    }
    
    if (typeof(ysplit) == "list") {
      for (i in 1:length(ysplit)) {
          ys = ysplit[[i]]
          snum = snum + 1
          data = data %>%
            mutate(block = ifelse(
              ((image == im) & (X %in% xvals) & (Y %in% ys)), 
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