#' @title Image Split
#'
#' @param data A tibble of data to be split into blocks with *X* and *Y* coordinates
#' @param labels A vector of labels corresponding to the data above
#' @param K An integer number of splits to be made
#'
#' @return A list containing the following:
#' @returns training data: A dataframe containing all of the unlabeled training data
#' @returns labels: A vector containing the block corresponding to each data point
#' @returns train_labels: A vector containing the binary training data labels
#' @returns val_data: A dataframe containing the validation dataset
#' @returns val_labels: A vector containing the labels for the validation dataset
#' @returns test_data: A dataframe containing the test dataset
#' @returns test_labels: A vector containing the labels for the test dataset
#' @returns snum: The number of blocks the images have been split into
#'
#' @export
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
      x_range = sort(x_range)
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
        xsplit = sort(xvals)
        ysplit = split(sort(yvals),
                       cut(seq_along(yvals),
                           val_blocks,
                           labels = FALSE))
      }
      else {
        xsplit = sort(xvals)
        ysplit = sort(yvals)
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
      xsplit = sort(xvals)
      ysplit = split(sort(yvals),
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
              ((image == im) & (X %in% xsplit) & (Y %in% ys)),
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
