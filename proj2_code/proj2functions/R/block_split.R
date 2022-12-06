#' @title Block Split
#'
#' @param training_data A tibble of the data to be split into blocks containing *X* and *Y* coordinates
#' @param training_labels A vector containing the labels for the data given
#' @param K The number of folds to split the data into
#' @param seed A random seed to permute the blocks
#'
#' @return A list containing the following:
#' @returns training_data: The training data with the blocks assigned in a column
#' @returns labels: a list of which block each label corresponds to
#' @returns snum: The number of blocks the images have been split into
#'
#' @export
block_split = function(training_data, training_labels, K, seed) {

  set.seed(seed)

  training_data$block = rep(0, nrow(training_data))
  training_labels_block = numeric()
  ims = distinct(training_data %>% dplyr::select(image)) %>% pull(image)
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

    xsplit = split(sort(xvals),
                   cut(seq_along(xvals),
                       K,
                       labels = FALSE))

    ysplit = split(sort(yvals),
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
