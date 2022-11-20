image_split <- function(data) {
  train <- data %>%
    filter(image == 1 | image == 3)
  
  val_test <- data %>%
    filter(image == 2)
  n <- nrow(val_test)
  half_i <- floor(nrow(val_test)/2)
  
  val <- val_test[1:half_i, ]
  test <- val_test[(half_i+1):n, ]
  
  return(list(
    "train" = train,
    "val" = val,
    "test" = test
  ))
}