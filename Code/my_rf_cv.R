#' Cross Validation for Random Forests
#'
#' This function runs k-fold cross validation for random forests using the penguins data set.
#'
#' @param k The number of folds to use for cross validation.
#'
#' @keywords inference
#'
#' @return The average MSE across all k folds.
#'
#' @importFrom stats model.frame model.matrix model.response predict pt sd na.omit
#'
#' @export
my_rf_cv <- function(k) {
  if (k <= 1) {
    stop("k must be an integer greater than 1")
  }
  if (k != round(k)) {
    stop("k must be an integer greater than or equal to 1")
  }
  # omit the NAs in my_penguins
  penguins_no_NA <- na.omit(project3part1package::my_penguins)
  # set the relevant columns of penguins as our data
  data <- data.frame("body_mass" = penguins_no_NA$body_mass_g,
                     "bill_length" = penguins_no_NA$bill_length_mm,
                     "bill_depth" = penguins_no_NA$bill_depth_mm,
                     "flipper_length" = penguins_no_NA$flipper_length_mm)
  
  n <-  length(data[, 1])
  # randomly assign each entry in the data set to a fold
  fold <- sample(rep(1:k, length = n))
  data$fold <- fold
  
  MSEs <- c()
  # loop through each fold
  for (i in 1:k) {
    data_train <- data %>% dplyr::filter(fold != i)
    data_test <- data %>% dplyr::filter(fold == i)
    
    rf_cv <- randomForest::randomForest(body_mass ~ bill_length + bill_depth + flipper_length,
                                        data = data_train, ntree = 100)
    
    rf_pred <- predict(rf_cv, data_test[, -1])
    
    # calculate the MSE for this fold
    MSEs[i] <- mean((rf_pred - data_test[, 1])^2)
  }
  
  # return the average MSE across all k folds
  return(mean(MSEs))
}
