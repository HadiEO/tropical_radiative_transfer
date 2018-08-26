RMSE <- function(pred, obs) {
  error <- pred - obs
  sqrt(mean(error^2))
}