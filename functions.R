rmse <- function(model_pred, actual){
  error = (model_pred - actual)^2
  mean_error = sum(error)/length(actual)
  rmse <- sqrt(mean_error)
  
  return(rmse)
  
}

rsquared <- function(model_pred, actual, train_mean){
  
  sse = sum((model_pred - actual)^2)
  sst = sum((actual - mean(train_mean, na.rm = T))^2)
  
  rsquared <- 1 - sse/sst
  return(rsquared)
  
}


MAE <- function(model_pred, actual){
  
  absolute_error <- sum(abs(model_pred - actual))
  MAE <- absolute_error/length(actual)
  
  return(MAE)
}









get_performance <- function(model_train, model_test){
  
  #In Sample 
  r2 <- rsquared(model_train, ames.train)
  mean_absolute_error <- MAE(model_train, ames.train)
  root_mean <- rmse(model_train, ames.train)
  
  insample <- c(r2, mean_absolute_error, root_mean)
  
  #Out of Sample 
  r2 <- rsquared(model_test, ames.test)
  mean_absolute_error <- MAE(model_test, ames.test)
  root_mean <- rmse(model_test, ames.test)
  
  outsample <- c(r2, mean_absolute_error, root_mean)
  
  
  metric <- c("R Squared", "MAE", "RMSE")
  metric_df <- cbind(metric, insample, outsample)
  colnames(metric_df) <- c("Metric", "In Sample Performance", "Out of Sample Performance")
  
  return(metric_df)
  
}
