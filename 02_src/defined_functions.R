## Define Plotting functions
ActualvsPredict <- function(data.test, 
                            testpred,
                            var_name){
  plot(
    x = data.test$MCPD_GLY,
    y = testpred,
    xlab = 'Actual',
    ylab = 'Prediction',
    main = paste0('Actual vs Predicted value ', '(', var_name,')'),
    #sub = 'test set'
  )
  
  testlinmod <- lm(testpred ~ data.test$MCPD_GLY)
  abline(testlinmod, col='blue', lwd=2.5, lty = 'solid')
  abline(a=0, b=1, col='red', lwd=2.5, lty = 'dashed')
  legend(
    'topleft',
    legend= c('Model', 'Base'),
    col = c('black','red'),
    lwd = 2.5,
    lty = c('solid','dashed')
  )
}


## Calculate Rsquare
Rsquare <- function(predicted_values, actual_values){
  SS_total <- sum((actual_values - mean(actual_values))^2)
  SS_residual <- sum((actual_values - predicted_values)^2)
  R2 <- 1 - (SS_residual / SS_total)
  return(R2)
}
