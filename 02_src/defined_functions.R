## 定义作图函数
#pdf('/Users/gsn/Desktop/MLR1.pdf')
ActualvsPredict <- function(){
  plot(
    x = fold.test$dietaryMCPD,
    y = testpred,
    xlab = 'Actual',
    ylab = 'Prediction',
    main = 'Actual value vs Predicted value',
    #sub = 'test set'
  )
  
  testlinmod <- lm(testpred~fold.test$dietaryMCPD)
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
#dev.off()

