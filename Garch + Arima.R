# Arima_Prediction_Creator - основна функция, лежит тут
library(forecast)
library(stratbuilder3)
library(stats)
library(tseries)
library(ggplot2)

USDRUB <- Data() %>%
  modify(period = 60,
         from = Sys.Date() - 300) %>%
  stock(c('SPFB.Si'),
        src = 'Finam') %>%
  getSymbols

utility = diff(log(USDRUB$mat$close))
utility <- utility[!is.na(utility)]

e <- new.env()
Arima_Prediction_Creator <- function(utility, number_of_predictions, arima.dim, level = 0.8){
  
  utility.prediction <- matrix(0, nrow = number_of_predictions, ncol = 3)
  
  for(i in 1:number_of_predictions){
    
    #model <- auto.arima(utility[i:(arima.dim + i - 1)], d = 0) # Строим ариму по последним arima.dim  
                                                        
    # Тут блок поиска лучшей арима-модели (auto.arima - не оч)
    final.aic <- 1e8 
    for (p in 0:2) for (q in 0:2){
      if (p == 0 && q== 0) next

      tmp_model = tryCatch(                                      
       arima(utility[i:(arima.dim + i - 1)], order=c(p, 0, q)),  # Чтоб не было ошибок
       error=function( err ) FALSE,
       warning=function( err ) FALSE
       )   

      if( !is.logical( tmp_model ) ) {
       current.aic <- AIC(tmp_model)
         if (current.aic < final.aic) {
          final.aic <- current.aic
          final.order <- c(p, 0, q)
          model <- tmp_model
          }
        } else {
          next
        }
    }
    # В итоге в model записана наша арима-модель

    arma.resid = model$residuals 
    prediction <- forecast(model, h = 1)$mean[1]        # Точечное предсказание аримой
    utility.garch <- garch(arma.resid, trace = F)       # Смоделировали остатки гарчем 
    
    # сигма_t+1 - предсказание по гарчу 
    sigma_prediction <- sqrt(utility.garch$coef[1] + utility.garch$coef[2]*arma.resid[arima.dim]^2 + utility.garch$coef[3]*utility.garch$fitted.values[arima.dim,1]^2)
    
    norm_level <- (level + 1) / 2
    
    utility.prediction[i,1] = - (qnorm(norm_level)) * sigma_prediction + prediction
    utility.prediction[i,2] = prediction
    utility.prediction[i,3] = (qnorm(norm_level)) * sigma_prediction + prediction
  }
  
  return(utility.prediction) #(3 вектора. 2й - предсказания, 1 и 3 - предсказания - доверительный интервал уровня level
}
# utility.prediction[i,2] ~  utility[arima.dim + i]

arima.dim <- 200
number_of_predictions <- 600
utility.prediction <- Arima_Prediction_Creator(utility, number_of_predictions, arima.dim, 0.8)
# В итоге в utility предикшн лежат предсказания в моменты (arima.dim+1, arima.dim+50)


# Количество значений, попавших в доверительный интервал
k <- 0
for(i in 1:number_of_predictions){
  if(utility.prediction[i,1] <= utility[arima.dim + i] & utility.prediction[i,3] >= utility[arima.dim + i] ){
    #print(c(utility.prediction[i,1], utility[arima.dim + i], utility.prediction[i,3] ))
    k <- k+1
  }
}

# Количество совпадений знаков 
signs <- 0
for(i in 1:number_of_predictions){
  if(sign(utility.prediction[i,2]) == sign(utility[arima.dim + i])){
    signs <- signs + 1
  }
}


# Какой сигнал лучше ?


  #position[i] - наша позиция в момент arima.dim + i
  position <- sign(utility.prediction[,2])
  # Позиция * приращение цены
  profit <- position *  (USDRUB$mat$close[(arima.dim + 2):(number_of_predictions + arima.dim + 1)] - USDRUB$mat$close[(arima.dim+1):(arima.dim + number_of_predictions)])

  plot(cumsum(profit) + USDRUB$mat$close[arima.dim+1], type = 'l')
  
  plot(USDRUB$mat$close[201:800] - USDRUB$mat$close[201], type = 'l', main = "Buy&Hold")
  lines(cumsum(profit), col = 'red', main = "ARIMA")
 
  a <- (utility.prediction[,2] / (utility.prediction[,3] - utility.prediction[,2]))
  length(a[abs(a) > 0.1])
  
  
  
# Черновик   
limit <- 0.6
level <- 0.8
sigma_tp1 <- (utility.prediction[,3] - utility.prediction[,2]) / qnorm((level+1)/2)
indicrator_long <- sigma_tp1 * qnorm((1-level)/2 + limit) - utility.prediction[,2]
sum(utility.prediction[,2] >0)
sum((indicrator_long <= 0) & utility.prediction[,2] >0)
indicator_short <- sigma_tp1 * qnorm (limit + (-level+1)/2) + utility.prediction[,2]
sum(indicator_short <= 0 & utility.prediction[,2] < 0)

trading_times <- sort(c(which(((indicrator_long <= 0) & utility.prediction[,2] >0)), which(((indicator_short <= 0) & utility.prediction[,2] <0))))
total_profit <- 1:length(trading_times)
k<-1

for(i in trading_times){

  position <- sign(utility.prediction[i,2])
  profit <- position * (USDRUB$mat$close[200+i+1] - USDRUB$mat$close[200+i])
  total_profit[k] <-  profit
  k<-k+1
}

plot(cumsum(total_profit), type = 'l')


