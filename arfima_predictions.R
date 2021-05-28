library(purrr)
library(forecast)
library(stratbuilder3)
library(stats)
library(tseries)
library(ggplot2)
library(forecast)
library(arfima)
library(mltools)

USDRUB.15 <- Data() %>%
  modify(period = 15,
         from = Sys.Date() - 300) %>%
  stock(c('SPFB.Si'),
        src = 'Finam') %>%
  getSymbols

Utility.Si.15 <- diff(log(USDRUB.15$mat$close))

arfima.length <- 55*5
arfima.order <- c(2,0,0)

predictions_arfima <- function(utility,arfima.length,arfima.order,number_of_predictions){
  
  predictions <- matrix(ncol = 2, nrow = number_of_predictions)
  for (i in 1:(number_of_predictions)){
    
        suppressMessages({ 
   modell <- arfima::arfima(
     utility[i:(arfima.length+i-1)],
      order = arfima.order,
     quiet = TRUE
        )
    })
    
    predictions[i,1] <- (predict(modell)[[1]])$Fo
    arma.resid = modell$modes[[1]]$residuals
    
    utility.garch <- garch(arma.resid, trace = F) # Смоделировали остатки гарчем 
    
    sigma_prediction <- sqrt(utility.garch$coef[1] + 
                               utility.garch$coef[2]*arma.resid[length(arma.resid)]^2 + 
                               utility.garch$coef[3]*utility.garch$fitted.values[length(arma.resid),1]^2)
    
    predictions[i,2] <- sigma_prediction
  }
  
  return(predictions)
   # predictions[i,2] - предсказание сигмы гарчем 
}  # predictions[i,1] ~ utility(arfima.length + i) = ln(x[t+1] / x[t])


predictions<-list()

# predictions[[i]] -- предсказания по арфиме, в которой длина модели = 55*i
compiler::setCompilerOptions(suppressAll = TRUE){
for(i in 1:12){
  print(i)
  system.time(
  predictions[[i]] <- predictions_arfima(Utility.Si.15, 55 * i ,arfima.order, 
                                  number_of_predictions = 55*30)
  )
}
} #prediction[[i]][j]  ~ utility(55 * i + j) (первое предсказывает второе)


for(i in 1:12){
print (rmse(predictions[[i]], Utility.Si.15[55*i + 1:(55*30)]))
} # среднеквадратичные ошибки прогноза

# x[t] = exp(u[t-1]) * x[t-1]


# Статистики прогнозов 
predictions.stats<-list()

for(i in 1:12){ 
  
  predictions.stats[[i]] <- data.frame(
    "Дата" = USDRUB.15$dates[55 * i + 1 + 1:(55*30)],
    "Приращения" = Utility.Si.15[55 * i + 1:(55*30)],
    "Прогноз" = predictions[[i]],
    "Точность" = (sign(predictions[[i]]) == sign(Utility.Si.15[55 * i + 1:(55*30)])), # число совп знаков
    "Относительная ошибка" = (Utility.Si.15[55 * i + 1:(55*30)] - predictions[[i]])/Utility.Si.15[55 * i + 1:(55*30)]
  )
  
}

View(predictions.stats[[1]])

mean(
  predictions.stats[[12]]$Относительная.ошибка[
                        is.finite(predictions.stats[[12]]$Относительная.ошибка)]
  )

#--------------------------------------------------------------------------------------------------------------
# Проверим, как влияет величина прогноза на его точность 

forecasts_plus <- which(predictions.stats[[1]]$Прогноз>=0)
forecasts_minus <- which(predictions.stats[[1]]$Прогноз<0)

quantils_plus <- quantile(predictions.stats[[1]]$Прогноз[forecasts_plus],
                          probs = seq(0,1,by=0.02))

quantils_minus <- quantile(predictions.stats[[1]]$Прогноз[forecasts_minus],
                          probs = seq(0,1,by=0.02))


for(i in 1:50){
  
tmp_p <- ((predictions.stats[[1]]$Прогноз[forecasts_plus] > quantils_plus[i])) # Те что больше квантили

tmp_m <- ((predictions.stats[[1]]$Прогноз[forecasts_minus] < quantils_minus[52-i])) 


print(sum(predictions.stats[[1]]$Точность[tmp_p]) / length(predictions.stats[[1]]$Точность[tmp_p]))

print(sum(predictions.stats[[1]]$Точность[tmp_m]) / length(predictions.stats[[1]]$Точность[tmp_m]))

print("---------")
}

parts_plus <- list() # Тут будут храниться доли точных прогнозов больших нуля
parts_minus <- list() # Тут будут храниться доли точных прогнозов меньших нуля

for( i in 1:12){
  
  parts_plus[[i]] <- rep(0,100) # Тут будут храниться доли 
  parts_minus[[j]] <- rep(0,100)
  
  forecasts_plus <- which(predictions.stats[[i]]$Прогноз>=0)
  forecasts_minus <- which(predictions.stats[[i]]$Прогноз<0)
  
  quantils_plus <- quantile(predictions.stats[[i]]$Прогноз[forecasts_plus],
                            probs = seq(0,1,by=0.01))

  quantils_minus <- quantile(predictions.stats[[i]]$Прогноз[forecasts_minus],
                             probs = seq(0,1,by=0.01))
  
  for(j in 1:100){
    
    tmp_p <- ((predictions.stats[[i]]$Прогноз[forecasts_plus] > quantils_plus[j])) # Те что больше квантили
    
    tmp_m <- ((predictions.stats[[i]]$Прогноз[forecasts_minus] < quantils_minus[102-j])) 
    
    parts_plus[[i]][j] <- sum(predictions.stats[[i]]$Точность[tmp_p]) / length(predictions.stats[[i]]$Точность[tmp_p])
    parts_minus[[i]][j] <- sum(predictions.stats[[i]]$Точность[tmp_m]) / length(predictions.stats[[i]]$Точность[tmp_m])
  
  }
  
}

parts_plus[[1]]
parts_minus[[9]]
plot(x = seq(0,0.99, by = 0.01), y = parts_minus[[12]])

# !!!   ЗАВИСИМОСТЬ МИНИМАЛЬНА (и это печально) 
# что по прогнозам >0, что <0 
#--------------------------------------------------------------------------------------------------------------


View(predictions.stats[[5]])
errors <- rmse(predictions[[5]],Utility.Si.15[55*5 + 1:(55*30)])
errors

# Какая часть вообще отбивает издержки ? 
deltas <- diff(USDRUB.15$mat$close) / USDRUB.15$mat$close[-length(USDRUB.15$mat$close)]
head(deltas)
sum (abs(deltas) > 0.0005) / length(deltas)




# Далее идут попытки предсказать не лог, а обычные приращения 
# Попробую предсказать по обычным приращениям 
#------------------------------------------------------------------------------------------------

increments <- diff(USDRUB.15$mat$close)

arfima.length <- 55*5
arfima.order <- c(2,0,0)

suppressMessages({
  system.time(usual_predictions <- predictions_arfima(increments, 55 * 5 ,arfima.order, 
                                       number_of_predictions = 55*30))
})

sum(sign(usual_predictions) == sign(increments[55*5 + 1:(55*30)]))
sum(sign(predictions[[5]]) == sign(Utility.Si.15[55*5 + 1:(55*30)]))

rmse(usual_predictions, increments[55*5 + 1:(55*30)])


tmp <- is.finite((increments[55*5 + 1:(55*30)] - usual_predictions)/increments[55*5 + 1:(55*30)])


View(as.matrix((increments[55*5 + 1:(55*30)] - usual_predictions)/increments[55*5 + 1:(55*30)]))



us_forecasts_plus <- which(usual_predictions>=0)
us_forecasts_minus <- which(usual_predictions<0)

us_quantils_plus <- quantile(usual_predictions[us_forecasts_plus],
                          probs = seq(0,1,by=0.01))

us_quantils_minus <- quantile(usual_predictions[us_forecasts_minus],
                           probs = seq(0,1,by=0.01))

parts_p <- rep(0,100)
part_m <- rep(0,100)


accuracy <- (sign(usual_predictions) == sign(increments[55*5 + 1:(55*30)]))

for(j in 1:100){
  
  tmp_p <- ((usual_predictions[us_forecasts_plus] > us_quantils_plus[j])) # Те что больше квантили
  
  tmp_m <- ((usual_predictions[us_forecasts_minus] < us_quantils_minus[102-j])) 
  
  
  parts_p[j] <- sum(accuracy[tmp_p]) / length(usual_predictions[tmp_p])
  
  part_m[j] <- sum(accuracy[tmp_m]) / length(usual_predictions[tmp_m])
  
}

plot(parts_p)

tmps <- which(usual_predictions > 0.0005 * USDRUB.15$mat$close[55*5 + 1 + 1:(55*30)])

#-----------------------------------------------------------------------------------------










