# Arima_Prediction_Creator -  лежит в "Garch + Arima.R"
# В этом файле небольшой бектестер и попытки попредсказывать рубль 

GSPC <- Data() %>% 
  modify(from = '2000-01-01',
         columns = c('Cl')) %>% 
  stock("^GSPC",
        src = 'yahoo') %>%
  getSymbols


GSPC.utility <- diff(log(GSPC$mat$close))

arima.dim <- 100
number_of_predictions <- 2000

GSPC.utility.prediction <- Arima_Prediction_Creator(GSPC.utility, number_of_predictions, arima.dim, 0.8)

# Количество значений, попавших в диапазон
k <- 0
for(i in 1:number_of_predictions){
  if(GSPC.utility.prediction[i,1] <= GSPC.utility[arima.dim + i] & GSPC.utility.prediction[i,3] >= GSPC.utility[arima.dim + i] ){
    #print(c(utility.prediction[i,1], utility[arima.dim + i], utility.prediction[i,3] ))
    k <- k+1
  }
}

# Количество совпадений знаков 
signs <- 0
for(i in 1:number_of_predictions){
  if(sign(GSPC.utility.prediction[i,2]) == sign(GSPC.utility[arima.dim + i])){
    signs <- signs + 1
  }
}


# Для данной аримы и гарча строит статистики торговли для разных порогов
Backtester <- function(data, utility.prediction, arima.dim, level){
  
  sigma_tp1 <- (utility.prediction[,3] - utility.prediction[,2]) / qnorm((level+1)/2)
  
  length <- 20
  
  profit <- vector(mode = "numeric", length = length)
  adjusted_profit <- vector(mode = "numeric", length = length)
  number_of_trades <- vector(mode = "numeric", length = length)
  number_of_longs <- vector(mode = "numeric", length = length)
  successful_trades <- vector(mode = "numeric", length = length)
  mean_successful_trade <- vector(mode = "numeric", length = length)
  mean_bad_trade <- vector(mode = "numeric", length = length)
  Sharp <- vector(mode = "numeric", length = length)
  
  

  i<-1
  for(limit in seq(0.5, 0.9, length.out = length)){
    
    indicrator_long <- sigma_tp1 * qnorm(limit) - utility.prediction[,2]
    indicator_short <- sigma_tp1 * qnorm(limit) + utility.prediction[,2]
    
    trading_times <- sort(c(which((indicrator_long <= 0)&(utility.prediction[,2]>=0)), which((indicator_short<=0)&(utility.prediction[,2]<=0))))
    
    position <- sign(utility.prediction[trading_times,2])
    
    results <- position * (data[arima.dim + trading_times + 1] - data[arima.dim + trading_times])
    
    Profits <- results / (data[arima.dim + trading_times])
    
    Sharp[i] <- mean(Profits) / sqrt(var(Profits)) # "в ковычках, конечно"
    
    comiss <- 0.0005 * abs(data[arima.dim + trading_times])
    
    profit[i] <- sum(results) / data[trading_times-1]
    adjusted_profit[i] <- profit[i] - sum(comiss)
    number_of_trades[i] <- length(trading_times)
    number_of_longs[i] <- sum(indicrator_long <= 0)
    successful_trades[i] <- sum(results >= 0)
    mean_successful_trade[i] <- mean(results[results >= 0])
    mean_bad_trade[i] <- mean(results[results < 0])
    
    i <- i+1
  }
  
  number_of_shorts <- number_of_trades - number_of_longs
  
  part_of_successful_trades <- successful_trades / number_of_trades
  
  result <- data.frame(
    "Порог" = seq(0.5, 0.9, length.out = length), 
    Profit = profit,
    #adjusted_profit,
    number_of_trades,
    number_of_longs,
    number_of_shorts,
    part_of_successful_trades,
    mean_successful_trade,
    mean_bad_trade,
    Sharp
    )
  
  return(result)

}

data <- Backtester(GSPC$mat$close, GSPC.utility.prediction, arima.dim, level)

View(data)


#-------------------------------------------
# Проверка на рубле

USDRUB <- Data() %>%
  modify(period = 15,
         from = Sys.Date() - 30) %>%
  stock(c('SPFB.Si'),
        src = 'Finam') %>%
  getSymbols

USD.utility = diff(log(USDRUB$mat$close))
USD.utility <- USD.utility[!is.na(USD.utility)]

system.time(
  USDRUB.utility.prediction <- Arima_Prediction_Creator(GSPC.utility, 1000, 100, 0.8)
)

USDRUBdata <- Backtester(USDRUB$mat$close, USDRUB.utility.prediction, arima.dim, level)

View(USDRUBdata)

Backtest <- list() # Сохрани егооо

for (i in 1:5){
  arima.dim <- 25 + i*25
  USDRUB.utility.prediction <- Arima_Prediction_Creator(GSPC.utility, 1000, arima.dim, 0.8)
  Backtest[[i]] <- Backtester(USDRUB$mat$close, USDRUB.utility.prediction, arima.dim, level)
  
}
# Неплохие: 100,

# Сравнение Buy & Hold и предсказаний

usd_position <- sign(USDRUB.utility.prediction[1:1000,2])
usd_results <- usd_position * (USDRUB$mat$close[100 + 1:1000 + 1] - USDRUB$mat$close[100 + 1:1000])
sum(usd_results)
plot(c(0,cumsum(usd_results)) + USDRUB$mat$close[101], type = 'l')
lines(USDRUB$mat$close[100 + 1:1001], col = 'red')

#

# Протестим 5, 15, 30 - минутки

USDRUB.30 <- Data() %>%
  modify(period = 30,
         from = Sys.Date() - 60) %>%
  stock(c('SPFB.Si'),
        src = 'Finam') %>%
  getSymbols

USD.utility.30 = diff(log(USDRUB.30$mat$close))
USD.utility.30 <- USD.utility.30[!is.na(USD.utility.30)]

USDRUB.15 <- Data() %>%
  modify(period = 15,
         from = Sys.Date() - 180) %>%
  stock(c('SPFB.Si'),
        src = 'Finam') %>%
  getSymbols

USD.utility.15 = diff(log(USDRUB.15$mat$close))
USD.utility.15 <- USD.utility.15[!is.na(USD.utility.15)]

USDRUB.5 <- Data() %>%
  modify(period = 5,
         from = Sys.Date() - 60,
         to = Sys.Date() - 30 ) %>% 
  stock(c('SPFB.Si'),
        src = 'Finam') %>%
  getSymbols

USD.utility.5 = diff(log(USDRUB.5$mat$close))
USD.utility.5 <- USD.utility.5[!is.na(USD.utility.5)]


Backtest5 <- list() 
Backtest15 <- list()
Backtest30 <- list() 

for (i in 1:7){
  
  arima.dim <- 20 + i*20
  
  USDRUB.utility.prediction.5 <- Arima_Prediction_Creator(USD.utility.5, 1000, arima.dim, 0.8)
  Backtest5[[i]] <- Backtester(USDRUB.5$mat$close, USDRUB.utility.prediction.5, arima.dim, level)
  
  USDRUB.utility.prediction.15 <- Arima_Prediction_Creator(USD.utility.15, 1000, arima.dim, 0.8)
  Backtest15[[i]] <- Backtester(USDRUB.15$mat$close, USDRUB.utility.prediction.15, arima.dim, level)
  
  USDRUB.utility.prediction.30 <- Arima_Prediction_Creator(USD.utility.30, 1000, arima.dim, 0.8)
  Backtest30[[i]] <- Backtester(USDRUB.30$mat$close, USDRUB.utility.prediction.30, arima.dim, level)
  
  print(i)
  
}


for (i in 1:5){

  arima.dim <- 10 + i*10

  USDRUB.utility.prediction.5 <- Arima_Prediction_Creator(USD.utility.5, 1000, arima.dim, 0.8)
  Backtest5[[i]] <- Backtester(USDRUB.5$mat$close, USDRUB.utility.prediction.5, arima.dim, level)
  print(i)

}

USDRUB.1 <- Data() %>%
  modify(period = 1,
         from = Sys.Date() - 60,
         to = Sys.Date() - 30 ) %>% 
  stock(c('SPFB.Si'),
        src = 'Finam') %>%
  getSymbols

USD.utility.1 = diff(log(USDRUB.1$mat$close))
USD.utility.1 <- USD.utility.1[!is.na(USD.utility.1)]

Backtest1 <- list() 

for (i in 1:7){
  
  arima.dim <- 25 + i*25
  
  USDRUB.utility.prediction.1 <- Arima_Prediction_Creator(USD.utility.1, 1000, arima.dim, 0.8)
  Backtest1[[i]] <- Backtester(USDRUB.1$mat$close, USDRUB.utility.prediction.1, arima.dim, level)
  print(i)
  
}

#--------------------------------------------------------------------------------------

GSPC.utility <- diff(log(GSPC$mat$close))

arima.dim <- 500
number_of_predictions <- 2000

system.time(
    GSPC.utility.prediction <- Arima_Prediction_Creator(GSPC.utility, number_of_predictions, arima.dim, 0.8)
)
data <- Backtester(GSPC$mat$close, GSPC.utility.prediction, arima.dim, level)

View(data) # Порог 0.85 весьма перспективный 

for(i in 1:7){
  if(i<=5) write.csv(Backtest5[[i]], file = (paste0("Backtests/Arima/USDRUB.5/USDRUB.5.23.03-",10+i*10,".csv")))
  write.csv(Backtest15[[i]], file = (paste0("Backtests/Arima/USDRUB.15/USDRUB.15.23.03-",20+i*20,".csv")))
  write.csv(Backtest30[[i]], file = (paste0("Backtests/Arima/USDRUB.30/USDRUB.30.23.03-",20+i*20,".csv")))
}



