# utility - лог приращения; arfima.length - размерность арфимы (3 числа) (lookback считай);
# number_of_predictions - сколько нужно предсказаний
# по прошлым arfima.length данным строит 1 новое предсказание, и так далее number_of_predictions раз 

# В ФАЙЛЕ arfima.predictions.R более новая версия этой функции 
predictions_arfima <- function(utility,arfima.length,arfima.order,number_of_predictions){
  
  predictions <- matrix(ncol = 2, nrow = number_of_predictions)
  for (i in 1:(number_of_predictions)){
    
    suppressMessages({
      modell <- arfima::arfima(
        utility[i:(arfima.length+i-1)],
        order = arfima.order
      )
    })

    predictions[i,1] <- (predict(modell)[[1]])$Forecast 
    arma.resid = modell$modes[[1]]$residuals
    
    utility.garch <- garch(arma.resid, trace = F) # Смоделировали остатки гарчем 
    sigma_prediction <- sqrt(utility.garch$coef[1] + 
                               utility.garch$coef[2]*arma.resid[length(arma.resid)]^2 + 
                               utility.garch$coef[3]*utility.garch$fitted.values[length(arma.resid),1]^2)
    
    predictions[i,2] <- sigma_prediction
  }
  return(predictions) # возвращает вектор, где:
}                     #prediction[i] ~ utility(arfima.length + i) = ln(x[t+1] / x[t])

#---------------------------------------------------------------------------------
# Евробакс 

EUR_USD.5 <- Data() %>%
  modify(period = 5,
         from = Sys.Date() - 360) %>%
  stock(c('EURUSD'),
        src = 'Finam') %>%
  getSymbols

Utility.EUR_USD.5 <- diff(EUR_USD.5$mat$close)

EUR_USD.15 <- Data() %>%
  modify(period = 15,
         from = Sys.Date() - 360) %>%
  stock(c('EURUSD'),
        src = 'Finam') %>%
  getSymbols

Utility.EUR_USD.15 <- diff(EUR_USD.15$mat$close)

EUR_USD.30 <- Data() %>%
  modify(period = 30,
         from = Sys.Date() - 600) %>%
  stock(c('EURUSD'),
        src = 'Finam') %>%
  getSymbols

Utility.EUR_USD.30 <- diff(EUR_USD.30$mat$close)

EUR_USD.60 <- Data() %>%
  modify(period = 60,
         from = Sys.Date() - 360*3) %>%
  stock(c('EURUSD'),
        src = 'Finam') %>%
  getSymbols

Utility.EUR_USD.60 <- diff(EUR_USD.60$mat$close)

EUR_USD.240 <- Data() %>%
  modify(period = 240,
         from = Sys.Date() - 1200) %>%
  stock(c('EURUSD'),
        src = 'Finam') %>%
  getSymbols
Utility.EUR_USD.240 <- diff(EUR_USD.240$mat$close)


# предсказываю кучу всего 
afrma_pred.eur_usd.5 <- list()
afrma_pred.eur_usd.15 <- list()
afrma_pred.eur_usd.30 <- list()
afrma_pred.eur_usd.60 <- list()
afrma_pred.eur_usd.240 <- list()

# перебор различных периодов для ARFIMA 
for(i in 1:6){
  print(i)
  afrma_pred.eur_usd.5[[i]] <- predictions_arfima(Utility.EUR_USD.5,165 + i*55, c(0,0,0), 55*40)
  afrma_pred.eur_usd.15[[i]]<- predictions_arfima(Utility.EUR_USD.15,110 + i*55, c(0,0,0), 55*40)
  afrma_pred.eur_usd.30[[i]]<- predictions_arfima(Utility.EUR_USD.30,55 + i*55, c(0,0,0), 55*40)
  afrma_pred.eur_usd.60[[i]]<- predictions_arfima(Utility.EUR_USD.60, i*55, c(0,0,0), 55*40)
  afrma_pred.eur_usd.240[[i]]<- predictions_arfima(Utility.EUR_USD.240, i*55, c(0,0,0), 55*40)
}

#       Теперь есть море прогнозов
#       afrma_pred.eur_usd.60[[i]][j,1] ~ Utility.EUR_USD.60[55*i+1 : 55*i + j]    !!!!! 


#------------------------------------------------------------------------------------------
# Бэкстестики

this <- Strategy() %>% 
  setParams(
    n = 1,
    matype = EMA,
    lvl = 0.65,
    take_pr = 0.001,
    st_l = 0.003
  ) %>% 
  addIndicator(
    name = 'predicted_value',
    expr = {
      afrma_pred.eur_usd_long.30[[n]][,1]   
    }
  ) %>%
  addRule(
    name = 'long',
    expr = ({
      sign(predicted_value[i])==1
    }
    ),
    block = 'blocklong',
    pathwise = TRUE,
    position  =   {
      trunc(getMoney(this) / data$mat$adjusted[i])    
    }
  ) %>% 
  addRule(
    name = 'short',
    expr = ({
      sign(predicted_value[i])==-1
    }
        ),
    pathwise = TRUE,
    block = 'blockshort',
    position_const  =   {
      -trunc(getMoney(this)  / data$mat$adjusted[i])
    }
  ) %>%
  addRule(
    name = 'take',
    expr = ({ 
      
      sum(unrealized_money_last) > getMoney(this) * (take_pr )
      }), #take_pr
    type = 'exit',
    pathwise = TRUE,
    block = 'all'
  ) %>%
  addRule(
    name = 'stop',
    expr = ({
        sum(unrealized_money_last) < getMoney(this) * (-st_l) 
    }),
    type = 'exit',
    pathwise = TRUE,
    block = 'all'
  ) %>%
  addToReport(Stats$return.pos.drawdown, Stats$sharpe, Stats$trades.year, Stats$ntrades, Stats$return.ann)

  
# в test_30_c[[i]] будут лежать тесты данного ТФ построенного на арфимах разных длин 
# пишу из будущего - уже это можно делать куда лучше ;) 
test_30_c <- list()
test_5_c <- list()
test_15_c <- list()


for(NN in 1:6){
  print(system.time({
  test_15_c[[NN]] <- data.frame()
  nums <- data.frame()
  
  x <- EUR_USD.30$mat$close[(55 + 55*NN + 1):( 55 + 55*NN + 55 * 350)]
  dataa <- data_from_xts(xts(x, EUR_USD.30$dates[(55 + 55*NN + 1):(55 + 55*NN + 55 * 350)]))
  
  for(takke in seq(0.001, 0.015, by = 0.002)){
    for(stopl in seq(0.001, 0.015, by = 0.002)){

      setParams(this,
                n = NN,
                matype = EMA,
                lvl = 0.65,
                take_pr = takke,
                st_l = stopl)

   setData(this, dataa)
   perform(this)
   print(getReport(this))
   
   nums <- rbind(nums, c(takke, stopl))
   test_15_c[[NN]] <- rbind(test_15_c[[NN]], getReport(this))
  }
  }
  test_15_c[[NN]]<- cbind(nums, test_15_c[[NN]])
  }))
} 

plotPnL(this)


#--------------------------------------------------------------------------------------------------
#      Получаю прогнозы 
#--------------------------------------------------------------------------------------------------
afrma_pred.eur_usd_long.240 <- list()
afrma_pred.eur_usd_long.60 <- list()
afrma_pred.eur_usd_long.30 <- list()
afrma_pred.eur_usd_long.15 <- list()
afrma_pred.eur_usd_long.5 <- list()

{
  
  for(i in 1:6){
    print(i)
    afrma_pred.eur_usd_long.240[[i]]<- predictions_arfima(Utility.EUR_USD.240,i*25, c(0,0,0), (5288 - 6*25))
  }
  
  for(i in 1:6){
    print(i)
    afrma_pred.eur_usd_long.60[[i]]<- predictions_arfima(Utility.EUR_USD.60,i*30, c(0,0,0), 300*55)
  }
  
  
  for(i in 1:6){
  print(i)
  afrma_pred.eur_usd_long.30[[i]]<- predictions_arfima(Utility.EUR_USD.30,55 + i*55, c(0,0,0), 350*55)
}

for(i in 1:6){
  print(i)
  if(i == 3) next
  afrma_pred.eur_usd_long.15[[i]]<- predictions_arfima(Utility.EUR_USD.15,110 + i*55, c(0,0,0), 55*400)
}

for(i in 1:7){
  print(i)
  if(i == 3) next
 print(system.time(
   afrma_pred.eur_usd_long.5[[i]]<- predictions_arfima(Utility.EUR_USD.5,110 + i*55, c(0,0,0), 55*750)
 ))
}
}

print(system.time(
  afrma_pred.eur_usd_long.5[[3]]<- predictions_arfima(Utility.EUR_USD.5,110 + 3*55, c(0,0,0), 55*750)
))

print(system.time(
  afrma_pred.eur_usd_long.15[[3]]<- predictions_arfima(Utility.EUR_USD.15,110 + 3*55, c(0,0,0), 55*400)
))
#--------------------------------------------------------------------------------------------------







