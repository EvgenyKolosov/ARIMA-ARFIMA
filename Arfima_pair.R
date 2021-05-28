# Тут попытки  применить ARFIMA для торговли спредом 

library(stratbuilder3)
library(forecast)
library(arfima)
library(tseries)
XLB_ACWI <- Data() %>% 
  modify(from = '2000-01-01',
         columns = c('Ad')) %>% 
  stock(c("ACWI", 'XLB'),
        src = 'yahoo') %>%
  getSymbols

this <- Strategy() %>% 
  setParams(
    n = 50,
    nsd = 1,
    matype = SMA,
    arma_pred_lookback=500,
    betas_lookback = 500,
    betas_lookforward = 5
  ) %>% 
  addIndicator(
    name = 'logadjusted',
    expr = {
      log(data$mat$adjusted)
    }
  ) %>%
  addIndicator(
    name = 'betas',
    lookback = betas_lookback,
    lookforward = betas_lookforward,
    expr = {
      statTest <- function(series){
        suppressWarnings({
          series <- series %>% na.omit %>% as.numeric
          fUnitRoots::adfTest(series, type = "ct")@test
        })
      }
      df <- as.data.frame(logadjusted[1:ii,]) %>% set_colnames(c('x', 'y'))
      
      model1 <- lm(y ~ x, data = df)
      model2 <- lm(x ~ y, data = df)
      tests <- list(statTest(model1$residuals), statTest(model2$residuals))
      stats <- sapply(tests, '[[', 'statistic') 
      if(stats[1] < stats[2]){
        model1$coefficients[2] %>% {c(-., 1)} %>% {. / sum(abs(.))}
      }else{
        model2$coefficients[2] %>% {c(1, -.)} %>%  {. / sum(abs(.))}
      }
    }
  ) %>%
  addIndicator(
    name = 'spread',
    expr = {
      logadjusted %*% cbind(betas)
    }
  ) %>%
  addIndicator(
    vars = c('arma_prediction', 'indicator_long', 'indicator_short'),
    lookback = arma_pred_lookback,
    lookforward = 0,
    expr = {
      spread.utility <- Diff(log(abs(spread))) # Тут знак ещё проверь.  Хз почему, но с двойным логарифмлм лучше
      
      data.arfima <- arfima::arfima(spread.utility[(i-arma_pred_lookback + 1):i],order=c(2,0,0))
       t <- ((predict(data.arfima)[[1]])$Forecast)
       arma.resid = data.arfima$modes[[1]]$residuals
      
    # data.arima <- forecast::auto.arima(spread.utility[(i-arma_pred_lookback + 1):i])
    #t <- forecast(data.arima, h = 1)$mean[1]
    #  arma.resid = data.arima$residuals 
      
      
      utility.garch <- garch(arma.resid, trace = F)       # Смоделировали остатки гарчем 
      sigma_prediction <- sqrt(utility.garch$coef[1] + 
                                 utility.garch$coef[2]*arma.resid[arma_pred_lookback]^2 + 
                                 utility.garch$coef[3]*utility.garch$fitted.values[arma_pred_lookback,1]^2)
      
      if(spread[i] < 0) {
        arma_prediction <- c(-t,sigma_prediction)
      }else{
        arma_prediction <- c(t,sigma_prediction)
      }
      indicator_long <- arma_prediction[2] * qnorm(0.6) - arma_prediction[1]
      indicator_short <- arma_prediction[2] * qnorm(0.6) + arma_prediction[1]
    }
  ) %>%
  addRule(
    name = 'long',
    expr = indicator_long <= 0,
    block = 'blocklong',
    pathwise = TRUE,
    position  =   {
      # print(c(indicator_long,"LONG"))
      trunc(getMoney(this) * betas / data$mat$adjusted[i,])    
    }
  ) %>% 
  addRule(
    name = 'short',
    expr = indicator_short <= 0,
    pathwise = TRUE,
    block = 'blockshort',
    position_const  =   {
      # print(c(indicator_short,"short"))
      -trunc(getMoney(this) * betas / data$mat$adjusted[i,])
    }
  ) %>%
  addRule(
    name = 'sell',
    expr = sign(arma_prediction[1]) == -1,
    type = 'exit',
    pathwise = TRUE,
    block = 'blocklong'
  ) %>%
  addRule(
    name = 'buy',
    expr = sign(arma_prediction[1]) == 1,
    type = 'exit',
    pathwise = TRUE,
    block = 'blockshort'
  ) %>%
  addToReport(Stats$return.pos.drawdown, Stats$sharpe,Stats$ntrades) 

dataa <- xts(
  XLB_ACWI$mat$adjusted,
  XLB_ACWI$dates
)
setData(this, XLB_ACWI) 

system.time(
  perform(this, end = '2012-01-01')
)

plotPnL(this)

getReport(this, recalc = TRUE)
