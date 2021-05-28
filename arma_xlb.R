# Тут попытки  применить ARMA для торговли спредом 
# Arima_Prediction_Creator -  лежит в "Garch + Arima.R"

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
    arma_pred_lookback=100,
  ) %>% 
  addIndicator(
    name = 'logadjusted',
    expr = {
      log(data$mat$adjusted)
    }
  ) %>%
  addIndicator(
    name = 'betas',
    lookback = 500,
    lookforward = 10,
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
    name = 'arma_prediction',
    lookback = arma_pred_lookback,
    lookforward = 0,
    expr = {
      
      spread.utility <- Diff(log(abs(spread))) # Тут знак ещё проверь
      
      data.arima <- forecast::auto.arima(spread.utility[(i-arma_pred_lookback+1):(i)])
      
      t <- forecast(data.arima, h = 1)$mean[1]
      
      arma.resid = data.arima$residuals 
      utility.garch <- garch(arma.resid, trace = F)       # Смоделировали остатки гарчем 
      sigma_prediction <- sqrt(utility.garch$coef[1] + utility.garch$coef[2]*arma.resid[arma_pred_lookback]^2 + utility.garch$coef[3]*utility.garch$fitted.values[arma_pred_lookback,1]^2)
      
      if(spread[i] < 0) {
        c(-t,sigma_prediction)
      }
      else c(t,sigma_prediction)
    }
  ) %>%
  addIndicator(
    name = 'indicator_long',
    lookback = 100,
    lookforward = 0,
    expr = {
      arma_prediction[2] * qnorm(0.6) - arma_prediction[1]
    }
    
  ) %>%
  addIndicator(
    name = 'indicator_short',
    lookback = 100,
    lookforward = 0,
    expr = {
      arma_prediction[2] * qnorm(0.6) + arma_prediction[1]
    }
    
  ) %>%
  addRule(
    name = 'long',
    expr = {
      indicator_long <= 0
    },
    block = 'blocklong',
    pathwise = TRUE,
    position  =   {
      print(c(indicator_long,"LONG"))
      trunc(getMoney(this) * betas / data$mat$adjusted[i,])    
    }
  ) %>% 
  addRule(
    name = 'short',
    expr = (indicator_short <= 0) ,
    pathwise = TRUE,
    block = 'blockshort',
    position_const  =   {
      print(c(indicator_short,"short"))
      
      -trunc(getMoney(this) * betas / data$mat$adjusted[i,])
    }
  ) %>%
  addRule(
    name = 'sell',
    expr = {
      (sign(arma_prediction[1]) == -1)
      
    },
    type = 'exit',
    pathwise = TRUE,
    block = 'blocklong'
  ) %>%
  addRule(
    name = 'buy',
    expr = (sign(arma_prediction[1]) == 1),
    type = 'exit',
    pathwise = TRUE,
    block = 'blockshort'
  ) %>%
  addToReport(Stats$return.pos.drawdown, Stats$sharpe) #%>%
#setCommission(data$mat$adjusted[i,] * abs(pos_change) * 0.0005)

x <- XLB_ACWI$mat$adjusted[1:600,]
dataa <- data_from_xts(xts(x, XLB_ACWI$dates[1:600]))


setData(this, dataa)

system.time(
  perform(this)
)
plotPnL(this)

getReport(this, recalc = TRUE)