#Appendix: R-Code for analysis

#Data examination
q1.raw = as.ts(read.csv(file = "q1train.csv")[2])
q2.raw = as.ts(read.csv(file = "q2train.csv")[2])
q3.raw = as.ts(read.csv(file = "q3train.csv")[2])
q4.raw = as.ts(read.csv(file = "q4train.csv")[2])
q5.raw = as.ts(read.csv(file = "q5train.csv")[2])

plot(q1.raw, type = "l") #quadratic or start from t = 300
plot(q2.raw, type = "l")
plot(q3.raw, type = "l") #possible cubic trend, outliers to remove
plot(q4.raw, type = "l")
plot(q5.raw, type = "l") #quadratic or linear trend

q1 = as.ts(q1.raw[300:536])
q2 = q2.raw
q3 = q3.raw
q4 = q4.raw
q5 = q5.raw

#---------------------------------Q1--------------------------------#
#Data exploration:
plot(q1)
acf(q1, lag = 100)
pacf(q1, lag = 100)
plot(diff(q1))
acf(diff(q1), lag = 100)
plot(diff(q1, lag = 52))
acf(diff(q1, lag = 52), lag = 100)
pacf(diff(q1, lag = 52), lag = 100)

#first, applying a linear model
t = 1:237
q1.lin = lm(q1~1+t)
q1.lin.resid = residuals(q1.lin)

#examining linear model
plot(q1.lin.resid, type = "l")
acf(q1.lin.resid)
pacf(q1.lin.resid, lag = 100)
acf(diff(q1.lin.resid, lag = 52), lag = 100)
pacf(diff(q1.lin.resid, lag = 52), lag = 100)

#looks like AR(3) process after seasonal differencing. 
mod.1 = arima(q1.lin.resid, order = c(1, 0, 0),
              seasonal = list(order = c(0, 1, 0), period = 52))
mod.2 = arima(q1.lin.resid, order = c(2, 0, 0),
              seasonal = list(order = c(0, 1, 0), period = 52))
mod.3 = arima(q1.lin.resid, order = c(3, 0, 0),
              seasonal = list(order = c(0, 1, 0), period = 52))
mod.4 = arima(q1.lin.resid, order = c(4, 0, 0),
              seasonal = list(order = c(0, 1, 0), period = 52))

aic = c("Model 1" = mod.1$aic, "Model 2" = mod.2$aic,
        "Model 3" = mod.3$aic, "Model 4" = mod.4$aic)
bic = c("Model 1" = BIC(mod.1), "Model 2" = BIC(mod.2),
        "Model 3" = BIC(mod.3), "Model 4" = BIC(mod.4))
aic
bic

#aic's and bic's of above models provide evidence that AR(3) is best. 
#applying sarima(3, 0, 0)(0, 1, 0)
q1.arima.model = arima(q1.lin.resid,
                       order = c(3, 0, 0),
                       seasonal = list(order = c(0, 1, 0),
                                       period = 52))
tsdiag(q1.arima.model) #Ljung-box p-values are promising

#obtaining arima predictions
q1.arima.predict = predict(q1.arima.model, n.ahead = 104)$pred
plot(q1.arima.predict) #looks beautiful
plot(q1)

#obtaining final predictions by adding to linear model predictions
q1.predict = predict(q1.lin, newdata = data.frame(t = 238:341))
            + q1.arima.predict
plot(q1.predict, type = "l")

plot(c(q1.raw, q1.predict), type = "l")

write.table(q1.predict, 
            sep = ",",
            col.names = FALSE,
            row.names = FALSE,
            file = "Q1_Paul_Kim_24699265.txt")
#-------------------------------------------------------------------#
#-------------------------------------------------------------------#


#---------------------------------Q2--------------------------------#
#Data Exploration
plot(q2) #no linear trend
plot(diff(q2))
plot(diff(q2, lag = 52))
plot(diff(diff(q2, lag = 52)))
plot(diff(diff(q2, lag = 52)))
acf(q2, lag.max = 100)
acf(diff(q2), lag.max = 100) #MA(3) with seasonal differencing?
pacf(diff(q2), lag.max = 100)

acf(diff(diff(q2, lag = 52)), lag.max = 100)
acf(diff(diff(diff(q2, lag = 52))), lag.max = 100) 
#looks like it could be (0, 1, 1)X(0, 1, 1)

#Arima (0,1,1)(0,1,1)?
arima(q2, order = c(0,0,1), 
      seasonal = list(order = c(0, 1, 1), period = 52))
arima(q2, order = c(0,1,1), 
      seasonal = list(order = c(0, 1, 0), period = 52)) #aic 2595
arima(q2, order = c(0,1,2), 
      seasonal = list(order = c(0, 1, 0), period = 52)) #ma2 better
arima(q2, order = c(0,1,2), 
      seasonal = list(order = c(0, 1, 1), period = 52)) #seasonalma?
arima(q2, order = c(0,0,1), 
      seasonal = list(order = c(0, 1, 0), period = 52)) #not as good
arima(q2, order = c(0,1,3), 
      seasonal = list(order = c(0, 1, 0), 
                      period = 52)) #trying overfitting

#looks like non-seasonal part is Ma(2). 
#trying out various seasonal models.
arima(q2, order = c(0,1,2),
      seasonal = list(order = c(0, 1, 0), period = 52)) #aic 2590
arima(q2, order = c(0,1,2),
      seasonal = list(order = c(0, 1, 1), period = 52)) #aic 2549!
arima(q2, order = c(0,1,2),
      seasonal = list(order = c(0, 1, 2), period = 52)) #aic 2535!
arima(q2, order = c(0,1,2),
      seasonal = list(order = c(0, 1, 3), period = 52)) #aic 2529
arima(q2, order = c(0,1,2),
      seasonal = list(order = c(0, 1, 4), period = 52)) #aic 2530

#I'm worried that I'm overfitting now. 
#Gonna look at BICs and ljung-box

BIC(arima(q2, order = c(0,1,2),
          seasonal = list(order = c(0, 1, 0), period = 52))) #2603
BIC(arima(q2, order = c(0,1,2),
          seasonal = list(order = c(0, 1, 1), period = 52))) #2566
BIC(arima(q2, order = c(0,1,2),
          seasonal = list(order = c(0, 1, 2), period = 52))) #2555
BIC(arima(q2, order = c(0,1,2),
          seasonal = list(order = c(0, 1, 3), period = 52))) #2554

tsdiag(arima(q2, order = c(0,1,2),
             seasonal = list(order = c(0, 1, 2), period = 52)))
tsdiag(arima(q2, order = c(0,1,2),
             seasonal = list(order = c(0, 1, 3), period = 52)))
tsdiag(arima(q2, order = c(0,1,2),
             seasonal = list(order = c(0, 1, 1), period = 52)))

#AIC, BIC, and tsdiag point to (0,1,2) X (0,1,3)

#Trying cross-validation
computeCVmse <- function(order.totry, seasorder.totry, data){
  MSE <- numeric()
  len = length(data)
  for(k in 3:1){
    train.dt <-data[1:(len - 52 * k)]
    test.dt <- data[(len - 52 * k + 1):(len - 52 * (k - 1))]
    mod <- arima(train.dt, order = order.totry, seasonal = 
                   list(order = seasorder.totry, period = 52))
    fcast <- predict(mod, n.ahead = 52)
    MSE[k] <- mean((fcast$pred - test.dt)^2)
  }
  return(MSE)
}

mse1 = computeCVmse(c(0, 1, 2), c(0, 1, 3), data = q2)
mse2 = computeCVmse(c(0, 1, 2), c(0, 1, 2), data = q2)
mse3 = computeCVmse(c(0, 1, 2), c(0, 1, 1), data = q2)

#ok so according to CVmse, the smallest model is actually best. 
#I'm going to use the smallest model, since the more complex
#model is probably overfitting the data. 

q2.arima.model.2 = arima(q2,
                         order = c(0, 1, 2), 
                         seasonal = list(order = c(0, 1, 1), 
                                         period = 52))
q2.arima.predict.2 = predict(q2.arima.model.2, 
                             n.ahead = 104)$pred
plot(q2.arima.predict.2, type = 'l')
plot(c(q2.raw, q2.arima.predict.2), type = 'l') #looks good

q2.predict = q2.arima.predict.2

write.table(q2.predict, 
            sep = ",",
            col.names = FALSE,
            row.names = FALSE,
            file = "Q2_Paul_Kim_24699265.txt")
#-------------------------------------------------------------------#
#-------------------------------------------------------------------#



#---------------------------------Q3--------------------------------#
#Data exploration
plot(q3)
acf(q3)
pacf(q3)

#some outliers I'd like to deal with later

#Differencing the data to attain stationarity
q3.d = diff(q3)
plot(q3)
q3.sd = diff(q3, lag = 52)
plot(q3.sd)
acf(q3.sd, lag.max = 60)
pacf(q3.sd, lag.max = 60)
q3.d.sd = diff(q3.sd)
plot(q3.d.sd) #now it looks pretty stationary
acf(q3.d.sd, lag.max = 60) #looks like MA(3)X(Seasonal something)
pacf(q3.d.sd, lag.max = 60) #not helpful

#trying out some models

q3.arima1 = arima(q3, 
                  order = c(0, 1, 3),
                  seasonal = list(order = c(0, 1, 0),
                                  period = 52)) 
#aic = 2478
tsdiag(q3.arima1)
#looks like a strong candidate

#lets try seasonal MA
q3.arima2 = arima(q3, 
                  order = c(0, 1, 3), 
                  seasonal = list(order = c(0, 1, 1),
                                  period = 52))
#aic = 2365
tsdiag(q3.arima2)
#a stronger candidate

#testing overfitting
q3.arima3 = arima(q3, 
                  order = c(0, 1, 3), 
                  seasonal = list(order = c(0, 1, 2),
                                  period = 52))
#aic = 2364
tsdiag(q3.arima3)
#basically identical to q3.arima2

q3.arima4 = arima(q3, 
                  order = c(0, 1, 3),
                  seasonal = list(order = c(0, 1, 3), 
                                  period = 52))
#aic = 2362
tsdiag(q3.arima4)
#really similar again. I think BIC's will suggest that 
#arima2 is the best. 

BIC(q3.arima1)
BIC(q3.arima2)
BIC(q3.arima3)
BIC(q3.arima4)

#confirmed, q3.arima2 appears strongest. 

q3.arima2.predict = predict(q3.arima2, n.ahead = 104)$pred
plot(q3.arima2.predict)
plot(c(q3, q3.arima2.predict), type = 'l')
#looks like it could be better, 
#I'd love to get rid of that outlier at 497 somehow.

#Outlier removal
#peak is index 496 and 497
#q3[497] = 71
q3.x = 1:length(q3)

color.vector = character(536)
for (i in 1:length(color.vector)){
  if (i == 497 || i == 445 || i == 393 || i == 341 ||
      i == 289 || i == 246 || i == 194 || i == 142)
    col.vector[i] = "red"
  
  else
    col.vector[i] = "black"
  
}

plot(q3, x = q3.x,  col = color.vector, type = 'o')
#looks like the corresponding peaks at previous years are far lower
#I'm going to use my arima model to predict the values for
#q3[496] and q3[497], then take the mean of that and the actual 
#q3[496] and q3[497] to reduce the impact of the outliers. 

q3.arima.outlier.removal = arima(q3[1:495], order = c(0, 1, 3), 
                                 seasonal = list(order = c(0, 1, 1), 
                                                 period = 52))
outlier.pred = predict(q3.arima.outlier.removal, n.ahead = 2)$pred
q3[496] = mean(q3[496], outlier.pred[1])
q3[497] = mean(q3[497], outlier.pred[2])

#now running arima(0,1,3)x(0,1,1) on the transformed data, 
#and obtaining predictions
plot(q3)
q3.arima2.2 = arima(q3, order = c(0, 1, 3), seasonal = list(order = c(0, 1, 1), period = 52))
q3.arima2.2.predict = predict(q3.arima2.2, n.ahead = 104)$pred
plot(c(q3, q3.arima2.2.predict), type = 'l')

q3.predict = q3.arima2.2.predict

write.table(q3.predict, 
            sep = ",",
            col.names = FALSE,
            row.names = FALSE,
            file = "Q3_Paul_Kim_24699265.txt")
#-------------------------------------------------------------------#
#-------------------------------------------------------------------#



#---------------------------------Q4--------------------------------#
#Data Exploration
plot(q4.raw)
q4 = q4.raw #gonna use the whole dataset

acf(q4, lag.max = 60) #suggests some kind of AR
pacf(q4, lag.max = 60)

#trying to difference
q4.d = diff(q4)
plot(q4.d) #not yet stationary
acf(q4.d, lag.max = 60)#correlations at 1-4 and 51, 52
pacf(q4.d, lag.max = 60)

#seasonal differencing
q4.sd = diff(q4, lag = 52)
plot(q4.sd) #looks more stationary, but some trend
#trend might not have a big impact at the end though, since
#if we use a differenced model, the our predictions will be based on
#more recent periods, which don't have a trend
acf(q4.sd, lag.max = 60)#Looks like AR
pacf(q4.sd, lag.max = 60)#Indeed, at least AR(4), maybe one seasonal
#AR after seasonal differencing

q4.d.sd = diff(q4.sd)
plot(q4.d.sd) #no obvious trend now
acf(q4.d.sd, lag.max = 60) 
pacf(q4.d.sd, lag.max = 60)

#trying some arima models with AR(3+), seasonal (1,1,0)
q4.model1 = arima(x = q4, 
                  order = c(3, 0, 0),
                  seasonal = list(order = c(1, 1, 0),
                                  period = 52))
q4.model2 = arima(x = q4, 
                  order = c(3, 1, 0),
                  seasonal = list(order = c(1, 1, 0),
                                  period = 52))
q4.model3 = arima(x = q4, 
                  order = c(4, 0, 0),
                  seasonal = list(order = c(1, 1, 0),
                                  period = 52))
q4.model4 = arima(x = q4, 
                  order = c(4, 1, 0),
                  seasonal = list(order = c(1, 1, 0),
                                  period = 52))
q4.model5 = arima(x = q4, 
                  order = c(5, 0, 0),
                  seasonal = list(order = c(1, 1, 0),
                                  period = 52))
q4.model6 = arima(x = q4, 
                  order = c(5, 1, 0),
                  seasonal = list(order = c(1, 1, 0),
                                  period = 52))
library(stats)
AIC(q4.model1, q4.model2, q4.model3, q4.model4, q4.model5, q4.model6)
BIC(q4.model1, q4.model2, q4.model3, q4.model4, q4.model5, q4.model6)
#models 2, 3, and 6 seem best.

tsdiag(q4.model2) #decent p-vals
tsdiag(q4.model3) #better
tsdiag(q4.model6) #also good

#maybe higher order AR will do better. I think I will use differencing
#seasonal differencing, as this combination produced stationary data.
#also will try adding some MA terms, since acf of double differenced
#data had a peak at lag 1
q4.model7 = arima(x = q4, 
                  order = c(6, 1, 0),
                  seasonal = list(order = c(1, 1, 0),
                                  period = 52))
q4.model8 = arima(x = q4, 
                  order = c(7, 1, 0),
                  seasonal = list(order = c(1, 1, 0),
                                  period = 52))
q4.model9 = arima(x = q4, 
                  order = c(6, 1, 1),
                  seasonal = list(order = c(1, 1, 0),
                                  period = 52))
#produces a non-fininte finite difference error,
#estimates fail to converge. 

q4.model10 = arima(x = q4, 
                  order = c(6, 1, 2),
                  seasonal = list(order = c(1, 1, 0),
                                  period = 52))
#same as model 9

q4.model11 = arima(x = q4, 
                   order = c(7, 1, 1),
                   seasonal = list(order = c(1, 1, 0),
                                   period = 52))
q4.model12 = arima(x = q4, 
                   order = c(7, 1, 2),
                   seasonal = list(order = c(1, 1, 0),
                                   period = 52))
q4.model13 = arima(x = q4, 
                   order = c(6, 1, 3),
                   seasonal = list(order = c(1, 1, 0),
                                   period = 52))

AIC(q4.model7, q4.model8, q4.model11, q4.model12, q4.model13)
BIC(q4.model7, q4.model8, q4.model11, q4.model12, q4.model13)
#wow model 13 performs very well

tsdiag(q4.model7)
tsdiag(q4.model8)
tsdiag(q4.model11)
tsdiag(q4.model12)
tsdiag(q4.model13)
#and has good p-values. 
#I would normally be more reluctant to use such a complicated model
#but the AIC and BIC are good, and the p-values as well. 
tsdiag(q4.model11, gof.lag = 30)
tsdiag(q4.model12, gof.lag = 30)
tsdiag(q4.model13, gof.lag = 30)
#p-values are strong for both 12 and 13. 
#on the basis of aic and bic, I will use model 13. 

q4.predict = predict(q4.model13, n.ahead = 104)$pred
plot (q4.predict)
plot(c(q4, q4.predict), type = "o")
t = 1:640
plot(c(q4, q4.predict), type = "o", x = t, col = ifelse(t > 537, "blue", "black"))
#looks like it works.
#going with this!
write.table(q4.predict, 
            sep = ",",
            col.names = FALSE,
            row.names = FALSE,
            file = "Q4_Paul_Kim_24699265.txt")
#-------------------------------------------------------------------#
#-------------------------------------------------------------------#



#---------------------------------Q5--------------------------------#
#Data Exploration
plot(q5)

#applying quadratic fit
q5.t = 1:length(q5)
q5.t.sqr = q5.t^2
q5.quad.model = lm(q5~1+q5.t+q5.t.sqr)
points(q5.quad.model$fitted.values, type = 'l') #looks good

#now to fit some arima model on the residuals
q5.quad.resid = q5.quad.model$residuals
plot(q5.quad.resid, type = 'l')

#differencing
q5.q.r.d = diff(q5.quad.resid)
plot(q5.q.r.d, type = 'l')
q5.q.r.sd = diff(q5.quad.resid, lag = 52)
plot(q5.q.r.sd, type = 'l')
acf(q5.q.r.sd, lag.max = 60)
pacf(q5.q.r.sd, lag.max = 60) #AR(3), seasonal AR(1)

q5.q.r.d.sd = diff(q5.q.r.sd)
plot(q5.q.r.d.sd, type = 'l')
acf(q5.q.r.d.sd, lag.max = 60) #MA(1)
pacf(q5.q.r.d.sd, lag.max = 60) #AR(6), seasonal AR(1)

q5.model1 = arima(q5.quad.resid,
                  order = c(2, 0, 0),
                  seasonal = list(order = c(1, 1, 0),
                                  period = 52))
q5.model2 = arima(q5.quad.resid,
                  order = c(3, 0, 0),
                  seasonal = list(order = c(1, 1, 0),
                                  period = 52))
q5.model3 = arima(q5.quad.resid,
                  order = c(4, 0, 0),
                  seasonal = list(order = c(1, 1, 0),
                                  period = 52))
q5.model4 = arima(q5.quad.resid,
                  order = c(5, 0, 0), 
                  seasonal = list(order = c(1, 1, 0),
                                  period = 52))

q5.model5 = arima(q5.quad.resid,
                  order = c(6, 1, 1),
                  seasonal = list(order = c(1, 1, 0),
                                  period = 52))
q5.model6 = arima(q5.quad.resid,
                  order = c(7, 1, 1),
                  seasonal = list(order = c(1, 1, 0),
                                  period = 52))
q5.model7 = arima(q5.quad.resid,
                  order = c(6, 1, 2),
                  seasonal = list(order = c(1, 1, 0),
                                  period = 52))

AIC(q5.model1,q5.model2,q5.model3,q5.model4,q5.model5,
    q5.model6,q5.model7)
BIC(q5.model1,q5.model2,q5.model3,q5.model4,q5.model5,
    q5.model6,q5.model7)
#between models 2, 3 and 4.
q5.model2
q5.model3
q5.model4
#the higher AR coefficients for model 4 
#are within 2x s.e., so are not significant

tsdiag(q5.model2)
tsdiag(q5.model3)
#model 3 has better p-values.
#I think I will go with model 3. 

q5.quad.resid.arima.model = q5.model3
plot(q5.quad.resid.arima.model$residuals)
q5.quad.resid.arima.pred = predict(q5.quad.resid.arima.model,
                                   n.ahead = 104)$pred
plot(q5.quad.resid.arima.pred, type = 'l')

plot(c(q5.quad.resid, q5.quad.resid.arima.pred), type = 'l')

#getting final predictions
q5.quad.pred = predict(q5.quad.model,
                       newdata = data.frame(q5.t = 537:640,
                                            q5.t.sqr = (537:640)^2))
q5.predict = q5.quad.pred + q5.quad.resid.arima.pred

plot(c(q5, q5.predict), type = 'l')
#looks good
write.table(q5.predict, 
            sep = ",",
            col.names = FALSE,
            row.names = FALSE,
            file = "Q5_Paul_Kim_24699265.txt")
#-------------------------------------------------------------------#
#-------------------------------------------------------------------#
