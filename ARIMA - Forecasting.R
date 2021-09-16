#install.packages("ggplot2")
#install.packages("tseries")
#install.packages("forecast")

library('ggplot2')
library('forecast')
library('tseries')

#Load the data
daily_data <- read.csv('C:\\R\\day.csv', header=TRUE, stringsAsFactors=FALSE)
daily_data

daily_data$Date = as.Date(daily_data$dteday)
ggplot(daily_data, aes(x = Date, y = cnt)) + geom_line() + scale_x_date('month')  + ylab("Daily Bike Rentals")

#Remove  outliers to smooth series:
No_Outliers <- ts(daily_data[, c('cnt')])

daily_data$Smoothed_rentals <- tsclean(No_Outliers)
write.csv(daily_data$Smoothed_rentals, file = 'C:\\R\\day1.csv')

ggplot(daily_data, aes(x = Date, y = Smoothed_rentals)) + geom_line() + scale_x_date('month')  + ylab("Bike Rentals")

daily_data$v7_MA = ma(daily_data$Smoothed_rentals, order=7)
daily_data$v30_MA = ma(daily_data$Smoothed_rentals, order=30)

ggplot() +
  geom_line(data = daily_data, aes(x = Date, y = Smoothed_rentals, colour = "Rentals")) + 
  geom_line(data = daily_data, aes(x = Date, y = v7_MA,   colour = "7-MA"))  +
  geom_line(data = daily_data, aes(x = Date, y = v30_MA, colour = "30-MA"))  

?stl

rental_ma <- ts(na.omit(daily_data$v7_MA), frequency=30)
decomp_rental <- stl(rental_ma, s.window="periodic")
plot(decomp_rental)
adj_rental <- seasadj(decomp_rental)
plot(adj_rental)

#arima
fit2 = arima(adj_rental, order=c(1,0,7))
fcast2 <- forecast(fit2, h=30)
plot(fcast2)

#auto-arima
fit<-auto.arima(adj_rental, seasonal=FALSE)
fcast <- forecast(fit, h=30)
plot(fcast)

#auto-arima seasonal
fit_s<-auto.arima(adj_rental, seasonal=TRUE)
fcast_s <- forecast(fit_s, h=30)
plot(fcast_s)
