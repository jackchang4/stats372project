require(ggplot2)
file_output <- "nordstrom_data.txt"
sink (file_output, append=FALSE, split=TRUE)
file <- "nordstrom.dat"
Sales_table <- read.table(file, header = FALSE, sep = "")
colnames(Sales_table) <- c("Time","Quarter", "Sales")
Sales_table$LogSales <- log(Sales_table$Sales)

p1 = ggplot(Sales_table) + geom_line(aes(x=Sales_table$Time, y=Sales_table$Sales))
print("Graph of sales over time")
print(p1)

Log_Sales_time_series <- ts(Sales_table[,4], frequency=4)
fit <- stl(Log_Sales_time_series, s.window=7)
plot(fit)
seasonal <- fit$time.series[,1]
A_stl <- exp(Sales_table$LogSales - seasonal)


figure <- ggplot()
figure <- figure + geom_point(aes(x=Sales_table$Time, y=A_stl), color="Black")
figure <- figure + geom_line(aes(x=Sales_table$Time, y=A_stl), linetype=1, color="Black")
figure <- figure + scale_y_continuous()
figure <- figure + ggtitle("Seasonally adjusted sales using STL") + xlab("Time") + ylab("A")
print("Seasonally Adjusted Sales")
print (figure)

Sales_table$log_A <- log(A_stl)
Sales_table$Time_sq <- Sales_table$Time^2
reg_output <- lm(log_A ~ Time + Time_sq, Sales_table)
cat ("\n", "Regression output for log_A = Alpha + Beta1*Time + Beta2*Time^2 is:", "\n")
print(summary(reg_output))
residuals <- reg_output$residuals
figure <- ggplot()
figure <- figure + geom_point(aes(x=Sales_table$Time,y=residuals))
figure <- figure + geom_line(aes(x=Sales_table$Time,y=residuals))
figure <- figure + ggtitle("Residuals vs. Time") + xlab("Time") + ylab("Residuals")
print("Graph of residuals from first model")
print (figure)
print("ACF of residuals")
acf_residuals <- acf(residuals)
#SHOW ACF
print(acf_residuals)

Sales_table$pred_A <- reg_output$coef[1] + reg_output$coef[2] * Sales_table$Time + reg_output$coef[3] * Sales_table$Time_sq
figure_forecast <- ggplot()
figure_forecast <- figure_forecast + geom_line(aes(x=Sales_table$Time,y=A_stl),linetype=1)
figure_forecast <- figure_forecast +
geom_line(aes(x=Sales_table$Time,y=exp(Sales_table$pred_A)),linetype=2)
figure_forecast <- figure_forecast + ggtitle("Forecasts of Sales") + xlab("Time") + ylab("Sales")
print("Fit of the seasonally adjusted data")
print(figure_forecast)

#Try with lag
Sales_table$log_A_lag[2:94] <- Sales_table$log_A[1:93]
Sales_table$log_A_lag2[3:94] <- Sales_table$log_A[1:92]
Sales_table$log_A_lag3[4:94] <- Sales_table$log_A[1:91]
is.na(Sales_table$log_A_lag[1])
print (head(Sales_table))

reg_output_lag <- lm(log_A ~ log_A_lag +log_A_lag2 + Time + Time_sq, Sales_table)
print("Two lags result")
print (summary(reg_output_lag))

residuals_lag <- reg_output_lag$residuals
figure <- ggplot()
figure <- figure + geom_point(aes(x=Sales_table$Time[3:94],y=residuals_lag))
figure <- figure + geom_line(aes(x=Sales_table$Time[3:94],y=residuals_lag))
figure <- figure + ggtitle("Residuals (from lagged model) vs. Time") + xlab("Time") + ylab("Residuals")
print (figure)
print("acf with lagged model")
acf_residuals_lag <- acf(residuals_lag)
print (acf_residuals_lag)

pred = exp(reg_output_lag$coef[1] + reg_output_lag$coef[2] * Sales_table$log_A[94] + reg_output_lag$coef[3] * Sales_table$log_A[93]+reg_output_lag$coef[4]*95+reg_output_lag$coef[5]*95^2 + seasonal[91])
lower = exp(reg_output_lag$coef[1] + reg_output_lag$coef[2] * Sales_table$log_A[94] + reg_output_lag$coef[3] * Sales_table$log_A[93]+reg_output_lag$coef[4]*95+reg_output_lag$coef[5]*95^2 + seasonal[91] - 1.28*summary(reg_output_lag)$s)
upper = exp(reg_output_lag$coef[1] + reg_output_lag$coef[2] * Sales_table$log_A[94] + reg_output_lag$coef[3] * Sales_table$log_A[93]+reg_output_lag$coef[4]*95+reg_output_lag$coef[5]*95^2 + seasonal[91] + 1.28*summary(reg_output_lag)$s)

print("80% CI and forecast for one period ahead")
cat("(",lower,",",pred,",",upper,")")

closeAllConnections()