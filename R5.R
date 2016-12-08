remove(regression, regression.fixed, price, sqft.living)
data <- read.table("cust_less_1800_cnt_wkds.txt",
                   header=TRUE,
                   sep=";")
data.duration <- read.table("DATA_ALL_wkds.txt",
                   header=TRUE,
                   sep=";")
na.fail(data)
data <- na.omit(data)
data.duration <- subset(subset(data.duration, duration < 1800), subscription_type=="Customer")
options(scipen=5)
cor(data[c(2:22)])
cor(data.duration[c(3:19, 21:23)])
count <- data[,22]
mean_temp <- data[,3]
temp.min <- min(mean_temp)
temp.max <- max(mean_temp)
temp.diff <- temp.max - temp.min
mean_temp <- mapply(function(temp) (temp - temp.min) / temp.diff, mean_temp)
plot(mean_temp, count, col="#75B9BE", type="p",
     pch=16,
     xlab="Средняя температура воздуха, F",
     ylab="Количество поездок")
regression <- lm(formula=count ~ mean_temp)
regression
summary(regression)
summary(regression.fixed)
abline(regression, col="#EE7674", lwd="3")
