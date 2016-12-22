remove(duration, data.cust, data.summer, data.sub, max_temp)
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
plot(mean_temp, count, col="#B8B42D", type="p",
     pch=20,
     xlab="Средняя температура воздуха, F",
     ylab="Количество поездок")
regression <- lm(formula=count ~ mean_temp)
regression
summary(regression)
abline(regression, col="#C81D25", lwd="3")
cor(count, mean_temp)
prognosis.temps <-c(53, 71, 64, 78, 46)
prognosis.counts <- predict(regression, data.frame(mean_temp=prognosis.temps), level=0.9, interval="confidence") 
prognosis.counts
points(prognosis.temps,
       prognosis.counts[,1],
       col="#3E363F",
       pch=17,
       add=TRUE)
