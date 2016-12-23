options(scipen=5)
gym.data <- read.table("data_gym.csv",
                       header = TRUE,
                       sep = ",")
gym.data[, 2] <- mapply(function(timestamp) timestamp / 3600, gym.data[, 2])
# требуемые нам атрибуты:
# timestamp (2) — кол-во секунд, прошедших с момента начала суток
# is_weekend (4) — день недели
# apparent_temperature (6) — ощущаемая температура воздуха, в фаренгейтах
# is_start_of_semester (8) — 1 - начало семестра, 0 - нет
# number_people (1) — количество людей в зале
write.csv(cor(gym.data), file = "gym_data_corr.csv")
cor(gym.data[, 1], gym.data[, c(2, 4, 6, 8)])
indices.train <- c(2375:7643, 9840:18466, 20071:26066)
indices.test <- c(1:2374, 7644:9839, 18467:20070)
length(indices.test) + length(indices.train)
gym.data.train <- gym.data[indices.train,]
gym.data.test <- gym.data[indices.test,]

# СПОСОБ 2
crowd.mul.regression <- lm(number_people ~ timestamp +
                             is_weekend +
                             apparent_temperature +
                             is_start_of_semester,
                           gym.data.train)
summary(crowd.mul.regression)
crowd.predict <- predict(crowd.mul.regression, gym.data.test)
crowd.true <- gym.data.test[,1]
plot(crowd.predict[70:370], crowd.true[70:370], col="#B8B42D", type="p",
     pch=20,
     xlab="Предсказанные значения",
     ylab="Настоящие значения")

# СПОСОБ 1
f <- 4
matrix.gym <- matrix(nrow = 0, ncol = f + 1)
temp <- read.table("data_gym.csv",
                         header = TRUE,
                         sep = ",")
temp[, 2] <- mapply(function(timestamp) timestamp / 3600, temp[, 2])
matrix.gym <- temp[, c(1, 2, 4, 6, 8)]
n <- length(matrix.gym[,1])
# первый столбец отвечает за переменную отклика, возьмем только факторные переменные
X.gym <- matrix.gym[, 2:(f + 1)]
Y.gym <- matrix.gym[, 1]
X1.gym <- matrix(nrow = n, ncol = f + 1)
for (k in 1:(f + 1)) {
  print(k)
  if (k == 1) {
    X1.gym[, k] <- runif(n, 1:1)
  }
  else {
    X1.gym[, k] <- X.gym[, k - 1]
  }
}
X1.gym.train <- X1.gym[indices.train,]
T <- t(X1.gym.train) %*% X1.gym.train 
library(MASS)
T.inverse <- ginv(T)
th <- (T.inverse%*%t(X1.gym.train)%*%Y.gym[indices.train])[, 1]
prognosis <- th%*%t(X1.gym[indices.test,])
plot(prognosis[70:370], Y.gym[indices.test][70:370], col="#B8B42D", type="p",
     pch=20,
     xlab="Предсказанные значения",
     ylab="Настоящие значения")
