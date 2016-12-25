data.diabet <- read.csv("data/diabetes.csv",
                        header = TRUE,
                        sep = ",")
na.fail(data.diabet)

# отберем только те наблюдения, которые не содержат логических ошибок
data.diabet <- data.diabet[data.diabet$Glucose > 0 &
                             data.diabet$BloodPressure > 0 &
                             data.diabet$SkinThickness > 0 &
                             data.diabet$Insulin > 0 &
                             data.diabet$BMI > 0,]
# определим объемы тестовой и обучающей выборок
data.size <- length(data.diabet[, 1])
train.size <- 317
test.size <- data.size - train.size

# определим индексы наблюдений тестовой и обучающей выборок
train.indices <- sample(1:data.size, train.size)
test.indices <- setdiff(1:data.size, train.indices)
diabet.train <- data.diabet[train.indices,]

# проверим соотношения классов в исходной и обучающей выборках
round(prop.table(table(data.diabet$Outcome)), digits = 2)
round(prop.table(table(diabet.train$Outcome)), digits = 2)

# запомним отклики для обучающей и тестовой выборок
diabet.train.labels <- diabet.train[, 9]
diabet.train <- diabet.train[, -9]
diabet.test <- data.diabet[test.indices,]
diabet.test.labels <- diabet.test[, 9]
diabet.test <- diabet.test[, -9]

# предскажем значение отклика для тестовой выборки
library("class")
diabet.test.prediction <- knn(train = diabet.train,
                              test = diabet.test,
                              cl = diabet.train.labels,
                              k = round(sqrt(train.size)))

# проанализируем полученные результаты
library("gmodels")
CrossTable(diabet.test.labels, diabet.test.prediction, prop.chisq = FALSE)

# спрогнозируем нзначение переменной-отклика для произвольных значений 
my.diabet.test <- matrix(nrow = 2, ncol = 8)
my.diabet.test[1,] <- c(2, 76, 70, 23, 67, 34.7, 0.23, 22)
my.diabet.test[2,] <- c(0, 90, 88, 26, 80, 31.4, 0.63, 25)
my.diabet.test <- as.data.frame(my.diabet.test)
colnames(my.diabet.test) <- colnames(diabet.train)
my.diabet.test.prediction <- knn(train = diabet.train,
                                 test = my.diabet.test,
                                 cl = diabet.train.labels,
                                 k = round(sqrt(train.size)))
my.diabet.test.prediction
