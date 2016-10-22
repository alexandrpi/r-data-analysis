# нормальное распределение
# в качесте предполгаемых данных возьмем массу мозга человека
# средняя масса мозга мужчины: 1350 г.
sample.norm <- rnorm(1000, 1350, 120)
hist(sample.norm,
     breaks = "Sturges",
     col = "#1B263B",
     main = "Нормальное распределение")
summary(sample.norm)
boxplot(sample.norm)
var(sample.norm)

# равномерное распределение
sample.unif <- runif(1000, min = 5, max = 17)
hist(sample.unif,
     breaks = "Sturges",
     col = "#242331",
     main = "Равномерное распределение")
summary(sample.unif)
boxplot(sample.unif)
var(sample.unif)

# экспоненциальное распределение
sample.exp <- rexp(1000, 4)
hist(sample.exp,
     breaks = "Sturges",
     col = "#F6CA83",
     main = "Экспоненциальное распределение")
summary(sample.exp)
boxplot(sample.exp)
var(sample.exp)

# распределение Стьюдента
sample.t <- rt(1000, 4)
hist(sample.t,
     breaks = "Sturges",
     col = "#283F3B",
     main = "Распределение Стьюдента")
summary(sample.t)
boxplot(sample.t)
var(sample.t)

# распределение Хи-квадрат
sample.chisq <- rchisq(1000, 7)
hist(sample.chisq,
     breaks = "Sturges",
     col = "#9CAFB7",
     main = "Распределение Хи-квадрат")
summary(sample.chisq)
boxplot(sample.chisq)
var(sample.chisq)