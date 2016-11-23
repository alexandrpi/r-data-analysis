# нормальное распределение
# в качесте предполгаемых данных возьмем массу мозга человека
# средняя масса мозга мужчины: 1350 г.
sample.norm <- rnorm(1000, 1350, 120)
hist(sample.norm,
     breaks = "Sturges",
     col = "#92140C",
     main = "Нормальное распределение")
summary(sample.norm)
boxplot(sample.norm, col = "#92140C")
sd(sample.norm)
var(sample.norm)

# равномерное распределение
sample.unif <- runif(1000, min = 5, max = 17)
hist(sample.unif,
     breaks = "Sturges",
     col = "#FFF8F0",
     main = "Равномерное распределение")
summary(sample.unif)
boxplot(sample.unif, col = "#FFF8F0")
var(sample.unif)

# экспоненциальное распределение
sample.exp <- rexp(1000, 4)
hist(sample.exp,
     breaks = "Sturges",
     col = "#F6CA83",
     main = "Экспоненциальное распределение")
summary(sample.exp)
boxplot(sample.exp, col = "#F6CA83")
var(sample.exp)

# распределение Стьюдента
sample.t <- rt(1000, 3)
hist(sample.t,
     breaks = "Sturges",
     col = "#9CAFB7",
     main = "Распределение Стьюдента")
summary(sample.t)
boxplot(sample.t, col = "#9CAFB7")
var(sample.t)

# распределение Хи-квадрат
sample.chisq <- rchisq(1000, 7)
hist(sample.chisq,
     breaks = "Sturges",
     col = "#FFCF99",
     main = "Распределение Хи-квадрат")
summary(sample.chisq)
boxplot(sample.chisq, col = "#FFCF99")
var(sample.chisq)