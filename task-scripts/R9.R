data.voice <- read.csv("data/voice.csv",
                       header = TRUE,
                       sep = ",")

data.voice <- na.omit(data.voice)
# перекодируем значение переменной отклика
# 0 — женщина, 1 — мужчина
levels(data.voice$label) <- c(0, 1)
data.voice$label <- as.numeric(levels(data.voice$label)[data.voice$label])
voice.size <- length(data.voice[, 1])

voice.regression <- glm(formula = label ~ meanfun + IQR,
                        family = binomial,
                        data = data.voice)

th <- coef(voice.regression)
fexp <- function(x, y) { 
  return (1 / (1 + exp(-(th[1] + th[2] * x + th[3] * y))))
  }
th

# зададим пользовательские значения факторов
my.meanfun <- c(0.173, 0.095, 0.146)
my.IQR <- c(0.1, 0.2, 0.09)
my.prob <- fexp(my.meanfun, my.IQR)
voices.my <- data.frame(my.meanfun,
                        my.IQR,
                        my.prob)

library("rgl")

# зададим размеры окна графика
par3d(windowRect = c(200, 200, 1000, 1000))
# зададим соотношение осей
aspect3d(1.4, 1.4, 0.3)
# изобразим точки, на самом деле маленькие сферы радиуса 0.001
# класса женщин
spheres3d(data.voice[data.voice$label == 0, c(13, 6, 21)], col = "#DF928E", r = 0.001)
# изобразим точки, на самом деле маленькие сферы радиуса 0.001
# класса мужчин
spheres3d(data.voice[data.voice$label == 1, c(13, 6, 21)], col = "#5398BE", r = 0.001)
# отобразим оси и зададим им лейблы
axes3d(box = TRUE)
title3d(xlab = "Average Fundamental Frequency",
        ylab = "IQR",
        zlab = "Response")
# построим логистическую поверхность
persp3d(fexp,
        xlim = c(min(data.voice$meanfun), max(data.voice$meanfun)),
        ylim = c(min(data.voice$IQR), max(data.voice$IQR)),
        col = "#FFE381",
        add = TRUE)
# изобразим точки прогноза пользовательских значений
spheres3d(voices.my,
          col = "#941B0C",
          r = 0.003)
voices.my

# выберем случайны объекты из выборки
random.instances <- sample(1:voice.size, 3)
voice.prognosis <- data.voice[random.instances, c(13, 6, 21)]
voice.prognosis$prob <- fexp(voice.prognosis$meanfun,
                             voice.prognosis$IQR)
voice.prognosis
# изобразим точки прогноза значений из обучающей выборки
spheres3d(voice.prognosis$meanfun,
          voice.prognosis$IQR,
          voice.prognosis$prob,
          col = "#136F63",
          r = 0.003)
# ф-ия для сохранения скриншота текущего видимого графика
rgl.snapshot("imgs/9/logistic.prognosis.2.png")
