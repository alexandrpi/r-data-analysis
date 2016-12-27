data.voice <- read.csv("data/voice.csv",
                       header = TRUE,
                       sep = ",")

# перекодируем значение переменной отклика
# 0 — женщина, 1 — мужчина
levels(data.voice$label) <- c(0, 1)
data.voice$label <- as.numeric(levels(data.voice$label)[data.voice$label])

voice.size <- length(data.voice[, 1])

plot(data.voice$meanfun,
     data.voice$label,
     type="n",
     main="Зависимость м/у средней основной частотой голоса\nи полом человека",
     xlab = "Средняя основная частота",
     ylab = "Пол человека")

for (i in 1:voice.size)
{
  colour <- ifelse(data.voice$label[i] == 1,
                   "#5398BE",
                   "#DF928E")
  points(data.voice$meanfun[i],
         data.voice$label[i],
         type="p",
         pch=16,
         col=colour,
         add="TRUE")
}

voice.regression <- glm(formula = data.voice$label ~ data.voice$meanfun,
                        family = binomial,
                        data = data.voice)

th <- coef(voice.regression)
fexp <- function(x){ return(1 / (1 + exp(-(th[1] + th[2] * x))))}
th
curve(fexp(x), 0, 0.3, col="#F58F29", lwd=3, add=TRUE)

my.meanfun <- c(0.173, 0.2, 0.095, 0.112, 0.146)
my.gender <- fexp(my.meanfun)
points(my.meanfun,
       my.gender,
       type="p",
       pch=16,
       col="#92140C",
       add="TRUE")

for(i in 1:length(my.meanfun)) 
{
  segments(my.meanfun[i], 0, my.meanfun[i], my.gender[i], col="gray", lty = 2, add="TRUE")
  segments(my.meanfun[i], my.gender[i], 0, my.gender[i], col="gray", lty = 2, add="TRUE")
}
