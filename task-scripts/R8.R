data.voice <- read.csv("data/voice.csv",
                       header = TRUE,
                       sep = ",")

# перекодируем значение переменной отклика
# 0 — женщина, 1 — мужчина
data.voice$label <- factor(data.voice$label,
                           levels = c("male", "female"),
                           labels = c(0, 1))

voice.size <- length(data.voice[, 1])

plot(data.voice$maxfun, data.voice$label, type="n", main="test")

for (i in 1:voice.size) 
{
  colour <- ifelse(data.voice$label[i] == 1,
                   "blue",
                   "red")
  points(data.voice$maxfun[i],
         data.voice$label[i],
         type="p",
         pch=16,
         col=colour,
         add="TRUE")
}