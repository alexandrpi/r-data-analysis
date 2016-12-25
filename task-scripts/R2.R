# укажем каталог рабочей директории
wd_path <- "C:\\Users\\user\\Desktop\\rwd"
setwd(wd_path)

# прочтём данные и узнаем объем выборки
particles_errors <- read.table("Error.txt", header=T, sep="\t")
errors <- particles_errors[,1]
len <- length(errors)

# построим гистограмму
hist(errors, breaks="Sturges", col="tomato2",
	main="Гистограмма распределения ошибок при измерении частиц",
	xlab="Величина ошибки",
	ylab="Частота")

# построим ящик с усами
boxplot(errors)

# найдем выборочные характеристики
summary(errors)

# не хватает только выборочной дисперсии и стандартного отклонения
# найдем их
var(errors)
sd(errors)