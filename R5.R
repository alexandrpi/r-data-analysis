remove(sqft_living, sample.chisq, sample.exp, sample.t, sample.unif)
data <- read.table("kc_house_data.csv",
                   header=TRUE,
                   sep=",")
data <- na.omit(subset(data, sqft_living < 7000))
options(scipen=5)
price <- data[,3]
sqft.living <- data[,6]
plot(sqft.living, price, col="#2C666E", type="p",
     xlab="Жилая площадь",
     ylab="Цена на квартиру")

regression <- lm(formula=price ~ sqft_living)
regression
regression.fixed <- lm(formula=price ~ sqft_living - 1)
regression.fixed
summary(regression)
summary(regression.fixed)
abline(regression, col="#3C0000", lwd="3")
abline(regression.fixed, col="#D69F7E", lwd="3")
