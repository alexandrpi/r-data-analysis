sample.norm <- rnorm(1000, 19, 3)
hist(sample.norm,
     breaks = "Sturges",
     col = "#5AA0D6")
summary(sample.norm)
var(sample.norm)

sample.unif <- runif(n = 1000, max = 5, min = 17)
hist(sample.unif,
     breaks = "Sturges",
     col = "#EF8B6B")
summary(sample.norm)
var(sample.norm)