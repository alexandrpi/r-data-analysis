remove(n.d.a)
news.data <- read.table("OnlineNewsPopularity.csv",
                   header = TRUE,
                   sep = ",")
# избавимся от "неполных" данных
na.omit(news.data)
indices <- c(3:13, 20:31, 40:60)
n.d.a <- data.frame(matrix(ncol = length(indices) + 1, nrow = 0))
colnames(n.d.a) <- c("day / channel", colnames(news.data[, indices]))
i <- 1
j <- 1
for (day in c(32:38)) {
  j <- 1
  for (channel in c(14:19)) {
    temp <- news.data[news.data[, channel] == 1 & news.data[, day] == 0,]
      n.d.a[(i - 1) * 6 + j,] <- c(paste(i, channel,
                             sep="/"),
                       cor(temp[, 61],
                           temp[, indices])
                       )
      j <- j + 1
      }
  i <- i + 1
  }
news.data.analysis <- subset(news.data, is_weekend == 1 & data_channel_is_entertainment == 1)
news.data.train <- news.data[c(1:4354, 5622:17864, 21570:38132),]
news.data.test <- news.data[c(4355:5621, 17865:21569, 38133:39644),]
length(news.data.train[,1]) + length(news.data.test[,1])
# требуемые нам атрибуты:
# n_tokens_title (3) — число слов в названии новости
# n_tokens_content (4) — число слов в новости
# num_hrefs (8) — число ссылок в новости
# num_imgs (10) — число изображений в новости
# num_videos (11) — число видео в новости
# num_keywords (13) — число ключевых слов в новости
# shares (61) — число "репостов" новости
corr_table <- cor(news.data.ww[,61], news.data.ww[, c(2:13, 20:31, 40:61)])
result <- apply(n.d.a, 2, function(column) column > 0.1)
temp2 <- n.d.a[, result]
cor(news.data.friday[, c(2:31, 40:61)])
news.mulp.regression <- lm(shares ~ -1 +
                        average_token_length +
                        num_hrefs +
                        num_imgs +
                        num_videos + 
                        num_keywords + 
                        rate_positive_words,
                      data = subset(news.data.train))
summary(news.mulp.regression)
news.predict <- predict(news.mulp.regression, news.data.test)
news.true <- news.data.test[,61]
plot(news.predict[1:20], news.true[1:20], col="#B8B42D", type="p",
     pch=20,
     xlab="Предсказанные значения",
     ylab="Настоящие значения")