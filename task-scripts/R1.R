# генератор равномерно распределенных псевдослучайных чисел
rnd_data <- runif(100, 2, 5)
rnd_data

# псевдослучайные целые числа
whole_r_nums <- sample(2:5, 100, replace=T)
whole_r_nums

marks_table <- table(whole_r_nums)
marks_table

# среднее
marks_mean <- mean(marks_table)

# сформируем data frame
marks_df <- as.data.frame(marks_table)
marks_df

# навесим лямбду на столбец с частотой и добавим производный столбец с процентами
# переименуем столбцы
len <- length(whole_r_nums)
marks_df[,3] <- mapply(function(n) n/len * 100, marks_df[,2])
colnames(marks_df) <- c("Оценка", "Кол-во", "%")
marks_df