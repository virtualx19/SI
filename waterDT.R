library("party")
data=read.csv("C:/Users/user/Desktop/water.csv")

data$ammonia = as.numeric(data$ammonia)

data$is_safe = as.numeric(as.character(data$is_safe))

data = data[complete.cases(data), ]

data$is_safe = as.factor(data$is_safe)

res = c()

for(i in 1:100) {
  idx=sample(1:nrow(data),0.8*nrow(data))
  
  train = data[idx,]
  test = data[-idx,]
  
  model = ctree(is_safe ~ ., data=train)
  p=predict(model,test)
  tab <- table(p, test$is_safe)
  quality = sum(diag(tab))/sum(tab)
  res = append(res, quality)
  
  cat("i", i, ": ", quality)
  print("")
  
}

mean_res <- mean(res)
sd_res <- sd(res)

cat("Srednia arytmetyczna:", mean_res, "\n")
cat("Odchylenie Standardowe:", sd_res, "\n")
tab