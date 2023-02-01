library(naivebayes)
data=read.csv("C:/Users/user/Desktop/water.csv")

data$ammonia = as.numeric(data$ammonia)

data$is_safe = as.numeric(as.character(data$is_safe))

data = data[complete.cases(data), ]

data$is_safe = as.factor(data$is_safe)

res = c()

for (i in 1:100) {
  idx=sample(2,nrow(data),replace=T,prob = c(0.8,0.2))
  
  train=data[idx==1,]
  test=data[idx==2,]
  
  model=naive_bayes(is_safe ~ .,data=train)
  
  p=predict(model,train)
  tab=table(p,train$is_safe)
  sum(diag(tab))/sum(tab)
  
  p1=predict(model,test)
  tab1=table(p1,test$is_safe)
  quality = sum(diag(tab1))/sum(tab1)
  
  res = append(res, quality)
  
  cat("i", i, ": ", quality)
  print("")
}

mean_res <- mean(res)
sd_res <- sd(res)

cat("Srednia arytmetyczna:", mean_res, "\n")
cat("Odchylenie Standardowe:", sd_res, "\n")
tab1