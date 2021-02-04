library(cluster)
library(nnet)
genre = read.csv("C:/tmdb/onlygenre.csv",header = TRUE)
daisy_genre_wei1 = daisy(genre[-1],metric="gower", type=list(asymm=c(1:20)),weights = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))
test_pam.g_mn = list("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")
for(i in 1:20){
  for(j in 2:50){
    pam.g_w1 = pam(daisy_genre_wei1, diss = TRUE, k=j)
    clust_pam.g_w1 = pam.g_w1$clustering
    
    f_pam.g_w1 = as.factor(clust_pam.g_w1)
    data_pam.g_w1 = cbind(genre[-1], f_pam.g_w1)
    
    n_w1 = nrow(data_pam.g_w1)
    train_pam.g_w1 = sample(n_w1, 0.7*n_w1, replace = FALSE) 
    TrainSet_pam.g_w1 = data_pam.g_w1[train_pam.g_w1,]
    ValidSet_pam.g_w1 = data_pam.g_w1[-train_pam.g_w1,]
    
    pam.g_w1.mn = multinom(f_pam.g_w1 ~ . ,data = TrainSet_pam.g_w1,MaxNWts = 1200)
    pre_pam.g_w1.mn = predict(pam.g_w1.mn, newdata = ValidSet_pam.g_w1, type = "class")
    (tab <- table(ValidSet_pam.g_w1$f_pam.g_w1,pre_pam.g_w1.mn,dnn=c("Actual","Predicted")))
    test_pam.g_mn[[i]][j]=1-sum(diag(tab))/sum(tab)
  }
}