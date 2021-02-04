library(cluster)
library(e1071)
genre = read.csv(file = "C:/tmdb/onlygenre.csv", header = TRUE)

#gower distance 사용 + 적절한 군집의 개수를 찾기 위한 과정
#각 영화별로 해당되는 장르에 1, 아니면 0으로 구성
#전체 영화에 대해 각 장르에 대한 빈도를 확인한 결과
#장르별 빈도수가 차이가 나지 않아 가중치는 모두 동일한 것으로 두고 실행
daisy_genre_wei1 = daisy(genre[-1],metric="gower", type=list(asymm=c(1:20)),weights = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))
test_pam.g_w1 = list("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")
for(i in 1:20){
  for(j in 2:50){
    #군집의 개수를 2~50으로 변경하면서 군집분석 실시
    pam.g_w1 = pam(daisy_genre_wei1, diss = TRUE, k=j)
    clust_pam.g_w1 = pam.g_w1$clustering
    
    #군집분석의 결과를 각 영화의 lable로 사용함
    f_pam.g_w1 = as.factor(clust_pam.g_w1)
    data_pam.g_w1 = cbind(genre[-1], f_pam.g_w1)
    
    #군집의 결과를 각 영화의 장르 lable로 취급하고 분류학습 실시
    n_w1 = nrow(data_pam.g_w1)
    train_pam.g_w1 = sample(n_w1, 0.7*n_w1, replace = FALSE)  # 0.7*n?? ?????? ?????? ????
    TrainSet_pam.g_w1 = data_pam.g_w1[train_pam.g_w1,]
    ValidSet_pam.g_w1 = data_pam.g_w1[-train_pam.g_w1,]
    
    pam.g_w1.svm = svm(f_pam.g_w1~., data = TrainSet_pam.g_w1, method = "C-classification", 
                       kernal="radial", gamma=0.1, cost=10)
    
    pre_pam.g_w1.svm = predict(pam.g_w1.svm, newdata = ValidSet_pam.g_w1)
    (tab <- table(ValidSet_pam.g_w1$f_pam.g_w1,pre_pam.g_w1.svm,dnn=c("Actual","Predicted")))
    test_pam.g_w1[[i]][j] = 1-sum(diag(tab))/sum(tab)
  }
}