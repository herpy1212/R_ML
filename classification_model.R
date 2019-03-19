####Decision_tree

library(MASS)
library(rpart)
set.seed(1111)#設定亂數種子

Decision_tree = function(data_set,test_idx){
  train_set = data_set[-test_idx,]
  test_set = data_set[test_idx,]
  formal = paste(predict_item,"~.",sep = "")
  cart = rpart(formal,data = train_set,control = rpart.control(cp = 0))#訓練CART模型
  pre = predict(cart, test_set,type = 'class')
  return(cal_confusion_matrix(test_set,pre))
}



###RandomForest
library(randomForest)
set.seed(71)

RandomForest = function(data_set,test_idx){
  train_set = data_set[-test_idx,]
  test_set = data_set[test_idx,]
  formal = paste(predict_item,"~.",sep = "")
  RF = randomForest(as.formula(formal), train_set, importance=TRUE,proximity=F)
  pre = predict(RF, test_set)
  return(cal_confusion_matrix(test_set,pre))
}



###SVM
library(e1071)

SVM = function(data_set,test_idx){
  train_set = data_set[-test_idx,]
  test_set = data_set[test_idx,]
  formal = paste(predict_item,"~.",sep = "")
  SVM_model = svm(as.formula(formal), train_set, importance=TRUE,proximity=TRUE)
  pre = predict(SVM_model, test_set)
  return(cal_confusion_matrix(test_set,pre))
}



###naiveBayes
library(bnlearn)
library(e1071)

NaiveBayes = function(data_set,test_idx){
  train_set = data_set[-test_idx,]
  test_set = data_set[test_idx,]
  formal = paste(predict_item," ~ .",sep = "")
  bn = naiveBayes(as.formula(formal),data = train_set)
  pre = predict(bn,test_set)
  return(cal_confusion_matrix(test_set,pre))
}


###Multilayer perceptron
library(RSNNS)
library(MASS)

MLP = function(data_set,test_idx){
  data_set = class_encode(data_set,cat_name)
  new_label_data = class_encode(data_set,label_name,T)
  data_set = new_label_data$data
  new_label_name = new_label_data$label_name
  
  train_set = data_set[-test_idx,]
  test_set = data_set[test_idx,]
  
  Train_value = train_set[,!(names(train_set) %in% new_label_name)]
  Train_target = train_set[,new_label_name]
  Train_value = normalizeData(Train_value,type = "0_1")
  Train_target = normalizeData(Train_target,type = "0_1")
  
  Test_value =  test_set[,!(names(test_set) %in% new_label_name)]
  Test_value = normalizeData(Test_value,type = "0_1")
  Test_target = test_set[,new_label_name]
  Test_target = normalizeData(Test_target,type = "0_1")
  
  model = mlp(Train_value,Train_target,size = 14,learnFuncParams=0.01,maxit=100)
  
  pre = predict(model,Test_value)
  confusion_matrix = confusionMatrix(Test_target,pre)
  confusion_matrix
  tn = confusion_matrix[1]
  tp = confusion_matrix[4]
  fn = confusion_matrix[2]
  fp = confusion_matrix[3]
  accuracy = sum(diag(confusion_matrix))/sum(confusion_matrix)#計算正確率
  recall = tp/(tp+fn)
  precision =  tp/(tp+fp)
  result = list(confusion_matrix = confusion_matrix,accuracy = accuracy,recall = recall,precision = precision)
  return(result)
}

###Logistic regression
Logistic_regression = function(data_set,test_idx){
  contrasts_table = contrasts(data_set[[predict_item]])
  train_set = data_set[-test_idx,]
  test_set = data_set[test_idx,]
  formal = paste(predict_item," ~ .",sep = "")
  glm_model = glm(as.formula(formal), train_set,family=binomial())
  pre = predict(glm_model,test_set, type="response")
  pre[pre > .5] = rownames(contrasts_table)[which(contrasts_table == 1)]
  pre[pre <= .5] = rownames(contrasts_table)[which(contrasts_table == 0)]
  return(cal_confusion_matrix(test_set,pre))
}