
cal_confusion_matrix = function(test_set,pre){
  confusion_matrix = table(Type = test_set[[predict_item]],predict=pre)#建立預策交叉矩陣
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

K_fold_prediction = function(model_name,data){
  K_fold = data.frame(fold = 1:10 ,accuracy = NA ,recall = NA ,precision = NA,F1_score = NA)
  k = 0 
  for(i in folds){
    k = k+1
    eval(parse( text= paste("pre_result = ",model_name,"(data,i)",sep = "" )))
    print(pre_result$confusion_matrix)
    K_fold[k,]$accuracy = pre_result$accuracy
    K_fold[k,]$recall = pre_result$recall
    K_fold[k,]$precision = pre_result$precision
    K_fold[k,]$F1_score = (2 * pre_result$recall * pre_result$precision ) / (pre_result$recall + pre_result$precision)
  }
  print(model_name)
  cat(" accuracy",mean(K_fold$accuracy),'\n',"recall",mean(K_fold$recall),'\n',"precision",mean(K_fold$precision),'\n',"F1_score",mean(K_fold$F1_score))
  return(K_fold)
}


convert_chr = function(data_list,chr_levels){
  size = length(chr_levels)
  new_levels = c(1:size)
  data_list = as.character(data_list)
  for(i in new_levels){
    data_list[which(data_list==chr_levels[i])] = i
  }
  return(as.integer(data_list))
}


require(nnet)
class_encode = function(data,cat_name,new_label_name = F){
  for(i in cat_name){
    new_class = class.ind(unlist(data[i],use.names = F))
    colnames(new_class) = paste(i,colnames(new_class),sep = "_")
    data = cbind(data,new_class)
  }
  data = data[,!(names(data) %in% cat_name)]
  if(new_label_name == T){
    list = list(data = data,label_name = colnames(new_class))
    return(list)
  }else{
    return(data)
  }
}




RMSE = function(x, y){
  return(sqrt(mean((x - y)^2)))
}

rsq = function(x, y){
  return(cor(x, y) ^ 2)
} 

K_fold_regression = function(model_name,data){
  K_fold = data.frame(fold = 1:10 ,RMSE = NA, R_squared = NA)
  k = 0 
  for(i in folds){
    k = k+1
    eval(parse( text= paste("pre_result = ",model_name,"(data,i)",sep = "" )))
    K_fold[k,]$RMSE = pre_result$RMSE
    K_fold[k,]$R_squared = pre_result$rsq
  }
  print(model_name)
  cat(" RMSE",mean(K_fold$RMSE),'\n',"R_squared",mean(K_fold$R_squared))
  return(K_fold)
}