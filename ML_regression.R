source('~/ML model/ML_env.R')

data_path = "./TEST1.csv"
data = read_csv(data_path)

cat_idx = c(1,4,5,6,8,9)
con_idx = c(2,3,7)
predict_idx = 7

predict_item = names(data[,predict_idx])
cat_name = names(data[,cat_idx])
con_name = names(data[,con_idx])


data[cat_name] <- lapply(data[cat_name], factor)

###splite K fold data index
library(caret)
data_size = nrow(data)
folds = createFolds((1:data_size))


Linear_regression = function(data_set,test_idx){
  train_set = data_set[-test_idx,]
  test_set = data_set[test_idx,]
  formal = paste(predict_item,"~.",sep = "")
  LM = lm(as.formula(formal), train_set)
  pre = predict(LM, test_set)
  result = list(RMSE = RMSE(test_set[[predict_item]],pre),rsq = rsq(test_set[[predict_item]],pre))
  return(result)
}

Linear_regression_K_fold_result = K_fold_regression("Linear_regression",data[,-9])


prediction_evaluation = data.frame(model_name = "Linear_regression" ,
                                   RMSE = mean(Linear_regression_K_fold_result$RMSE),
                                   R_squared = mean(Linear_regression_K_fold_result$R_squared))

print(prediction_evaluation)
