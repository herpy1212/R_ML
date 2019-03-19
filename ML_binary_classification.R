source('~/ML model/classification_model.R')
source('~/ML model/ML_environment.R')

data_path = "./TEST1.csv"
data = read_csv(data_path)


data$HOSP_DAY_class = NA
data$HOSP_DAY_class[data$HOSP_DAY >= 6.67] = 2
data$HOSP_DAY_class[data$HOSP_DAY < 6.67] = 1
data = data[,-7]

cat_idx = c(1,4,5,6,7,8)
con_idx = c(2,3)
label_idx = 9

cat_name = names(data[,cat_idx])
con_name = names(data[,con_idx])
label_name = names(data[,label_idx])
predict_item = label_name

data[cat_name] <- lapply(data[cat_name], factor)
data[label_name] <- lapply(data[label_name], factor)

###splite K fold data index
library(caret)
data_size = nrow(data)
folds = createFolds((1:data_size))

prediction_evaluation = data.frame(model_name = NA ,accuracy = 0 ,recall = 0 ,precision = 0, F1_score = 0)

#Run Decision Tree model
Decision_tree_K_fold_result = K_fold_prediction("Decision_tree",data)

#Run NaiveBayes model
NaiveBayes_K_fold_result = K_fold_prediction("NaiveBayes",data)

#Run RandomForest model
RandomForest_K_fold_result = K_fold_prediction("RandomForest",data[,-8])

#Run MLP model
MLP_K_fold_result = K_fold_prediction("MLP",data)

#Run SVM model
SVM_K_fold_result = K_fold_prediction("SVM",data)

#Run Logistic regression model
Logistic_regression_K_fold_result = K_fold_prediction("Logistic_regression",data)

model_names = c("Decision_tree","NaiveBayes","RandomForest","MLP","SVM","Logistic_regression")
for(i in model_names){
  model_result_name = as.name(paste(i,"_K_fold_result",sep = ""))
  prediction_evaluation = rbind(prediction_evaluation,c(i,
                                                        mean(eval(model_result_name)$accuracy),
                                                        mean(eval(model_result_name)$recall),
                                                        mean(eval(model_result_name)$precision),
                                                        mean(eval(model_result_name)$F1_score))
  )
}

prediction_evaluation = prediction_evaluation[-1,]

print(prediction_evaluation)
