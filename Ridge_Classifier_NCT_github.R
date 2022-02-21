# load the packages ####
library(stringr)
library(R.matlab)
library(viridis)
library("lattice")
library(robustbase)
library(matrixStats)
require(AUC)
library(AUC)
library(cvTools)
library(caret)
library(CrossValidate)
library("doParallel")
library(doParallel)
library(doMC)
library(devtools)
library(DMwR)
library("foreach")
library(foreach)
library(pROC)
library(PRROC)
library(plotly)
library(ROCR)
library(rminer)
library(rpart)
library(randomForest)
library(R.utils)
library(R.matlab)
library(ROSE)
library(iterators)
library(elasticnet)
library(e1071)
library(glmnet)
library(ggplot2)
library(grid)
library(lattice)
library(MASS)
library(mclust)
library(nnet)
library(neuralnet)
library(stringr)
library(R.matlab)
library(viridis)
library("lattice")
library(robustbase)
library(matrixStats)
library(vioplot)

length(data_used1)
for(i in 1:(length(data_used1))){print(dim(data_used1[[i]]))}
lambdainterval<-10^(-4:4)
parallelnumber<-35 
myCluster <- makeCluster(parallelnumber)
registerDoMC(parallelnumber)
{
  multiResultClass <- function(result1=NULL,result2=NULL,result3=NULL,result4=NULL,
                               result5=NULL,result6=NULL,result7=NULL,result8=NULL,result9=NULL){me <- list(
                                 result1 = result1,
                                 result2 = result2,
                                 result3 = result3,
                                 result4 = result4,
                                 result5 = result5,
                                 result6 = result6,
                                 result7 = result7,
                                 result8 = result8,
                                 result9 = result9)
                               #Set the name for the class
                               class(me) <- append(class(me),"multiResultClass")
                               return(me)}
  
  predict_all_outer<-NULL;predict_all_connect_all_outer<-NULL;predict_all_ensemble_all_outer<-NULL
  varImp_all_outer<-list();varImp_SC_FC_all_outer<-NULL;varImp_connect_all_outer<-NULL
  auc_outer_outerloop<-NULL; outer_auc_connect_all_outer<-NULL; outer_auc_ensemble_all_outer<-NULL
  best_hyper_outer<-NULL; best_hyper_connect_outer<-NULL; best_hyper_ensemble_outer<-NULL
  best_threshold_all_outer<-NULL;best_threshold_all_connect_all_outer<-NULL;best_threshold_all_ensemble_all_outer<-NULL
  result_ConfMatrix_outer<-NULL;result_ConfMatrix_outer_connect_all_outer<-NULL;result_ConfMatrix_outer_ensemble_all_outer<-NULL
  err_cv_outer<-NULL;err_cv_connect_outer<-NULL;err_cv_ensemble_outer<-NULL
  dim_data_outer<-NULL;result_ConfMatrix_outer1<-NULL
  auc_single_ensemble_outerloop<-NULL
  result_ConfMatrix_outer05<-NULL
  result_ConfMatrix_outer05_1<-NULL
  result_ConfMatrix_outerAUC<-NULL
  result_ConfMatrix_outerAUC_1<-NULL;varImp_SC_FClist<-list()
  aucpr_outer_outerloop<-NULL;brierscore_allmodels<-NULL;predicted_observed_brier_outerloop<-NULL
  mean_pred_ensemble_outer12345_weighted<-NULL;predict_all_ensemble_all_outer_weighted<-NULL;cor_pred<-NULL
  predict_test_enet_class_outer<-NULL;predict_test_enet_class_outer_sensitivty<-NULL
  OuterKfold<-5;InnerKfold<-5;InnerIterNumber<-5;connecnumberbegin<-2
  # mtry_interval<-list(seq(2,5,1),seq(2,20,2))
  varImp_SC_FC<-NULL;confusionMatrixresultstable<-NULL
  varImp_SC_FClist_haufe_mean<-list() ; varImp_SC_FClist_haufe_beta<-list()
}

ridge_results<-foreach(it=rep(1:35,3),
.combine = rbind,.multicombine=TRUE,.packages=c("nnet","rminer","caret","AUC","e1071","randomForest")) %dopar% {
cat(paste("iter=",it))

varimplistnumber<-1

for(outerloop in 1:OuterKfold){

  auc_outer_outerloop_onlyforthisouterloop<-NULL
  aucpr_outer_outerloop_onlyforthisouterloop<-NULL
  for(model in c(4,5)){

    data_used<-data_used1[[model]]
    outputs<-data_used$Output_class
    folds_outerloop<-createFolds(factor(outputs),k=OuterKfold,list = FALSE)  
    data_used<-as.data.frame(data_used)
    names1<-names(data_used)
    names(data_used)<-make.names(names1, unique = TRUE, allow_ = TRUE)
    data_used$Output_class<-as.factor(data_used$Output_class) 

trainData <- data_used[folds_outerloop != outerloop, ]
testData <- data_used[folds_outerloop == outerloop, ]

err_cv<-NULL
# Start inner loop ####
for (iterinner in 1:InnerIterNumber) {
  folds_outerloop_inner<-createFolds(factor(trainData$Output_class),k=InnerKfold,list = FALSE)
  
  for(innerloop in 1:InnerKfold){
    
    trainingData <- trainData[folds_outerloop_inner != innerloop, ]
    validationData <- trainData[folds_outerloop_inner == innerloop, ]
    dim(validationData)
    
    normParam_training <- preProcess(trainingData,method = c("center", "scale"))
    trainingData <- predict(normParam_training, trainingData)
    validationData <- predict(normParam_training, validationData)
    
    trainingData <- SMOTE(Output_class ~ ., trainingData, perc.over = 100)
    trainingData<-na.omit(trainingData)
    
    
    for (lambda1 in lambdainterval) {
      # Fit the model using a couple of the hyperparameters
      x<-data.matrix(trainingData[,-which(names(trainingData) %in% "Output_class")])
      y<-data.matrix(trainingData[,which(names(trainingData) %in% "Output_class")])
      innermodel_enet_prob <- glmnet(x,y,lambda=lambda1,alpha=0,family = "binomial",standardize = FALSE) 
      
      # Predict the validation dataset
      validationData$Output_class<-NULL
      predict_validation_prob<-predict(innermodel_enet_prob,newx = data.matrix(validationData),s=lambda1,type="response") 
      
      validationData <- trainData[folds_outerloop_inner == innerloop, ]
      
      auc_inner <- ROSE::roc.curve(validationData$Output_class, predict_validation_prob,plotit = FALSE)$auc
      err_cv<-rbind(err_cv,c(iterinner,innerloop,lambda1,auc_inner,model))
      
    }
    
  }
  
}
err_cv_outer<-rbind(err_cv_outer,err_cv)

# End inner loop ####

trainData <- data_used[folds_outerloop != outerloop, ]
testData <- data_used[folds_outerloop == outerloop, ]

trainData <- SMOTE(Output_class ~ ., trainData, perc.over = 100)
trainData<-na.omit(trainData)


# Best hyperparam ###
param_median_auc<-NULL
for(lambdabest in levels(as.factor(err_cv[,3]))){
  row1<-which(err_cv[,3]==lambdabest)
  param_median_auc<-rbind(param_median_auc,c(as.numeric(lambdabest),
                                             as.numeric(median(err_cv[row1,4]))))
}

best_hyper<-param_median_auc[which.max(param_median_auc[,2]),1]

best_hyper_outer<-rbind(best_hyper_outer,best_hyper)

normParam_train <- preProcess(trainData,method = c("center", "scale"))
trainData <- predict(normParam_train, trainData)
testData <- predict(normParam_train, testData)


x<-data.matrix(trainData[,-which(names(trainData) %in% "Output_class")])
y<-data.matrix(trainData[,which(names(trainData) %in% "Output_class")])

outermodel_enet_prob <- glmnet(x,y,lambda=best_hyper,alpha=0,family = "binomial",standardize = FALSE) 

varImp_SC_FC<-rbind(varImp_SC_FC,as.matrix(coef(outermodel_enet_prob,s=best_hyper)))
varImp_SC_FClist[[varimplistnumber]]<-as.matrix(coef(outermodel_enet_prob,s=best_hyper))

# HAUFE transformation ####
length_Identity<-dim(x)[2];length_Identity
mean_positiveclass<-colMeans(x[which(y==1),])
mean_negativeclass<-colMeans(x[which(y==0),])
parameter_coef<-outermodel_enet_prob$beta[,1];length(parameter_coef)
predict_haufe_transform<-predict(outermodel_enet_prob,newx = data.matrix(x),s=best_hyper,type="response") 

a<-cov(x)*t((cov(x)+best_hyper*diag(length_Identity)));dim(a)
b<-as.data.frame(mean_positiveclass-mean_negativeclass);dim(b)
c<-sd(predict_haufe_transform)*sd(predict_haufe_transform)
d<-as.data.frame(as.matrix(coef(outermodel_enet_prob,s=best_hyper)))
parametercoef_haufe<-a*b*c;length(parametercoef_haufe[,1])
parametercoef_haufe_usingbeta<-a*d*c;length(parametercoef_haufe[,1])

varImp_SC_FClist_haufe_mean[[varimplistnumber]]<-cbind(parametercoef_haufe[,1],
                                                       as.matrix(coef(outermodel_enet_prob,s=best_hyper)[-1]),
                                                       rep(model,length(parametercoef_haufe[,1])),
                                                       rep(outerloop,length(parametercoef_haufe[,1]))
)
varImp_SC_FClist_haufe_beta[[varimplistnumber]]<-cbind(parametercoef_haufe_usingbeta[,1],
                                                       as.matrix(coef(outermodel_enet_prob,s=best_hyper)[-1]),
                                                       rep(model,length(parametercoef_haufe[,1])),
                                                       rep(outerloop,length(parametercoef_haufe[,1]))
)

varimplistnumber<-varimplistnumber+1

length1<-length(coef(outermodel_enet_prob,s=best_hyper))
varImp_all_outer<-rbind(varImp_all_outer,as.matrix(coef(outermodel_enet_prob,s=best_hyper)))

# Predict the validation dataset

testData$Output_class<-NULL
predict_test_prob<-predict(outermodel_enet_prob,newx = data.matrix(testData),s=best_hyper,type="response") 

predict_test_enet_class_outer_sensitivty<-list(predict_test_enet_class_outer_sensitivty,
                                               cbind(predict_test_prob,
                                                     as.numeric(as.character(testData$Output_class)),
                                                     rep(model,length(predict_test_prob)),
                                                     rep(outerloop,length(predict_test_prob))))

# prediction of the classes as binary outcomes
testData$Output_class<-NULL
predict_test_enet_class<-predict(outermodel_enet_prob,newx = data.matrix(testData),s=best_hyper,type="class") 

predict_test_enet_class_outer<-list(predict_test_enet_class_outer,cbind(predict_test_enet_class,
                                                                        as.numeric(as.character(testData$Output_class)),
                                                                        rep(model,length(predict_test_enet_class)),
                                                                        rep(outerloop,length(predict_test_enet_class))))


testData <- data_used[folds_outerloop == outerloop, ]
auc_inner <- ROSE::roc.curve(testData$Output_class, predict_test_prob,plotit = FALSE)$auc


predict_all_outer<-rbind(predict_all_outer,cbind(predict_test_prob,
                                                 as.numeric(as.character(testData$Output_class)),
                                                 rep(model,length(predict_test_prob)),
                                                 rep(outerloop,length(predict_test_prob))))   

# AUC
auc_outer <- ROSE::roc.curve(testData$Output_class, predict_test_prob,plotit = FALSE)$auc
# save AUC for all outer loop iterations
auc_outer_outerloop_onlyforthisouterloop<-rbind(auc_outer_outerloop_onlyforthisouterloop,c(auc_outer,model=model,
outerloop=outerloop))
# Brier score
squareofthedifference<-NULL
for(i in 1:length(testData$Output_class)){
  squareofthedifference<-rbind(squareofthedifference,(predict_test_prob[i]-(as.numeric(testData$Output_class)[i]-1))*(predict_test_prob[i]-(as.numeric(testData$Output_class)[i]-1)))
}
brierscore<-sum(squareofthedifference)/length(testData$Output_class)
#confusion matrix
confusionMatrixresults<-confusionMatrix(as.factor(predict_test_enet_class), testData$Output_class,positive="1")
confusionMatrixresultstable<-rbind(confusionMatrixresultstable,c(outerloop=outerloop, model=model,
                                                                 as.numeric(confusionMatrix(as.factor(predict_test_enet_class), testData$Output_class,positive="1")$table)))
# save all classification results 
auc_outer_outerloop<-rbind(auc_outer_outerloop,c(auc_outer,
                                                 outerloop,
                                                 model,
                                                 brierscore,
                                                 confusionMatrixresults$byClass["Sensitivity"],
                                                 confusionMatrixresults$byClass["Specificity"],
                                                 confusionMatrixresults$byClass["Balanced Accuracy"],
                                                 confusionMatrixresults$byClass["Recall"],
                                                 confusionMatrixresults$byClass["F1"]))

# Data dimensions  
dim_data<-c(dim(data_used)[1],
            dim(data_used)[2],
            length(which(data_used$Output_class==1)),
            length(which(data_used$Output_class==0)),
            length(which(testData$Output_class==1)),
            length(which(testData$Output_class==0)),
            length(which(trainData$Output_class==1)),
            length(which(trainData$Output_class==0)))
dim_data_outer<-rbind(dim_data_outer,dim_data)

  }## end of the model loop
}## end of the outer loop
result <- multiResultClass()
result$result1 <- auc_outer_outerloop
result$result2 <- varImp_all_outer
result$result3 <- err_cv_outer
result$result4 <- best_hyper_outer
result$result5 <- list(dim_data_outer,predict_test_enet_class_outer_sensitivty)
result$result6 <- varImp_SC_FClist
result$result7 <- confusionMatrixresultstable
result$result8 <- varImp_SC_FClist_haufe_mean
result$result9 <- varImp_SC_FClist_haufe_beta
return(result)


}## end of iteration loop

