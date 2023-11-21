# loads data and random forest library
setwd("C:/Users/dwexler/Desktop/R/nps-repo/rf")
x <- read.csv("rf_data.csv")
y <- read.csv("rf_labels.csv")
suppressWarnings({library(randomForest)})
# -----------------------------------------------------------------------------
# removes patches without a labeled change type
x <- x[y$ChangeType!="",]
y <- y[y$ChangeType!="",]
# removes patches without recovery information
# y <- y[x$tcbPst07Mn!="0",]
# x <- x[x$tcbPst07Mn!="0",]
# filters by variable type and converts data to numeric
variables <- c("durMn","durSd","idxMagMn","idxMagSd","tcbMagMn","tcbMagSd",
               "tcgMagMn","tcgMagSd","tcwMagMn","tcwMagSd","tcbPreMn",
               "tcbPreSd","tcgPreMn","tcgPreSd","tcwPreMn","tcwPreSd",
               "tcbPstMn","tcbPstSd","tcgPstMn","tcgPstSd","tcwPstMn",
               "tcwPstSd","area","perim","shape_1","tcbPst01Mn","tcbPst01Sd",
               "tcbPst03Mn","tcbPst03Sd","tcbPst07Mn","tcbPst07Sd","tcbPst15Mn",
               "tcbPst15Sd","tcgPst01Mn","tcgPst01Sd","tcgPst03Mn","tcgPst03Sd",
               "tcgPst07Mn","tcgPst07Sd","tcgPst15Mn","tcgPst15Sd","tcwPst01Mn",
               "tcwPst01Sd","tcwPst03Mn","tcwPst03Sd","tcwPst07Mn","tcwPst07Sd",
               "tcwPst15Mn","tcwPst15Sd")
x <- data.frame(sapply(x[,variables],as.numeric))
y <- data.frame(y[c("ChangeType")])
# filters by disturbance type 
disturbances <- c("Avalanche","Inter-annual Variability","Clearing",
                  "Mass Movement","Progressive Defoliation","Riparian",
                  "Fire","Post Fire Defoliation","Tree Toppling",
                  "Development","Post Clearing","Post Tree Toppling","Water")
# EDIT 'disturbances' TO CHANGE DISTURBANCES INCLUDED IN MODEL
disturbances <- disturbances[c(1,2,5,6,7,8,9)]
# disturbances <- disturbances[c(1,2,3,4,5,6,7,8,9,11)]
x <- x[y[,1]%in%disturbances,]
y <- y[y[,1]%in%disturbances,,drop=FALSE]
# y <- factor(y[convert data frame to[,1]%in%disturbances,])
# creates a training and testing set
x_train <- data.frame(matrix(0,0,ncol(x)))
y_train <- data.frame(matrix("",0,1))
x_test <- data.frame(matrix(0,0,ncol(x)))
y_test <- data.frame(matrix("",0,1))
for (disturbance in disturbances) {
  x_subset <- x[y[,1]==disturbance,]
  y_subset <- y[y[,1]==disturbance,,drop=FALSE]
  num_rows <- nrow(x_subset)
  index_subset <- sample.int(num_rows,num_rows*0.75)
  x_train <- rbind(x_train,x_subset[index_subset,])
  y_train <- rbind(y_train,y_subset[index_subset,,drop=FALSE])
  x_test <- rbind(x_test,x_subset[-index_subset,])
  y_test <- rbind(y_test,y_subset[-index_subset,,drop=FALSE])
}
y_train <- factor(sapply(y_train,as.factor))
y_test <- factor(sapply(y_test,as.factor))
# -----------------------------------------------------------------------------
# constructs vector used for sampling data and sets number of trees
sample <- table(y_train)
sample <- replace(sample,sample>100,100)
sample_print <- as.data.frame(sample)
colnames(sample_print) <- c("disturbance","freq")
# EDIT 'reps' TO CHANGE THE NUMBER OF TREES IN EACH RANDOM FOREST RUN
reps <- 50
# runs random forest and extracts most important variables
cat("initial rf run, finding best predictors","\r")
forest <- randomForest(x=x_train,y=y_train,importance=TRUE,ntree=reps,sampsize=sample)
gini <- forest$importance[,"MeanDecreaseGini"]
# runs random forest using different combinations of variables
# EDIT 'percentages' TO CHANGE PERCENTILE CUTOFFS FOR VARIABLES
percentages <- seq(0,0.9,0.1)
results <- data.frame(matrix(0,length(percentages),9))
colnames(results) <- c("numPredictors","oobError","avgClassError","maxError",
                       "1stPredictor","2ndPredictor","3rdPredictor",
                       "4thPredictor","5thPredictor")
predictions <- data.frame(matrix(0,length(percentages),length(disturbances)))
colnames(predictions) <- sort(disturbances)
min_class_error <- 99999
best_variables <- NULL
for (i in 1:length(percentages)) {
  x_subset <- x_train[,gini>quantile(gini,probs=percentages[i])]
  cat(paste("rf iteration ",i,"/",length(percentages),
            " with the top ",ncol(x_subset)," predictors","\r",sep=""))
  forest_subset <- randomForest(x=x_subset,y=y_train,importance=TRUE,ntree=reps,sampsize=sample)
  gini_subset <- data.frame(forest_subset$importance[,"MeanDecreaseGini"])
  colnames(gini_subset) <- c("gini")
  results[i,1] <- ncol(x_subset)
  results[i,2] <- round(forest_subset$err.rate[reps,"OOB"]*100,2)
  avg_class_error <- round(mean(forest_subset$confusion[,"class.error"])*100,2)
  # stores predictor variables used during the lowest error run
  results[i,3] <- avg_class_error
  if (avg_class_error < min_class_error) {
    min_class_error <- avg_class_error
    best_variables <- colnames(x_subset)
  }
  results[i,4] <- round(max(forest_subset$confusion[,"class.error"])*100,2)
  sorted_gini <- rownames(gini_subset)[order(gini_subset$gini,decreasing=TRUE)]
  results[i,c(5,6,7,8,9)] <- sorted_gini[c(1,2,3,4,5)]
  predictions[i,] <- round(forest_subset$confusion[,"class.error"]*100,2)
}
# prints results to console
message("***frequency distribution of disturbances used to train model***")
print(sample_print)
message("***error rates and top predictors from all runs***")
print(results)
message("***disturbance specific errors from all runs***")
print(predictions)
# -----------------------------------------------------------------------------
# runs random forest with most predictive variables and prints results
x_train_subset <- x_train[,best_variables]
x_test_subset <- x_test[,best_variables]
best_forest <- randomForest(x=x_train_subset,y=y_train,xtest=x_test_subset,ytest=factor(y_test),importance=TRUE,ntree=reps,sampsize=sample)
message(paste("***confusion matrix for best run with ",ncol(x_train_subset)," variables***",sep=""))
confusion_train <- best_forest$confusion
confusion_train[,"class.error"] <- round(confusion_train[,"class.error"]*100,2)
colnames(confusion_train)[length(colnames(confusion_train))] <- "error"
writeLines(paste("oob error: ",round(best_forest$err.rate[reps,"OOB"]*100,2),sep=""))
writeLines(paste("avg class error: ",round(mean(best_forest$confusion[,"class.error"])*100,2),sep=""))
print(confusion_train)
confusion_test <- best_forest$test$confusion
confusion_test[,"class.error"] <- round(confusion_test[,"class.error"]*100,2)
colnames(confusion_test)[length(colnames(confusion_test))] <- "error"
message(paste("***confusion matrix for test set with ",ncol(x_train_subset)," variables***",sep=""))
writeLines(paste("avg class error: ",round(mean(best_forest$test$confusion[,"class.error"])*100,2),sep=""))
print(confusion_test)
# print(best_forest$test$votes)
