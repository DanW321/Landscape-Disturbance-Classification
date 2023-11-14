# loads data and random forest library
setwd("C:/Users/dwexler/Desktop/R/nps-repo/rf")
data <- read.csv("rf_data.csv")
labels <- read.csv("rf_labels.csv")
suppressWarnings({library(randomForest)})
# -----------------------------------------------------------------------------
# removes patches without a labeled change type
x_train <- data[labels$ChangeType!="",]
y_train <- labels[labels$ChangeType!="",]
# removes patches without recovery information
y_train <- y_train[x_train$tcbPst07Mn!="0",]
x_train <- x_train[x_train$tcbPst07Mn!="0",]
# removes extraneous columns (ids, years, etc.) and converts data to numeric
x_train <- data.frame(sapply(x_train[,-c(1,2,3)],as.numeric))
y_train <- y_train[c("ChangeType")]
# filters by disturbance type 
disturbances <- c("Avalanche","Inter-annual Variability","Clearing",
                  "Mass Movement","Progressive Defoliation","Riparian",
                  "Fire","Post Fire Defoliation","Tree Toppling",
                  "Development","Post Clearing","Post Tree Toppling","Water")
# EDIT 'disturbances' TO CHANGE DISTURBANCES INCLUDED IN MODEL
disturbances <- disturbances[c(1,2,3,5,6,7,8,9)]
# disturbances <- disturbances[c(1,2,3,4,5,6,7,8,9,10,11,12,13)]
x_train <- x_train[y_train[,1]%in%disturbances,]
y_train <- factor(y_train[y_train[,1]%in%disturbances,])
# -----------------------------------------------------------------------------
# constructs vector used for sampling data and sets number of trees
sample <- table(y_train)
sample <- replace(sample,sample>100,100)
sample_print <- as.data.frame(sample)
colnames(sample_print) <- c("disturbance","freq")
# EDIT 'reps' TO CHANGE THE NUMBER OF TREES IN EACH RANDOM FOREST RUN
reps <- 500
# runs random forest and extracts most important variables
cat("initial rf run, finding best predictors","\r")
forest <- randomForest(x=x_train,y=y_train,importance=TRUE,ntree=reps,sampsize=sample)
gini <- forest$importance[,"MeanDecreaseGini"]
# runs random forest using different combinations of variables
# EDIT 'percentages' TO CHANGE PERCENTILE CUTOFFS FOR VARIABLES
percentages <- seq(0,0.9,0.1)
results <- data.frame(matrix(0,length(percentages),7))
colnames(results) <- c("numPredictors","oobError","avgClassError","maxError",
                       "1stPredictor","2ndPredictor","3rdPredictor")
predictions <- data.frame(matrix(0,length(percentages),length(disturbances)))
colnames(predictions) <- sort(disturbances)
min_class_error <- 99999
best_variables <- NULL
cat("\r")
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
  results[i,5] <- (rownames(gini_subset)[order(gini_subset$gini,decreasing=TRUE)])[1]
  results[i,6] <- (rownames(gini_subset)[order(gini_subset$gini,decreasing=TRUE)])[2]
  results[i,7] <- (rownames(gini_subset)[order(gini_subset$gini,decreasing=TRUE)])[3]
  predictions[i,] <- round(forest_subset$confusion[,"class.error"]*100,2)
}
# prints results to console
message("---frequency distribution of disturbances---")
print(sample_print)
message("---error rates and top predictors from all runs---")
print(results)
message("---disturbance specific errors from all runs---")
print(predictions)
# -----------------------------------------------------------------------------
x_subset <- x_train[,best_variables]
best_forest <- randomForest(x=x_subset,y=y_train,importance=TRUE,ntree=reps,sampsize=sample)
message(paste("---confusion matrix for best run with ",ncol(x_subset)," variables---",sep=""))
best_confusion <- best_forest$confusion
best_confusion[,"class.error"] <- round(best_confusion[,"class.error"]*100,2)
print(best_confusion)

