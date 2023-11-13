# loads data and random forest library
setwd("C:/Users/dwexler/Desktop/R/rf")
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
message("initial random forest run")
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
for (i in 1:length(percentages)) {
  message(paste("random forest iteration ",i,"/",length(percentages),sep=""))
  x_subset <- x_train[,gini>quantile(gini,probs=percentages[i])]
  forest_subset <- randomForest(x=x_subset,y=y_train,importance=TRUE,ntree=reps,sampsize=sample)
  gini_subset <- data.frame(forest_subset$importance[,"MeanDecreaseGini"])
  colnames(gini_subset) <- c("gini")
  results[i,1] <- ncol(x_subset)
  results[i,2] <- round(forest_subset$err.rate[reps,"OOB"]*100,2)
  results[i,3] <- round(mean(forest_subset$confusion[,"class.error"])*100,2)
  results[i,4] <- round(max(forest_subset$confusion[,"class.error"])*100,2)
  results[i,5] <- (rownames(gini_subset)[order(gini_subset$gini,decreasing=TRUE)])[1]
  results[i,6] <- (rownames(gini_subset)[order(gini_subset$gini,decreasing=TRUE)])[2]
  results[i,7] <- (rownames(gini_subset)[order(gini_subset$gini,decreasing=TRUE)])[3]
  predictions[i,] <- round(forest_subset$confusion[,"class.error"]*100,2)
}
# prints results to console
message("----------")
print(sample_print)
message("----------")
print(results)
message("----------")
print(predictions)
