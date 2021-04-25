library(neuralnet)
library(caret)
library(datasets)

# FIT AND TRAIN THE NN ###################################################

iris_shuffle <- iris[sample(1:nrow(iris)),] #randomize row order of dataset
row.names(iris_shuffle) <- 1:nrow(iris_shuffle) #renumber rows
training_data <- iris_shuffle[c(1:125),] #take first 125 rows as training data

nn=neuralnet(Species ~ .,training_data, hidden=c(6,4)) 

plot(nn, arrow.length = .15, 
     col.entry = "purple",
     col.hidden = "purple",
     col.out = "purple",
     col.intercept = "blue")

# TESTING THE NN ##########################################################

test_data <- iris_shuffle[c(126:150),] #use last 25 records from iris for test

pred = neuralnet::compute(nn, test_data[,c(1,2,3,4)])
df <- data.frame(pred$net.result)

pred_df = data.frame()
for (i in 1:25){
  pred_df <- rbind(pred_df, which.max(pred$net.result[i,]))
}

colnames(pred_df)[1] <- "Result"
pred_df$Result <- gsub(1, "setosa", pred_df$Result)
pred_df$Result <- gsub(2, "versicolor", pred_df$Result)
pred_df$Result <- gsub(3, "virginica", pred_df$Result)

df$max <- apply(df, 1, max)
df$min <- apply(df, 1, min)
colnames(df)[4] <- "Max_Prediction_Value"
colnames(df)[5] <- "Min_Prediction_Value"

prediction <- as.factor(pred_df$Result)
Actual <- test_data[,5]

Prediction <- data.frame(Actual,pred_df,
                         df$Max_Prediction_Value,df$Min_Prediction_Value)

confusionMatrix(prediction, Actual)
Prediction
