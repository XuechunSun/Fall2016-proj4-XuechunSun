#### pca on feature matrix
feature_matrix_pca <- prcomp(feature_matrix, scale. = TRUE)
dim(feature_matrix_pca$rotation)
summary(feature_matrix_pca)

#### draw explained variance
runPCA <- function(mat = 'Unadjusted matrix') eigen(cor(apply(mat, 2, function(i) i - mean(i))))

pca <- runPCA(feature_matrix)

index <- rep(1:dim(feature_matrix)[2] , 1)
varExplained <- function(eigenList) {
  line_0.8 <- index[cumsum(eigenList$value) / sum(eigenList$value) >= 0.8]
  line_0.9 <- index[cumsum(eigenList$value) / sum(eigenList$value) >= 0.9]
  par(mfrow = c(1,2))
  plot(
    eigenList$value / sum(eigenList$value), pch = 19, col = 'red', bg = '#549cc4', ylim = c(0, 0.05), xlab = 'Principal Component', ylab = 'Variance Explained'
  )
  #plot the proportion variance explained by each principle component
  plot(
    cumsum(eigenList$value) / sum(eigenList$value), pch = 21,
    col = 'blue', bg = '#549cc4', ylim = c(0, 1), xlab = 'Principal Component', ylab = 'Cumulative Variance Explained'
  ) + abline(h = 0.8) + abline(h = 0.90) + abline(v = line_0.8[1]) + abline(v = line_0.9[1])
  #plot the cumulative proportion variance explained by principle components 
  print(line_0.8[1]) # number of PCs explain 80% if total variance
  print(line_0.9[1]) # number of PCs explain 90% if total variance
}

varExplained(pca)

#### visualization
par(mfrow = c(1,1))
feature_2dim <- feature_matrix %*% feature_matrix_pca$rotation[,1:2]
dim(feature_2dim)
plot(feature_2dim)

feature.vis.2dim <- cbind(feature_2dim, topic_of_song[,K+1])
feature.vis.2dim <- as.data.frame(feature.vis.2dim)
feature.vis.2dim[,3] <- as.factor(as.character(feature.vis.2dim[,3]))
colnames(feature.vis.2dim)[3] <- "Topic"
str(feature.vis.2dim)
library(ggplot2)
ggplot(data = feature.vis.2dim, aes(x=feature.vis.2dim[,1], y=feature.vis.2dim[,2], color=Topic))+
  geom_point()+
  ggtitle("Visulization of topics based on Features") +
  labs(x="Features 1st PC",y="Features 2nd PC") 


#####################################
#multinomial regression
#####################################
Y <-  as.matrix(topic_of_song[,K+1])
dim(Y)
str(Y)
feature_50dim <- feature_matrix %*% feature_matrix_pca$rotation[,1:50]
str(feature_50dim)
dim(feature_50dim)
colnames(feature_50dim)
model.data <- cbind(Y,feature_50dim)
str(model.data)
dim(model.data)
colnames(model.data)[1] <- "Y"
unique(model.data[,1])
model.data <- as.data.frame(model.data)
model.data[,1] <- as.factor(as.character(model.data[,1]))
str(model.data)

#### fit multinomial regression
library(nnet)
mulnomial.model <- multinom(Y ~., data = model.data)
mulnomial.model.summary <- summary(mulnomial.model)

#### test model
# dimension reduction
feature_50dim_test <- feature_matrix_test %*% feature_matrix_pca$rotation[,1:50]
feature_50dim_test <- as.data.frame(feature_50dim_test)

#### multinomial test prediction
test_prediction <- predict(mulnomial.model, newdata = feature_50dim_test, "probs")
colnames(test_prediction)
prediction_word_rank_100 <- matrix(NA, nrow = length(unique(Y)), ncol = dim(fit$topics)[2])
test_prediction_index <- as.integer(colnames(test_prediction))
for (i in 1 : length(colnames(test_prediction))){
  prediction_word_rank_100[i,] <- fit$topics[test_prediction_index[i],]
}
#View(prediction_word_rank_100)
View(prediction_word_rank_100)
View(fit$topics)
dim(test_prediction)
dim(prediction_word_rank_100)
test_word_modeling <- test_prediction %*% prediction_word_rank_100
colnames(test_word_modeling)<- colnames(fit$topics)

View(rbind(names(sort(test_word_modeling[1,], decreasing = TRUE))[1:50],
           colnames(sort(lyr.test[1,], decreasing = TRUE))[1:50]))

#check top 50 error rate
error.rate <- matrix(NA, nrow = 1, ncol = 100)
for (i in 1: 100){
  v <- colnames(sort(lyr.test[i,], decreasing = TRUE))[1:50]
  v.prediction <- names(sort(test_word_modeling[i,], decreasing = TRUE))[1:50]
  length(v.prediction)
  error <- 0
  for (j in 1:50){
    if (is.element(v.prediction[j], v) == FALSE)
      error = error + 1
  }
  error.rate[i] <- error / 50
}
mean(error.rate)


########################################
# Random Forest
########################################
feature_225dim <- feature_matrix %*% feature_matrix_pca$rotation[,1:225]
str(feature_225dim)
dim(feature_225dim)
colnames(feature_225dim)
model.data <- cbind(Y,feature_225dim)
str(model.data)
dim(model.data)
colnames(model.data)[1] <- "Y"
#View(model.data)
unique(model.data[,1])
model.data <- as.data.frame(model.data)
model.data[,1] <- as.factor(as.character(model.data[,1]))
str(model.data)

# fit Random Forest
library(randomForest)
tuneRF(as.data.frame(feature_225dim),as.factor(as.character(Y)),
       stepFactor=1.5, improve=1e-5, ntree=1000)
#mtry = 22 	OOB error = 64.53% 
rf.model <- randomForest(Y ~., data = model.data, mtry = 22,stepFactor=1.5,ntree=1000,improve=1e-5 )

#### Random Forest prediction
feature_225dim_test <- feature_matrix_test %*% feature_matrix_pca$rotation[,1:225]
feature_225dim_test <- as.data.frame(feature_225dim_test)

rf_test_prediction <- predict(rf.model, newdata = feature_225dim_test, "prob")
dim(rf_test_prediction)
View(rf_test_prediction)

colnames(rf_test_prediction)
rf_prediction_word_rank_100 <- matrix(NA, nrow = length(unique(Y)), ncol = dim(fit$topics)[2])
rf_test_prediction_index <- as.integer(colnames(rf_test_prediction))
for (i in 1 : length(colnames(rf_test_prediction))){
  rf_prediction_word_rank_100[i,] <- fit$topics[rf_test_prediction_index[i],]
}
View(fit$topics)
View(rf_prediction_word_rank_100)
#View(prediction_word_rank_100)
dim(rf_test_prediction)
dim(rf_prediction_word_rank_100)
rf_test_word_modeling <- rf_test_prediction %*% rf_prediction_word_rank_100
colnames(rf_test_word_modeling)<- colnames(fit$topics)
View(rf_test_word_modeling)

View(rbind(names(sort(rf_test_word_modeling[5,], decreasing = TRUE))[1:50],
           colnames(sort(lyr.test[5,], decreasing = TRUE))[1:50]))

View(rbind(names(sort(rf_test_word_modeling[1,], decreasing = TRUE))[1:100],
           names(sort(rf_test_word_modeling[2,], decreasing = TRUE))[1:100],
           names(sort(rf_test_word_modeling[3,], decreasing = TRUE))[1:100],
           names(sort(rf_test_word_modeling[4,], decreasing = TRUE))[1:100],
           names(sort(rf_test_word_modeling[5,], decreasing = TRUE))[1:100],
           names(sort(rf_test_word_modeling[6,], decreasing = TRUE))[1:100]))
     
# check error rate
error.rate <- matrix(NA, nrow = 1, ncol = 100)
for (i in 1: 100){
  v <- colnames(sort(lyr.test[i,], decreasing = TRUE))[1:50]
  v.prediction <- names(sort(rf_test_word_modeling[i,], decreasing = TRUE))[1:50]
  length(v.prediction)
  error <- 0
  for (j in 1:50){
    if (is.element(v.prediction[j], v) == FALSE)
      error = error + 1
  }
  error.rate[i] <- error / 50
}
mean(error.rate)

####################################################
# rank
rank.100 <- matrix(NA, nrow = 100, ncol = dim(fit$topics)[2])
colnames(rank.100) <- colnames(fit$topics)
for (i in 1 : 100){
  rank.100[i,] <- rank(-rf_test_word_modeling[i,], ties.method = "min")
}
View(rank.100)

evaluation <- matrix(NA, nrow = 100, ncol = 1)
for (i in 1 : 100){
  r_bar <- sum(rank.100[i,]) / 4973
  evaluation[i,1] <-sum(rank.100[1,which(lyr.test[i,] > 0)]) / length(which(lyr.test[i,] > 0)) / r_bar
}


mean(evaluation)


