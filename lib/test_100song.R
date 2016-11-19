#######################################
# 100 test songs
library(data.table)

test_submission <- read.csv("/Users/sun93/Documents/ADS/pro4/TestSongFile100/sample_submission.csv", header = TRUE)
test_submission <- test_submission[c(1:100),]
dim(test_submission)
View(test_submission)

test_100_dir.h5 <- '/Users/sun93/Documents/ADS/pro4/TestSongFile100/'
test_100_files.list <- paste0("/Users/sun93/Documents/ADS/pro4/TestSongFile100/testsong",1:100)
test_100_files.list <- paste0(test_100_files.list,".h5")
length(test_100_files.list)

############################
# extract feature
test_100_nsong <- 100

test_100_feature_matrix <- matrix(NA, nrow = test_100_nsong, ncol = 573)
set.seed(333)
for (i in 1 : test_100_nsong){
  # get feature for each song
  a <- c()
  sound <- h5read(test_100_files.list[i],"/analysis")
  
  bars_start <- sound$bars_start
  beats_start <- sound$beats_start
  sections_start <- sound$sections_start
  seg_loudness_max <- sound$segments_loudness_max
  seg_loudness_start <- sound$segments_loudness_start
  tatums_start <- sound$tatums_start
  
  # get quantiles and mean feature from raw data
  summary(bars_start)
  bars_start_f <- c(quantile(bars_start, probs = c(0,0.25,0.5,0.75,1)), mean(bars_start), sd(bars_start))
  beats_start_f <- c(quantile(beats_start, probs = c(0,0.25,0.5,0.75,1)), mean(beats_start),sd(beats_start))
  sections_start_f <- c(quantile(sections_start, probs = c(0.25,0.5,0.75,1)), mean(sections_start),sd(sections_start))
  tatums_start_f <- c(quantile(tatums_start, probs = c(0,0.25,0.5,0.75,1)), mean(tatums_start),sd(tatums_start))
  
  seg.dim <- length(seg_loudness_max)
  seg.seq <- seq(1, seg.dim, by = 1)
  seg.seq <- as.integer(quantile(seg.seq, probs = seq(0, 1, by = 0.05)))
  
  
  #seg_loudness_max_f <- c(quantile(seg_loudness_max, probs = c(0,0.25,0.5,0.75,1)), mean(seg_loudness_max),sd(seg_loudness_max))
  #seg_loudness_start_f <- c(quantile(seg_loudness_start, probs = c(0,0.25,0.5,0.75,1)), mean(seg_loudness_start),sd(seg_loudness_start))
  seg_loudness_max_f <- seg_loudness_max[seg.seq]
  seg_loudness_start_f <- seg_loudness_start[seg.seq]
  
  # for matrix feature, random select 20 vectors, from raw dataset
  #index_20 <- sample(dim(sound$segments_pitches)[2], 20, replace = TRUE)
  seg_pitches_f <- sound$segments_pitches[,seg.seq]
  seg_timbre_f <- sound$segments_timbre[,seg.seq]
  
  # add to a single row 
  a <- rbind(as.matrix(bars_start_f), as.matrix(beats_start_f), as.matrix(sections_start_f),
             as.matrix(tatums_start_f), as.matrix(seg_loudness_max_f), as.matrix(seg_loudness_start_f))
  for (j in 1 : length(seg.seq)){
    a <- rbind(a, as.matrix(seg_pitches_f[,j]))
  }
  for (j in 1 : length(seg.seq)){
    a <- rbind(a, as.matrix(seg_timbre_f[,j]))
  }
  test_100_feature_matrix[i,] <- t(a)
}

# remove NA value
for (i in 1 : dim(test_100_feature_matrix)[1]){
  for (j in 1 : dim(test_100_feature_matrix)[2]){
    if (is.na(test_100_feature_matrix[i,j]) == TRUE)
      test_100_feature_matrix[i,j] <- mean(test_100_feature_matrix[,j], na.rm = TRUE)
  }
}

# test

for (i in 1 : dim(test_100_feature_matrix)[1]){
  if (is.na(test_100_feature_matrix[i,]) == TRUE)
    print(i)
}

############################
# prediction
############################

# multinomial
feature_50dim_test_100 <- test_100_feature_matrix %*% feature_matrix_pca$rotation[,1:50]
feature_50dim_test_100 <- as.data.frame(feature_50dim_test_100)

#### multinomial test prediction
test_100_prediction_m <- predict(mulnomial.model, newdata = feature_50dim_test_100, "probs")
colnames(test_100_prediction_m)
prediction_word_rank_100_m <- matrix(NA, nrow = length(unique(Y)), ncol = dim(fit$topics)[2])
test_prediction_index_m <- as.integer(colnames(test_100_prediction_m))
for (i in 1 : length(colnames(test_100_prediction_m))){
  prediction_word_rank_100_m[i,] <- fit$topics[test_prediction_index_m[i],]
}
#View(prediction_word_rank_100)
View(prediction_word_rank_100_m)
View(fit$topics)
dim(test_100_prediction_m)
dim(prediction_word_rank_100_m)
test_word_modeling_m <- test_100_prediction_m %*% prediction_word_rank_100_m
colnames(test_word_modeling_m)<- colnames(fit$topics)

#### rank
rank.100_m <- matrix(NA, nrow = 100, ncol = dim(fit$topics)[2])
colnames(rank.100_m) <- colnames(fit$topics)
for (i in 1 : 100){
  rank.100_m[i,] <- rank(-test_word_modeling_m[i,], ties.method = "random")
}


###########
# random forest
feature_225dim_test_100 <- test_100_feature_matrix %*% feature_matrix_pca$rotation[,1:225]
feature_225dim_test_100 <- as.data.frame(feature_225dim_test_100)

rf_test_prediction_rf <- predict(rf.model, newdata = feature_225dim_test_100, "prob")
dim(rf_test_prediction_rf)
View(rf_test_prediction_rf)

colnames(rf_test_prediction_rf)
rf_prediction_word_rank_100_rf <- matrix(NA, nrow = length(unique(Y)), ncol = dim(fit$topics)[2])
rf_test_prediction_index_rf <- as.integer(colnames(rf_test_prediction_rf))
for (i in 1 : length(colnames(rf_test_prediction_rf))){
  rf_prediction_word_rank_100_rf[i,] <- fit$topics[rf_test_prediction_index_rf[i],]
}
View(fit$topics)
View(rf_prediction_word_rank_100_rf)
#View(prediction_word_rank_100)
dim(rf_test_prediction_rf)
dim(rf_prediction_word_rank_100_rf)
rf_test_word_modeling_rf <- rf_test_prediction_rf %*% rf_prediction_word_rank_100_rf
colnames(rf_test_word_modeling_rf)<- colnames(fit$topics)

#### rank
rank.100_rf <- matrix(NA, nrow = 100, ncol = dim(fit$topics)[2])
colnames(rank.100_rf) <- colnames(fit$topics)
for (i in 1 : 100){
  rank.100_rf[i,] <- rank(-rf_test_word_modeling_rf[i,], ties.method = "min")
}

View(rank.100_rf)

####################################################
# get prediction csv
number_index <- c(1,2,5:29)
number_assignment <- matrix(4987, ncol = length(number_index), nrow = 100)
test_submission[,(number_index + 2)] <- number_assignment
test_submission[,(c(3,4,30:5000) +2)] <- rank.100_rf
View(test_submission)

setwd("/Users/sun93/Documents/ADS/pro4")
getwd()
write.csv(test_submission, file = "/Users/sun93/Documents/ADS/pro4/submission_xs2254.csv")
save(test_submission, file = "submission_xs2254.Rdata")



