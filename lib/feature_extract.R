source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")
library(rhdf5)

# test
h5ls("/Users/sun93/Documents/ADS/pro4/Project4_data/data/A/A/A/TRAAABD128F429CF47.h5")
h5ls("/Users/sun93/Documents/ADS/pro4/Project4_data/data/A/B/B/TRABBOP128F931B50D.h5")
sound1<-h5read("/Users/sun93/Documents/ADS/pro4/Project4_data/data/A/A/A/TRAAABD128F429CF47.h5",
               "/analysis")
sound1$segments_pitches
sound2<-h5read("/Users/sun93/Documents/ADS/pro4/Project4_data/data/A/B/B/TRABBOP128F931B50D.h5",
              "/analysis")
sound2$segments_pitches


setwd("/Users/sun93/Documents/ADS/pro4/Fall2016-proj4-XuechunSun/data/feature_data")
getwd()
dir.h5 <- '/Users/sun93/Documents/ADS/pro4/Fall2016-proj4-XuechunSun/data/feature_data/'
files.list <- as.matrix(list.files(dir.h5, recursive = TRUE))
length(files.list)
files.list <- paste0(dir.h5,files.list)

# train files list 
files.list.train <- files.list[-ntrain]

# test files list 
files.list.test <- files.list[ntrain]

#########################################
# train feature extract
nsong <- 2250

feature_matrix <- matrix(NA, nrow = nsong, ncol = 573)
set.seed(333)
for (i in 1 : nsong){
  # get feature for each song
  a <- c()
  sound <- h5read(files.list.train[i],"/analysis")
  
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
  feature_matrix[i,] <- t(a)
}

 # remove NA value
for (i in 1 : dim(feature_matrix)[1]){
  for (j in 1 : dim(feature_matrix)[2]){
  if (is.na(feature_matrix[i,j]) == TRUE)
    feature_matrix[i,j] <- mean(feature_matrix[,j], na.rm = TRUE)
  }
}

# test

for (i in 1 : dim(feature_matrix)[1]){
    if (is.na(feature_matrix[i,]) == TRUE)
      print(i)
}
H5close()
str(feature_matrix)
View(feature_matrix)


#########################################
# test feature extract
## 1. get test features
nsong_test <- 100
feature_matrix_test <- matrix(NA, nrow = nsong_test, ncol = 573)
set.seed(333)
#length(files.list.test)
for (i in 1 : nsong_test){
  # get feature for each song
  a <- c()
  sound <- h5read(files.list.test[i],"/analysis")
  
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
  feature_matrix_test[i,] <- t(a)
}
dim(feature_matrix_test)
# remove NA value
for (i in 1 : dim(feature_matrix_test)[1]){
  for (j in 1 : dim(feature_matrix_test)[2]){
    if (is.na(feature_matrix_test[i,j]) == TRUE)
      feature_matrix_test[i,j] <- mean(feature_matrix_test[,j], na.rm = TRUE)
  }
}
# test
for (i in 1 : dim(feature_matrix_test)[1]){
  if (is.na(feature_matrix_test[i,]) == TRUE)
    print(i)
}


