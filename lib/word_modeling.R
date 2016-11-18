# load Bag of Word DATA
load("/Users/sun93/Documents/ADS/pro4/Project4_data/lyr.RData")
str(lyr)
dim(lyr)
lyr <- as.data.frame(lyr)

################################################################
# Topic Modeling
#### Construct documents list
lyr.new <- lyr[,-c(1,2,3,6:30)]
#View(lyr.new)
vocab <- colnames(lyr.new)
# length of vocab = 4973
length(vocab)
set.seed(333)

# get train index
ntrain <- sample(rep(1 : 2350,1), 100, replace = FALSE)

lyr.train <- lyr.new[-ntrain,]
dim(lyr.train)

# test data
lyr.test <- lyr.new[ntrain,]

nsong <- 2250
documents <- list()
for (i in 1: nsong){
  x <- as.integer(lyr.train[i,])
  doc.word <- c()
  doc.count <- c()
  k <- 0
  for (j in 1 : length(x)){
    if (x[j] > 0 & is.na(x[j]) == FALSE){
      k <- k + 1
      doc.word[k] <- vocab[j]
      doc.count[k] <- lyr.train[i,j]
    }
  }
  index <- match(doc.word, vocab) - 1
  index <- index[!is.na(index)]
  documents[[i]] <- as.matrix(rbind(as.integer(index), as.integer(doc.count)))
}

# Compute some statistics related to the data set:
D <- length(documents)  # number of documents (2,250)
W <- length(vocab)  # number of terms in the vocab (4,973)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [226, 139, 160, 203, 189, ...]
N <- sum(doc.length)  # total number of tokens in the data (550804)

# MCMC and model tuning parameters:
K <- 18
G <- 1000
alpha <- 0.1
eta <- 0.1

##### Fit the model:
library(lda)

t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1  # about 2 minutes on laptop

# check top 10 words for each topic
top.words <- top.topic.words(fit$topics, 10, by.score=TRUE)

colnames(top.words) <- paste0("Topic",1:18)
rownames(top.words) <- paste0("Top",1:10)
View(top.words)

################################################################
 # in results of assignments, n = index of topic - 1
 # level 0 means topic 1, level 1 means topic 2, etc...

topic_of_song <- matrix(NA, nrow = nsong, ncol = K+1)
str(table(fit$assignments[[1]]))
for (i in 1 : nsong){
  matrix.song <- as.matrix(table(fit$assignments[[i]]))
  sum.song <- sum(matrix.song[,1])
  t <- c(as.integer(rownames(matrix.song)) + 1)
  for (j in 1 : K){
    if (is.na(match(j, t)) == TRUE){
      topic_of_song[i,j] <- 0
    }
    else {
      topic_of_song[i,j] <- round(matrix.song[match(j, t),1]/sum.song,2)
    }
  }
  topic_of_song[i, K+1] <- as.integer(which(topic_of_song[i,1:K] == max(topic_of_song[i,1:K])))[1]
  if (length(which(topic_of_song[i,1:K] == max(topic_of_song[i,1:K]))) > 1)
    print(i)
}

colnames(topic_of_song)[K+1] <- "Final_Topic"
colnames(topic_of_song)[1:K] <- paste0("Topic",1:K)
rownames(topic_of_song)[1:nsong] <- paste0("Song",1:nsong)
View(topic_of_song)

table(topic_of_song[,K+1])

topic_of_song_matrix <- t(as.matrix(table(topic_of_song[,K+1])))
dim(topic_of_song_matrix)
rownames(topic_of_song_matrix) <- "Frequency"
View(topic_of_song_matrix)

par(mfrow = c(1,2))

#### word cloud
for (i in 1 : K){
  wordcloud(words = names(sort(fit$topics[i,], decreasing = TRUE)),
            as.vector(sort(fit$topics[i,], decreasing = TRUE)),scale=c(4,.7),
            colors=brewer.pal(8, "Dark2"),max.words = 90)
  text(x=0.5, y=1, paste("Word Cloud of Topic ", i))
}





