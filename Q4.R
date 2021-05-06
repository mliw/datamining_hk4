library(readtext)
library(tm) 
library(caret) 
library(kernlab)
library(kknn)
library(e1071)


# 1 Load and preprocess data
author_list = list.dirs("Data/ReutersC50/C50train")
author_list = author_list[2:length(author_list)]
split_str = function(x) {
  tem_str = strsplit(x, "/")[[1]]
  return(tem_str[length(tem_str)])
}
for (i in 1:length(author_list)){
  author_list[i]=split_str(author_list[i])
}
train_data = data.frame()
test_data = data.frame()
for (name in author_list){
  tem_train = readtext(paste0("Data/ReutersC50/C50train/",name,"/*"),dvsep = "\n")
  tem_test = readtext(paste0("Data/ReutersC50/C50test/",name,"/*"),dvsep = "\n")
  
  tem_train_data = data.frame(tem_train$text)
  tem_train_data$author = name
  train_data = rbind(train_data,tem_train_data)
  
  tem_test_data = data.frame(tem_test$text)
  tem_test_data$author = name
  test_data = rbind(test_data,tem_test_data)
}
train_data$author <- as.factor(train_data$author)
test_data$author <- as.factor(test_data$author)
colnames(train_data) = c("text","author")
colnames(test_data) = c("text","author")

# Preprocess data
# Creating Corpus
train_data_corpus =  Corpus(VectorSource(train_data$text))
test_data_corpus =  Corpus(VectorSource(test_data$text))

# Clean data
train_data_corpus_clean = tm_map(train_data_corpus, tolower)
train_data_corpus_clean = tm_map(train_data_corpus_clean, removePunctuation)
train_data_corpus_clean = tm_map(train_data_corpus_clean, removeWords, stopwords())
train_data_corpus_clean = tm_map(train_data_corpus_clean, removeNumbers)
train_data_corpus_clean = tm_map(train_data_corpus_clean, stripWhitespace)

test_data_corpus_clean = tm_map(test_data_corpus, tolower)
test_data_corpus_clean = tm_map(test_data_corpus_clean, removePunctuation)
test_data_corpus_clean = tm_map(test_data_corpus_clean, removeWords, stopwords())
test_data_corpus_clean = tm_map(test_data_corpus_clean, removeNumbers)
test_data_corpus_clean = tm_map(test_data_corpus_clean, stripWhitespace)
# inspect(test_data_corpus_clean[1])
# inspect(train_data_corpus_clean[1])
# Change to matrix
train_mat = DocumentTermMatrix(train_data_corpus_clean)
freq_words = findFreqTerms(train_mat,5) # Select a part of words
train_mat = DocumentTermMatrix(train_data_corpus_clean, list(freq_words))
test_mat = DocumentTermMatrix(test_data_corpus_clean, list(freq_words))

# Convert x
trans_matrix = function(x) {
  return(x)
}
# train_mat_data test_mat_data
train_mat_data = apply(train_mat, MARGIN = 2, trans_matrix)
test_mat_data = apply(test_mat, MARGIN = 2, trans_matrix)
# Use common_names
colname_test_mat = colnames(test_mat_data)
common_names = intersect(freq_words,colname_test_mat)
train_mat_data = train_mat_data[,common_names]
test_mat_data = test_mat_data[,common_names]

# one-hot coding
onehot = function(x) {
  x = ifelse(x > 0, 1, 0)
  x = factor(x, levels = c(0, 1), labels = c("False", "True")) 
  return(x)
}
train_mat_data = apply(train_mat_data, MARGIN = 2, onehot)
test_mat_data = apply(test_mat_data, MARGIN = 2, onehot)

# Fitting
data_classifier = naiveBayes(train_mat_data, train_data$author)

# predicting
data_test_pred = predict(data_classifier, test_mat_data)
accuracy = mean(data_test_pred == test_data$author)
print(accuracy)


# [1] 0.674 ХыМе
                   
