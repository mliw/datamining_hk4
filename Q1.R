library(psych)
library(factoextra)
library(ModelMetrics)
library(nnet)


# 1 Clustering and PCA
wine = read.csv("data/wine.csv", stringsAsFactors=TRUE)
wine$color = as.numeric(wine$color=="red")
test_features = wine[,1:11]
# We have 6497 features
print(dim(wine))

# 1_1 Benchmark model
# Benchmark model for color
model <- glm(color ~ .-color-quality, family = binomial(), data=wine)
summary(model)
fitted_class = predict(model,type = "response")
# The f1-score is 
print(f1Score(wine$color,fitted_class))
# Benchmark model for quality
model <- lm(quality ~ .-color-quality, data=wine)
summary(model)
fitted_value = predict(model)
# The rmse is 
print(rmse(wine$quality,fitted_value))

# 1_2 PCA model
pca = prcomp(test_features, nfactors=3, rotate="varimax", scores=TRUE,scale = TRUE)
# The summary of pca is:
print(summary(pca))
pca_data = predict(pca,test_features)
pca_data = as.data.frame(pca_data)
pca_data$color = wine$color
pca_data$quality = wine$quality
# pca model for color(Only one component)
model <- glm(color ~ PC1, family = binomial(), data=pca_data)
fitted_class = predict(model,type = "response")
# The f1-score is 
f1Score(pca_data$color,fitted_class)
# pca model for quality
model <- lm(quality ~PC1, data=pca_data)
summary(model)
fitted_value = predict(model)
# The rmse is 
print(rmse(pca_data$quality,fitted_value))

# 1_3 Kmeans model
set.seed(100)
kmeans_features = kmeans(test_features, centers=2)
wine$new_cluster = kmeans_features$cluster
print(kmeans_features$centers)
# The details of kmeans model for color(Only two classes)
model <- glm(color ~ new_cluster, family = binomial(), data=wine)
print(summary(model))
fitted_class = predict(model,type = "response")
# The f1-score is 
print(f1Score(wine$color,fitted_class))
# kmeans model for quality
model <- lm(quality ~ new_cluster, data=wine)
summary(model)
fitted_value = predict(model)
# The rmse is 
print(rmse(wine$quality,fitted_value))

