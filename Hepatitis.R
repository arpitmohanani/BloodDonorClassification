
###################### Installing Required Libraries ##########################


#Installing required packages
install.packages("naivebayes")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggfortify")

#importing required packages
library(dplyr)
library(ggplot2)
library(ggfortify)
library(naivebayes)




###################### Data Preparation ##########################


#reading the dataset
df <- read.csv(file.choose(), header = T)

#dropping irrelevant columns(features)
df$X = NULL
df$Sex = NULL

#displaying the summary of the data
summary(df)

#dropping all rows containing null value(s)
df <- df[!is.na(df$ALB),]
df <- df[!is.na(df$ALP),]
df <- df[!is.na(df$ALT),]
df <- df[!is.na(df$CHOL),]
df <- df[!is.na(df$PROT),]


#checking unique values in target column and changing it to factor type
unique(df[c("Category")])


#Univariate Analysis
ggplot(df, aes(factor(Category), fill = factor(Category))) + geom_bar()
ggplot(df, aes(factor(Age), fill = 'red')) + geom_bar()


df$Category[df$Category == "0s=suspect Blood Donor"] = "0=Blood Donor"
df$Category = as.factor(df$Category)





###################### Naive Bayes ##########################


#Dividing the dataframe into train and test 
ind = sample(2, nrow(df),replace = T, prob = c(0.8,0.2))
train = df[ind == 1,]
test = df[ind == 2,]


#Building Naive bayes model
model = naive_bayes(Category ~ ., data = train, usekernel = T)

#Prediction on training set, confusion matrix and accuracy calculation
pred1 = predict(model, train)
(confMatrix = table(pred1,train$Category))
sum(diag(confMatrix))/sum(confMatrix)

#Prediction on testing set, confusion matrix and accuracy calculation
pred2 = predict(model, test)
(confMatrix2 = table(pred2,test$Category))
sum(diag(confMatrix2))/sum(confMatrix2)








###################### K-means Clustering ##########################


#Removing first column which is the target column from the dataframe for unsupervised learning
clusterData = df[,-c(1,1)]

summary(clusterData)


#Function to return optimum number of clusters
wssplot = function(data, nc=15, seed=123)
{
  wss = (nrow(data)-1)*sum(apply(data, 2, var))
  for(i in 2:nc){
    set.seed(seed)
    wss[i] = sum(kmeans(data, centers = i)$withinss)
  }
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab = "within groups sum of squares")
}

#Function called
wssplot(clusterData)

#Building the model
KM = kmeans(clusterData, 4)

#Plotting clusters
autoplot(KM, clusterData, frame = TRUE)
