library(readxl)
library(psych)
library(caret)
head(CSM)

CSM <- CSM[,2:14]
str(CSM)


colSums(is.na(CSM))

CSM <- CSM[, -c(5,6,13)]
str(CSM)

#cor(CSM, method = "spearman")

pairs.panels(CSM)
summary(CSM)

CSM$Ratings <- as.factor(CSM$Ratings)
CSM$Year <- as.numeric(CSM$Year)
CSM$Genre <- as.numeric(CSM$Genre)
CSM$Sequel <- as.numeric(CSM$Sequel)


str(CSM)

#Split data
set.seed(123)
sampel <- sample(2, nrow(CSM), replace = T, prob = c(0.8,0.2))
trainingdat <- CSM[sampel==1, ]
testingdat <- CSM[sampel==2, ]
print(paste("Jumlah Train Data: ", nrow(trainingdat), "| Jumlah Test Data: ", nrow(testingdat)))

model1<-glm(Ratings~., data=trainingdat, family = "binomial", maxit = 100)
summary(model1)

model2<-glm(Ratings~Likes, data=trainingdat, family = "binomial", maxit = 100)
summary(model2)

coefficients(model1)

prediksilogreg <- predict(model1, testingdat, type="response") #output berupa peluang
prediksilogreg

pred <- ifelse(prediksilogreg>0.25, 0.5, 0.75, 0)
pred
