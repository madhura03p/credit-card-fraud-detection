creditc <- read.csv("F:/madhura/R/creditc.csv")

#data exploration
creditc$Fraud = factor(creditc$Fraud, levels = c(0,1))

head(creditc,7)

tail(creditc)

nrow(creditc)

ncol(creditc)

sum(is.na(creditc))

summary(creditc)

creditc$Zip[which(is.na(creditc$Zip))] = mean(is.na(creditc$Zip),na.rm = TRUE)

sum(is.na(creditc))

table(creditc$Fraud)

barplot(table(creditc$Fraud))

# Legit and fraud transactions in the data sets
prop.table(table(creditc$Fraud))

library(caret)
#pie chart
labels <- c("legit","fraud")
labels <- paste(labels, round(100*prop.table(table(creditc$Fraud)),2))
labels <- paste(labels ,"%")

par("mar")
par(mar=c(1,1,1,1))
par("mar")

pie(table(creditc$Fraud),labels, col = c("orange" , "red"),
    main = "Pie chart of Credit Card Transactions")

set.seed(123)

#splitting dataset into training and testing
k = sample(1:nrow(creditc), 0.8*nrow(creditc))

train = creditc[k,]

test = creditc[-k,]


library(randomForest)

library(ROSE)

#Random under-sampling

sampling = ovun.sample(Fraud ~., data = train, method = "both",seed = 123)$data


table(sampling$Fraud)

barplot(table(sampling$Fraud), xlab ="Data distribution", ylab = "Number of observation", main = "Splitting of data into sample and test")

classifier_both =randomForest(Fraud ~ Card + Year+ Amount + MCC, data = sampling)

fraud_pred_both = predict(classifier_both,test)

test$fraud_pred_both = fraud_pred_both

table(test$Fraud)

table(test$fraud_pred_both)

#Random over-sampling

oversampling = ovun.sample(Fraud ~., data = train, method = "over",seed = 123)$data

table(oversampling$Fraud)

barplot(table(oversampling$Fraud))

classifier_over = randomForest(Fraud ~ Card + Year+ Amount + MCC, data = oversampling)

fraud_pred_over = predict(classifier_over,test)

test$fraud_pred_over = fraud_pred_over

table(test$Fraud)

table(test$fraud_pred_over)

#install.packages("rpart")
library(rpart)

# Decision tree without SMOTE

CART_model <-rpart(Fraud~ ., train[,-1])
library(rpart.plot)
rpart.plot(CART_model,extra = 0, type = 5, tweak = 1.2)

#Predict fraud classes
predicted_val <-predict(CART_model, test[-1], type = 'class')
confusionMatrix(predicted_val, test$Fraud)
