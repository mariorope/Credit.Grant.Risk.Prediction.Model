{r}
# Loading packages
library(dplyr)
library(plyr)
library(corrplot)
library(ggplot2)
library(ROSE)
library(caret)
library(randomForest)
library(e1071)
library(naivebayes)

# Loading the source script 'plot_utils.R'
source('fun_utils.R')

# Setting seed
set.seed(123)

{r}
# Loading dataset
df <- read.csv('credit_dataset.csv')

# Checking data types
glimpse(df)

# Visualizing the dataset
View(df)

# Calculating correlations between numeric variables and target variable
df_cor <- cor(df[,c(1,3,6,14)])

# Visualizing correlations
df_cor

# Plotting correlations
corrplot(df_cor)

# Looking at the distribution of the variable age
ggplot(data = df) +
  geom_histogram(aes(x = age), bins = 10)

# Creating vectors containing the class for age breaks (1 and 2)
age_div_class1 <- round(seq(18, max(df$age) + 3, 5),0)
age_div_class2 <- round(seq(18, max(df$age) + 3, 10),0)

# Creating two new categorical variables, named age_class1 and age_class2,
# based on the vectors containing the class for age breaks (1 and 2)
df <- dplyr::mutate(df,
                    age_class1 = cut(age, breaks = age_div_class1),
                    age_class2 = cut(age, breaks = age_div_class2))

# Plotting the number of clients by age_class1
ggplot(df) +
  geom_bar(aes(x = age_class1))

# Plotting the number of clients by age_class2
ggplot(df) +
  geom_bar(aes(x = age_class2))

# Looking at the distribution of the variable credit.duration.months
ggplot(data = df) +
  geom_histogram(aes(x = credit.duration.months), bins = 10)

# Creating vectors containing the class breaks for credit.duration.months (1 and 2)
credit.duration_div_class1 <- round(seq(0, max(df$credit.duration.months), 6),0)
credit.duration_div_class2 <- round(seq(0, max(df$credit.duration.months), 12),0)

# Creating two new categorical variables, named credit.duration_class1 and
# credit.duration_class2, based on the vectors containing the class for
# credit.duration.months breaks (1 and 2)
df <- dplyr::mutate(df,
                    credit.duration_class1 = cut(credit.duration.months, breaks = credit.duration_div_class1),
                    credit.duration_class2 = cut(credit.duration.months, breaks = credit.duration_div_class2))

# Plotting the number of clients by credit.duration_class1
ggplot(df) +
  geom_bar(aes(x = credit.duration_class1))

# Plotting the number of clients by credit.duration_class1
ggplot(df) +
  geom_bar(aes(x = credit.duration_class2))

# Looking at the distribution of the variable credit.amount
ggplot(data = df) +
  geom_histogram(aes(x = credit.amount), bins = 10)

# Creating vectors containing the class breaks for credit.amount (1, 2 and 3)
credit.amount_div_class1 <- round(seq(0, round_any(max(df$credit.amount), 1000, f = ceiling), 1000), 0)

credit.amount_div_class2 <- round(seq(0, round_any(max(df$credit.amount), 3000, f = ceiling), 3000), 0)

credit.amount_div_class3 <- round(seq(0, round_any(max(df$credit.amount), 5000, f = ceiling), 5000), 0)

# Creating three new categorical variables, named credit.amount_class1,
# credit.amount_class2 and credit.amount_class3, based on the vectors
# containing the class for credit.amount breaks (1 and 2)
df <- dplyr::mutate(df,
                    credit.amount_class1 = cut(credit.amount, breaks = credit.amount_div_class1),
                    credit.amount_class2 = cut(credit.amount, breaks = credit.amount_div_class2),
                    credit.amount_class3 = cut(credit.amount, breaks = credit.amount_div_class3))

# Plotting the number of clients by credit.amount_class1
ggplot(df) +
  geom_bar(aes(x = credit.amount_class1)) +
  theme(
    axis.text.x = element_text(angle = 90)
  )

# Plotting the number of clients by credit.amount_class2
ggplot(df) +
  geom_bar(aes(x = credit.amount_class2)) +
  theme(
    axis.text.x = element_text(angle = 90)
  )

# Plotting the number of clients by credit.amount_class3
ggplot(df) +
  geom_bar(aes(x = credit.amount_class3)) +
  theme(
    axis.text.x = element_text(angle = 90)
  )

# Let's check our new variables in the modified dataset
View(df)

# Checking if the target variable (credit.rating) is balanced
prop.table(table(df$credit.rating))

# Checking the data types again
glimpse(df)

# Transforming all variables (excepted the ones that are already factors) to factor
df <- to_factors(df, colnames(df)[-c(22:28)])

# Creating the df.bal dataset with ROSE, which should be a balanced now
df.bal <- ROSE(credit.rating ~ ., data=df, N = 1000, p = 0.5)$data

# Checking if the target variable (credit.rating) is now balanced!
prop.table(table(df.bal$credit.rating))

# Creating a vector containing all the names of numerical variables
numericVars <- c('credit.duration.months',
                 'credit.amount',
                 'age')

# Executing the function (to_numeric) to transform the numerical variables to numeric type
df.bal <- to_numeric(df.bal, numericVars)

# Transforming credit.rating to integer
df.bal$credit.rating <- as.integer(as.character(df.bal$credit.rating))

# Calculating correlations
df.bal_cor <- cor(df.bal[,c(1,3,6,14)])

# Visualizing correlations
df.bal_cor

# Plotting correlations
corrplot(df.bal_cor)

# Transforming credit.rating back to factor
df.bal$credit.rating <- as.factor(df.bal$credit.rating)

# Preparing plotting area to receive four plots in one area
par(mfrow=c(2,2))

# Calculating the means age for both datasets (df and df.bal), considering both target labels
mean(as.numeric(df[df$credit.rating == '0',]$age))
mean(df.bal[df.bal$credit.rating == '0',]$age)
mean(as.numeric(df[df$credit.rating == '1',]$age))
mean(df.bal[df.bal$credit.rating == '1',]$age)

# Plotting histograms for age, consideting both datasets and target labels
hist(as.numeric(df[df$credit.rating == '0',]$age), breaks = 5)
hist(as.numeric(df[df$credit.rating == '1',]$age), breaks = 5)
hist(df.bal[df.bal$credit.rating == '0',]$age, breaks = 5)
hist(df.bal[df.bal$credit.rating == '1',]$age, breaks = 5)

# Preparing plotting area to receive four plots in one area
par(mfrow=c(2,2))

# Calculating the means age for both datasets (df and df.bal), considering both target labels
mean(as.numeric(df[df$credit.rating == '0',]$credit.duration.months))
mean(df.bal[df.bal$credit.rating == '0',]$credit.duration.months)
mean(as.numeric(df[df$credit.rating == '1',]$credit.duration.months))
mean(df.bal[df.bal$credit.rating == '1',]$credit.duration.months)

# Plotting histograms for age, consideting both datasets and target labels
hist(as.numeric(df[df$credit.rating == '0',]$credit.duration.months), breaks = 5)
hist(as.numeric(df[df$credit.rating == '1',]$credit.duration.months), breaks = 5)
hist(df.bal[df.bal$credit.rating == '0',]$credit.duration.months, breaks = 5)
hist(df.bal[df.bal$credit.rating == '1',]$credit.duration.months, breaks = 5)

# Preparing plotting area to receive four plots in one area
par(mfrow=c(2,2))

# Calculating the means age for both datasets (df and df.bal), considering both target labels
mean(as.numeric(df[df$credit.rating == '0',]$credit.amount))
mean(df.bal[df.bal$credit.rating == '0',]$credit.amount)
mean(as.numeric(df[df$credit.rating == '1',]$credit.amount))
mean(df.bal[df.bal$credit.rating == '1',]$credit.amount)

# Plotting histograms for age, consideting both datasets and target labels
hist(as.numeric(df[df$credit.rating == '0',]$credit.amount), breaks = 5)
hist(as.numeric(df[df$credit.rating == '1',]$credit.amount), breaks = 5)
hist(df.bal[df.bal$credit.rating == '0',]$credit.amount, breaks = 5)
hist(df.bal[df.bal$credit.rating == '1',]$credit.amount, breaks = 5)

# Setting plotting area back to one plot
par(mfrow=c(1,1))

# Checking df.bal for NA values
sum(is.na(df.bal))

# Scalling numeric variables
df.bal <- to_scale(df.bal, numericVars)

# Transforming scaled variables (type matrix) back to numeric again
df.bal <- to_numeric(df.bal, numericVars)

# Checking the changes in data types
glimpse(df.bal)

# class1
df.bal %>%
  group_by(credit.rating, age_class1) %>%
  dplyr::summarise(temp = n()) %>%
  dplyr::arrange(desc(age_class1)) %>%
  ggplot() +
  geom_bar(aes(x=age_class1, y=temp, fill=credit.rating),
           stat = 'identity',
           position = position_dodge())

# class2
df.bal %>%
  group_by(credit.rating, age_class2) %>%
  dplyr::summarise(temp = n()) %>%
  dplyr::arrange(desc(age_class2)) %>%
  ggplot() +
  geom_bar(aes(x=age_class2, y=temp, fill=credit.rating),
           stat = 'identity',
           position = position_dodge())

# class1
df.bal %>%
  group_by(credit.rating, credit.duration_class1) %>%
  dplyr::summarise(temp = n()) %>%
  dplyr::arrange(desc(credit.duration_class1)) %>%
  ggplot() +
  geom_bar(aes(x=credit.duration_class1, y=temp, fill=credit.rating),
           stat = 'identity',
           position = position_dodge())

# class2
df.bal %>%
  group_by(credit.rating, credit.duration_class2) %>%
  dplyr::summarise(temp = n()) %>%
  dplyr::arrange(desc(credit.duration_class2)) %>%
  ggplot() +
  geom_bar(aes(x=credit.duration_class2, y=temp, fill=credit.rating),
           stat = 'identity',
           position = position_dodge())

# class1
df.bal %>%
  group_by(credit.rating, credit.amount_class1) %>%
  dplyr::summarise(temp = n()) %>%
  dplyr::arrange(desc(credit.amount_class1)) %>%
  ggplot() +
  geom_bar(aes(x=credit.amount_class1, y=temp, fill=credit.rating),
           stat = 'identity',
           position = position_dodge()) +
  theme(
    axis.text.x = element_text(angle = 90)
  )

# class2
df.bal %>%
  group_by(credit.rating, credit.amount_class2) %>%
  dplyr::summarise(temp = n()) %>%
  dplyr::arrange(desc(credit.amount_class2)) %>%
  ggplot() +
  geom_bar(aes(x=credit.amount_class2, y=temp, fill=credit.rating),
           stat = 'identity',
           position = position_dodge()) +
  theme(
    axis.text.x = element_text(angle = 90)
  )

# class3
df.bal %>%
  group_by(credit.rating, credit.amount_class3) %>%
  dplyr::summarise(temp = n()) %>%
  dplyr::arrange(desc(credit.amount_class3)) %>%
  ggplot() +
  geom_bar(aes(x=credit.amount_class3, y=temp, fill=credit.rating),
           stat = 'identity',
           position = position_dodge()) +
  theme(
    axis.text.x = element_text(angle = 90)
  )

options(warn = -1)

# Creating the lists to hold the results
vList <- list()
chiList <- list()

# Performing the test for all categorical variables combined with the target variable
for (i in colnames(df.bal)[-c(1,3,6,14)]) {
  chi <- chisq.test(df.bal$credit.rating, df.bal[,i])
  tbl <- table(df.bal$credit.rating, df.bal[,i])
  
  v <- sqrt(chi$statistic / sum(tbl))
  vList[[i]] <- v
  chiList[[i]] <- chi
}

# Creating data.frame to hold all results from chi-squared test
chisq_test_results <- data.frame('X_squared' = rep(0,length(colnames(df.bal)[-c(1,3,6,14)])),
                                 'p-value' = rep(0,length(colnames(df.bal)[-c(1,3,6,14)])),
                                 "Crammer_index_V" = rep(0,length(colnames(df.bal)[-c(1,3,6,14)])),
                                 row.names = colnames(df.bal)[-c(1,3,6,14)])

# Filling in the new data.frame with the results of the previous test
for (i in 1:length(chisq_test_results$X_squared)) {
  chisq_test_results[i,1] <- chiList[[i]]$statistic
  chisq_test_results[i,2] <- chiList[[i]]$p.value
  chisq_test_results[i,3] <- vList[[i]]
}

# Sorting the dataframe acording to Crammer's index in a descent order
chisq_test_results <- chisq_test_results %>%
  dplyr::arrange(desc(Crammer_index_V))

# Visualizing the test results in a table
chisq_test_results

# Creating a model with random forest
rf_varSelection <- randomForest(credit.rating ~ ., data = df.bal, importance = TRUE)

# Visualizing the results
rf_varSelection

# Plotting variables importance
varImpPlot(rf_varSelection)

# Removing class2 and class3 variables
df.bal <- df.bal %>%
  select(-c(23, 25, 27, 28))

# Creating another model with random forest
rf_varSelection2 <- randomForest(credit.rating ~ ., data = df.bal, importance = TRUE)

# Visualizing the results
rf_varSelection2

# Plotting variables importance
varImpPlot(rf_varSelection2)

# Creating another model with random forest
rf_varSelection3 <- randomForest(credit.rating ~ ., data = df.bal[,-c(3,6,14)], importance = TRUE)

# Visualizing the results
rf_varSelection3

# Plotting variables importance
varImpPlot(rf_varSelection3)

# Creating another model with random forest
rf_varSelection4 <- randomForest(credit.rating ~ ., data = df.bal[,-c(22,23,24)], importance = TRUE)

# Visualizing the results
rf_varSelection4

# Plotting variables importance
varImpPlot(rf_varSelection4)

# Removing the numerical variables
df.bal <- df.bal[,-c(3,6,14)]

# Checking data types again
glimpse(df.bal)

options(warn = -1)
# Creating the lists to hold the results
vList2 <- list()
chiList2 <- list()

# Performing the test for all categorical variables combined with the target variable
for (i in colnames(df.bal)[-1]) {
  chi <- chisq.test(df.bal$credit.rating, df.bal[,i])
  tbl <- table(df.bal$credit.rating, df.bal[,i])
  
  v <- sqrt(chi$statistic / sum(tbl))
  vList2[[i]] <- v
  chiList2[[i]] <- chi
}

# Creating data.frame to hold all results from chi-squared test
chisq_test_results2 <- data.frame('X_squared' = rep(0,length(colnames(df.bal)[-1])),
                                  'p-value' = rep(0,length(colnames(df.bal)[-1])),
                                  "Crammer_index_V" = rep(0,length(colnames(df.bal)[-1])),
                                  row.names = colnames(df.bal)[-1])

# Filling in the new data.frame with the results of the previous test
for (i in 1:length(chisq_test_results2$X_squared)) {
  chisq_test_results2[i,1] <- chiList2[[i]]$statistic
  chisq_test_results2[i,2] <- chiList2[[i]]$p.value
  chisq_test_results2[i,3] <- vList2[[i]]
}

# Sorting the dataframe acording to Crammer's index in a descent order
chisq_test_results2 <- chisq_test_results2 %>%
  dplyr::arrange(desc(Crammer_index_V))

# Visualizing the test results in a table
chisq_test_results2

# First set of predictors - entire dataset
testVars1 <- colnames(df.bal)
writeLines(c('Predictors:', testVars1))

# Second set of predictors - removing a few variables that presented p-value > 0.05 during the chi-squared test
testVars2 <- testVars1[-c(9,10,15,16)]
writeLines(c('Predictors:', testVars2))

testVars3 <- testVars2[-13]
writeLines(c('Predictors:', testVars3))

testVars4 <- testVars3[-12]
writeLines(c('Predictors:', testVars4))

testVars5 <- testVars4[-7]
writeLines(c('Predictors:', testVars5))

testVars6 <- testVars5[-11]
writeLines(c('Predictors:', testVars6))

testVars7 <- testVars6[-10]
writeLines(c('Predictors:', testVars7))

testVars8 <- testVars7[-9]
writeLines(c('Predictors:', testVars8))

testVars9 <- testVars8[-7]
writeLines(c('Predictors:', testVars9))

testVars10 <- testVars9[-6]
writeLines(c('Predictors:', testVars10))

testVars11 <- testVars10[-6]
writeLines(c('Predictors:', testVars11))

testVars12 <- testVars11[-6]
writeLines(c('Predictors:', testVars12))

testVars13 <- testVars12[-4]
writeLines(c('Predictors:', testVars13))

# Visualizing the importance of variables
data.frame(feature = rownames(varImp(rf_varSelection3)),
           importance_value = data.frame(varImp(rf_varSelection3))$X0) %>%
  dplyr::arrange(desc(importance_value))

testVars14 <- c('credit.rating', rownames(chisq_test_results2)[c(1,2,3,4,7)])
writeLines(c('Predictors:', testVars14))

testVars15 <- c('credit.rating', rownames(chisq_test_results2)[c(1,2,3,4,7,8)])
writeLines(c('Predictors:', testVars15))

testVars16 <- c('credit.rating', rownames(chisq_test_results2)[c(1,2,3,4,6,7,8)])
writeLines(c('Predictors:', testVars16))

testVars17 <- c('credit.rating', rownames(chisq_test_results2)[c(1,2,3,4,6,7,8,9)])
writeLines(c('Predictors:', testVars17))

testVars18 <- c('credit.rating', rownames(chisq_test_results2)[c(1,2,3,4,6,7,8,9,14)])
writeLines(c('Predictors:', testVars18))

testVars19 <- c('credit.rating', rownames(chisq_test_results2)[c(1,2,3,4,5,6,7,8,9,14)])
writeLines(c('Predictors:', testVars19))

testVars20 <- c('credit.rating', rownames(chisq_test_results2)[c(1,2,3,4,5,6,7,8,9,14,17)])
writeLines(c('Predictors:', testVars20))

testVars21 <- c('credit.rating', rownames(chisq_test_results2)[c(1,2,3,4,5,6,7,8,9,10,14,17)])
writeLines(c('Predictors:', testVars21))

testVars22 <- c('credit.rating', rownames(chisq_test_results2)[c(1,2,3,4,5,6,7,8,9,10,11,14,17)])
writeLines(c('Predictors:', testVars22))

testVars23 <- c('credit.rating', rownames(chisq_test_results2)[c(1,2,3,4,5,6,7,8,9,10,11,14,17,18)])
writeLines(c('Predictors:', testVars23))

testVars24 <- c('credit.rating', rownames(chisq_test_results2)[c(1,2,3,4,5,6,7,8,9,10,11,12,14,17,18)])
writeLines(c('Predictors:', testVars24))

testVars25 <- c('credit.rating', rownames(chisq_test_results2)[c(1,2,3,4,5,6,7,8,9,10,11,12,14,15,17,18)])
writeLines(c('Predictors:', testVars25))

testVars26 <- c('credit.rating', rownames(chisq_test_results2)[c(1,2,3,4,5,6,7,8,9,10,11,12,14,15,16,17,18)])
writeLines(c('Predictors:', testVars26))

# Preparing a list with all the set of variables that will be tested
testList <- list(testVars1, testVars2, testVars3, testVars4, testVars5, testVars6, testVars7, testVars8, testVars9, testVars10, testVars11, testVars12, testVars13, testVars14, testVars15, testVars16, testVars17, testVars18, testVars19, testVars20, testVars21, testVars22, testVars23,
                 testVars24, testVars25, testVars26)

# Creating an index list to separate the train and test datasets
indexes_t <- as.integer(sample(rownames(df.bal), size = 0.7*length(rownames(df.bal)), replace = F))

# Dividing the dataset into train_data and test_data
train_data <- df.bal[indexes_t,]
test_data <- df.bal[-indexes_t,]

# Checking the dimensions of the train_data and test_data
dim(train_data)
dim(test_data)

# Creating two lists to hold the results and the accuracy of the RF models

rfModelList <- list(rep(0,length(testList)))
rfTestAccuracyList <- list(rep(0,length(testList)))

# Iterating through the testing variables list to create the strings that
# will represent the formula for the training
for (i in 1:length(testList)) {
  temp1 <- paste(testList[[i]][1:2], collapse = ' ~ ')
  temp2 <- paste(testList[[i]][3:length(testList[[i]])], collapse = ' + ')
  temp3 <- paste(temp1, temp2, sep = ' + ')
  
  # Transforming the string version of a formula to a proper formula
  formula1 <- as.formula(object = temp3)
  
  # Performing the training with RF
  rf <- randomForest(formula1, data = train_data)
  
  # Appending new results of the created models into the RF model list
  rfModelList[[i]] <- rf
  
  # Making the predictions for the test data
  pred <- predict(object = rf, newdata = test_data[,testList[[i]][-1]])
  
  # Calculating the accuracy of the model and appending it to the RF accuracy model list
  rfTestAccuracyList[[i]] <- round(sum(pred == test_data[,1]) / length(test_data[,1]) * 100, 2)
}

# Creating two lists to hold the results and the accuracy of the SVM Linear models

svmLinearModelList <- list(rep(0,length(testList)))
svmLinearTestAccuracyList <- list(rep(0,length(testList)))

# Iterating through the testing variables list to create the strings that

# will represent the formula for the training

for (i in 1:length(testList)) {
  
  temp1 <- paste(testList[[i]][1:2], collapse = ' ~ ')
  
  temp2 <- paste(testList[[i]][3:length(testList[[i]])], collapse = ' + ')
  
  temp3 <- paste(temp1, temp2, sep = ' + ')
  
  # Transforming the string version of a formula to a proper formula
  formula1 <- as.formula(object = temp3)
  
  # Performing the training with SVM linear
  svm1 <- svm(formula = formula1, data = train_data, type = 'C-classification', kernel = 'linear')
  
  # Appending new results of the created models into the SVM linear model list
  svmLinearModelList[[i]] <- svm1
  
  # Making the predictions for the test data
  pred <- predict(object = svm1, newdata = test_data[,testList[[i]][-1]])
  
  # Calculating the accuracy of the model and appending it to the SVM linear accuracy model list
  svmLinearTestAccuracyList[[i]] <- round(sum(pred == test_data[,1]) / length(test_data[,1]) * 100, 2)
}

# Creating two lists to hold the results and the accuracy of the SVM Radial models
svmRadialModelList <- list(rep(0,length(testList)))
svmRadialTestAccuracyList <- list(rep(0,length(testList)))

# Iterating through the testing variables list to create the strings that
# will represent the formula for the training
for (i in 1:length(testList)) {
  temp1 <- paste(testList[[i]][1:2], collapse = ' ~ ')
  temp2 <- paste(testList[[i]][3:length(testList[[i]])], collapse = ' + ')
  temp3 <- paste(temp1, temp2, sep = ' + ')
  
  # Transforming the string version of a formula to a proper formula
  formula1 <- as.formula(object = temp3)
  
  # Performing the training with SVM radial
  svm1 <- svm(formula = formula1,
              data = train_data,
              type = 'C-classification',
              kernel = 'radial')
  
  # Appending new results of the created models into the SVM radial model list
  svmRadialModelList[[i]] <- svm1
  
  # Making the predictions for the test data
  pred <- predict(object = svm1, newdata = test_data[,testList[[i]][-1]])
  
  # Calculating the accuracy of the model and appending it to the SVM radial accuracy model list
  svmRadialTestAccuracyList[[i]] <- round(sum(pred == test_data[,1]) / length(test_data[,1]) * 100, 2)
}

# Creating two lists to hold the results and the accuracy of the NB models
nbModelList <- list(rep(0,length(testList)))
nbTestAccuracyList <- list(rep(0,length(testList)))

# Iterating through the testing variables list to create the strings that
# will represent the formula for the training
for (i in 1:length(testList)) {
  temp1 <- paste(testList[[i]][1:2], collapse = ' ~ ')
  temp2 <- paste(testList[[i]][3:length(testList[[i]])], collapse = ' + ')
  temp3 <- paste(temp1, temp2, sep = ' + ')
  
  # Transforming the string version of a formula to a proper formula
  formula1 <- as.formula(object = temp3)
  
  # Performing the training with NB
  nb <- naive_bayes(formula = formula1,
                    data = train_data,
                    usekernel = T,
                    laplace = 1)
  
  # Appending new results of the created models into the NB model list
  nbModelList[[i]] <- nb
  
  # Making the predictions for the test data
  pred <- predict(object = nb, newdata = test_data[,testList[[i]][-1]])
  
  # Calculating the accuracy of the model and appending it to the NB accuracy model list
  nbTestAccuracyList[[i]] <- round(sum(pred == test_data[,1]) / length(test_data[,1]) * 100, 2)
}

# Visualizing the best accuracy for each machine learning algorithm tested
max(data.frame(nbTestAccuracyList))
max(data.frame(svmRadialTestAccuracyList))
max(data.frame(svmLinearTestAccuracyList))
max(data.frame(rfTestAccuracyList))

rfTestAccuracyList

testVars20

# Making the predictions again using the test data
pred <- predict(rfModelList[[20]], test_data[,-1])

# Creating a Confusion Matrix for the Prediction X Observed
confMat <- confusionMatrix(data = pred, reference = test_data[,1])

# Calculating error_rate of model ( (FN + FP) / (TP + TN + FP + FN) )
error_rate <- (confMat$table[1,2] + confMat$table[2,1]) / sum(confMat$table)

# Calculating the accuracy of the model ( 1 - error_rate )
accuracy <- 1 - error_rate

# Calculating the precision of the model (sensitivity) ( TP / (FP + TP) )
precision <- confMat$table[2,2] / (confMat$table[2,1] + confMat$table[2,2])

# Calculating the recall of the model ( TP / (FN + TP) )
recall <- confMat$table[2,2] / (confMat$table[1,2] + confMat$table[2,2])

# Calculating the f-measure of model ( (2 \* Precision \* recall) / (Precision \* recall) )
f <- (2 * precision * recall) / (precision + recall)

# Visualizing the results on the screen
writeLines(c('Error rate: ', paste0(round(error_rate * 100,1), '%'),
             'Accuracy: ', paste0(round(accuracy * 100,1), '%'),
             'Precision: ', paste0(round(precision * 100,1), '%'),
             'Recall: ', paste0(round(recall * 100,1), '%'),
             'f-measure: ', paste0(round(f * 100,1), '%'),
             '\n',
             'Sensitivity: ', round(precision, 3),
             'Specificity: ', round(confMat$table[1,1] / (confMat$table[1,2] + confMat$table[1,1]), 3)))

print('Confusion Matrix')
print(confMat$table)

# Creating a predictions object to plot the ROC and the PR graphics
predictions <- prediction(as.integer(as.character(test_data[,1])), as.integer(as.character(pred)))

# Preparing the plotting area to receive two plots
par(mfrow = c(1,2))

# Plotting the ROC curve using the functions from plot_utils.R
plot.roc.curve(predictions, title.text = "Curva ROC")

# Plotting the PR curve using the functions from plot_utils.R
plot.pr.curve(predictions, title.text = "Curva Precision/Recall")

