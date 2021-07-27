
#can we predict the type of load conditions that will be for a given hour in NYC, given only air temp, prices and congestion?

rm(list=ls())

#install and load any required packages
if (!require("pacman")) install.packages("pacman")
p_load(caret,tidyverse,readr, tidyquant)

# Load data
data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTIjoOjq4Re7UioMhXRPSPK2XWx4I_Ehr07V1wprxio9GTMUARCcbf9jQcS7w5p0O4i7F-iLbwwB-d6/pub?output=csv", col_types = cols(date = col_date(format = "%Y-%m-%d")))

# Rename some columns for friendliness
colnames(data)[3] <- "Price"
colnames(data)[4] <- "Load"
colnames(data)[5] <- "Congestion"

# visualize the dataset to predict on
visualModelData <- data %>%
      ggplot(aes(x = date, y = Load)) +
      geom_point(alpha = 0.5, color = palette_light()[[1]]) +
      labs(title = "NYC Load Values at HE 16", x = "Date") +
      theme_tq()

visualModelData

# Classify the load levels into 5 bins, High, med-high, med, med-low, low
data$loadClass <- ifelse(data$Load > 9500,"High",
                     ifelse(data$Load > 7500 & data$Load <= 9500,"Med-high",
                            ifelse(data$Load > 6200 & data$Load <= 7500, "Normal",
                                   ifelse(data$Load > 5000 & data$Load <= 6200, "Med-Low",
                                          ifelse(data$Load > 3800 & data$Load <= 5000, "Low","")))))

# Select the final variables for the training set
data <- data %>%
      select(Price, Load, Congestion, Air_Temp, Dewpoint,loadClass)

# Split data into a test index, then into training and test sets
set.seed(1972)
test_index <- createDataPartition(y = data$loadClass, times = 1, p = 0.1, list = FALSE)
train <- data[-test_index, ]
test <- data[test_index, ]

# Set up the models to use 10-fold cross validation and Accuracy metrics
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# 1 - linear algorithm
set.seed(1972)
lda <- train(loadClass~., data=train, method="lda", metric=metric, trControl=control)

# 2 - nonlinear algorithm
set.seed(1972)
cart <- train(loadClass~., data=train, method="rpart", metric=metric, trControl=control)

# 3 - kernel nearest neighbor
set.seed(1972)
knn <- train(loadClass~., data=train, method="knn", metric=metric, trControl=control)

# 4 - SVM
set.seed(1972)
svm <- train(loadClass~., data=train, method="svmRadial", metric=metric, trControl=control)

# 5 - Random Forest
set.seed(1972)
rf <- train(loadClass~., data=train, method="rf", metric=metric, trControl=control)


# summarize accuracy of models
results <- resamples(list(lda=lda, cart=cart, knn=knn, svm=svm, rf=rf))
# Show results
summary(results)

# Hard to tell from table, lets visualize the results in a dotplot
dotplot(results)

# summarize Best Model
print(knn)

# Now that we have our model, lets predict using the test set
predictions <- predict(knn, test)

#Show results of predictions against validation/hold-out set.
confusionMatrix(predictions, factor(test$loadClass))
