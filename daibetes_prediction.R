install.packages("dplyr")
install.packages("tidyr")
install.packages("caret")
install.packages("randomForest")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("class")
install.packages("gmodels")
install.packages("janitor")



library(janitor)
library(corrplot)
library(dplyr)
library(tidyr)
library(caret)
library(ggplot2)
library(class)
library(gmodels)
library(randomForest)

getwd()

df <- read.csv("diabetes.csv")

dataset



missing_values <- colSums(is.na(df)) #to calculate the number of missing values 

is_duplicate <- duplicated(df)
is_duplicate
sum(is_duplicate) # there is no duplicated value

# Cleaning and Transforming the dataset
df$Outcome <- as.character(df$Outcome)
diabetes_df <- df %>%
  mutate(Outcome = factor(Outcome,
                          levels = c("1", "0"),
                          labels = c("Diabetic", "Non Diabetic")))
#Creating a normalize function
normalize <- function(x){
  (x-min(x))/(max(x)-min(x))
}
diabetes_df_n <- as.data.frame(lapply(df[1:8],normalize))


#EDA part


summary(df)
str(df)


#Age of patients
ggplot(data =df, aes(x = Age)) + geom_histogram(bins = 30, color = "black", fill = "blue") + facet_wrap(~Outcome) + theme_dark() + ylab("Number of Patients") + labs(title = "Age(s) of Patients")

#The ages of the patients are skewed to the right with most of the patients being between the ages of 20 to 40.

#patient Blood Pressure
ggplot(data = df, aes(x = BloodPressure)) + geom_histogram(bins = 30, color = "black", fill = "blue") + facet_wrap(~Outcome) + theme_dark() +  ylab("Number of Patients") + labs(title = "Patient Blood Pressure")

#BMI of Patients
ggplot(data = df, aes(x = BMI)) + geom_histogram(bins = 30, color = "black", fill = "blue") + facet_wrap(~Outcome)+  theme_dark()  + ylab("Number of Patients") + labs(title = "BMI of Patients")

# Assuming diabetes_correlation_df is your correlation dataframe
diabetes_correlation_df <- cor(diabetes_correlation_df)

# Create the correlation plot
diabetes_correlation_df <- df[-9]
diabetes_correlation_df <- cor(diabetes_correlation_df)
corrplot(diabetes_correlation_df, method = "color", type = "lower", addCoef.col = "black", col = COL2("RdYlBu"), number.cex = 0.8, tl.cex = 0.8)





#model training & evaluation

#Seperating the dataset into the Train and Test Data
X <- df[, -9]  # Exclude the Outcome column
y <- df$Outcome

# Step 2: Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_index <- sample(1:nrow(df), 0.7 * nrow(df))
X_train <- X[train_index, ]
X_test <- X[-train_index, ]
y_train <- y[train_index]
y_test <- y[-train_index]


# Train the KNN model
k <- 5  # Number of nearest neighbors to consider
knn_model <- knn(train = X_train, test = X_test, cl = y_train, k = k)

# Evaluate the performance of the model
confusion_matrix <- table(knn_model, y_test)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
f1_score <- 2 * precision * recall / (precision + recall)

# Print the evaluation metrics
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")




#Random Forest
X <- df[, -9]  # Exclude the Outcome column
y <- df$Outcome

# Step 2: Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_index <- sample(1:nrow(df), 0.7 * nrow(df))
X_train <- X[train_index, ]
X_test <- X[-train_index, ]
y_train <- y[train_index]
y_test <- y[-train_index]

# Step 3: Train the Random Forest model
rf_model <- randomForest(x = X_train, y = y_train, ntree = 500)

# Step 4: Make predictions on the testing data
predictions <- predict(rf_model, newdata = X_test)

# Step 5: Evaluate the performance of the model
confusion_matrix <- table(predictions, y_test)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
f1_score <- 2 * precision * recall / (precision + recall)

# Print the evaluation metrics
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")








