install.packages("caret")
install.packages("glmnet")


# Load necessary libraries
library("tidyverse")
library("ggplot2")
library("caret") # for data splitting and confusionMatrix
library("glmnet") # for logistic regression

# Set the file path (use forward slashes instead of backslashes)
file_path <- "C:/PSMDS/Preliminary-Midterm Examination/WA_Fn-UseC_-HR-Employee-Attrition (1).csv"

# Read the CSV file
attrition_employee <- read.csv(file_path)

# Display the first few rows of the data
head(attrition_employee)


# Convert Attrition to a binary variable (Yes = 1, No = 0)
attrition_employee$Attrition <- ifelse(attrition_employee$Attrition == "Yes", 1, 0)

# Create a new variable for income below the average
average_month_income <- mean(attrition_employee$MonthlyIncome)
attrition_employee$Below_Average_Income <- ifelse(attrition_employee$MonthlyIncome < average_month_income, 1, 0)

# Split the data into training and test sets (80% training, 20% test)
set.seed(42)
splitIndex <- createDataPartition(attrition_employee$Attrition, p = 0.8, list = FALSE)
train_data <- attrition_employee[splitIndex, ]
test_data <- attrition_employee[-splitIndex, ]

# Fit a logistic regression model
model <- glm(Attrition ~ Below_Average_Income + JobRole + Department, data = train_data, family = "binomial")

# Predict attrition probabilities on the test data
predicted_probs <- predict(model, newdata = test_data, type = "response")

# Convert probabilities to binary predictions using a 0.5 threshold
predicted_attrition <- ifelse(predicted_probs > 0.5, 1, 0)

# Calculate the confusion matrix and overall accuracy
confusion <- confusionMatrix(as.factor(predicted_attrition), as.factor(test_data$Attrition))
accuracy <- confusion$overall["Accuracy"]

# Print the model summary and accuracy
print(summary(model))
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))