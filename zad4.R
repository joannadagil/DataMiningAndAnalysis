# 1. Load required libraries
library(readxl)
library(e1071)
library(ggplot2)
library(caret)

# 2. Dynamic Data Download from UCI URL 
url <- "https://archive.ics.uci.edu/static/public/602/dry+bean+dataset.zip"
zip_file <- "dry_bean_dataset.zip"

if (!file.exists(zip_file)) {
  download.file(url, destfile = zip_file, mode = "wb")
}

unzip(zip_file, exdir = "dry_bean_data")
excel_path <- "dry_bean_data/DryBeanDataset/Dry_Bean_Dataset.xlsx"
beans_data <- read_excel(excel_path)

# Ensure the dataset loaded properly and fix the data type of the target column
#beans_data$Class <- as.factor(beans_data$Class)

#(a)Explain the data and summarize the data using statistical tools.

print("Dataset Structure Overview")
str(beans_data)

print("Class Distribution counts")
print(table(beans_data$Class))

print("Class Distribution percentages")
print(round(prop.table(table(beans_data$Class)) * 100, 2))

# Descriptive statistics for the core morphological features
key_features <- c("Area", "Perimeter", "MajorAxisLength", "MinorAxisLength", "Eccentricity", "Solidity")
summary_table <- data.frame(
  Mean = colMeans(beans_data[, key_features]),
  Std_Dev = apply(beans_data[, key_features], 2, sd),
  Minimum = apply(beans_data[, key_features], 2, min),
  Maximum = apply(beans_data[, key_features], 2, max)
)
print("Statistical Summary of Morphological Features")
print(as.data.frame(round(summary_table, 2)))

# Visualizing Area Distribution across Classes using ggplot2
ggplot(beans_data, aes(x = Class, y = Area, fill = Class)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Bean Area Distribution by Class",
       x = "Bean Class",
       y = "Area (pixels)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Correlation matrix analysis
cor_matrix <- cor(beans_data[, 1:16])
print("Correlation Matrix Subset (First 5 Features)")
print(round(cor_matrix[1:5, 1:5], 2))

#(b) Use a suitable classification or clustering method, such as SVM or kNN, to
#carry out the analysis. If classification or clustering is not possible, try to carry
#out some regression analysis (such as linear, nonlinear regression and/or logistic regression)
set.seed(123) # Ensure reproducibility

# Splitting data into Training (70%) and Testing (30%) datasets
trainIndex <- createDataPartition(beans_data$Class, p = 0.7, list = FALSE)
train_set <- beans_data[trainIndex, ]
test_set  <- beans_data[-trainIndex, ]

# Z-score Normalization (Standardization) based strictly on Training parameters
preProcValues <- preProcess(train_set[, 1:16], method = c("center", "scale"))
train_scaled  <- predict(preProcValues, train_set)
test_scaled   <- predict(preProcValues, test_set)

# Ensure target variables retain factor structure after preprocessing
train_scaled$Class <- as.factor(train_scaled$Class)
test_scaled$Class  <- as.factor(test_scaled$Class)

print("Training Baseline SVM Model (Cost = 1, Gamma = 0.1)")
model_svm <- svm(Class ~ ., data = train_scaled, kernel = "radial", cost = 1, gamma = 0.1)
svm_preds <- predict(model_svm, test_scaled)

print("Baseline SVM Model Evaluation")
baseline_cm <- confusionMatrix(svm_preds, test_scaled$Class)
print(baseline_cm)

#(C) Choose the parameter values, such as k in kNN and γ in SVM, to see if your
#results change? If it’s regression, change the parameter or degree of polynomials
#to see if your results may be changed significantly. Discuss your results using
#your own understanding.
# Define hyperparameter grid combinations for Cost and Gamma
cost_values  <- c(0.1, 1, 10)
gamma_values <- c(0.01, 0.1, 1)
svm_tuning_results <- expand.grid(Cost = cost_values, Gamma = gamma_values)
svm_tuning_results$Accuracy <- NA

cat("Starting SVM Grid Search. Training multiple models...\n")

for (i in 1:nrow(svm_tuning_results)) {
  current_cost  <- svm_tuning_results$Cost[i]
  current_gamma <- svm_tuning_results$Gamma[i]
  
  cat(sprintf("Evaluating Model %d/%d: Cost = %s, Gamma = %s...\n", 
              i, nrow(svm_tuning_results), current_cost, current_gamma))
  
  # Train temporary grid model
  temp_model <- svm(Class ~ ., data = train_scaled, kernel = "radial", 
                    cost = current_cost, gamma = current_gamma)
  
  # Predict and compute test dataset accuracy
  temp_preds <- predict(temp_model, test_scaled)
  cm <- confusionMatrix(temp_preds, test_scaled$Class)
  svm_tuning_results$Accuracy[i] <- cm$overall["Accuracy"]
}

print("Comprehensive Grid Search Tuning Summary")
print(svm_tuning_results)

best_idx <- which.max(svm_tuning_results$Accuracy)
cat("\nOptimal Hyperparameters Found\n")
cat("Highest Test Accuracy:", svm_tuning_results$Accuracy[best_idx], "\n")
cat("Parameters Chosen: Cost =", svm_tuning_results$Cost[best_idx], 
    ", Gamma =", svm_tuning_results$Gamma[best_idx], "\n\n")

# Performance Visualization Plot
svm_tuning_results$Gamma_Factor <- as.factor(paste("Gamma =", svm_tuning_results$Gamma))

ggplot(svm_tuning_results, aes(x = as.factor(Cost), y = Accuracy, group = Gamma_Factor, color = Gamma_Factor)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3.5) +
  theme_minimal() +
  labs(title = "Effects of Cost and Gamma Hyperparameters on SVM Accuracy",
       subtitle = "Dataset: Dry Bean Dataset (UCI Repository)",
       x = "Cost Parameter (Regularization Constraint)",
       y = "Test Set Prediction Accuracy",
       color = "Gamma Parameters") +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14))