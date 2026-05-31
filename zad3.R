data <- read.csv("sggw_c.csv")
#View(data)

# -------------------------------------------------------------
## (a) Summary of the data including mean, median and variance.

data_summary <- data.frame(
  variable = names(data),
  min = sapply(data, min, na.rm = TRUE),
  max = sapply(data, max, na.rm = TRUE),
  mean = sapply(data, mean, na.rm = TRUE),
  median = sapply(data, median, na.rm = TRUE),
  variance = sapply(data, var, na.rm = TRUE)
)

print(data_summary)

## saving to latex table

install.packages("xtable")
library(xtable)

latex_table <- xtable(
  data_summary,
  caption = "Descriptive statistics of the dataset.",
  label = "tab:summary_stats"
)

print(
  latex_table,
  file = "report/summary.tex",
  include.rownames = FALSE
)

# checking class distribution
table(data[[ncol(data)]])

# -------------------------------------------------------------
# visualization of the data (e.g., boxplot and/or histogram).

for (col in names(data)) {
  hist(data[[col]],
       main = paste("Histogram of", col),
       xlab = col)
  
  boxplot(data[[col]],
          main = paste("Boxplot of", col),
          ylab = col)
}


## saving as pdfs for raport

dir.create("report/plots", recursive = TRUE, showWarnings = FALSE)

for (col in names(data)) {
  
  # Make safe file name
  safe_col <- gsub("[^A-Za-z0-9_]", "_", col)
  
  # Histogram PDF
  pdf(file = paste0("report/plots/histogram_", safe_col, ".pdf"),
      width = 7,
      height = 5)
  
  hist(
    data[[col]],
    main = "",
    xlab = col,
    col = "lightblue",
    border = "white"
  )
  
  dev.off()
  
  # Boxplot PDF
  pdf(file = paste0("report/plots/boxplot_", safe_col, ".pdf"),
      width = 7,
      height = 5)
  
  boxplot(
    data[[col]],
    main = "",
    ylab = col,
    col = "lightgreen"
  )
  
  dev.off()
}

# -------------------------------------------------------------
# (b) SVM analysis

install.packages("e1071")
library(e1071)

# converting targets to factors (so the model does classification, not regression)
data[[ncol(data)]] <- factor(
  data[[ncol(data)]],
  levels = c(1, 2),
  labels = c("Good", "Bad")
)

# split into train and test sets
set.seed(42)
train_index <- sample(
  1:nrow(data),
  size = 0.8 * nrow(data)
)

data_train <- data[train_index, ]
data_test <- data[-train_index, ]

# ... and split into features and labels

data_train_x <- data_train[ , 1:(ncol(data)-1)]
data_train_y <- data_train[ , ncol(data)]

data_test_x <- data_test[ , 1:(ncol(data)-1)]
data_test_y <- data_test[ , ncol(data)]

# -----------------------------------------------------
## checking class distribution of train and test sets
# because there is a limited amount of data we have decided to check the sampling split 
# similar proportions suggest that the split is representative of the original dataset

class_distribution <- function(y) {
  counts <- table(y)
  proportions <- prop.table(counts)
  
  data.frame(
    class = names(counts),
    count = as.vector(counts),
    proportion = round(as.vector(proportions), 3)
  )
}
full_distribution <- class_distribution(data[[ncol(data)]])
train_distribution <- class_distribution(data_train_y)
test_distribution <- class_distribution(data_test_y)

print("Full dataset distribution:")
print(full_distribution)

print("Training set distribution:")
print(train_distribution)

print("Test set distribution:")
print(test_distribution)

# -------------------------------------------------------

# training
model <- svm(
  x = data_train_x,
  y = data_train_y,
  kernel = "radial",
  scale = TRUE)

summary(model)

# predictions

predictions <- predict(model, data_test_x)
confusion_matrix <- table(Predicted=predictions, Actual=data_test_y)
confusion_matrix

# Accuracy
accuracy <- mean(predictions == data_test_y)
print(paste("Accuracy:", round(accuracy, 4)))

# as we can see the model works, but barely
# it predicts 'Good' for almost any case
# it happens because the data is unbalanced
# we can try to fix it by implementing class weights
# as to penalize more harshly false 'Good' prediction
# (which is probably optimal for credit risk problems)

# ------------------------------------------------------

class_weights <- c(
  Good = 1,
  Bad = sum(data_train_y == "Good") / sum(data_train_y == "Bad")
)

model_weighted <- svm(
  x = data_train_x,
  y = data_train_y,
  kernel = "radial",
  scale = TRUE,
  class.weights = class_weights
)

summary(model_weighted)

# predictions
predictions_weighted <- predict(model_weighted, data_test_x)
confusion_matrix_weighted <- table(Predicted=predictions_weighted, Actual=data_test_y)
confusion_matrix_weighted

# accuracy
accuracy_weighted <- mean(predictions_weighted == data_test_y)
print(paste("Accuracy:", round(accuracy_weighted, 4)))

# the result overall accuracy is clearly worst, but we have limited false 'Good' predictions
# so one can arguably defend that the model with weighted classes is indeed better

# ----------------------------------------------------------------------------------
# saving result tablets to latex

# standard
confusion_df <- as.data.frame.matrix(confusion_matrix)
confusion_df <- cbind(Predicted = rownames(confusion_df), confusion_df)
rownames(confusion_df) <- NULL

confusion_latex <- xtable(
  confusion_df,
  caption = "Confusion matrix for the SVM model.",
  label = "tab:svm_confusion_matrix"
)

print(
  confusion_latex,
  file = "report/svm_confusion_matrix.tex",
  include.rownames = FALSE
)

# weighted
confusion_weighted_df <- as.data.frame.matrix(confusion_matrix_weighted)
confusion_weighted_df <- cbind(Predicted = rownames(confusion_weighted_df), confusion_weighted_df)
rownames(confusion_weighted_df) <- NULL

confusion_weighted_latex <- xtable(
  confusion_weighted_df,
  caption = "Confusion matrix for the SVM model with weighted classes.",
  label = "tab:svm_confusion_matrix_weighted"
)

print(
  confusion_weighted_latex,
  file = "report/svm_confusion_matrix_weighted.tex",
  include.rownames = FALSE
)

# --------------------------------------------------------------------------------------
# let's save also all evaluation metrics for the report

calculate_metrics <- function(conf_mat) {
  TP <- conf_mat["Bad", "Bad"]
  TN <- conf_mat["Good", "Good"]
  FP <- conf_mat["Bad", "Good"]
  FN <- conf_mat["Good", "Bad"]
  
  accuracy <- (TP + TN) / sum(conf_mat)
  precision_bad <- TP / (TP + FP)
  recall_bad <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  f1_bad <- 2 * precision_bad * recall_bad / (precision_bad + recall_bad)
  balanced_accuracy <- (recall_bad + specificity) / 2
  
  data.frame(
    Accuracy = accuracy,
    Precision_Bad = precision_bad,
    Recall_Bad = recall_bad,
    Specificity = specificity,
    F1_Bad = f1_bad,
  )
}

metrics_standard <- calculate_metrics(confusion_matrix)
metrics_weighted <- calculate_metrics(confusion_matrix_weighted)

metrics_comparison <- rbind(
  Standard_SVM = metrics_standard,
  Weighted_SVM = metrics_weighted
)

metrics_comparison <- round(metrics_comparison, 4)

print(metrics_comparison)

metrics_latex <- xtable(
  metrics_comparison,
  caption = "Comparison of evaluation metrics for standard and weighted SVM models.",
  label = "tab:svm_metrics"
)

print(
  metrics_latex,
  file = "report/svm_metrics.tex",
  include.rownames = TRUE
)
