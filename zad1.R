## A.1.

# Function that only fits model and returns useful results
fit_logistic <- function(hours, result, title, color) {
  data <- data.frame(hours, result)
  
  model <- glm(result ~ hours, data = data, family = binomial(link = "logit"))
  
  data$predicted <- predict(model, type = "response")
  
  beta0 <- coef(model)[1]
  beta1 <- coef(model)[2]
  threshold <- -beta0 / beta1
  
  cat("beta0 =", beta0, "\n")
  cat("beta1 =", beta1, "\n")
  cat("threshold =", threshold, "\n")
  
  
  #plotting the input points 
  plot(
    data$hours, data$result,
    xlab = "Hours spent programming",
    ylab = "Result: first class",
    #main = title,
    pch = 19
  )
  
  # plotting model
  curve(
    predict(model, newdata = data.frame(hours = x), type = "response"),
    add = TRUE,
    lwd = 2,
    col = color
  )
  
  #plotting threshold
  abline(v = threshold, lty = 2, lwd = 2)
  
  return(list(
    data = data,
    model = model,
    threshold = threshold,
    beta0 = beta0,
    beta1 = beta1
  ))
}

## A.1.a.

# load data
hours_a  = c(25, 35, 40, 50, 55, 60, 70, 80+8, 90+2, 100)
result_a = c( 0,  0,  0,  1,  0,  1,  1,    1,   1,    1)

res_a = fit_logistic(hours_a, result_a, "Logistic regression", "blue")

# -------------------------

## A.1.b.
# load data
hours_b  = c(25, 35, 40, 50, 55, 60, 70, 80+8, 90+2)
result_b = c( 0,  0,  0,  1,  0,  1,  1,    1,   1)

res_b = fit_logistic(hours_b, result_b, "Logistic regression with last point deleted", "red")


# -------------------------

## A.1.c.
# load data
hours_c  = c(25, 35, 40, 50, 55, 60, 70, 80+8, 90+2, 100, 115)
result_c = c( 0,  0,  0,  1,  0,  1,  1,    1,    1,   1,   1)

res_c = fit_logistic(hours_c, result_c, "Logistic regression with additional point","darkgreen")

# ------
# all on one graph

# Plot all observed points from the largest data set
plot(
  hours_c, result_c,
  xlab = "Hours spent programming",
  ylab = "Probability of first-class result",
  main = "Comparison of logistic regression models",
  pch = 19,
  ylim = c(-0.05, 1.05),
  xlim = c(min(hours_c), max(hours_c))
)

# Curve 1: original data
curve(
  predict(res_a$model, newdata = data.frame(hours = x), type = "response"),
  from = min(hours_c),
  to = max(hours_c),
  add = TRUE,
  lwd = 2,
  col = "blue"
)

# Curve 2: without (100, 1)
curve(
  predict(res_b$model, newdata = data.frame(hours = x), type = "response"),
  from = min(hours_c),
  to = max(hours_c),
  add = TRUE,
  lwd = 2,
  col = "red"
)

# Curve 3: with (115, 1)
curve(
  predict(res_c$model, newdata = data.frame(hours = x), type = "response"),
  from = min(hours_c),
  to = max(hours_c),
  add = TRUE,
  lwd = 2,
  col = "darkgreen"
)

# Add threshold lines
abline(v = res_a$threshold, col = "blue", lty = 2)
abline(v = res_b$threshold, col = "red", lty = 2)
abline(v = res_c$threshold, col = "darkgreen", lty = 2)

# Legend
legend(
  "bottomright",
  legend = c(
    "Original data",
    "Without (100, 1)",
    "With (115, 1)"
  ),
  col = c("blue", "red", "darkgreen"),
  lwd = 2,
  lty = 1,
  bty = "n"
)

comparison <- data.frame(
  model = c("Original", "Without (100,1)", "With (115,1)"),
  threshold = c(res_a$threshold, res_b$threshold, res_c$threshold),
  beta0 = c(res_a$beta0, res_b$beta0, res_c$beta0),
  beta1 = c(res_a$beta1, res_b$beta1, res_c$beta1)
)

print(comparison)

library(knitr)

latex_table <- kable(
  comparison,
  format = "latex",
  booktabs = TRUE,
  digits = 4,
  caption = "Comparison of logistic regression models"
)

cat(latex_table)