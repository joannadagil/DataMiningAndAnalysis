x <- c(0.10, 0.50, 1.00, 1.50, 2.05, 2.50, 3.00)
y <- c(0.23, 0.65, 0.90, 0.80, 0.70, 0.60, 0.50)
df <- data.frame(x = x, y = y)

m1 <- nls(y ~ a * x / (1 + c * x^2),  data = df, start = list(a = 2, c = 1.2))
m2 <- nls(y ~ A * x * exp(-b * x),    data = df, start = list(A = 2.5, b = 1))
m3 <- lm(y ~ x + I(x^2) + I(x^3),    data = df)

print(summary(m1))
print(summary(m2))
print(summary(m3))

r1 <- y - fitted(m1)
r2 <- y - fitted(m2)
r3 <- y - fitted(m3)

ss_tot <- sum((y - mean(y))^2)
pR2 <- function(r) 1 - sum(r^2) / ss_tot

cat("\nModel     RSS        RMSE       MAE        AIC      BIC    pseudo-R2\n")
cat(strrep("-", 72), "\n")
for (nm in c("f1", "f2", "f3")) {
  m <- list(f1 = m1, f2 = m2, f3 = m3)[[nm]]
  r <- list(f1 = r1, f2 = r2, f3 = r3)[[nm]]
  cat(sprintf("%-8s  %8.5f   %8.5f   %8.5f   %7.3f  %7.3f   %.4f\n",
              nm, sum(r^2), sqrt(mean(r^2)), mean(abs(r)), AIC(m), BIC(m), pR2(r)))
}

p4 <- c(f1 = predict(m1, list(x = 4)),
        f2 = predict(m2, list(x = 4)),
        f3 = predict(m3, list(x = 4)))
cat("\nExtrapolation x=4 (true y=0.25):\n")
print(round(p4, 4))
cat("Absolute errors:\n")
print(round(abs(0.25 - p4), 4))

xs <- seq(0.05, 5, length = 400)
p1s <- predict(m1, list(x = xs))
p2s <- predict(m2, list(x = xs))
p3s <- predict(m3, list(x = xs))

COL <- c("#f0883e", "#3fb950", "#ff7b72")
COBS <- "#58a6ff"; CNEW <- "#ffa657"

plot(x, y, xlim = c(0, 3.2), ylim = c(0, 1.15), pch = 19, col = COBS,
     main = "(a) Model fitting", xlab = "x", ylab = "y", las = 1)
lines(xs[xs <= 3.2], p1s[xs <= 3.2], col = COL[1], lwd = 2)
lines(xs[xs <= 3.2], p2s[xs <= 3.2], col = COL[2], lwd = 2)
lines(xs[xs <= 3.2], p3s[xs <= 3.2], col = COL[3], lwd = 2, lty = 2)
legend("topright", legend = c("data", "f1: ax/(1+cx^2)", "f2: Axe^(-bx)", "f3: poly3"),
       col = c(COBS, COL), lty = c(NA, 1, 1, 2), pch = c(19, NA, NA, NA), bty = "n")

plot(x, r1, pch = 19, col = COL[1], ylim = c(-0.08, 0.08),
     main = "(b) Residuals (y - y_hat)", xlab = "x", ylab = "residual", las = 1)
abline(h = 0, lty = 2)
points(x, r2, pch = 17, col = COL[2])
points(x, r3, pch = 15, col = COL[3])
legend("topright", legend = c("f1", "f2", "f3"), col = COL, pch = c(19, 17, 15), bty = "n")

plot(x, y, xlim = c(0, 4.8), ylim = c(-0.3, 1.5), pch = 19, col = COBS,
     main = "(c) Extrapolation (x=4)", xlab = "x", ylab = "y", las = 1)
lines(xs, p1s, col = COL[1], lwd = 2)
lines(xs, p2s, col = COL[2], lwd = 2)
lines(xs, p3s, col = COL[3], lwd = 2, lty = 2)
points(4, 0.25, pch = 8, cex = 2, col = CNEW, lwd = 2)
for (i in 1:3) {
  points(4, p4[i], pch = c(19, 17, 15)[i], col = COL[i], cex = 1.3)
  segments(4, p4[i], 4, 0.25, col = COL[i], lty = 3, lwd = 1.5)
}
legend("topright", legend = c("data", "(4, 0.25)", "f1", "f2", "f3"),
       col = c(COBS, CNEW, COL), pch = c(19, 8, 19, 17, 15), bty = "n")
