x <- c(0.10, 0.50, 1.00, 1.50, 2.05, 2.50, 3.00)
y <- c(0.23, 0.65, 0.90, 0.80, 0.70, 0.60, 0.50)
df <- data.frame(x = x, y = y)

cat("=== DATA ===\n"); print(df); cat("\n")

cat("--- Model 1: f1(x) = a*x / (1 + c*x²) ---\n")
m1 <- nls(y ~ a * x / (1 + c * x^2), data = df,
          start = list(a = 2, c = 1.2),
          control = nls.control(maxiter = 1000))
print(summary(m1))

# --- Model 2: f2(x) = A*x*exp(-b*x) ---
cat("--- Model 2: f2(x) = A*x*exp(-b*x) [reparametryzacja f2 z c=0] ---\n")
cat("Start: A=2.5, b=1  [z obliczeń analitycznych]\n")
m2 <- nls(y ~ A * x * exp(-b * x), data = df,
          start = list(A = 2.5, b = 1),
          control = nls.control(maxiter = 1000))
print(summary(m2))

# --- Model 3: f3(x) = a + b*x + c*x^2 + d*x^3 ---
cat("--- Model 3: f3(x) = a + b*x + c*x² + d*x³ (wielomian st. 3) ---\n")
cat("Użycie lm() – wielomian jest liniowy w parametrach.\n")
m3 <- lm(y ~ x + I(x^2) + I(x^3), data = df)
print(summary(m3))


pred1 <- predict(m1)
pred2 <- predict(m2)
pred3 <- fitted(m3)
r1 <- y - pred1;  r2 <- y - pred2;  r3 <- y - pred3

cat(sprintf("%-26s %9s %9s %9s %8s %8s %5s\n",
            "Model","RSS","RMSE","MAE","AIC","BIC","df"))
cat(strrep("-",80),"\n")
mods  <- list(f1=m1, f2=m2, f3=m3)
resids <- list(f1=r1, f2=r2, f3=r3)
npar  <- c(f1=2, f2=2, f3=4)
for(nm in c("f1","f2","f3")){
  r <- resids[[nm]]; m <- mods[[nm]]
  cat(sprintf("%-26s %9.6f %9.6f %9.6f %8.3f %8.3f %5d\n",
              nm, sum(r^2), sqrt(mean(r^2)), mean(abs(r)),
              AIC(m), BIC(m), 7-npar[nm]))
}

cat("\nPrzewidywania i reszty per obserwacja:\n")
print(round(data.frame(x=x, y=y,
                       hatF1=pred1, resF1=r1,
                       hatF2=pred2, resF2=r2,
                       hatF3=pred3, resF3=r3), 4))


p1n <- predict(m1, newdata=data.frame(x=4))
p2n <- predict(m2, newdata=data.frame(x=4))
p3n <- predict(m3, newdata=data.frame(x=4))
errs <- c(f1=abs(0.25-p1n), f2=abs(0.25-p2n), f3=abs(0.25-p3n))


ss_tot <- sum((y - mean(y))^2)
pR2 <- function(resids) 1 - sum(resids^2)/ss_tot
cat(sprintf("  f1: pseudo R² = %.4f\n", pR2(r1)))
cat(sprintf("  f2: pseudo R² = %.4f\n", pR2(r2)))
cat(sprintf("  f3:   adj R² = %.4f\n", summary(m3)$adj.r.squared))

par(mfrow=c(1,3),
    bg="white", col.axis="black", col.lab="black",
    col.main="black", fg="black", mar=c(4.5,4.5,3.5,1.5))

xs   <- seq(0.05, 5.0, length=600)
COL1 <- "#f0883e"; COL2 <- "#3fb950"; COL3 <- "#ff7b72"
COBS <- "#58a6ff"; CNEW <- "#ffa657"

p1s <- predict(m1, newdata=data.frame(x=xs))
p2s <- predict(m2, newdata=data.frame(x=xs))
p3s <- predict(m3, newdata=data.frame(x=xs))

idx <- xs <= 3.2
plot(x, y, xlim=c(0,3.2), ylim=c(0,1.15),
     pch=19, cex=1.6, col=COBS, las=1,
     main="(a) Dopasowanie modeli", xlab="x", ylab="y")
grid(col="#21262d", lty=1)
lines(xs[idx], p1s[idx], col=COL1, lwd=2.5)
lines(xs[idx], p2s[idx], col=COL2, lwd=2.5)
lines(xs[idx], p3s[idx], col=COL3, lwd=2.5, lty=2)
points(x, y, pch=19, cex=1.6, col=COBS)
legend("topright", bty="n", text.col="black", cex=0.72, lwd=2.2,
       legend=c("dane","f1: ax/(1+cx²)","f2: Axe^(-bx)","f3: wielomian st.3"),
       col=c(COBS,COL1,COL2,COL3), lty=c(NA,1,1,2), pch=c(19,NA,NA,NA))

plot(x, r1, pch=19, col=COL1, cex=1.6, ylim=c(-0.075,0.075), las=1,
     main="(b) Reszty (y − ŷ)", xlab="x", ylab="reszta")
grid(col="#21262d", lty=1)
abline(h=0, col="white", lty=2, lwd=1.5)
points(x, r2, pch=17, col=COL2, cex=1.6)
points(x, r3, pch=15, col=COL3, cex=1.6)
lines(x, r1, col=COL1, lwd=1, lty=3)
lines(x, r2, col=COL2, lwd=1, lty=3)
lines(x, r3, col=COL3, lwd=1, lty=3)
legend("topright", legend=c("f1","f2","f3"), bty="n", cex=0.85,
       col=c(COL1,COL2,COL3), pch=c(19,17,15), text.col="black")

ylim3 <- c(min(-0.3, p3n-0.1), 1.5)
plot(x, y, xlim=c(0,4.8), ylim=ylim3,
     pch=19, cex=1.6, col=COBS, las=1,
     main="(c) Ekstrapolacja (x=4)", xlab="x", ylab="y")
grid(col="#21262d", lty=1)
lines(xs, p1s, col=COL1, lwd=2.5)
lines(xs, p2s, col=COL2, lwd=2.5)
lines(xs, p3s, col=COL3, lwd=2.5, lty=2)
points(x, y, pch=19, cex=1.6, col=COBS)
points(4, 0.25, pch=8, cex=2.5, col=CNEW, lwd=3)
text(4.1, 0.25, "(4, 0.25)", col=CNEW, adj=0, cex=0.75)
pts  <- c(p1n, p2n, p3n)
cols <- c(COL1,COL2,COL3)
pchs <- c(19,17,15)
for(i in 1:3){
  points(4, pts[i], pch=pchs[i], col=cols[i], cex=1.5)
  segments(4, pts[i], 4, 0.25, col=cols[i], lty=3, lwd=2)
}
legend("topright", bty="n", text.col="black", cex=0.72,
       legend=c("dane","nowy (4,0.25)","f1","f2","f3"),
       col=c(COBS,CNEW,COL1,COL2,COL3), pch=c(19,8,19,17,15))


