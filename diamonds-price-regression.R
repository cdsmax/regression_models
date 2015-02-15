# example of regression with confidence intervals for the coefficients
# based on the diamond data
# full calc example for this:
# fit <- lm(y ~ x)
# summary(fit)$coefficients

library(UsingR)
data(diamond)

# we assume price of diamonds depends on their mass in carats
x <- diamond$carat
y <- diamond$price
n <- length(y)

# unbiased degrees of freedom
freedom <- n - 2

# generate the coefficients
# y = beta0 + beta1*x + e
# beta0 is estimated price for a 0-carat diamond
# beta1 is estimated price increase for 1 carat change
beta1 <- cor(y, x) * sd(y)/sd(x)
beta0 <- mean(y) - beta1 * mean(x)

# get the residuals
e <- y - beta0 - beta1 * x

# unbiased (n-2) residual variance
sigma <- sqrt(sum(e^2) / freedom)

# sum of squared error for X
ssx <- sum( (x - mean(x))^2 )

# standard error for beta0
seBeta0 <- (1/n + mean(x)^2 / ssx)^0.5 * sigma

# standard error for beta1
seBeta1 <- sigma / sqrt(ssx)

# t-distributions for beta0 and beta1
tBeta0 <- beta0 / seBeta0
tBeta1 <- beta1 / seBeta1

# p-values for beta0 and beta1
pBeta0 <- 2 * pt( abs(tBeta0), df = freedom, lower.tail=FALSE )
pBeta1 <- 2 * pt( abs(tBeta1), df = freedom, lower.tail=FALSE )

# make a good looking summary -- summary(fit)$coeffients
coefTable <- rbind( c(beta0, seBeta0, tBeta0, pBeta0), c(beta1, seBeta1, tBeta1, pBeta1) )
colnames(coefTable) <- c("Estimate", "Std Error", "T values", "p-values for P(>|t)")
rownames(coefTable) <- c("Intercept", "X")

# 95% confidence intervals for beta0 and beta1 - two sided t-test
confBeta0 <- coefTable[1, 1] + c(-1, 1) * qt(0.975, df=freedom) * coefTable[1, 2]
confBeta1 <- coefTable[2, 1] + c(-1, 1) * qt(0.975, df=freedom) * coefTable[2, 2]

# prediction: what is the expected price at average carat value, with confidence interval
fit <- lm(y ~ x)
predict(fit, data.frame(x=mean(x)), interval="confidence")

# prediction: what is the expected price for a 1 carat diamond, with prediction interval
predict(fit, data.frame(x=1), interval="prediction")

# scaling a coefficient
# what if we're working with deci-carats, ie carat * 10
# notice that we /
fit_default <- lm(y ~ x)
fit_deci <- lm(y ~ I(x/10))
fit_default$coefficients
fit_deci$coefficients # slope is *10

# what about working in 10th of a carat
# notice is *
fit_tenth <- lm(y ~ I(x*10))
fit_tenth$coefficients