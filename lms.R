setwd("~/wentworth/machine-learning/least-means-squares")

test <- read.csv('ohms_test.csv')

train <- read.csv('ohms_train.csv')
train.size <- nrow(train)

iterations <- 100000
convergenceThreshold <- .0001
d <- numeric()
theta.0 <- numeric() # y intercept
theta.1 <- numeric() # slope

# intial guess
alpha <- .0001
theta.1[1] <- ((train$I[train.size] - train$I[1]) / (train$V[train.size] - train$V[1])) # slope
theta.0[1] <- train$I[1] - (theta.1[1] * train$V[1]) # y-intercept
for (i in 2:iterations) {
  theta.0[i] = theta.0[i - 1]
  theta.1[i] = theta.1[i - 1]
  for (j in 1:train.size) {
    x <- train$V[j]
    y <- train$I[j]
    theta.0[i] <- theta.0[i] - alpha * ((theta.1[i-1] * x - theta.0[i-1]) - y)
    theta.1[i] <- theta.1[i] - alpha * ((theta.1[i-1] * x - theta.0[i-1]) - y) * x
  }
  oldSlope <- theta.1[i-1]
  slope <- theta.1[i]
  oldIntercept <- theta.0[i - 1]
  intercept <- theta.0[i]
  d[i] <- abs(intercept - oldIntercept) + abs(slope - oldSlope)
  if (d[i] < convergenceThreshold) {
    print("Converged!")
    break
  }
}
print(i)
print('Intercept')
print(intercept)
print('Slope')
print(slope)

lin.R <- lm(train$I ~ train$V)
print(lin.R)

s <- summary(lin.R)

plot(train$V, train$I, main = 'ohms', xlab = 'Voltage (V)', ylab = 'Current (I)')
abline(a = intercept, b = slope)
abline(a = s$coefficients[1], b = s$coefficients[2])


