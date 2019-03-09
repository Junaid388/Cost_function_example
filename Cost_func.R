library(dplyr)
library(ggplot2)
n<-200# number of observations
bias<-4
slope<-3.5
dot <- `%*%` # defined for personal preference
X<-rnorm(n)*2
x_b <- cbind(X, rep(1, n))
y <- bias + slope * X + rnorm(n)
df <- data_frame(x = X, y = y)
learning_rate <- 0.05
n_iterations <- 100
theta <- matrix(c(20, 20))
b0 <- vector("numeric", length = n_iterations)
b1 <- vector("numeric", length = n_iterations)
sse_i <- vector("numeric", length = n_iterations)
for (iteration in seq_len(n_iterations)) { 
  yhat               <- dot(x_b, theta)          # predict using weights in theta
  residuals_b        <- yhat - y                 # calculate the residuals
  gradients          <- 2/n * dot(t(x_b), residuals_b) # calculate the gradients of MSE w.r.t model weights 
  theta              <- theta - learning_rate * gradients # update theta 
  
  sse_i[[iteration]] <- sum((y - dot(x_b, theta))**2)
  b0[[iteration]]    <- theta[2]
  b1[[iteration]]    <- theta[1]
  
}


model_i <- data.frame(model_iter = 1:n_iterations, 
                      sse = sse_i, 
                      b0 = b0, 
                      b1 = b1)

p1 <- df %>% 
  ggplot(aes(x=x, y=y)) + 
  geom_abline(aes(intercept = b0, 
                  slope = b1, 
                  colour = -sse, 
                  frame = model_iter), 
              data = model_i, 
              alpha = .50 
  ) +
  geom_point(alpha = 0.4) + 
  geom_abline(aes(intercept = b0, 
                  slope = b1), 
              data = model_i[100, ], 
              alpha = 0.5, 
              size = 2, 
              colour = "dodger blue") +
  geom_abline(aes(intercept = b0, 
                  slope = b1),
              data = model_i[1, ],
              colour = "red", 
              alpha = 0.5,
              size = 2) + 
  scale_color_continuous(low = "red", high = "grey") +
  guides(colour = FALSE) +
  theme_minimal()