?rnorm

# dnorm gives the density
# pnorm gives the distribution function
# qnorm gives the quantile function
# rnorm generates random deviates

# analogicznie exp, gamma, unif, pois, ...

mi <- 0.5
sigma <- 2 # sigma jest funkcj¹ R, ale mo¿emy nadpisaæ zmienn¹
N <- 1000

X <- rnorm(n = N, mean = mi, sd = sigma) # generujemy wektor X (d³ugoœci N) z rozk³adu normalnego
X
mean(X)
var(X)
hist(X)
hist(X, probability = TRUE)

hist(X, probability = TRUE, breaks = 50)
curve(dnorm(x, mean = mi, sd = sigma), add = TRUE, col = "purple")

plot(pnorm)
plot(pnorm, from = -3, to = 3)
plot(qnorm)

VaR_norm <- function(c, mean = 0, sd = 1){
  mean + sd * qnorm(c)
} 

VaR_norm(0.95, 172, 15)

c <- 0.95

plot(X)
abline(a = VaR_norm(c, mi, sigma), b = 0, col = "red")


X>VaR_norm(c, mi, sigma)
X[X>VaR_norm(c, mi, sigma)]
length(X[X>VaR_norm(c, mi, sigma)])
length(X[X>VaR_norm(c, mi, sigma)])/N

# ka¿dy mo¿e sobie zrobiæ teraz dla rozk³adu jakiego tam chce
# exp

lambda <- 15
Y <- rexp(N, lambda)

hist(Y, probability = TRUE, breaks = 50)
curve(dexp(x, lambda), add = TRUE, col = "blue")

plot(Y)
abline(a = -log(1-c)/lambda, b = 0, col = "green")
length(Y[Y>-log(1-c)/lambda])



