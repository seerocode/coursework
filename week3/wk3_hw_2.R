library(glmnet)

#N = 1000
#M = 100
#d = 0.5
#a = .05

########## 2a

get_power <- function(N, M=100, d, a=0.05){
  p = vector(M, mode = "integer")
  
for(i in 1:M) {
  df = data.frame(x = 1:N)
  df$w = matrix(c(c(rep(0,N/2)), c(rep(1,N/2))),N,1)
  df$e = matrix(rnorm(N))
  df$y = d * df$w + df$e
  linear = glm(formula = y ~ w, data = df)
  p[i] = ifelse(summary(linear)$coefficients["w",4]<a,1,0) #if coef of w (the power) at col 4 (power of w) less than a, return 1, otherwise return 0
  }
  return(mean(p))
}
get_power(100, 0.5, M=100, a=0.05)
get_power(100, 0.5, M=100, a=0.01)


###HW answer
get_power <- function(N, M=100, d, a=0.05){
  p = c() #hw answer
  
  for(i in 1:M) {
    df = data.frame(x = 1:N)
    df$w = as.integer(df$x>(N/2)) #hw answer
    df$e = rnorm(N) #hw answer
    df$y = d * df$w + df$e
    linear = glm(formula = y ~ w, data = df)
    p = summary(linear)$coefficients[2,4] #hw answer
    ps = c(ps, p) #hw answer
  }
  power = mean(ps<a) #hw answer
  return(power) #hw answer
}
get_power(100, 0.5, M=100, a=0.05)
get_power(100, 0.5, M=100, a=0.01)


########## 2b (b) Build a higher-level function that takes d and power = 0.80 
#that outputs N required. This will call the base function. 

#get_N2 <- function(M, d, p){
 # a= .01
  #for (N in seq(4,2000, 2)) {
   # if(get_power(N, M, d, a)==p) return(N)
  #}
#}
#get_N2(100, .05, .80)

## HW answer
get_min_N <- function(d, power = 0.80) {
  N = 4
  while(get_power(N, d) < power){ 
    N = N + 2
  }
  return(N)
}
get_min_N(0.5)


############ 2c (c) Build a higher-level function that takes N and power = 0.80 that outputs d. 
# This will call the base function

#HW answer
get_min_d <- function(N, power = 0.80) {
  power_se <- function(d) {
    new_power = get_power(N, d)
    return((new_power-power)^2)
  }
    ret = optim(0.5, power_se, method="BFGS")
    return(ret$par)
  }
get_min_d(100) #100 is the sample size and the result is the minimum effect size (d)

