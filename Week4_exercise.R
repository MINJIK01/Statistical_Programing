# 4th Week Excercise

#==============================================================================
#==============================================================================
# 1. Bisection method
Bisection = function(f, x0, x1, eps = 10^(-5)){ # eps: tolerance parameter
  if (f(x0) * f(x1) > 0){
    return("worng initial interval!")
  }
  
  N = 0; err = x1 - x0 ; x2_set = NULL# err means length of the interval
  
  while(err > eps & N <= 1000){
    x2 = (x0 + x1)/2
    
    if (f(x0) * f(x2) < 0){
      x1 = x2
    } else{
      x0 = x2
    }
    
    err = x1 - x0
    N = N + 1
    x2_set = c(x2_set, x2)
    
  }
  
  return(list(root = x2, root_set = c(x2_set, x2)))
}

my_fun = function (x){
  x^2 + x - 5
}

# 1st Experiment
x0 = -5
x1 = 0

Bisection(my_fun, x0, x1, eps = 10^(-5))
my_fun(x0)
my_fun(x1)

# 2nd Experiment
x0 = 0
x1 = 4

Bisection(my_fun, x0, x1, eps = 10^(-15))

# 3rd Experiment
my_func = function(x){
  0.01 / (x-2)
}

x = 1.9; x1 = 2.1
Bisection(my_func, x0, x1, eps = 10^(-5))

# 4th Experiment
my_funct = function(x){
  x^2
}

x0 = -0.5; x1 = 0.5
Bisection(my_funct, x0, x1, eps = 10^(-5))

# 5th Experiment
my_funct = function(x){
  x^2 - 5
}

x0 = 0; x1 = 5
root_set = Bisection(my_funct, x0, x1, eps = 10^(-5))$root_set

plot(1:length(root_set), abs(root_set - sqrt(5)))

#==============================================================================
#==============================================================================
# 2. Linear interpolation


Linear_Interpolation = function(f, x0, x1, eps = 10^(-5)){
  if (f(x0) * f(x1) > 0){
    return("worng initial interval!")
  }
  
  N = 0; err = 10; errors = NULL
  
  while(err > eps & N <= 1000){
    
    x2 = (f(x1)*x0 - f(x0)*x1) / (f(x1) - f(x0))
    err = abs(f(x2) - 0); errors = c(errors, err); N = N + 1
    
    if (f(x0) * f(x2)<0){
      x1 = x2
    } else{
      x0 = x2}
  }
  
  return(list(solution = x2, errors = round(errors, 4)))
}


# 1st Experiment

my_fun = function (x){
  x^2 + x - 5
}
x0 = -5
x1 = 0

Linear_Interpolation(my_fun, x0, x1, eps = 10^(-5))

library(tictoc)

# time checking
tic()
Bisection(my_fun, x0, x1, eps = 10^(-5))
toc()

tic()
Linear_Interpolation(my_fun, x0, x1, eps = 10^(-5))
toc()

#==============================================================================
#==============================================================================
# 3. Newton's method

Newton_Method = function(f, fp, x, eps = 10^(-5)){
  # fp is the derivative fucntion of f.
  
  N = 0; err = 10; err_set = NULL # err의 초기 값을 크게 놔야 while문이 돌아간다.
  
  while(err > eps & N <= 1000){
    x0 = x
    x = x - f(x) / fp(x)
    err = f(x) # differnece between recent x and previous x
    err_set = c(err_set, err)
    N = N + 1
  }
  
  return(list(solution = x, num_repeat = N, errors = round(err_set,4)))
}


# 1st Experiment
my_fun = function (x){
  x^6 - 6.7*x^5 + 8.8*x^4 - 6.7*x^3 + 8.8*x^2 - 6.7*x + 7.8
}
my_fun_prime = function (x){
  6*x^5 - 33.5*x^4 + 35.2*x^3 - 20.1*x^2 + 17.6*x - 6.7
}
x0 = 1.3
x1 = 2.05

Bisection(my_fun, x0, x1, eps = 10^(-5))
Linear_Interpolation(my_fun, x0, x1, eps = 10^(-5))
Newton_Method(my_fun, my_fun_prime, (x0 + x1)/2, eps = 10^(-5))


errors = Newton_Method(my_fun, my_fun_prime, (x0 + x1)/2, eps = 10^(-5))$errors
plot(1:length(errors), errors, type = 'b') # type b: contain line and point "both"

# 2nd Experiment