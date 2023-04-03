Forward = function(f, x, h, print = TRUE){
  
  if (h <= 0){
    return("Choose positive h!")
  }
  
  derivative = (f(x + h) - f(x)) / h
  
  if (print == TRUE){
    print(paste("FD gives the derivative value", derivative, "of the fucntion f at the point", x))
  
  }
  return(derivative)
  
}
my_func = function(x) exp(sin(x) + cos(x)^2)

Forward(my_func, 2.5, 0.05)

# =============================================================================
ForwaForward_Backward = function(f, x, h, type = 1, print = TRUE){
  
  if (h <= 0){
    return("Choose positive h!")
  }
  
  if (type ==1){
    derivative = (f(x + h) - f(x)) / h
    Type = "Forward"
  }else{
    derivative = (f(x) - f(x - h)) / h
    Type = "Backward"
  }
  
  if (print == TRUE){
    print(paste(Type, "gives the derivative value", derivative, "of the fucntion f at the point", x))
    
  }
  return(derivative)
  
}

my_func = function(x) x^3+2*x - 5

ForwaForward_Backward(my_func, 2, 0.05, 1)
ForwaForward_Backward