Trapezoidal = function(f, a, b, n, print = TRUE){
  if (n <= 0){
    return("Choose a positive integer n!")
  }
  x0 = sort(c(a,b))[1]; xn = sort(c(a,b))[2]
  h = (xn  -x0)/n
  
  integral = 0
  for (i in 1:n){
   #integral = integral + h*f(x0+(h*i))
    integral  = integral + 0.5 * h * (f(x0 + i*h) + f(x0 + (i-1)*h))
  }
  
  
  if (print == TRUE){
    print(paste("Trapezoidal gives", integral))}
  return(integral = integral)
}


func_y = function(x){x^2 - 2*x + 4}
Trapezoidal(func_y, 1, 2, 10)

# ############################## Question ###################################
func_y = function(x){exp(-0.5*x^2)/sqrt(2*pi)}

#1)
Trapezoidal(func_y, -2, 4, 10)

#2)
Trapezoidal(func_y, -5,1,10)*Trapezoidal(func_y, -1, 2, 10)

#############################################################################
func_y = function(x){exp(-0.5*x^2)/sqrt(2*pi)}
Trapezoidal(func_y, -2, 4, 10)


int_func_y = function(x){pnorm(x)}
int_func_y(4) - int_func_y(-2)

###############################################################################

n_set = seq(5,100,by =5)
result1 = sapply(n_set, Trapezoidal, f=func_y, a = -2, b = 4, print = FALSE) - (int_func_y(4) - int_func_y(-2))

result1

#######################################################
plot(n_set, abs(result1), main = "n vs Error", xlab = "n", ylab = "error", cex = 1.5, cex.lab = 2, ylim = c(0,0.1))

lines(n_set, 87/(n_set^2), col = "red", )