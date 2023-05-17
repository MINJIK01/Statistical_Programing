f = function(x){
  (x[1] - 2)^4 + 3*(x[2] + 1)^2
 + 5*(x[3] - 3)^2 + 2}

result = optim(c(0,0,0), f)
result

#================================================
library(quadprog)
solve.QP()