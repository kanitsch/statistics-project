library(smoof)
library(ecr)

prs <- function(budget,dim,domain,f){
  min_value <- Inf
  for (i in 1:budget){
    point <- runif(dim,domain[,1],domain[,2])
    value <- f(point)
    if (value < min_value){
      min_value <- value
    }
  }
  return (min_value)
}

ga <- function(budget, dim, domain, f) {

  terminator <- stopOnEvals(max.evals = budget)
  lower = domain[,1]
  upper = domain[,2]

  result <- ecr(
    fitness.fun = f,
    representation = "float",
    n.objectives = 1,
    minimize = TRUE,
    n.dim = dim,
    lower = lower,
    upper = upper,
    mu = 50,
    lambda = 20,
    mutator = setup(mutGauss, sdev = 2, lower = lower, upper = upper),
    terminators = list(terminator),
  )

  return(result$best.y)
}

#przykladowe wywolanie funkcji

f <- makeAckleyFunction(2)
domain <- matrix(rep(c(-32.768, 32.768),2), ncol = 2, byrow = TRUE) #dziedzina musi być w postaci macierzy
print(prs(1000,2,domain, f))
print(ga(1000,2,domain,f))

