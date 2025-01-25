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

# Funkcja celu: sumowanie kwadratów zmiennych
f <- function(x) sum(x^2)

# Przestrzeń rozwiązań dla 2 wymiarów
domain <- matrix(c(-5, 5, -5, 5), ncol = 2,byrow = TRUE)

# Uruchomienie algorytmu genetycznego
result <- ga(budget = 1000, dim = 2, domain = domain, f = f)

# Wyświetlenie najmniejszej wartości funkcji celu
print(result)
