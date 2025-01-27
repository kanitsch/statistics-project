#Egghloder domyślnie jest zdefioniowany dla tylko 2 wymiarów dlatego tu zrobiłem EggHoldera tylko na 2 wymiarach
library(smoof)
library(ecr)

# Funkcja PRS
prs <- function(budget, dim, domain, f) {
  min_value <- Inf
  for (i in 1:budget) {
    point <- runif(dim, domain[, 1], domain[, 2])
    value <- f(point)
    if (value < min_value) {
      min_value <- value
    }
  }
  return(min_value)
}

# Funkcja GA
ga <- function(budget, dim, domain, f) {
  terminator <- stopOnEvals(max.evals = budget)
  lower <- domain[, 1]
  upper <- domain[, 2]
  
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
    terminators = list(terminator)
  )
  
  return(result$best.y)
}

# Funkcja porównawcza
compar <- function(f, dimensions, bounds, function_name) {
  print(paste("Funkcja:", function_name, "| Wymiary:", dimensions))
  
  domain <- matrix(rep(bounds, dimensions), ncol = 2, byrow = TRUE)
  
  prs_results <- replicate(50, prs(1000, dimensions, domain, f))
  ga_results <- replicate(50, ga(1000, dimensions, domain, f))
  
  print("PRS:")
  print(paste("Średnia wartość:", mean(prs_results)))
  print("GA:")
  print(paste("Średnia wartość:", mean(ga_results)))
  
  return(list(prs = prs_results, ga = ga_results))
}

# Funkcje testowe
ackley_bounds <- c(-32.768, 32.768)
eggholder_bounds <- c(-512, 512)

# Funkcja Ackley
print("Funkcja Ackleya")
ackley2D <- compar(makeAckleyFunction(2), 2, ackley_bounds, "Ackley")
ackley10D <- compar(makeAckleyFunction(10), 10, ackley_bounds, "Ackley")
ackley20D <- compar(makeAckleyFunction(20), 20, ackley_bounds, "Ackley")

# Funkcja Eggholder (tylko 2D)
print("Funkcja Eggholder")
eggholder2D <- compar(makeEggholderFunction(), 2, eggholder_bounds, "Eggholder")

# Analiza wyników
ackley2D
eggholder2D

