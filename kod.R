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

#dziedzina
alpine02_bounds <- c(0, 10)
alpine01_bounds <- c(-10,10)

#Alpine 01
print("Funkcja Alpine 01")
alpine01_2D <- compar(makeAlpine01Function(dimensions = 2), 2, alpine01_bounds, "Alpine 01")
alpine01_10D <- compar(makeAlpine01Function(dimensions = 10), 10, alpine01_bounds, "Alpine 01")
alpine01_20D <- compar(makeAlpine01Function(dimensions = 20), 20, alpine01_bounds, "Alpine 01")

#Alpine 02
print("Funkcja Alpine 02")
alpine02_2D <- compar(makeAlpine02Function(dimensions = 2), 2, alpine02_bounds, "Alpine 02")
alpine02_10D <- compar(makeAlpine02Function(dimensions = 10), 10, alpine02_bounds, "Alpine 02")
alpine02_20D <- compar(makeAlpine02Function(dimensions = 20), 20, alpine02_bounds, "Alpine 02")

# Wyniki
print("Analiza wyników:")
print("Alpine 01 2D")
print(alpine01_2D)
print("Alpine 01 10D")
print(alpine01_10D)
print("Alpine 01 20D")
print(alpine01_20D)

print("Alpine 02 2D")
print(alpine02_2D)
print("Alpine 02 10D")
print(alpine02_10D)
print("Alpine 02 20D")
print(alpine02_20D)


