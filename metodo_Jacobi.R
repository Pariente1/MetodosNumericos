jacobi <- function(A, b, tol = 0.0001, max_iter = 1000) {
  n <- nrow(A)  # Número de ecuaciones
  x <- rep(0, n)  # Vector inicial de soluciones (inicializamos en ceros)
  x_prev <- x  # Vector para almacenar la iteración previa
  iter <- 0
  error <- tol + 1  # Inicializamos el error por encima de la tolerancia
  
  while (error > tol && iter < max_iter) {
    for (i in 1:n) {
      # Calcula la suma de los elementos de la fila excepto el de la diagonal
      suma <- sum(A[i, -i] * x_prev[-i])
      x[i] <- (b[i] - suma) / A[i, i]
    }
    # Calcula el error como la norma de la diferencia entre las iteraciones
    error <- sqrt(sum((x - x_prev)^2))
    x_prev <- x
    iter <- iter + 1
  }
  
  if (iter >= max_iter) {
    warning("Se alcanzó el número máximo de iteraciones sin cumplir la tolerancia.")
  }
  
  return(list(solution = x, iterations = iter))
}

# Definimos la matriz de coeficientes y el vector de constantes
A <- matrix(c(10, 3, 1, 5, -10, 3, 1, 3, 10), nrow = 3, byrow = TRUE)
b <- c(14, -5, 14)

# Ejecutamos el método de Jacobi
resultado <- jacobi(A, b, tol = 0.0001)
cat("La solución aproximada es:", resultado$solution, "\n")
cat("Número de iteraciones:", resultado$iterations, "\n")
