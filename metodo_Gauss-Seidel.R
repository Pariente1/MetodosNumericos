gauss_seidel <- function(A, b, tol = 0.0001, max_iter = 1000) {
  n <- nrow(A)  # Número de ecuaciones
  x <- rep(0, n)  # Vector inicial de soluciones (inicializamos en ceros)
  iter <- 0
  error <- tol + 1  # Inicializamos el error por encima de la tolerancia
  
  while (error > tol && iter < max_iter) {
    x_old <- x  # Guardamos la solución anterior para calcular el error
    
    for (i in 1:n) {
      # Calcula la suma de los elementos de la fila, excepto el de la diagonal
      suma <- sum(A[i, -i] * x[-i])
      x[i] <- (b[i] - suma) / A[i, i]
    }
    
    # Imprime los valores de x, y, z en cada iteración
    cat("Iteración", iter + 1, ": x =", x[1], ", y =", x[2], ", z =", x[3], "\n")
    
    # Calcula el error como la norma de la diferencia entre las iteraciones
    error <- sqrt(sum((x - x_old)^2))
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


# Ejecutamos el método de Gauss-Seidel
resultado <- gauss_seidel(A, b, tol = 1e-7)
cat("La solución aproximada es:", resultado$solution, "\n")
cat("Número de iteraciones:", resultado$iterations, "\n")
