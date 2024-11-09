steffensen <- function(f, y0, tol = 1e-7, max_iter = 100) {
  y <- y0  # Valor inicial
  iter <- 0  # Contador de iteraciones
  
  repeat {
    # Calcular f(y) y f(y + f(y))
    fy <- f(y)
    fy_plus_fy <- f(y + fy)
    
    # Calcular el siguiente valor de y usando la fórmula de Steffensen
    y_new <- y - (fy^2) / (fy_plus_fy - fy)
    
    # Imprimir el valor de y en cada iteración
    cat("Iteración", iter + 1, ": y =", y_new, "\n")
    
    # Comprobar la tolerancia
    if (abs(y_new - y) < tol || iter >= max_iter) {
      break
    }
    
    # Actualizar y y el contador de iteraciones
    y <- y_new
    iter <- iter + 1
  }
  
  if (iter >= max_iter) {
    warning("Se alcanzó el número máximo de iteraciones sin cumplir la tolerancia.")
  }
  
  return(list(solution = y_new, iterations = iter))
}

# Definir la función f(y) = y^3 - y - 1
f <- function(y) {
  y^3 - y - 1
}

# Intervalo [1, 2], comenzamos con un valor inicial en el intervalo, por ejemplo, y0 = 1.5
y0 <- 1.5

# Ejecutar el método de Steffensen
resultado <- steffensen(f, y0, tol = 1e-7)
cat("La solución aproximada es: y =", resultado$solution, "\n")
cat("Número de iteraciones:", resultado$iterations, "\n")
