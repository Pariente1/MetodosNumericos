install.packages("Deriv")

# Cargar la librería
library(Deriv)

# Definir la función
f <- function(x) {
  2*x^3 - 8*x^2 + 10*x - 6.1
}

# Calcular la derivada de f con respecto a x
f_prime <- Deriv(f, "x")

# imprimir fprime

f_prime

# Parámetros iniciales
x0 <- 210            # Valor inicial
tol <- 10^-4        # Error admisible
max_iter <- 100    # Número máximo de iteraciones

# Método de Newton-Raphson
newton_raphson <- function(f, f_prime, x0, tol, max_iter) {
  x <- x0
  for (i in 1:max_iter) {
    print(x)
    x_new <- x - f(x) / f_prime(x)   # Fórmula de Newton-Raphson
    
    # Verificar la tolerancia
    if (abs(x_new - x) < tol) {
      cat("La raíz aproximada es:", x_new, "en la iteración:", i, "\n")
      return(x_new)
    }
    
    # Actualizar y
    x <- x_new
  }
  cat("No se alcanzó la convergencia después de", max_iter, "iteraciones.\n")
  return(NA)
}

# Ejecutar el método
newton_raphson(f, f_prime, x0, tol, max_iter)
