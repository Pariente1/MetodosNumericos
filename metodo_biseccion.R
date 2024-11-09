biseccion <- function(f, a, b, tol = 1e-7, max_iter = 1000) {
  # Verifica si el intervalo es válido
  if (f(a) * f(b) > 0) {
    stop("La función debe cambiar de signo en el intervalo [a, b].")
  }
  
  # Inicialización de variables
  iter <- 0
  error <- tol + 1  # Inicializamos el error por encima de la tolerancia
  
  while (error > tol && iter < max_iter) {
    c <- (a + b) / 2  # Punto medio del intervalo
    
    if (f(c) == 0) {  # Si encontramos la raíz exacta
      return(c)
    } else if (f(a) * f(c) < 0) {
      b <- c  # La raíz está en [a, c]
    } else {
      a <- c  # La raíz está en [c, b]
    }
    
    error <- abs(b - a) / 2  # Calcula el error
    iter <- iter + 1
  }
  
  # Imprime mensaje de advertencia si no se alcanza la tolerancia
  if (iter >= max_iter) {
    warning("Se alcanzó el número máximo de iteraciones sin cumplir la tolerancia.")
  }
  
  return(c)  # Devuelve el valor aproximado de la raíz
}

# Ejemplo de uso
f <- function(x) x^3 - x - 2  # Definimos la función
raiz <- biseccion(f, a = 1, b = 2, tol = 1e-7)
cat("La raíz aproximada es:", raiz)

