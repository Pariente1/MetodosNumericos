#definir la funcion
f <- function(theta){
  return(sin(theta) + cos(1 - theta^2) - 1)
}
#implementar metodo de la secante
secante <- function(f,x0,x1,tol = 1e-7, max_iter = 100){
  for (i in 1:max_iter){
    # Calcular el siguiente valor de la secante
    fx0 <- f(x0)
    fx1 <- f(x1)
    #evitar division por 0
    if (abs(fx1 - fx0) < tol){
      stop("Division por cero detectada. Intenta con otros valores iniciales")
    }
    #Formula del metodo de la secante
    x2 <- x1 - fx1 * (x1 - x0) / (fx1 - fx0)
    #verificar criterio de convergencia
    if(abs(x2-x1) < tol) {
      return(x2)
    }
    #actualizar los valores
    x0 <- x1
    x1 <- x2
  }
  stop("no se alcanzó la convergencia despues del numero maximo de iteraciones")
}
#aplicar el metodo de la secante
5
# Valores iniciales
theta0 <- 0.5
theta1 <- 1.0
# ejecutar el metodo de la secante
resultado <- secante(f, theta0, theta1)
#mostrar el resultado
cat("La raiz aproximada es:", resultado, "\n")
