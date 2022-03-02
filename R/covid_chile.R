#' Casos nuevos diarios Covid-19 en Chile.
#'
#' @param NULL Esta funcion no usa parametros.
#' @return Retorna los contagiados de los ultimos 7 dias y un grafico.
#' @examples
#' covid_chile()
covid_chile <- function(){
  test <- suppressWarnings(require(rio))
  if (test == T) {
    
    ruta1 <- "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/"
    ruta2 <- "master/output/producto5/TotalesNacionales.csv"
    ruta <- paste(ruta1, ruta2, sep = "")
    
    data = import(ruta, format = "csv", header = T)
    n <- length(data)
    ultimos <- data[7, c((n - 6):n)]
    
    print("Los contagiados de los 7 días anteriores son:")
    print(ultimos)
    
    suppressWarnings(casos_nuevos <- as.vector(na.omit(as.numeric(data[7, 
    ]))))
    
    fecha <- as.Date(colnames(data)[-1])
    n <- length(casos_nuevos)
    tiempo <- 1:n
    
    plot(tiempo, casos_nuevos, pch = 20, col = "skyblue3", 
         lwd = 2, type = "l", xlab = "Tiempo", ylab = "Casos Nuevos", 
         las = 1, bty = "n", main = "Casos Diarios Covid-19 ")
    
    linea <- smooth.spline(tiempo, casos_nuevos, spar = 0.5)
    lines(linea, col = "orange", lwd = 2)
    
    print(
      '________________________________________________________________________'
    )
    
    print('Un resumen de los últimos 7 días:')
    print(
      summary(casos_nuevos[(length(casos_nuevos)-6): length(casos_nuevos)])
    )
  }
  else {
    print("Se necesita instalar la librería rio")
  }
}

