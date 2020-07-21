#' Casos nuevos diarios Covid-19 en Chile.
#'
#' @param NULL Esta funcion no usa parametros.
#' @return Retorna los contagiados de los ultimos 7 dias y un grafico.
#' @examples
#' covid_chile()
covid_chile <- function(){

  ruta1 <- 'https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/'
  ruta2 <- 'master/output/producto5/TotalesNacionales.csv'
  ruta <- paste(ruta1, ruta2, sep = '')
  data = import(ruta)

  #Predicción para Lunes 13 de Julio:

  n <- length(data)
  ultimos <- data[7, c((n-6):n)]
  print('Los contagiados de los 7 días anteriores son:')
  print(ultimos)
  # Casos nuevos  -----------------------------------------------------------


  suppressWarnings(casos_nuevos <- as.vector(na.omit(as.numeric(data[7,]))))
  fecha <- as.Date(colnames(data)[-1])


  # Regresión Lineal --------------------------------------------------------

  n <- length(casos_nuevos)
  tiempo <- 1:n
  tiempo2 <- tiempo^2
  dummy <- c(rep(0, 70), rep(1, (n-70)))

  # Gráfico -----------------------------------------------------------------

  plot(
    tiempo,
    casos_nuevos,
    pch = 20,
    col = 'steelblue',
    lwd = 3,
    type = 'l',
    xlab = 'Tiempo',
    ylab = 'Casos Nuevos',
    las = 1,
    bty = 'n',
    main = 'Casos Diarios Covid-19 '
  )


  linea <- smooth.spline(tiempo, casos_nuevos, spar = 0.8)
  lines(linea, col = 'orange', lwd = 2)

}

