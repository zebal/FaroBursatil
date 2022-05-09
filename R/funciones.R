# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


formato_robot <- function(robot, StartDay, EndDay){
  robot = robot[,c(2,6)]
  colnames(robot) <- c('Fecha',"Retorno")

  #robot$Fecha = paste(substr(robot$Fecha, 7, 10), "-")
  robot$Fecha = as.POSIXct(substr(robot$Fecha,1,10), format = "%d/%m/%Y")#tz = "UTC")
  robot$Retorno = gsub(",", ".", robot$Retorno)
  robot$Retorno = gsub("%", "", robot$Retorno)
  robot$Retorno = as.numeric(robot$Retorno) / 100
  robot = stats::aggregate(robot$Retorno, by=list(Fecha=robot$Fecha), FUN=sum)
  rownames(robot) = robot$Fecha
  colnames(robot) <- c('Fecha',"Retorno")
  robot = xts::as.xts(robot)
  robot = robot[,c(2)]
  robot = as.data.frame(robot)
  robot$Retorno = as.numeric(robot$Retorno)
  robot = xts::as.xts(robot)
  robot = stats::window(robot, start = StartDay, end = EndDay)

  return(robot)
}

  test_function <- function(frase){


    return(frase)
}

