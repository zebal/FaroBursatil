\name{formato_robot}
\alias{formato_robot}
\title{Formato Robot}
\usage{
formato_robot()
}
\description{
Le da formato al fichero csv descargado de Prorealtime.
Los campos de entrada son:
Robot -> dataset del csv descargado de Prorealtime
StartDay -> Desde que día quieres que coja datos
EndDay -> Hasta que día quieres que coja datos
}
\examples{
formato_robot(robot,'2021-01-01','2021-12-31')
}
