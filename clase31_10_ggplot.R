#Clase 31/10. ggplot
rm(list=ls())
#setwd("/home/clinux01/Escritorio/Cande/Practica_4/")
setwd("/Users/cande/Desktop/Labo/Practica_4/")
library(ggplot2)
#ggplot tiene distintas capas.

#primera capa: DATOS. Siempre datos en df
datos<-iris

#segunda capa: MAPEO. Como los datos del df se relacionan. Indico quien es x, quien y.
              #Tamb cosas esteticas. 
ggplot(datos, aes(x = Petal.Length, y = Petal.Width, color =Species, shape= Species))

#tercera capa: ESTADISTICAS. En funcion de los datos q le doy. Esta bueno por ej para regresion lineal, box plot y te ubica medias y percentiles porq los calcula solito
ggplot(datos, aes(x = Petal.Length, y = Petal.Width)) +
  geom_smooth(method = "lm", se = FALSE, aes(color = Species))

#cuarta capa: ESCALAS. le puedo dar la escala q quiero. Puede ser continua o discreta o tiempo. Tamb tiene en cuenta la escala de colores
ggplot(datos, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(aes(color = Species, shape = Species), size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, aes(color = Species))
scale_color_manual(values = c("darkorange", "purple", "cyan4")) 

#quinta capa:GEOMETRIAS.define el tipo de graf q quiero q haga. El valor con una forma en particular
ggplot(datos, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(aes(color = Species, shape = Species), size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, aes(color = Species))

#sexta capa:PANELES. En cada uno grafica algo en particular
ggplot(datos, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(aes(color = Species, shape = Species), size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, aes(color = Species))
scale_color_manual(values = c("darkorange", "purple", "cyan4")) +
  facet_wrap(~Species)

#septima capa: COORDENADAS

#octava capa: TEMA. estilo del grafico. Aesthetic

#-------------------------------------------------------------------------------
#Practica series temporales 
rm(list=ls())
#setwd("/home/clinux01/Escritorio/Cande/Practica_4/")
setwd("/Users/cande/Desktop/Labo/Practica_4/")
# Cargo las librerias que voy a necesitar
library(ggplot2)
library(lubridate)
library(metR)
#Quedemosnos unicamente con el punto mas cercano a la estacion OCBA (-34,-58)
#archivo <- "/home/clinux01/Escritorio/Cande/Practica_4/air.mon.mean.nc" #donde esta el archivo
archivo <-"air.mon.mean.nc"
GlanceNetCDF(archivo)
datos_OCBA<- ReadNetCDF(archivo, vars = "air", subset = list(lat =-34, lon = 360-58))
# Me quedo con el periodo 1990-2010
datos_OCBA_periodo<- datos_OCBA[which(year(datos_OCBA$time) %in% 1990:2010),]
head(datos_OCBA_periodo)# Miro los datos

#Primera capa
grafico <- ggplot(data = datos_OCBA_periodo, mapping = aes(x= time, y= air))
grafico

# Ahora agrego la geometria que quiero, en este caso como es una serie temporal puedo
#usar lineas. Uso geom_line
grafico<- grafico + geom_line()
grafico
grafico <- grafico + geom_line(color= "blue") #quiero linea en azul
grafico

grafico <- grafico + geom_smooth(method = "lm", se = FALSE)

#agrego titulos, subtitulos, ejes
grafico <- grafico + labs(title = "Temperatura OCBA", subtitle = "Periodo 1990-2010",
                          x = "mes",
                          y = "Temperatura(?C)" )
grafico

#-------------------------------------------------------------------------------
#EJERCICIO
rm(list=ls())
#setwd("/home/clinux01/Escritorio/Cande/Practica_4/")
#setwd("/Users/cande/Desktop/Labo/Practica_4/")
setwd("/home/clinux01/Escritorio/Cande Labo Jueves/Practica_4/")
#Calcular el promedio de temperatura anual (promedio de los 12 meses del
#anio) y graficar la serie resultante con lineas y puntos. Ademas agregar la
#linea de tendencia lineal.
library(ggplot2)
library(lubridate)
library(metR)
#archivo<-"/home/clinux01/Escritorio/Cande/Practica_4/air.mon.mean.nc"
archivo<-"air.mon.mean.nc"
GlanceNetCDF(archivo) #Vemos las dimensiones del archivo con la función GlanceNetCDF. Este te devuelve en forma de df
datos <- ReadNetCDF(archivo, vars = "air")

#Si quiero los datos de cada anio
#anios <- years(datos$time)
#chequear desp con el resuelto
prom_anual<-c()
for(anio in 1948:2021){
  dato_anio<-mean(datos$air[which(year(datos$time)==anio)],na.rm = T)
  prom_anual<-c(prom_anual, dato_anio)
}

head(prom_anual)
tail(prom_anual)

anios<-1948:2021
df_datos<-data.frame("anios"=anios,"promedio"=prom_anual)
#Quiero graficar el promedio anual de temperatura
#PRIMERA CAPA
grafico <- ggplot(data = df_datos, mapping = aes(x= anios, y= promedio))
grafico

colors()
# Ahora agrego la geometria que quiero, en este caso como es una serie temporal puedo
#usar lineas. Uso geom_line
grafico <- grafico + geom_line(color="blue") #quiero linea en azul
grafico
grafico <- grafico + geom_point(color="red") #quiero linea en azul
grafico

#agrego titulos, subtitulos, ejes
grafico <- grafico + labs(title = "Promedio de temperatura anual", subtitle = "Periodo 1948-2021",
                          x = "anio",
                          y = "Temperatura(°C)" )
grafico





