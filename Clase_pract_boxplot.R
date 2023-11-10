#clase practica grafico de barras, histogramas y boxplots. 02/11
rm(list=ls())
setwd("/home/clinux01/Escritorio/Cande Labo Jueves/Practica_4/")

# Cargo las librerias a utilizar
library(ggplot2)
library(lubridate)

#-------------------------------------------------------------------------------
#BARRAS
#Para precipitacion esta bueno barras
gualeguaychu <- read.csv("gualeguaychu.csv") #creo q tamb podria read.table pero deberia poner q la separacion es una coma
head(gualeguaychu) #1° col me da un codigo de estacion

#Como esta serie de tiempo es muy larga nos quedamos con un periodo más corto (2010-2018) y un año (2018) que guardo en respectivos data.frames

gualeguaychu <- gualeguaychu[year(gualeguaychu$Fecha) >= 2010 & 
                               year(gualeguaychu$Fecha) <= 2018,]
gualeguaychu_2018 <- gualeguaychu[year(gualeguaychu$Fecha) == 2018,]

#Ahora vamos a graficar los datos de precipitación del año 2018 con la funcion geom_col() de ggplot que directamente genera columnas y la altura nos indicara la cantidad de preciítación
p <- ggplot(data = gualeguaychu_2018, mapping = aes(x=Fecha, y=pre))
p <- p + geom_col()

#Para que el eje x se lea mejor, voy a usar month(Fecha), es decir de la variable fecha selecciono solo los meses
p <- ggplot(data = gualeguaychu_2018, mapping = aes(x= month(Fecha), y=pre))
p<- p + geom_col() #Ggplot calculo la suma de los valores de precipitación para cada mes para poder graficar una barra para cada mes

#Podemos agregarle los títulos, nombres de los ejes al gráfico. Tambien defino el eje x como continuo y los breaks para que tenga el valor de cada mes
p<- ggplot(data = gualeguaychu_2018, mapping = aes(x= month(Fecha), y=pre))
p<- p + geom_col()
p <- p + labs(title = "Precipitación mensual acumulada 2018",
              subtitle = "Estación: Gualeguaychu",
              x = "Mes",
              y = "Precipitación (mm)")
p <- p + scale_x_continuous(breaks = c(1:12))

#Podria agregar color a las barras con los argumentos color=“ “ y fill =“ ” dentro de geom_col()

#Ahora supongamos que queremos ver la información de varios años. Usemos los datos del periodo 2010-2018 y calculemos la suma de la precipitación para cada mes de ese periodo. Usamos aggregate.
#Vamos a usar una nueva función del paquete lubridate para manipular fechas. Fecha lo paso a una variable tipo Date (con as.Date),y lo que hace floor_date(Fecha, “month”) es “redondear para abajo” la fecha hasta el mes. Entonces por ejemplo todas las fechas del mes de enero de 2000 pasan a ser “2000-01-01” sin importar el día.

gualeguaychu$year <- year(gualeguaychu$Fecha) #para poder separar en grupos y hacer el aggregate
gualeguaychu$month <- month(gualeguaychu$Fecha)
suma_PP_mensual <- aggregate(x=gualeguaychu$pre,by=list(gualeguaychu$year,gualeguaychu$month),FUN="sum") #acumulado mensual. Tengo q hacer subgrupos
#Cambio los nombres de las columnas
colnames(suma_PP_mensual) <- c("Year", "Month", "Precip")
head(suma_PP_mensual)

#Ahora miremos la suma (acumulado) precipitación para cada mes y año y juguemos con el argumento position.
suma_PP_mensual$Fecha<- paste(suma_PP_mensual$Year,suma_PP_mensual$Month,sep="-")
#suma_PP_mensual$Fecha2<- seq.Date(from = as.Date("2010-01-01"),to=as.Date("2018-10-01"),by="month")
p<- ggplot(suma_PP_mensual, aes(x=Fecha, y=Precip)) +
  geom_col(aes(fill = factor(Year)), position = "dodge") +
  labs(title = "Precipitación mensual en Gualeguaychu",
       x = "Mes",
       y = "Precipitación [mm]",
       fill = "Year") #pintalas en funcion de mi columna anio

#Si bien nos da algo de informacion, puede ser muy confuso. Veamos otra opcion que nos permite generar un panel para cada año usando facet_wrap().
p<-ggplot(suma_PP_mensual, aes(Month, Precip)) +
  geom_col() +
  facet_wrap(~Year, ncol =4) +
  labs(title = "Precipitación mensual en Gualeguaychu",
       x = "Mes",
       y = "Precipitación [mm]") + scale_x_continuous(breaks =seq(1,12,1))

#-------------------------------------------------------------------------------
#HISTOGRAMAS: grafico de barras que en el eje y muestra la frecuencia. Entonces para los histogramas, en el eje x ponemos la variable continua que estamos estudiando. Esta variable la dividimos en espacios para contar cuantos datos caen en cada uno. Estos espacios se llaman bins o intervalos de clase. En el eje y de los histogramas va el conteo de datos por bin. Hacer estos conteos nos da una distribución de la frecuencia y sirve para tener una idea de donde está la mayoría de los datos.

#Vamos a construir un histograma con todos los datos de precipitación del periodo 2010-2018.
p<-ggplot(gualeguaychu, mapping = aes(x=pre))+ geom_histogram()

#Tenemos muchos valores que son 0, esto suele ocurrir con la precipitación. Podemos no considerarlos, y asi ver que valores predominan. Entonces en un nuevo data.frame guardo los dias donde la pp fue mayor a 0 mm
precip_gualeguaychu<- gualeguaychu[gualeguaychu$pre>0, ]
p <-ggplot(data = precip_gualeguaychu ,aes(pre))
p <- p + geom_histogram(breaks= seq(0,200,10))

#Vemos que la mayoria de los datos estan entre 0 y 10 mm. Podemos agregar color a las barras, los titulos, etc.
p <- p + geom_histogram(breaks= seq(0,200,10), fill= "blue", col= "black")
p <- p + labs(title = "Histograma de precipitación diaria en Gualeguaychu",
              subtitle = "Periodo 2000-2010", x = "Precipitación (mm)",
              y = "Frecuencia")

#-------------------------------------------------------------------------------
#BOXPLOT
#Grafiquemos el boxplot de la precipitación sin ceros
box <-ggplot(data = precip_gualeguaychu ,aes(y= pre))
box <- box + geom_boxplot()

#Podemos retocar algunos argumentos para que el boxplot quede más lindo
box <- box + geom_boxplot(outlier.colour = "black", outlier.size = 1, notch = TRUE,fill = "#E69F00")
box <- box + labs(title = "Boxplot de precipitación diaria en Gualeguaychu",
                  subtitle = "Periodo 2000-2010",
                  y = "Precipitación (mm)")
box <- box + stat_boxplot(geom = "errorbar") +xlim(c(-2, 2))




