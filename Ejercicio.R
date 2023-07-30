#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#**Algunas funciones básicas para el manejo de datos**         #  
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#     El análisis de datos permite brindar una mirada precisa de la información

#**Estadística:** ¿Para qué nos sirve?
#     Analizar información y tomar decisiones frente a situaciones inciertas
#     Adquirir, organizar y analizar datos
#     Cuantificar un determinado escenario de la vida real

#**Estadistica descriptiva** y estadística inferencial
#     La estadística descriptiva está formada por procedimientos empleados 
#      para resumir y describir las características importantes de un conjunto 
#      de mediciones. 
#      La estadística inferencial está formada por procedimientos empleados 
#      para extraer ideas y aproximaciones acerca de características 
#      poblacionales, a partir de información contenida en una muestra

#**Análisis exploratorio de los datos (Describirlos)**
#     ¿Cuántas observaciones tenemos?
#     ¿Cuántas variables? 
#     ¿En qué unidad se encuentran almacenados?
#     ¿Cuál es la periodicidad de nuestros datos?
#     ¿Cómo están distribuidos? 

#**Existen diversos programas**, aplicaciones, paquetes, softwares e, incluso, 
#      lenguajes de programación que permiten realizar exploración, análisis, 
#      cálculos, inferencia, ilustración, entre otros, de los datos:
#      Stata, SQL, R, R-studio, Spss, Excel, Python
#      R: Lenguaje de código abierto (gratuito)

#**Una librería** es un conjunto de códigos de programación que facilitan el trabajo
#      con lenguajes de programación. Existen librerías enfocadas a distintas tareas. 
#      Las librerías contienen funciones para realizar esas tareas.
#      Tidyverse: Es una colección de paquetes para R diseñados para la ciencia de datos
#      Ggplot2: Es un paquete parte de Tidyverse para crear gráficos. 
#      En ggplot, aes() hace referencia al contenido estético del gráfico. 
#      Es decir, la función le dará indicios a ggplot2 sobre cómo dibujar los 
#      distintos trazos, formas, colores y tamaños
#      Dplyr: manipulación de marcos de datos    

install.packages("tidyverse")
install.packages("summarytools")
library(tidyverse); library(summarytools)

#**Importar Data**
      #Podemos importar bases de datos de Excel y otro tipo de archivos 
      # con extensión csv (Comma Separated Values (en inglés) o valores separados por comas),
      # así como archivos del programa estadístico Stata

# Iniciando la sesion de trabajo podemos fijar el directorio 
# donde vamos a realizar esta, por ejemplo, el directorio que contiene la base de 
# datos a importar

# Fijando el directorio
#setwd("C:/.../Métodos cuantitativos/Ejercicio")

# Cargando la base de datos
#install.packages("readxl")
library(readxl)
data <- read_excel("wage1.xlsx")
# Descripción de los datos: http://fmwww.bc.edu/ec-p/data/wooldridge/wage1.des
# Datos que han sido recopilados por otros y se encuentran organizados en la 
# forma tradicional de una base de datos (filas y columnas)

head(data)
tail(data)
#View(data)
str(data) #conocer la estructura de la bbdd
min(data$wage)
max(data$wage)

# Estadísticas descriptivas de las variables de interés: salarios, educación y sexo
descr(data, stats = "common", transpose = TRUE)
descr(data[,c("wage","educ","female")], stats = "common", transpose = TRUE)

stview(dfSummary(data[,c("wage","educ","female")]))

freq(data$female)
freq(data$educ)

# Relación entre salarios y educación
ggplot(data = data, mapping = aes(educ, wage)) +
  geom_point(size=2) + 
  geom_smooth(method = "lm", se=F) +
  labs(x="Años de educación", y="Salario hora", title="Relación salarios-educación")

cor(data[,c("wage","educ")])  

# Relación entre salarios y educación distinguiendo por hombre y mujer

# Media de los salarios y educación entre hombres y mujeres
data |> group_by(female) |> 
  summarise(mean_salario = mean(wage),
            mean_educ = mean(educ))

# Gráfico de caja (boxplot) de salarios entre hombres y mujeres
ggplot(data = data) + 
  geom_boxplot(aes(factor(female), wage)) +
  labs(y="Salario hora", x="Género") +
  scale_x_discrete(labels=c("Hombres","Mujeres"))

# Relación entre salarios y educación distinguiendo entre hombres y mujeres
datamujer <- data |> 
  filter(female==1)

datahombre <- data |> 
  filter(female==0)

cor(datamujer[,c("wage","educ")])
cor(datahombre[,c("wage","educ")])

ggplot(data = data, aes(educ, wage, color = factor(female))) +
  geom_point(show.legend = FALSE) + 
  geom_smooth(method="lm", se=FALSE, show.legend = FALSE) + 
  facet_grid(~female, labeller = labeller(female=c("0" = "Hombres", "1" = "Mujeres"))) +
  labs(x="Años de educación", y="Salario hora", title="Relación salarios-educación")
