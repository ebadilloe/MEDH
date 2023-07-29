library(tidyverse); library(summarytools)

# Cargando el directorio
setwd("C:/.../Métodos cuantitativos/Ejercicio")

# Cargando la base de datos
# Descripción de los datos: http://fmwww.bc.edu/ec-p/data/wooldridge/wage1.des
data <- read_excel("wage1.xlsx")

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