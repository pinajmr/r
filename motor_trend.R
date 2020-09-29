#Motor Trend Car Road Test
#Estructura de los datos
str(mtcars)
#Informacion 
help(mtcars)

#Trasnformacion de variables
mtcars$vs = as.logical(mtcars$vs)
mtcars$am = as.logical(mtcars$am)

#Resumen 
summary(mtcars.new)

#Transformando libras a Kg
mtcars.new <- transform(mtcars, wt = wt*1000/2.2)

#Algunos valores
mtcars[mtcars$qsec > 20,]

#Subset
mtcars.new <- subset(mtcars, qsec >= 20 & cyl > 4)

#Ver valores
head(mtcars)
tail(mtcars)
glimpse(mtcars)

#EDA
#Scatter plot mgp vs hp
plot(mtcars$mpg ~ mtcars$cyl,
     xlab ="Cilindros", ylab ="Milas por galon",
     main ="Relacion Cilindros y millas por galon")

plot(mtcars$mpg ~ mtcars$hp,
     xlab ="Caballos de fuerza", ylab ="Millas por galon",
     main ="Relacion Cilindros y Caballos de fuerza")

#Histograma
#con funcion nativa de R
hist(mtcars$hp,
      geom = "histograma",
      xlab = "Caballos de fuerza",
      ylab = "Carros",
      main = "Carros segun caballos de fuerza")

#Libreria ggplot2
ggplot(mtcars, aes(x=hp))+
  geom_histogram()+
  labs(x="Caballos de Fuerza", y ="Cantidad de carros",
       title = "Caballos de fuerza en carros seleccionada")

ggplot(mtcars, aes(x=hp))+
  geom_histogram(binwidth = 30)+
  labs(x="Caballos de fuerza", y="Cantidad de carros",
       title = "Caballos de fuerza en carros seleccionados")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),panel.grid.major =  element_blank(),
        panel.grid.minor = element_blank())

#Forma mas completa
ggplot()+ geom_histogram(data = mtcars,
                         aes(x=hp),fill="blue",color="red",
                         binwidth =  20)+
  labs(x="Caballos de fuerza", y= "Cantidad de carros",
       title="Caballos de fuerza en carros seleccionados")+
  xlim(c(80,280))+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),panel.grid.major =  element_blank(),
        panel.grid.minor = element_blank())

#Boxplot

boxplot(mtcars$hp,
        ylab="Caballos de Fuerza",
        main= "Caballos de Fuerza en carros mtcars")

#Usando ggplot
ggplot(mtcars, aes(x=as.factor(cyl),y=hp,fill=cyl))+
  geom_boxplot(alpha = 0.6)+
  labs(x="cilindros",y="caballos de fuerza",
       title ="Caballos de Fuerza segun cilindros en mtcars")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),panel.grid.major =  element_blank(),
        panel.grid.minor = element_blank())
  


ggplot(mtcars, aes(x=am, y=mpg, fill=am))+
    geom_boxplot()+
    labs(x="Tipo de caja", y="millas por galon",
         title ="Millas por galon segun tipo de caja-mtcars")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),panel.grid.major =  element_blank(),
        panel.grid.minor = element_blank())

mtcars$am <- factor(mtcars$am, levels=c(TRUE,FALSE),
                    labels =c("Manual","Automatico"))

#Scatter plot - dos variables
ggplot(mtcars, aes(hp,mpg))+
  geom_point()+
  labs(x="Caballo Fuerza",y="Millas por galon",
       title="Relacion caballos de fuerza y millas por galon")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major =  element_blank(),
        panel.grid.minor = element_blank())

#
ggplot(mtcars, aes(wt,hp))+
  geom_point()+
  labs(x="Peso", y="Potencia",
       title ="Relacion Peso-Potencia")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major =  element_blank(),
        panel.grid.minor = element_blank())

#
ggplot(mtcars, aes(hp,qsec))+
  geom_point(aes(color=am,size=cyl))+
  labs(s="Caballos de fuerza", y ="tiempo en 1/4 milla",
       title="Caballos-Velocidad segun cilindraje y
       tipo de caja")

##Correlacion con Pair
pairs(mtcars[,2:6])

#Sacando un subset Funcion SELECT
newdata <- subset(mtcars, select = c(2,7:8,11,12))
pairs(newdata)

#Dejando afuera las columnas
pairs(mtcars[, -c(1,3,4,5,6,9,10)])

#Funcion Filter
Eficientes <- filter(mtcars, mpg >= 30)
Eficientes
pairs(Eficientes[,2:6])

#Filtro por tipo caracteres usando stringr
merc <- mtcars %>%
  filter(str_detect(model,"Merc"))
pairs(merc[,2:6])


#Usando la funcion cor
cor(mtcars[,2:6])

cor(newdata)

cor(merc[,2:6])


#Coeficiente de Variacion

prom <- mean(mtcars$mpg)
desv <- sd(mtcars$mpg)
CoefVar <- (desv/prom)*100
CoefVar

#Adjustando datos para mejorar visualizaciones
eficientes <- mean(mtcars$mpg)
eficientes

mtcars <- mtcars %>% 
  mutate(Mas_efecientes = ifelse(mpg<eficientes,
                                 "bajo el promedio",
                                 "en o sobre promedio"))

Mas_veloces <- mtcars[mtcars$qsec<16,]
Mas_veloces

mtcars <- mtcars %>%
  mutate(Velocidad_Cuarto_milla=ifeslse(qsec <16,
                                        "Menos 16s",
                                        "Mas 16s"))
#
mtcars <- mtcars %>%
  mutate(Peso_kilos=(wt/2*1000))

mtcars <- mtcars %>%
  mutate(Peso_kilos=ifelse(Peso_kilos <= 1500,
                     "Livianos",
                     "Pesados"))

#
mtcars %>% 
  arrange(desc(Peso_kilos))

Mas_pesados <- mtcars %>%
  filter(model %in% c("Lincoln Continental","Chrysler Imperial",
                      "Cadillac Fleetwood","Merc 450SE"))
Mas_pesados

#
ggplot(Mas_pesados, aes(x=hp,y=mpg))+
  geom_point()+
  facet_wrap(~model)
                      
#
ggplot(mtcars,aes(x=cyl,y=mpg,size=Peso_kilos))+
geom_point()+
  facet_wrap(~am)