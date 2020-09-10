#Motor Trend Car Road Test
#Estructura de los datos
str(mtcars)
#Informacion 
help(mtcars)

#Trasnformacion de variables
mtcars$vs = as.logical(mtcars$vs)
mtcars$am = as.logical(mtcars$am)