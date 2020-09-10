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



