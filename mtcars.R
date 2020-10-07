#Motor Trend Car Road Test

##Datos Structure
str(mtcars)
###Information 
help(mtcars)

###Transformation of variables
mtcars$vs = as.logical(mtcars$vs)
mtcars$am = as.logical(mtcars$am)

##Summary
summary(mtcars.new)

###Transformed pounds to Kg
mtcars.new <- transform(mtcars, wt = wt*1000/2.2)

###Some Values
mtcars[mtcars$qsec > 20,]

###Subset
mtcars.new <- subset(mtcars, qsec >= 20 & cyl > 4)

##Show values
head(mtcars)
tail(mtcars)
glimpse(mtcars)

#EDA
##Scatter plot mgp vs hp
plot(mtcars$mpg ~ mtcars$cyl,
     xlab ="Cylinders", ylab ="Miles for gallon",
     main ="Relationship between cylinders and miles for gallon")

plot(mtcars$mpg ~ mtcars$hp,
     xlab ="Horsepower", ylab ="Miles for gallon",
     main ="Relacion Cilindros y Caballos de fuerza")

#Histogram
#with native function of R
hist(mtcars$hp,
      geom = "Histogram",
      xlab = "Horsepower (HP)",
      ylab = "Cars",
      main = "Cars according with horsepower")

#Use ggplot2
ggplot(mtcars, aes(x=hp))+
  geom_histogram()+
  labs(x="Hoursepower", y ="Number of cars",
       title = "Hoursepower in select cars")

ggplot(mtcars, aes(x=hp))+
  geom_histogram(binwidth = 30)+
  labs(x="Hoursepower", y="Number of cars",
       title = "Hourspower in select cars")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),panel.grid.major =  element_blank(),
        panel.grid.minor = element_blank())

#More complete form
ggplot()+ geom_histogram(data = mtcars,
                         aes(x=hp),fill="blue",color="red",
                         binwidth =  20)+
  labs(x="Hoursepower", y= "Number of cars",
       title="Hoursepower in select cars")+
  xlim(c(80,280))+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),panel.grid.major =  element_blank(),
        panel.grid.minor = element_blank())

#Boxplot

boxplot(mtcars$hp,
        ylab="Hoursepower",
        main= "Hoursepower  in mtcars")

## Use ggplot
ggplot(mtcars, aes(x=as.factor(cyl),y=hp,fill=cyl))+
  geom_boxplot(alpha = 0.6)+
  labs(x="Cylinders",y="Hoursepower",
       title ="Hoursepower in cylinders")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),panel.grid.major =  element_blank(),
        panel.grid.minor = element_blank())
  


ggplot(mtcars, aes(x=am, y=mpg, fill=am))+
    geom_boxplot()+
    labs(x="Type car-box", y="Miles of gallon",
         title ="Miles for gallon according types of car-box")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),panel.grid.major =  element_blank(),
        panel.grid.minor = element_blank())

mtcars$am <- factor(mtcars$am, levels=c(TRUE,FALSE),
                    labels =c("Manual","Automatic"))

#Scatter plot - Two variables
ggplot(mtcars, aes(hp,mpg))+
  geom_point()+
  labs(x="Hoursepowe",y="Miles for gallon",
       title="Relationship hoursepower with miles for gallon")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major =  element_blank(),
        panel.grid.minor = element_blank())

#
ggplot(mtcars, aes(wt,hp))+
  geom_point()+
  labs(x="Weight", y="Power",
       title ="Relationship Weight-Power")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major =  element_blank(),
        panel.grid.minor = element_blank())

#
ggplot(mtcars, aes(hp,qsec))+
  geom_point(aes(color=am,size=cyl))+
  labs(s="Hoursepower", y ="Time in 1/4 miles",
       title="Hoursepower-Velocity according cylinder capacity and
       type of box")

##Correlation with Pair
pairs(mtcars[,2:6])

###Subset Function SELECT
newdata <- subset(mtcars, select = c(2,7:8,11,12))
pairs(newdata)

###leaving out the colummns
pairs(mtcars[, -c(1,3,4,5,6,9,10)])

###Function Filter
Eficientes <- filter(mtcars, mpg >= 30)
Eficientes
pairs(Eficientes[,2:6])

#Filter for type character using stringr
merc <- mtcars %>%
  filter(str_detect(model,"Merc"))
pairs(merc[,2:6])


### Using cor function
cor(mtcars[,2:6])

cor(newdata)

cor(merc[,2:6])


#Coefficient of varation

prom <- mean(mtcars$mpg)
desv <- sd(mtcars$mpg)
CoefVar <- (desv/prom)*100
CoefVar

#Adjusting data to improve visualizations
eficientes <- mean(mtcars$mpg)
eficientes

mtcars <- mtcars %>% 
  mutate(Mas_efecientes = ifelse(mpg<eficientes,
                                 "below average",
                                 "at or above average"))

Mas_veloces <- mtcars[mtcars$qsec<16,]
Mas_veloces

mtcars <- mtcars %>%
  mutate(Velocidad_Cuarto_milla=ifeslse(qsec <16,
                                        "less that 16s",
                                        "more that 16s"))
#
mtcars <- mtcars %>%
  mutate(Peso_kilos=(wt/2*1000))

mtcars <- mtcars %>%
  mutate(Peso_kilos=ifelse(Peso_kilos <= 1500,
                     "Light",
                     "Heavy"))

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