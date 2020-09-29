#Economia Naranja

#Preprocesamiento
str(orangeec)

#Resumen
summary(orangeec)

#Ver valores importantes
orangeec[orangeec$Inflation > 6,]
orangeec[orangeec$Internet.penetration...population> 90,]

#Subset
orangeec.new <- subset(orangeec ,Inflation < 2 & Unemployment <= 5 | Median.age <30
                       , select = Creat.Ind...GDP)

#instalando paquete
install.packages("plyr")

#Cambiar nombre de columna
rename(orangeec,c("Creat.Ind...GDP"="AporteEcNja"))

#Ver valores
head(orangeec, n = 10)
tail(orangeec, n =5)
glimpse(orangeec)

#EDA
#Scatter plot
plot(orangeec$Unemployment ~ orangeec$Education.invest...GDP,
     xlab ="inversion en educacion (%PIB)", ylab ="Desempleo",
     main ="Relacion inversion en educacion y desempleo")

plot(orangeec$GDP.PC ~ orangeec$Creat.Ind...GDP,     
     xlab = "Aporte economia naraja al PIB(%)", 
     ylab = "PIB per capita ",
     main = "Relacion economia naranja y pib per capita")

#Histograma

ggplot() +geom_histogram(data = orangeec,
                         aes(x=Internet.penetration...population),fill ="pink", color ="black",
                         binwidth = 5)+
  scale_x_continuous(breaks = seq(40,max(100),5))+
  labs(x="Penetracion de Internet", y="Cantidad de Paises",
       title = "Penetracion de internet en los Paises de LATAM")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),panel.grid.major =  element_blank(),
        panel.grid.minor = element_blank())
#Boxplot
#Clasificacion de paises

#Variable promedio de Pir per capita
economy <- mean(orangeec$GDP.PC)

#
orangeec <- orangeec %>%
  mutate(Strong_economy = ifelse(GDP.PC < economy,
  "Por debajo del promedio PIB",
  "Sobre-Arriba del promedio PIB"))

ggplot(orangeec, aes(x=Strong_economy, y= Creat.Ind...GDP,
                     fill = Strong_economy))+
  geom_boxplot(alpha=0.4)+
  labs(x="Tipo de pais", y ="Aporte economia naranaja al pib",
       title ="Aporte economia naranja en PIB paises LATAM con alto y bajo pip per capita")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),panel.grid.major =  element_blank(),
        panel.grid.minor = element_blank())


ggplot(orangeec, aes(x=Strong_economy, y=Internet.penetration...population,
                     fill = Strong_economy))+
  geom_boxplot(alpha = 0.4)+
  labs(x="Tipo de pais",y="Penetracion de internet(%)",
       title="Penetracion de Internet en paises :Atam con alto
       y bajo PIB")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),panel.grid.major =  element_blank(),
        panel.grid.minor = element_blank())


#
ggplot(orangeec,aes(Internet.penetration...population,Creat.Ind...GDP))+
  geom_point(aes(color=factor(Strong_economy),size=GDP.Growth..))+
  labs(x="Penetracion Internet",y="Aporte economia naranja PIB",
       title="Internet y aporte economia naranja segun ecomia y
       crecimiento PIB")
#
ggplot(orangeec,aes(Education.invest...GDP,Unemployment))+
  geom_point(aes(color=factor(Strong_economy),size=X..pop.below.poverty.line))+
  labs(x="Inversion en Educacion",y="Desempleo",
       title="Inversion en educacion segun el desemplo")

#Scatter plot dinamico
my_graph <- ggplot(orangeec, aes(Internet.penetration...population,
                                 Creat.Ind...GDP,label=row.names(orangeec)))+
  geom_point()+
  labs(x="Penatracion internet",y="Aporte economia naranja",
       title="Penetracion Internet y aporte economia naranja")
my_graph
p= ggplotly(my_graph)
p


#Correlacion con pairs
pairs(orangeec[,2:6])
newdata <- subset(orangeec, select = c(5,6,10,11))

pairs(newdata)

#Correlacion con cor
cor(orangeec[,2:6])
#borrando los NAN
cor(orangeec[,2:6],use="complete.obs")

cor(newdata,use="complete.obs")

#Coeficiente de Variacion
desv <- sd(orangeec$Internet.penetration...population)
desv

prom <- mean(orangeec$Internet.penetration...population)
prom

CoefVar <- (desv/prom)*100
CoefVar

#Otras variables borrando NA\'s
prom <- mean(orangeec$Creat.Ind...GDP, na.rm = TRUE)
desv <- sd(orangeec$Creat.Ind...GDP, na.rm = TRUE)

coe <- (desv/prom)*100
coe

#Ajustando datos para una mejor visualizacion
orangeec <- orangeec %>%
  mutate(Crecimiento_GDP = ifelse(GDP.Growth.. >= 2.5,
                                  "2.5% o mas","Menos 2.5%"))

orangeec <- orangeec %>%
  mutate(Anaranjados=ifelse(Creat.Ind...GDP >= 2.5,
                            "Mas anaranjados",
                            "Menos anaranjados"))

#ranking 
orangeec %>%
  arrange(desc(Creat.Ind...GDP))

TopNaranjas <- orangeec %>%
  filter(Country %in% c("Mexico","Panama","Argentina",
                        "Colombia","Brazil","Paraguay"))

TopNaranjas %>% 
  arrange(desc(Creat.Ind...GDP))

#
ggplot(TopNaranjas, aes(x=Internet.penetration...population,
                        y= Services...GDP, size=GDP.PC))+
  geom_point()+
  facet_wrap(~Country)

ggplot(TopNaranjas, aes(x=Education.invest...GDP,
                        y=Creat.Ind...GDP, size=Unemployment))+
  geom_point()+
  facet_wrap(~Country)

#install RColorBrewe
myColors <- brewer.pal(9,"Reds")

ggplot(TopNaranjas, aes(x=Internet.penetration...population,
                        y=GDP.PC, fill=Creat.Ind...GDP))+
  geom_tile()+
  facet_wrap(~Country)+
  scale_fill_gradientn(colors=myColors)