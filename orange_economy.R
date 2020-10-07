#Orange economy

##Packages
install.packages("plyr")

#Preprocessing
str(orangeec)

###Sumary
summary(orangeec)

###Show important values
orangeec[orangeec$Inflation > 6,]
orangeec[orangeec$Internet.penetration...population> 90,]

###Subset
orangeec.new <- subset(orangeec ,Inflation < 2 & Unemployment <= 5 | Median.age <30
                       , select = Creat.Ind...GDP)




###Change column name
rename(orangeec,c("Creat.Ind...GDP"="AporteEcNja"))

###Show values
head(orangeec, n = 10)
tail(orangeec, n =5)
glimpse(orangeec)


#EDA

###Scatter plot
plot(orangeec$Unemployment ~ orangeec$Education.invest...GDP,
     xlab ="Investment in education(%GDP)", ylab ="Unemployment",
     main ="Relationship between Innvestment in education and Unemployment")

plot(orangeec$GDP.PC ~ orangeec$Creat.Ind...GDP,     
     xlab = "Orange economy contribution to GDP(%)", 
     ylab = "GDP per capita ",
     main = "Relationship between Orange Economy and GDP per capita")

#Histogram

ggplot() +geom_histogram(data = orangeec,
                         aes(x=Internet.penetration...population),fill ="pink", color ="black",
                         binwidth = 5)+
  scale_x_continuous(breaks = seq(40,max(100),5))+
  labs(x="Internet penetrationt", y="Number of countries",
       title = "Interne penetration in LATAM")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),panel.grid.major =  element_blank(),
        panel.grid.minor = element_blank())

##Boxplot
###Country classification

###Average varibale GDP per capita
economy <- mean(orangeec$GDP.PC)

#
orangeec <- orangeec %>%
  mutate(Strong_economy = ifelse(GDP.PC < economy,
  "Below average GDP",
  "Above average GDP"))

ggplot(orangeec, aes(x=Strong_economy, y= Creat.Ind...GDP,
                     fill = Strong_economy))+
  geom_boxplot(alpha=0.4)+
  labs(x="Country type", y ="Orange economy contribution to GDP",
       title ="Orange economy contribution to PIB countries LATAM ")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),panel.grid.major =  element_blank(),
        panel.grid.minor = element_blank())


ggplot(orangeec, aes(x=Strong_economy, y=Internet.penetration...population,
                     fill = Strong_economy))+
  geom_boxplot(alpha = 0.4)+
  labs(x="Country type",y="Internet penetration(%)",
       title="Internet penetration in countries LATAM with high and low GDP")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),panel.grid.major =  element_blank(),
        panel.grid.minor = element_blank())


#
ggplot(orangeec,aes(Internet.penetration...population,Creat.Ind...GDP))+
  geom_point(aes(color=factor(Strong_economy),size=GDP.Growth..))+
  labs(x="Internet penetration",y="Orange economy contribution to GDP",
       title="Internet penetration and orange economy contribution according to economy
       and growth")
#
ggplot(orangeec,aes(Education.invest...GDP,Unemployment))+
  geom_point(aes(color=factor(Strong_economy),size=X..pop.below.poverty.line))+
  labs(x="Investment in education",y="Unemployment",
       title="Investment in education according to unemployment")

#Scatter plot dynamic
my_graph <- ggplot(orangeec, aes(Internet.penetration...population,
                                 Creat.Ind...GDP,label=row.names(orangeec)))+
  geom_point()+
  labs(x="Internet penetration",y="Orange economy contribution",
       title="Internet penetration and orange economy contribution")
my_graph
p= ggplotly(my_graph)
p


#Correlation with pairs
pairs(orangeec[,2:6])
newdata <- subset(orangeec, select = c(5,6,10,11))

pairs(newdata)

#Correlation with cor
cor(orangeec[,2:6])
#Erasing the NAN
cor(orangeec[,2:6],use="complete.obs")

cor(newdata,use="complete.obs")

#Coefficient of variation
desv <- sd(orangeec$Internet.penetration...population)
desv

prom <- mean(orangeec$Internet.penetration...population)
prom

CoefVar <- (desv/prom)*100
CoefVar

#Other variables deleting NA\'s
prom <- mean(orangeec$Creat.Ind...GDP, na.rm = TRUE)
desv <- sd(orangeec$Creat.Ind...GDP, na.rm = TRUE)

coe <- (desv/prom)*100
coe

#Adjusting data for better visualization
orangeec <- orangeec %>%
  mutate(Crecimiento_GDP = ifelse(GDP.Growth.. >= 2.5,
                                  "2.5% or more","less that 2.5%"))

orangeec <- orangeec %>%
  mutate(Anaranjados=ifelse(Creat.Ind...GDP >= 2.5,
                            "More orange",
                            "Less orange"))

#Ranking 
orangeec %>%
  arrange(desc(Creat.Ind...GDP))

TopNaranjas <- orangeec %>%
  filter(Country %in% c("Mexico","Panama","Argentina",
                        "Colombian","Brazil","Paraguay"))

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

#Install RColorBrewe
myColors <- brewer.pal(9,"Reds")

ggplot(TopNaranjas, aes(x=Internet.penetration...population,
                        y=GDP.PC, fill=Creat.Ind...GDP))+
  geom_tile()+
  facet_wrap(~Country)+
  scale_fill_gradientn(colors=myColors)