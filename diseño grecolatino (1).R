####################################
###   CUADRADO GRECOLATINO
###############################

orden_ensamble<-c(1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4)
operador<-c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4)
met_ensamble<-c("C","B","A","D","B","C","D",
                "A","D","A","B","C","A","D","C","B")
tiempo_ensamble<-c(10,8,6,11,10,15,14,8,12,7,11,10,7,14,13,8)
lugar_trab<-c(2,1,4,3,3,4,1,2,4,3,2,1,1,2,3,4)


####transformamos las variables
orden_ensamble<-factor(orden_ensamble)
met_ensamble<-factor(met_ensamble)
lugar_trab<-factor(lugar_trab)
operador<-factor(operador)


####Modelo
modelo<-lm(tiempo_ensamble~orden_ensamble+met_ensamble+operador+lugar_trab)
anova<-aov(modelo)
summary(anova)


library(agricolae)
## el método de ensamble es significativo.
"analizamos las diferencias por medio del procedimiento LSD"
# metodos de ensamble
lsd_modelo<-LSD.test(y=anova, trt = "met_ensamble", group = T,console = T)

## analizando en forma gráfica
bar.group(x=lsd_modelo$groups, horiz = T,col="red",
          xlab="Tiempo de ensamble",
          ylab="Método de ensamble",
          xlim=c(0,18),
          main="Comparación de los métodos de ensamble por medio del procedimiento LSD")
"los métodos de ensamble que requeiren menos tiempo son los met A y B"
"Mientras que si queremos aquellos con mayor tiempo de ensamble serian D Y C"


#########################
#################################
operador<-c(1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,
            1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4)
dia<-c("l","l","l","l","k","k","k","k","m","m",
       "m","m","j","j","j","j","l","l","l","l",
       "k","k","k","k","m","m","m","m","j","j","j","j")
lugar<-c("A","C","B","D","C","A","D","B","B","D","A","C",
         "D","B","A","C","A","C","B","D","C","A","D","B",
         "B","D","A","C","A","D","B","A")

tratamiento<-c("D","A","C","B","C","B","D","A","B","C","A","D",
               "A","D","B","C","D","A","C","B","C","B","D","A",
               "B","C","A","D","D","B","C","A")

length(num_prod)
num_prod<-c(825,650,700,475,585,540,650,560,550,580,635,650,
            580,850,450,670,750,725,675,480,610,560,740,
            615,580,635,540,725,650,770,550,730)
###
tratamiento<-factor(tratamiento)
lugar<-factor(lugar)
dia<-factor(dia)
operador<-factor(operador)
modelo<-lm(num_prod~tratamiento+lugar+dia+operador)
anova<-aov(modelo)
summary(anova)
library(agricolae)
## el método de ensamble es significativo.
"analizamos las diferencias por medio del procedimiento LSD"
# metodos de ensamble
lsd_modelo<-LSD.test(y=anova, trt = "tratamiento", group = T,console = T)

## analizando en forma gráfica
bar.group(x=lsd_modelo$groups, horiz = T,col="red",
          xlab="Número de prod",
          ylab="Tratamiento",
          xlim=c(0,800),
          main="Comparación de los tratamientos")

#############################################
##  Ejercicio 3"
"Diseño Cuadrado Latino
Comenzaremos diseñando el Cuadrado Latino,
de este nace el Grecolatino.

Primero creamos los vectores teniendo en cuenta que
los factores se deben crear como factor."
Formulacion = factor(c("A","B","C","D","E",
                       "B","C","D","E","A",
                       "C","D","E","A","B",
                       "D","E","A","B","C",
                       "E","A","B","C","D"))

Rapidez =c(-1,-8,-7,1,-3,
           -5,-1,13,6,5,
           -6,5,1,1,-5,
           -1,2,2,-2,4,
           -1,11,-4,-3,6)

Lote =factor( c(rep("1",1), 
                rep("2",1), 
                rep("3",1), 
                rep("4",1), 
                rep("5",1)))

Operador = factor(c(rep("1",5),
                    rep("2",5),
                    rep("3",5),
                    rep("4",5),
                    rep("5",5)))

data = data.frame(Rapidez,Formulacion,Lote,Operador)

Lote=data$Lote

data          
modeloLatino= lm(Rapidez ~ Formulacion + Lote + Operador, data)
anovaLatino=aov(modeloLatino)

summary(anovaLatino)
Montajes =factor(c("b","c","d","e","f",
                   "d","e","f","b","c",
                   "f","b","c","d","e",
                   "c","d","e","f","b",
                   "e","f","b","c","d")) 
dataGreco = data.frame(Rapidez,Formulacion,Lote,Operador,Montajes)

dataGreco

#cargamos los siguientes paquetes
library(gridExtra)
library(ggplot2)
Form <- ggplot(dataGreco, aes(x = Formulacion, y = Rapidez, fill=Formulacion)) +
  geom_boxplot() + theme(legend.position = "none")
Mont <- ggplot(dataGreco, aes(x = Montajes, y = Rapidez, fill=Montajes)) +
  geom_boxplot() + theme(legend.position = "none")
Lot <- ggplot(dataGreco, aes(x = Lote, y = Rapidez, fill=Lote)) +
  geom_boxplot() + theme(legend.position = "none")
Oper <- ggplot(dataGreco, aes(x = Operador, y = Rapidez, fill=Operador)) +
  geom_boxplot() + theme(legend.position = "none")
grid.arrange(Form,Mont,Lot,Oper, nrow=2,ncol=2)


modeloGreco= lm(Rapidez ~ Formulacion + Lote + Operador  + Montajes, dataGreco)
anovaGreco=aov(modeloGreco)
summary(anovaGreco)

"Se puede inferir basado en evidencia estadística que
las formulaciones (Letras latinas) son significativas, 
es decir, existe alguna diferencia entre ellas;
El error experimental disminuye al sacar la 
variabilidad debida a los montajes de prueba 
cuando se pasa al diseño Grecolatino, disminuyen 
los grados de libertad.

Existe evidencia estadística para inferir que los 
montajes de prueba no son significativos, puesto 
que no existe diferencia entre cada uno de ellos 
no se rechaza la hipotesis de que sus efectos medios
son iguales)"

TukeyHSD(anovaGreco)
par(mfrow=c(2,2))
plot(TukeyHSD(anovaGreco))
library(agricolae)
LSD.test(anovaGreco,"Formulacion",console=TRUE)  #prueba lsd
HSD.test(anovaGreco, "Formulacion",console=TRUE) # tukey
SNK.test(anovaGreco, "Formulacion",console=TRUE) #prueba de Student-Newman-Keuls (SNK)
scheffe.test(anovaGreco, "Formulacion",console=TRUE) #Prueba de Scheffé
duncan.test(anovaGreco, "Formulacion",console=TRUE) #p duncan
LSD.test(anovaGreco, "Formulacion", p.adj= "bon",console=TRUE) #prueba de bonferroni


########## supuestos del modelo
shapiro.test(anovaGreco$res) # normalidad
summary(anovaGreco$residuals)
boxplot(anovaGreco$residuals,col="lightblue")  #graficamente
hist(anovaGreco$residuals, col="lightgreen", main = "Histograma de los Residuos",
     freq = F, xlab="Residuos",ylab="Densidad")
lines(density(anovaGreco$residuals), col="red", lwd=3)
qqnorm(anovaGreco$residuals) 
qqline(anovaGreco$residuals)

plot(anovaGreco$residuals,main = "Residuos vs Observaciones") #independencia
boxplot(anovaGreco$residuals~Formulacion, xlab="Formulacion",ylab="Residuos",
        col = c("yellow", "blue", "white","green", "red")) # Homocedasticidad

#Gráfico de predichos contra residuos estandarizados
pred=fitted(anovaGreco)
resid=rstandard(anovaGreco)
plot(pred,resid,xlab="Valores predichos", ylab="Residuos estandarizados",abline(h=0))

bartlett.test(anovaGreco$residuals ~ Formulacion) #homocedasticidad
bartlett.test(anovaGreco$residuals ~ Montajes)
bartlett.test(anovaGreco$residuals ~ Lote)
bartlett.test(anovaGreco$residuals ~ Operador)

"En la Prueba Levene para Homogenidad de varianzas 
no se rechaza la hipotesis nula que indica homogeneidad 
de varianza en el tratamiento"
library(car)
leveneTest(Rapidez~Formulacion, dataGreco, center = "median")
