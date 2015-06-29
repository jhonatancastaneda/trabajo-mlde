library(readxl)
excel_sheets(path = file.path("data_rls_uti.xlsx"))
data <- read_excel("data_rls_uti.xlsx",sheet = "Hoja1",col_names = TRUE, na="")

summary (data)
str(data)
View(data)

data[,"Utilidad"] <- data[,"Utilidad"]-mean(data[,"Utilidad"])
data[,"Ventas"] <- data[,"Ventas"]-mean(data[,"Ventas"])

#Regresion lineal

reg <- lm(Utilidad~Ventas , data)
str(reg)
anova<-aov(reg)
summary(reg)
summary(anova)

#Valor cuartil t de student, fisher
qt(0.975 , df =38)
qf(0.95 , df1=1,df2=38)

#B1 no es significativo, por lo que se debe centrar los datos
#------------------------------------------------------------

#intervalos de confianza
confint(reg , level=0.95)


names(reg)

res<-reg[["residuals"]]
pred<-reg[["fitted.values"]]

data2<-data.frame(data,Predicciones=pred,Residuos=res)

hist(res,15)
mean(res)


#prueba de normalidad
qqnorm(res)
qqline(res,col="red")


plot(data[,"Ventas"],data[,"Utilidad"])
plot(res,pred)
plot(res, data[,"Ventas"])

