######
## Datos necesarios para la creación de un modelo logit

library(DescTools)
library(pROC)
library(lubridate)


# Creamos la variable "y", la cual será nuestra variable respuesta de 1 si hubo involucadra la
#  una lesión o muerte dee una persona y 0 si solo hubo daños en el carro 

for (i in 1:length(accidentalidad@data$GRAVEDAD)) {
  if(accidentalidad@data[i,"GRAVEDAD"]=="SOLO DAÑOS"){
    accidentalidad@data$var_res[i]=0
  }
  else{
    accidentalidad@data$var_res[i]=1
  }
}

# Ahora vamos a modelar con la variable Barrio, Hora y Diseno

accidentalidad@data <- na.omit(accidentalidad@data)  # Limpiamos la base de datos.

# Ajustamos el modelo logit

mod<- glm(var_res~HORA+DISENO+BARRIO,family = "binomial", data = accidentalidad@data)

PseudoR2(mod,which="Effron")
mod_auc<-roc(accidentalidad@data$var_res~predict(mod,type="response"))
print(auc(mod_auc))
plot(mod_auc) #Curva ROC

#funcion probabilidad


input<-NULL
input$HORA=parse_date_time("07:00 PM", '%I:%M %p') #Acá cómo ingresar
input$BARRIO="Naranjal"
input$DISENO="Interseccion"
prob<-function(input){
  log.odds<-predict(mod,data.frame(HORA=input$HORA,BARRIO=input$BARRIO,DISENO=input$DISENO))
  round(1/(exp(-log.odds)+1),4)
}

prob(input)

plot(c(0,10),c(0,10), type = "n", axes = FALSE,
     ylab = "", xlab = "", asp = 1)
text(3,5,paste("Probabilidad de sufrir accidente = ",prob(input)*100, "%"),cex=2,col="red")
