
odd <- function(x) x%%2 != 0

even <- function(x) x%%2 == 0


nros.registro<-123:146


lista.gral<-list(NA)

for (i in nros.registro) {
  
  set.seed(i)
  sim<-round(runif(100,0,4))#23 uniformes de 0 a 4
  muestra<-sample(sim,3)#elige 3 que son el orden del modelo
  muestra[2]<-odd(muestra[2])#si el del medio es impar, entonces la funcion odd lo transforma en un 1, y lo uso para indicar que es ARIMA
  
  
  
  set.seed(i)
  Coefar<-runif(muestra[1],0,0.5) #simulo los coeficientes de la parte ar
  Coefma<-runif(muestra[3],0,0.5) #simulo los coeficientes de la parte ma
  
  
  
  set.seed(i)
  serie<-arima.sim(n=100,model = list(order=muestra,ar=Coefar,ma=Coefma),sd=1)
  #simulo la serie de 100 obs, en los argumentos del modelo orden=muestra (el vector con el que simule el orden del modelo)
  #ar=Coefar, los coeficientes que simule para la parte ar, idem ma=Coefma
  
  #en teoria todo esta simulado con el "seed" i, tendria que ser reproducible 
  
  sublista<-list(i,muestra,Coefar,Coefma,serie)
  names(sublista)<-c("i","orden","Coeficientes AR","Coeficientes MA", "serie simulada")

  lista.gral[[i]]<-sublista  
}


View(lista.gral)




