library(data.table)
library(forecast)


#setwd()
setwd("/Users/JoseManuel/Documents/DianaMunch")
data<-fread("accFatal.csv")
names(data)<-c("Year","Total","Fatal","noF","danos","conapo")

class(data$conapo)
conapo2<-as.numeric(gsub(".","",data$conapo,fixed=TRUE))
data$conapo<-conapo2


m<-length(data$Fatal)
vector<-data$Fatal[-c((m-1),m)]
Fatal<-ts(vector,start=1997,end=2013,frequency=1)

#para reducir el numero de muertes en 35% (130 muertos)
#con la tendencia actual deber??amos llegar a los 130 muertos en 2050
#Encontremos el mejor modelo de entre 27 
AR<-seq(0,3,1)
I<-seq(0,3,1)
MA<-seq(0,3,1)

j<-1
fit2<-list()

for(ar in AR)
{
for(i in I)
{
for(ma in MA)  
{
fit2[[j]]<-tryCatch(Arima(Fatal, order=c(ar,i,ma) , include.drift=TRUE))
j<-j+1
}  
}
}

n<-length(fit2)
who<-c()
bic<-20000
auxbic<-0
bics<-list()
for(N in 1:n)
{
auxbic<-fit2[[N]]$bic
bics[[N]]<-auxbic
print(auxbic)
print(N)
if(auxbic<bic){
                bic<-auxbic
                who<-N
              }
}


#Nos quedamos con los 10 mejores modelos de acuerdo con el BIC 
number<-length(fit2)
bics<-unlist(bics)
orden<-order(bics)[1:number]

selectedModels<-list()
for(i in 1:number)
{
selectedModels[[i]]<-fit2[[orden[i]]]
}

#Veamos cu??les son los forecasts para vision zero y el -35%
muertes2015<-data$Fatal[c((m-1),m)]
expected<-muertes2015*(1-.35)

forecast<-list()
pos<-list()
reachYear<-list()
zero<-list()
reachYearzero<-list()
forecast2014<-list()
forecast2015<-list()

for(i in 1:length(orden))
{
  forecast[[i]]<-forecast.Arima(fit2[[i]],h=20)
  forecast2014[[i]]<-forecast[[i]]$mean[1]
  forecast2015[[i]]<-forecast[[i]]$mean[2]
  pos[[i]]<-which.min(forecast[[i]]$mean>expected)
  reachYear[[i]]<-2015+pos[[i]]
  zero[[i]]<-which.min(forecast[[i]]$mean>0)
  reachYearzero[[i]]<-2015+zero[[i]]
}

#122
autofit<-auto.arima(Fatal,seasonal=FALSE,allowdrift=TRUE,max.P=3,max.Q=3)
fauto2014<-forecast.Arima(autofit,h=20)$mean[1]
fauto2015<-forecast.Arima(autofit,h=20)$mean[2]

real2014<-data$Fatal[(m-1)]
real2015<-data$Fatal[m]
  
dif14<-unlist(lapply(forecast2014,function(x) (real2014-x)^2))
dif15<-unlist(lapply(forecast2015,function(x) (real2015-x)^2))

sum_sqdif<-sqrt((dif14+dif15)/2)
df<-data.frame(real2014,real2015,unlist(forecast2014),unlist(forecast2015),dif14,dif15,sum_sqdif,seq(1:length(dif15)),bics)
names(df)<-c("real2014","real2015","f2014","f2015","dif14","dif15","sumsqdif","model","BIC")
ind<-order(df[,7])
df<-df[ind,]

#####
minbics<-sort(bics)[1:20]

best10BIC<-match(minbics,bics)
best10Pred<-df[1:10,8]
ind<-sort(c(best10BIC,best10Pred))
length(ind)
df2<-df[match(ind,df[,8]),]
df2[order(df2[,7]),]



years<-seq(data$Year[1],(reachYear+1),1)
forecasts<-c(data$Fatal,forecast$mean[1:(pos+1)])
plot(years,forecasts,type="l",xlim=c(data$Year[1],reachYear))  
abline(h=reachYear)  

muertes2015<-data$Fatal[dim(data)[1]]
expected<-muertes2015*(1-.35)

pos<-which.min(forecast$mean>expected)
zero<-which.min(forecast$mean>0)

reachYear<-2015+pos
reachYearzero<-2015+zero

print(paste("el a??o al que se llega a:", expected, " muertes es: ", reachYear,sep=" "))
print(paste("el a??o al que se llega a:", 0, " muertes es: ", reachYearzero,sep=" "))

years<-seq(data$Year[1],(reachYearzero+1),1)
forecasts<-c(data$Fatal,forecast$mean[1:(zero+1)])
plot(years,forecasts,type="l",xlim=c(data$Year[1],(reachYearzero+1)),ylim=c(0,620))

abline(v=reachYear,col="red",pch="2")  
abline(v=2015,col="black",pch="2")  
abline(v=2018,col="black",pch="2")  
abline(v=reachYearzero,col="cyan",pch="2")  
abline(v=2018,col="black",pch="2")  

#c??mo obtener la diferencia. 

#calcular los puntos de la tendencia real al 2018
trendX<-seq(2015,(reachYearzero),1)
trendY<-forecast$mean[1:zero]

expectedX<-trendX

#ajustar una recta que llegue a 2018 en 130. 
#(x1,y1)_2015==(2015,200)
#(x2,y2)_2018==(2018,130)

x1<-2015
y1<-200
x2<-2018
y2<-130

m<-(y2-y1)/(x2-x1)
intercept<-(y1-m*x1)


#la formula es de la recta es: y - y1 = m*(x -x1)  
# la formula es y = m*x  + y1 - m*x1
#la formula es y = m*x + intercept 
punto2018<-2018-2015
muertosTendencia<-forecast$mean[1:punto2018]
muertosRecta<-c((m*2016+intercept),(m*2017+intercept),(m*2018+intercept))

muertos2018<-sum(muertosTendencia)-sum(muertosRecta)


punto2022<-2022-2015
muertosTendencia<-forecast$mean[1:(punto2022-1)]
sum(muertosTendencia)
muertosRecta<-c()
for(i in 2016:reachYearzero)
{
muertosRecta<-sum(muertosRecta) +  (m*i+intercept)
}

sum(muertosRecta)-sum(muertosTendencia)
muertos2018<-sum(muertosTendencia)-sum(muertosRecta)


punto2019<-2019-2015
muertosTendencia<-forecast$mean[1:punto2019]
sum(rep(200,length(1:punto2019)))-sum(muertosTendencia).


muertosRecta<-c((m*2016+intercept),(m*2017+intercept),(m*2018+intercept))

muertos2018<-sum(muertosTendencia)-sum(muertosRecta)



#plot
plot(years,forecasts,type="l")
segments(x0=x1,y0=y1,x1=x2,y1=y2,col="red")
abline(a=intercept,b=m,col="cyan")



#La diferencia de muertos en 2016,2017 y 2018 es de: 

  
  








