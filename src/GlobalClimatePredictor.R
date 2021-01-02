## ----libs, size="scriptsize",message=FALSE,results="hide"-----------------------------------------------------------------------------------------------------------------------------------------------------------------
library(rgdal)        
library(raster)        
library(sp)            
library(lubridate)    
library(DescTools)
library(randomForest)  
library(simputation)


## ----funcion1,size="scriptsize", message=FALSE,results="hide"-------------------------------------------------------------------------------------------------------------------------------------------------------------
readLL = Vectorize(function(x) {
   n = nchar(x)
   dir = substr(x, n, n)
   x = as.numeric(substr(x,1,n-1))
   if (dir %in% c("S","W")) m = -1 else m = 1
   return(m*x)
})


## ----funcion2, size="scriptsize",message=FALSE,results="hide"-------------------------------------------------------------------------------------------------------------------------------------------------------------
invBoxCox <- function(x, lambda) {
    if (lambda == 0) exp(x) else (lambda*x + 1)^(1/lambda) 
}


## ----funcion3,size="scriptsize", message=FALSE,results="hide"-------------------------------------------------------------------------------------------------------------------------------------------------------------
normaliza <- function(x) {
   mn = min(x, na.rm=TRUE); mx = max(x, na.rm=TRUE)
   return(list(min=mn, max=mx, nx=(x-mn)/(mx-mn)))
}


## ----funcion4,size="scriptsize", message=FALSE,results="hide"-------------------------------------------------------------------------------------------------------------------------------------------------------------
denormaliza <- function(nx,mn,mx) return(nx*(mx-mn) + mn)


## ----carga, size="scriptsize",cache=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
datos = read.table("GlobalLandTemperaturesByCity.csv",
                   header=TRUE, sep=",", quote="\"")


## ----explora, size="scriptsize", cache=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(datos)
summary(datos)
dim(datos)


## ----conversion, size="scriptsize",cache=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
datos$Longitude = readLL(datos$Longitude)
datos$Latitude  = readLL(datos$Latitude)
datos$fecha     = ymd(datos$dt)
datos$year      = year(datos$fecha)
datos$mes       = month(datos$fecha)
datos$AbsLat    = abs(datos$Latitude)
datos$solst     = ifelse(datos$Latitude<0, 
                         cos(2*pi*(datos$mes-1)/12),
                        -cos(2*pi*(datos$mes-1)/12))


## ----SPDF,size="scriptsize", cache=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
coordinates(datos) =~ Longitude+Latitude
proj4string(datos) = CRS("+init=epsg:4326")


## ----plot1,size="scriptsize", cache=FALSE, echo=2, output=FALSE, message=FALSE, warning=FALSE, message=FALSE--------------------------------------------------------------------------------------------------------------
png("mapa1.png", width=800, height=400)
plot(datos, axes=TRUE, cex=0.5, pch=1)
dev.off()


## ----extract, size="scriptsize",cache=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
MDE=raster::raster("alwdgg.tif")
distSea=raster::raster("distSea.tif")
proj4string(MDE) = CRS("+init=epsg:4326")
proj4string(distSea) = CRS("+init=epsg:4326")


## ----extract2,size="scriptsize", cache=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
datos$elev  = raster::extract(MDE, datos)
datos$dist  = raster::extract(distSea, datos)


## ----extract3,size="scriptsize", fig.width=10, fig.height=5---------------------------------------------------------------------------------------------------------------------------------------------------------------
par(mfrow=c(1,2))
hist(datos$elev, main="", xlab="Elevación (m.s.n.m.)")
hist(datos$dist, main="", xlab="Distancia al mar")


## ----extract4,size="scriptsize", cache=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
datos$elev  = pmax(datos$elev,0)


## ----nas0, size="scriptsize",cache=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
length(which(is.na(datos$AverageTemperature)))
length(which(is.na(datos$year)))
length(which(is.na(datos$solst)))
length(which(is.na(datos$AbsLat)))
length(which(is.na(datos$elev)))
length(which(is.na(datos$dist)))
length(which(is.na(datos$year)))


## ----nas, size="scriptsize",cache=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
wNA = which(is.na(datos$AverageTemperature))
100*length(wNA)/nrow(datos)


## ----nas1,size="scriptsize", cache=FALSE, fig.height=5, fig.width=10------------------------------------------------------------------------------------------------------------------------------------------------------
par(mfrow=c(1,2))
barplot(table(datos@data[wNA,"mes"]), xlab="Mes", ylab="Frecuencia", 
        cex.axis=0.8)
hist(datos@data[wNA,"year"], breaks=250, 
     main="", xlab="Año", ylab="Frecuencia")


## ----nas2,size="scriptsize", cache=FALSE, echo=FALSE, eval=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------
## table(datos$year[which(datos@data[wNA,"year"]>2000)])


## ----mapa2, size="scriptsize",cache=FALSE, output=FALSE, message=FALSE, results=FALSE, echo=2-----------------------------------------------------------------------------------------------------------------------------
png("mapa2.png", width=800, height=400)
plot(datos[wNA,], axes=TRUE, cex=0.5, pch=1)
dev.off()


## ----datos2, size="scriptsize",cache=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
datos2 = datos[which(datos$year>=1900 & datos$year<2013),]
dim(datos2)


## ----hist1, size="scriptsize", cache=FALSE, fig.height=4, fig.width=4, fig.align="center"---------------------------------------------------------------------------------------------------------------------------------
x = datos2$AverageTemperature
hist((x-mean(x))/sd(x), xlab="Z scores", ylab="Frecuencia", main="")


## ----zonas, size="scriptsize", cache=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
datos2$Zona = cut(datos2$Latitude, c(-90,-66.5,-23.44,23.44,66.5,90))
table(datos2$Zona)


## ----outliers1, size="scriptsize", cache=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
par(mfrow=c(2,2))
boxplot(AverageTemperature~mes, datos2[which(as.character(datos2$Zona)=="(-66.5,-23.4]"),],
        main="Zona templada sur")
boxplot(AverageTemperature~mes, datos2[which(as.character(datos2$Zona)=="(-23.4,23.4]"),],
        main="Zona intertropical")
boxplot(AverageTemperature~mes, datos2[which(as.character(datos2$Zona)=="(23.4,66.5]"),],
        main="Zona templada norte")
boxplot(AverageTemperature~mes, datos2[which(as.character(datos2$Zona)=="(66.5,90]"),],
        main="Zona polar norte")


## ----outliers2, size="scriptsize"-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
datos2$decada = as.numeric(cut(datos2$year, seq(1899,2020,10)))

par(mfrow=c(2,2))

boxplot(AverageTemperature~decada, 
        datos2[which(datos2$Zona=="(-66.5,-23.4]"),], 
        main="Zona templada sur")
abline(h=mean(datos2@data[which(datos2$Zona=="(-66.5,-23.4]"),2]), lty=3)

boxplot(AverageTemperature~decada, 
        datos2[which(datos2$Zona=="(-23.4,23.4]"),],
        main="Zona intertropical")
abline(h=mean(datos2@data[which(datos2$Zona=="(-23.4,23.4]"),2]), lty=3)

boxplot(AverageTemperature~decada, 
        datos2[which(datos2$Zona=="(23.4,66.5]"),],
        main="Zona templada norte")
abline(h=mean(datos2@data[which(datos2$Zona=="(23.4,66.5]"),2]), lty=3)

boxplot(AverageTemperature~decada, 
        datos2[which(datos2$Zona=="(66.5,90]"),],
        main="Zona polar norte")
abline(h=mean(datos2@data[which(datos2$Zona=="(66.5,90]"),2]), lty=3)


## ----shapiro, size="scriptsize", cache=FALSE, fig.height=5, fig.width=5---------------------------------------------------------------------------------------------------------------------------------------------------
shapiro.test(datos2$AverageTemperature[sample(1:nrow(datos2),5000)])


## ----coxbox, size="scriptsize", cache=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
lambda=BoxCoxLambda(datos2$AverageTemperature)
lambda 
datos2$AT.bc = BoxCox(datos2$AverageTemperature, lambda=lambda)


## ----coxbox2, size="scriptsize", cache=FALSE, fig.height=4.5, fig.width=4.5, fig.align="center"---------------------------------------------------------------------------------------------------------------------------
l4 = round(lambda,4)
xl2 = paste0("Transformación Box-Cox de la temperatura (lambda=",l4,")")
hist(datos2$AT.bc, main="", xlab=xl2, ylab="Frecuencia")
shapiro.test(datos2$AT.bc[sample(1:nrow(datos2),5000)])


## ----levene, size="scriptsize", cache=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
car::leveneTest(AverageTemperature~as.factor(mes), datos2)
car::leveneTest(AverageTemperature~Zona, datos2)
car::leveneTest(AverageTemperature~as.factor(decada), datos2)
car::leveneTest(AT.bc~as.factor(mes), datos2)
car::leveneTest(AT.bc~Zona, datos2)
car::leveneTest(AT.bc~as.factor(decada), datos2)


## ----kruskal, size="scriptsize", cache=FALSE, echo=2:4--------------------------------------------------------------------------------------------------------------------------------------------------------------------
datos2$decada = as.numeric(cut(datos2$year, seq(1899,2020,10)))
kruskal.test(AverageTemperature~Zona, datos2)
kruskal.test(AverageTemperature~as.factor(mes), datos2)
kruskal.test(AverageTemperature~decada, datos2)


## ----kruskal2, size="scriptsize", cache=FALSE, echo=-1--------------------------------------------------------------------------------------------------------------------------------------------------------------------
datos2$decada = as.numeric(cut(datos2$year, seq(1899,2020,10)))
for (p in 1:11) {
       decadas = levels(as.factor(datos2$decada))[c(p,p+1)]
       subset = datos2[which(datos2$decada %in% decadas),]
       wt = wilcox.test(AverageTemperature~decada, subset)
       cat(p,"-", p+1, wt$p.value,"\n")
}


## ----lm1, size="scriptsize", cache=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
datos2@data[1:10,c("mes","solst")]
mod1 = lm(AverageTemperature~year+solst+AbsLat+Longitude+elev+dist, datos2)
summary(mod1)


## ----lm2, size="scriptsize", cache=FALSE, echo=c(2,3), results="hide"-----------------------------------------------------------------------------------------------------------------------------------------------------
png("lm1.png")
par(mfrow=c(2,2))
plot(mod1)
dev.off()


## ----lmcv, size="scriptsize", cache=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
kf = sample(1:nrow(datos2), nrow(datos2))
cortes = round(seq(1,nrow(datos2), length=11))
pred = rep(NA, nrow(datos2))
for (f in 1:10) {
   wval = kf[cortes[f]:cortes[f+1]]
   mod = lm(AverageTemperature~year+solst+AbsLat+Longitude+elev+dist, 
            datos2[-wval,])
   pred[wval] = predict(mod,datos2[wval,])
}


## ----lmcvRes, size="scriptsize", cache=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cor(pred, datos2$AverageTemperature)^2
sqrt(mean((pred-datos2$AverageTemperature)^2))
mean(pred-datos2$AverageTemperature)


## ----europa, size="scriptsize", cache=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Europa = c("Spain","Portugal","France","Italy","Germany","United Kingdom",
           "Ukraine","Sweden","Switzerland","Serbia","Romania","Poland",
           "Norway","Netherlands","Montenegro","Moldova","Macedonia", 
           "Latvia","Ireland","Hungary","Iceland","Greece","Georgia",
           "Estonia", "Czech Republic","Denmark","Cyprus","Croatia",
           "Bulgaria","Bosnia And Herzegovina","Belarus","Belgium",
           "Austria","Albania")
datosE = datos2[which(datos2$Country %in% Europa),]


## ----mod1E, size="scriptsize", cache=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mod1E = lm(AverageTemperature~year+solst+AbsLat+Longitude+elev+dist, datosE)
summary(mod1E)


## ----mod2E, size="scriptsize", cache=FALSE, echo=c(2,3), results=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------
png("lmE.png")
par(mfrow=c(2,2))
plot(mod1E)
dev.off()


## ----lmcvE, size="scriptsize", cache=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
options(width=100)
kf = sample(1:nrow(datosE), nrow(datosE))
cortes = round(seq(1,nrow(datosE), length=11))
pred = rep(NA, nrow(datosE))
for (f in 1:10) {
   wval = kf[cortes[f]:cortes[f+1]]
   mod = lm(AverageTemperature~year+solst+AbsLat+Longitude+elev+dist, 
            datosE[-wval,])
   pred[wval] = predict(mod,datosE[wval,])
}


## ----lmcvResE, size="scriptsize", cache=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cor(pred, datosE$AverageTemperature)^2
sqrt(mean((pred-datosE$AverageTemperature)^2))
mean(pred-datosE$AverageTemperature)


## ----plotLME, size="scriptsize", cache=FALSE, echo=2:3--------------------------------------------------------------------------------------------------------------------------------------------------------------------
png("plotResLM.png")
plot(datosE$AverageTemperature, pred,cex=0.5, 
     xlab="Observado", ylab="Predicho")
abline(0,1,col="red")
dev.off()


## ----RFcv, size="scriptsize",eval=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## predRF = rep(NA, nrow(datosE))
## for (f in 1:10) {
##    cat(f,10,"\n")
##    wval = kf[cortes[f]:cortes[f+1]]
##    mod = randomForest(AverageTemperature~year+solst+AbsLat+Longitude+elev+dist,
##             datosE[-wval,])
##    predRF[wval] = predict(mod,datosE[wval,])
##    rm(mod);gc()
## }


## ----RFcvRes, size="scriptsize", echo=-1----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
load("temsp.RDATA")
cor(predRF, datosE$AverageTemperature)^2
sqrt(mean((predRF-datosE$AverageTemperature)^2))
mean(predRF-datosE$AverageTemperature)


## ----plotRF, size="scriptsize", cache=FALSE, echo=2:3---------------------------------------------------------------------------------------------------------------------------------------------------------------------
png("plotResRF.png")
plot(datosE$AverageTemperature, pred,cex=0.5, 
     xlab="Observado", ylab="Estimados")
abline(0,1,col="red")
dev.off()


## ----anexob1, size="scriptsize", cache=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
years <- unique(datos2$year)
temperatures <- years
for (i in 1:length(years)){
  temperatures[i] <- mean(datos2[datos2$year == years[i], ]$AverageTemperature)
  
}

data <- data.frame(years,temperatures)


## ----anexob2, size="scriptsize", cache=FALSE,fig.height=4, fig.width=10---------------------------------------------------------------------------------------------------------------------------------------------------
par(mfrow=c(1,2))
plot(years,temperatures,
     main = "World Average Temperature (1900-2013)", xlab = "Year",ylab = "Temperature")
plot(data[data$years > 1970,]$years,data[data$years > 1970,]$temperatures,
     main = "World Average Temperature (1971-2013)", xlab = "Year",ylab = "Temperature")


## ----anexob3, size="scriptsize", cache=FALSE,fig.height=4, fig.width=10---------------------------------------------------------------------------------------------------------------------------------------------------
model <- lm(temperatures ~years, data = data)
summary(model)
model1 <- lm(temperatures ~years, data = data[data$years < 1971,])
summary(model1)
model2 <- lm(temperatures ~years, data = data[data$years > 1970,])
summary(model2)

plot(years,temperatures,
     main = "World Average Temperature (1900-2013)", xlab = "Year",ylab = "Temperature")
abline(model)
plot(data[data$years < 1971,]$years,data[data$years < 1971,]$temperatures,
     main = "World Average Temperature (1900-1970)", xlab = "Year",ylab = "Temperature")
abline(model1)
plot(data[data$years > 1970,]$years,data[data$years > 1970,]$temperatures,
     main = "World Average Temperature (1971-2013)", xlab = "Year",ylab = "Temperature")
abline(model2)


## ----anexob4, size="scriptsize", cache=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tail(names(sort(table(datos2$City))), 1)
tail(names(sort(table(datos2$Country))), 1)


## ----anexob5, size="scriptsize", cache=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
datos3 <- datos2[datos2$City == "Worcester",]

years <- unique(datos3$year)
temperatures <- years
for (i in 1:length(years)){
  temperatures[i] <- mean(datos3[datos3$year == years[i], ]$AverageTemperature)
  
}

data <- data.frame(years,temperatures)


## ----anexob6, size="scriptsize",fig.height=5, fig.width=10, cache=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------
model <- lm(temperatures ~years, data = data)
summary(model)
model1 <- lm(temperatures ~years, data = data[data$years < 1971,])
summary(model1)
model2 <- lm(temperatures ~years, data = data[data$years > 1970,])
summary(model2)

plot(years,temperatures,
     main = "Worcester Average Temperature (1900-2013)", xlab = "Year",ylab = "Temperature")
abline(model)
plot(data[data$years < 1971,]$years,data[data$years < 1971,]$temperatures,
     main = "Worcester Temperature (1900-1970)", xlab = "Year",ylab = "Temperature")
abline(model1)
plot(data[data$years > 1970,]$years,data[data$years > 1970,]$temperatures,
     main = "Worcester Temperature (1971-2013)", xlab = "Year",ylab = "Temperature")
abline(model2)


## ----anexob7, size="scriptsize",fig.height=5, fig.width=10, cache=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------
datos4 <- datos2[datos2$Country == "India",]

years <- unique(datos4$year)
temperatures <- years
for (i in 1:length(years)){
  temperatures[i] <- mean(datos4[datos4$year == years[i], ]$AverageTemperature)
  
}

data <- data.frame(years,temperatures)


## ----anexob8, size="scriptsize",fig.height=5, fig.width=10, cache=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------
model <- lm(temperatures ~years, data = data)
summary(model)
model1 <- lm(temperatures ~years, data = data[data$years < 1971,])
summary(model1)
model2 <- lm(temperatures ~years, data = data[data$years > 1970,])
summary(model2)

plot(years,temperatures,
     main = "India Average Temperature (1900-2013)", xlab = "Year",ylab = "Temperature")
abline(model)
plot(data[data$years < 1971,]$years,data[data$years < 1971,]$temperatures,
     main = "India Temperature (1900-1970)", xlab = "Year",ylab = "Temperature")
abline(model1)
plot(data[data$years > 1970,]$years,data[data$years > 1970,]$temperatures,
     main = "India Temperature (1971-2013)", xlab = "Year",ylab = "Temperature")
abline(model2)


## ----missing1, size="scriptsize"------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
datosE2 = datosE[,c(2,8:11,13:15)]
miss = sample(1:nrow(datosE),round(nrow(datosE)/20))
datosE2[miss, "AverageTemperature"] = NA


## ----imp1, size="small"---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
predMissMed = rep(mean(datosE2$AverageTemperature, na.rm=TRUE), 
                  length(miss))


## ----imp2, size="small"---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
predMissMd = rep(median(datosE2@data$AverageTemperature, na.rm=TRUE), 
                 length(miss))


## ----imp3, size="small"---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
predMissMdCnd = impute_median(datosE2@data, 
                              AverageTemperature ~ mes + Lat)[miss,1]


## ----imp4, size="small"---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
predMissLM = impute_lm(datosE2@data, 
                      AverageTemperature~year+elev+solst+AbsLat+dist)[miss,1]


## ----missingRF, eval=FALSE, size="small"----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## predMissRF = impute_rf(datosE2@data,
##                       AverageTemperature~year+elev+solst+AbsLat+dist)[miss,1]


## ----loadmissingRF, echo=FALSE, size="small"------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
load("predMissRF.RDATA")


## ----normaliza, size="small"----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
normYear = normaliza(datosE2$year)
datosE2@data$year = normYear$nx

normSolst = normaliza(datosE2$solst)
datosE2$solst = normSolst$nx

normAbsLat = normaliza(datosE2$AbsLat)
datosE2$AbsLat = normAbsLat$nx

normElev = normaliza(datosE2$elev)
datosE2$elev = normElev$nx

normDist = normaliza(datosE2$dist)
datosE2$dist = normDist$nx


## ----imp5,cache=FALSE, size="small"---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
predMiss5NN = impute_knn(datosE2@data, 
                      AverageTemperature~year+elev+solst+AbsLat+dist,
                      k=5)[miss,1]


## ----imp6, cache=FALSE, size="small"--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
predMiss20NN = impute_knn(datosE2@data, 
                      AverageTemperature~year+elev+solst+AbsLat+dist,
                      k=20)[miss,1]


## ----valida, size="scriptsize", fig.width=6, fig.height=6, fig.align="center"---------------------------------------------------------------------------------------------------------------------------------------------
errores = c(predMissMed - datosE$AverageTemperature[miss],
            predMissMd - datosE$AverageTemperature[miss],
            predMissMdCnd - datosE$AverageTemperature[miss],
            predMissLM - datosE$AverageTemperature[miss],
            predMissRF - datosE$AverageTemperature[miss],
            predMiss5NN - datosE$AverageTemperature[miss],
            predMiss20NN - datosE$AverageTemperature[miss])
metodos = rep(c("Media","Mediana","Mediana cnd","LM", "RF","5NN","20NN"),
              each=length(miss))
boxplot(errores~metodos, ylab="Error", xlab="Método", cex.axis=0.7)
abline(h=0, lty=3)


## ----valida2, size="scriptsize"-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
acc = matrix(rep(NA,21),ncol=7)
rownames(acc) = c("r2", "RMSE","bias")
colnames(acc) = c("Media","Mediana","Mediana cnd","LM", "RF","5NN","20NN")

acc[1,1] = cor(predMissMed, datosE$AverageTemperature[miss])^2
acc[1,2] = cor(predMissMd, datosE$AverageTemperature[miss])^2
acc[1,3] = cor(predMissMdCnd, datosE$AverageTemperature[miss])^2
acc[1,4] = cor(predMissLM, datosE$AverageTemperature[miss])^2
acc[1,5] = cor(predMissRF, datosE$AverageTemperature[miss])^2
acc[1,6] = cor(predMiss5NN, datosE$AverageTemperature[miss])^2
acc[1,7] = cor(predMiss20NN, datosE$AverageTemperature[miss])^2


## ----valida3, size="scriptsize"-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
acc[2,1] = sqrt(mean((predMissMed - datosE$AverageTemperature[miss])^2))
acc[2,2] = sqrt(mean((predMissMd - datosE$AverageTemperature[miss])^2))
acc[2,3] = sqrt(mean((predMissMdCnd - datosE$AverageTemperature[miss])^2))
acc[2,4] = sqrt(mean((predMissLM - datosE$AverageTemperature[miss])^2))
acc[2,5] = sqrt(mean((predMissRF - datosE$AverageTemperature[miss])^2))
acc[2,6] = sqrt(mean((predMiss5NN - datosE$AverageTemperature[miss])^2))
acc[2,7] = sqrt(mean((predMiss20NN - datosE$AverageTemperature[miss])^2))


## ----valida4, size="scriptsize"-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
acc[3,1] = mean(predMissMed - datosE$AverageTemperature[miss])
acc[3,2] = mean(predMissMd - datosE$AverageTemperature[miss])
acc[3,3] = mean(predMissMdCnd - datosE$AverageTemperature[miss])
acc[3,4] = mean(predMissLM - datosE$AverageTemperature[miss])
acc[3,5] = mean(predMissRF - datosE$AverageTemperature[miss])
acc[3,6] = mean(predMiss5NN - datosE$AverageTemperature[miss])
acc[3,7] = mean(predMiss20NN - datosE$AverageTemperature[miss])


## ----valida5, size="scriptsize"-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
options(width=220)
acc


## ----anexo1, size="scriptsize", cache=FALSE, fig.width=5, fig.height=5----------------------------------------------------------------------------------------------------------------------------------------------------
x = seq(1, 12, by=1)
y_north = -cos(2*pi*x/12)
y_north_variant = -cos(2*pi*(x-1)/12)
y_south = cos(2*pi*x/12)
y_south_variant = cos(2*pi*(x-1)/12)

north <- datos2[datos2$Latitude > 0,]
south <- datos2[datos2$Latitude < 0,]

average_north <- x
average_south <- x


for (i in x)  {
  average_north[i] = mean(north[north$mes == i,]$AverageTemperature)
  average_south[i] = mean(south[south$mes == i,]$AverageTemperature)
}

average_north <- rescale(average_north , to=c(-1,1))
average_south <- rescale(average_south , to=c(-1,1))

plot(x,y_north, main = "Solstice Calculation - North Hemisphere",xlab = "Month",
     ylab = "Solstice variable, north",col = 1,pch=1)
points(x,average_north,col = 2,pch = 2)
points(x,y_north_variant,col = 3, pch=3)
legend("bottom", legend = c("-cos(2*pi*x/12)","calculated temperature average",
                            "-cos(2*pi*(x-1)/12)"), 
       col = 1:3, pch = 1:3, bty = "n")
     
plot(x,y_south, main = "Solstice Calculation - South Hemisphere",xlab = "Month",
     ylab = "Solstice variable, south",col = 1,pch=1)
points(x,average_south,col = 2,pch = 2)
points(x,y_south_variant,col = 3, pch=3)
legend("top", legend = c("cos(2*pi*x/12)","calculated temperature average",
                         "cos(2*pi*(x-1)/12)"), 
       col = 1:3, pch = 1:3, bty = "n")


## ----anexo2, size="scriptsize", cache=FALSE, eval=FALSE,echo=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------
## coordinates(datos2) =~ Longitude+Latitude
## proj4string(datos2) = CRS("+init=epsg:4326")
## MDE=raster::raster("alwdgg.tif")
## distSea=raster::raster("distSea.tif")
## proj4string(MDE) = CRS("+init=epsg:4326")
## proj4string(distSea) = CRS("+init=epsg:4326")
## datos2$elev = raster::extract(MDE, datos2)
## datos2$dist = raster::extract(distSea, datos2)


## ----anexo2b, size="scriptsize"-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
datos2$solst1 = ifelse(datos2$Latitude<0,cos(2*pi*datos2$mes/12),-cos(2*pi*datos2$mes/12))
datos2$solst_alt = ifelse(datos2$Latitude<0,cos(2*pi*(datos2$mes-1)/12),-cos(2*pi*(datos2$mes-1)/12))


## ----anexo3, size="scriptsize", cache=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mod_original = lm(AverageTemperature~year+solst+AbsLat+Longitude+elev+dist, datos2)
mod_alternative = lm(AverageTemperature~year+solst_alt+AbsLat+Longitude+elev+dist, datos2)


## ----anexo4, size="scriptsize", cache=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(mod_original)
summary(mod_alternative)

