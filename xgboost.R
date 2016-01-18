#Zmieñ œcie¿kê do katalogu
sciezka_do_katalogu <- "C:/Users/Armin/Documents/GitHub/konkurs"
setwd(sciezka_do_katalogu)

#wczytywandoDanych
trainCSV<-read.table("train.csv", header=TRUE, sep=",")
testCSV<-read.table("test.csv", header=TRUE, sep=",")

#dodajemy do testowej response i odró¿nianie zbiorów
testCSV$Response<-NA
testCSV$CzyTrain<-0
trainCSV$CzyTrain<-1

#czy kolejnoœæ zmiennych siê zgadza?
names(testCSV)==names(trainCSV)
allData<-rbind(trainCSV,testCSV)

#Wyjebiamy co niepotrzebne, Ram siê przyda do klepania modeli
rm(testCSV)
rm(trainCSV)


#popatrzmy sobie jak dzia³a to cudo
library("xgboost")

#przygotowanie danych, usuwanie faktorów etc.
labels <- as.vector(train[128])
levels(allData[[3]]) <- 1:19
allData[3] <- as.numeric(allData[[3]])

#Odwo³anie do testowego i ucz¹cego
test<-allData[which(allData$CzyTrain==0),]
train<-allData[which(allData$CzyTrain==1),]

#model training
bstSparse <- xgboost(data = as.matrix(train[2:127]), label = as.matrix(train[128]), missing = c('NA', 'NAN'), nrounds = 1000)

#model assesment
asses <- predict(bstSparse, as.matrix(train[2:127]), missing = c('NA', 'NAN'))
asses <- round(asses)
err <- mean(asses - train[[128]])

#prediction
pred <- predict(bstSparse, as.matrix(test[2:127]), missing = c('NA', 'NAN'))
pred <- round(pred)
pred[pred<=0] <- 1
pred[pred>8] <- 8 

#creating outputfile 
output <- data.frame(x = cbind(test[1], pred))
names(output) <- c('Id',"Response")
write.csv(output, file = "wyn.csv", row.names = FALSE)

qplot(Response, data=output) 
