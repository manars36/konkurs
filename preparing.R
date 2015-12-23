##TEST

#Zmieñ œcie¿kê do katalogu
sciezka_do_katalogu <- getwd()
setwd(sciezka_do_katalogu)

#wczytywandoDanych
trainCSV<-read.table("train.csv", header=TRUE, sep=",")
testCSV<-read.table("test.csv", header=TRUE, sep=",")

#podstawy danych jak chcesz przejrzeæ
str(trainCSV)
str(testCSV)

#dodajemy do testowej response i odró¿nianie zbiorów
testCSV$Response<-NA
testCSV$CzyTrain<-0
trainCSV$CzyTrain<-1

#czy kolejnoœæ zmiennych siê zgadza?
names(testCSV)==names(trainCSV)
allData<-rbind(trainCSV,testCSV)


#################
## zrobienie factorów ze zmiennych kategorialnych

wektorCategorial<-c("Product_Info_1","Response", "Product_Info_2", "Product_Info_3", "Product_Info_5", "Product_Info_6", "Product_Info_7", "Employment_Info_2", "Employment_Info_3", "Employment_Info_5", "InsuredInfo_1", "InsuredInfo_2", "InsuredInfo_3", "InsuredInfo_4", "InsuredInfo_5", "InsuredInfo_6", "InsuredInfo_7", "Insurance_History_1", "Insurance_History_2", "Insurance_History_3", "Insurance_History_4", "Insurance_History_7", "Insurance_History_8", "Insurance_History_9", "Family_Hist_1", "Medical_History_2", "Medical_History_3", "Medical_History_4", "Medical_History_5", "Medical_History_6", "Medical_History_7", "Medical_History_8", "Medical_History_9", "Medical_History_10", "Medical_History_11", "Medical_History_12", "Medical_History_13", "Medical_History_14", "Medical_History_16", "Medical_History_17", "Medical_History_18", "Medical_History_19", "Medical_History_20", "Medical_History_21", "Medical_History_22", "Medical_History_23", "Medical_History_25", "Medical_History_26", "Medical_History_27", "Medical_History_28", "Medical_History_29", "Medical_History_30", "Medical_History_31", "Medical_History_33", "Medical_History_34", "Medical_History_35", "Medical_History_36", "Medical_History_37", "Medical_History_38", "Medical_History_39", "Medical_History_40", "Medical_History_41")

for(i in 1:length(wektorCategorial)){
  allData[,wektorCategorial[i]]<-as.factor(allData[,wektorCategorial[i]])
}
#Wyjebiamy co niepotrzebne, Ram siê przyda do klepania modeli
rm(testCSV)
rm(trainCSV)
#Odwo³anie do testowego i ucz¹cego
testowy<-allData[which(allData$CzyTrain==0),]
uczacy<-allData[which(allData$CzyTrain==1),]

### Zaczynamy zabawê
library(ggplot2) #jeœli tu Ci siê wyjeba³ b³¹d to przeproœ, i zainstaluj pakiet
library(gridExtra) #wiêcej wykresów na jednej stronie
library(grid) # wiêcej wykresów vol2

#rozk³ad odpowiedzi
p1<-qplot(Response, data=uczacy, xlab="Rozk³ad odpowiedzi - ucz¹cy", ylab="Liczba",fill=Response) 
windows()
p1



######### produkt info 1
p2<-qplot(Product_Info_1, data=uczacy, xlab="Produkt info 1", ylab="Liczba",fill=Response)
#tab2 <- table(uczacy$Response,uczacy$Product_Info_1)
#tab2prop<-prop.table(tab2,2)
#summary(tab2)# - teœcik chi-2 jakby ktoœ chcia³
#grid.table(round(tab2prop,digits=3),rows=paste("Res",rownames(tab2prop)))

windows()
p2
######### produkt info 1 koniec


######### produkt info 2
p3<-qplot(Product_Info_2, data=uczacy, xlab="Produkt info 2", ylab="Liczba",fill=Response)
windows()
p3
#mo¿na pogrupowaæ po literkach
######### produkt info 2 koniec



######### produkt info 3
p4<-qplot(Product_Info_3, data=uczacy, xlab="Produkt info 3", ylab="Liczba",fill=Response)
windows()
p4
######### produkt info 3 koniec

######### produkt info 4
p5<-qplot(Product_Info_4, data=uczacy, xlab="Produkt info 5", ylab="Rozk³ad",fill=Response,geom="Density",alpha=I(.4))
windows()
p5

p51<-qplot(Response,Product_Info_4, data=uczacy, xlab="OdpowiedŸ", ylab="Produkt info 5",fill=Response, geom="Boxplot")
windows()
p51


p52<-qplot(Product_Info_4, data=uczacy, xlab="Produkt info 5", ylab="Rozk³ad")
windows()
p52
######### produkt info 4 koniec



######### produkt info 5
p6<-qplot(Product_Info_5, data=uczacy, xlab="Produkt info 5", ylab="Liczba",fill=Response)
windows()
p6
######### produkt info 5 koniec


######### produkt info 6
p7<-qplot(Product_Info_6, data=uczacy, xlab="Produkt info 6", ylab="Liczba",fill=Response)
windows()
p7
tab<- table(uczacy$Response,uczacy$Product_Info_6)
tabprop<-prop.table(tab,2)
summary(tab) # teœcik chi-2 jakby ktoœ chcia³

######### produkt info 6 koniec

######### produkt info 7
p8<-qplot(Product_Info_7, data=uczacy, xlab="Produkt info 7", ylab="Liczba",fill=Response)
windows()
p8
######### produkt info 7 koniec

###############################################################################
######################### AGE I WIELKOŒÆ ######################################
a<-qplot(Ins_Age, data=uczacy, xlab="Wiek", ylab="Rozk³ad",fill=Response,geom="Density",alpha=I(.4))
windows()
a

a3<-qplot(Response,Ins_Age, data=uczacy, xlab="OdpowiedŸ", ylab="Wiek",fill=Response, geom="Boxplot")
windows()
a3


a2<-qplot(Ins_Age, data=uczacy, xlab="Wiek", ylab="Rozk³ad")
windows()
a2

#wielkoœæ
wielkoœæ<-qplot(Wt,Ht, data=uczacy, xlab="", ylab="",colour=Response)
windows()
wielkoœæ

#bmi

bmi<-qplot(Response,BMI, data=uczacy, xlab="OdpowiedŸ", ylab="BMI",fill=Response, geom="Boxplot")
windows()
bmi

bmi2<-qplot(BMI, data=uczacy, xlab="BMI", ylab="Rozk³ad",fill=Response,geom="Density",alpha=I(.4))
windows()
bmi2
