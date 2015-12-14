rm(list = ls())
library(readr)
library(plyr)
library(h2o)
Iowa_CRD_Centroid_Long_Lat <- file(description = "C:\\Users\\sz424\\Desktop\\Iowa_CRD_Centroid_Long_Lat")
open(Iowa_CRD_Centroid_Long_Lat)
temp <- readLines(Iowa_CRD_Centroid_Long_Lat)
close(Iowa_CRD_Centroid_Long_Lat)
rm(Iowa_CRD_Centroid_Long_Lat)
temp <- strsplit(temp, "[\t]+")
Iowa_CRD_Centroid_Long_Lat <- data.frame(
  sapply(temp[-1], "[", 1), 
  as.numeric(sapply(temp[-1], "[", 2)), 
  as.numeric(sapply(temp[-1], "[", 3)))
names(Iowa_CRD_Centroid_Long_Lat) <- substr(temp[[1]], 
                                            2, 
                                            nchar(temp[[1]])-1)
rm(temp)
month_index <- expand.grid(month.abb, 1950:2014)
month_index <- mdply(month_index, 'paste', sep = "")
month_index <- data.frame(ind = 1:(nrow(month_index) + 6),
                          month = c(month_index[, 3], 
                                    paste(month.abb[1:6], 
                                          "2015", 
                                          sep = "")), 
                          stringsAsFactors = FALSE
)
Yield <- 
  read.table("C:\\Users\\sz424\\Desktop\\IowaCornYieldAnom19502015test.dat")
#  col.names = c("Year",as.character(Iowa_CRD_Centroid_Long_Lat$Location)[order(Iowa_CRD_Centroid_Long_Lat$Location)]))


lonlatSST <- read.table(
  "C:\\Users\\sz424\\Desktop\\lonlatSST.dat", 
  col.names = c("lon", "lat")
)

lonlatTempPrecip <- read.table("C:\\Users\\sz424\\Desktop\\lonlatTempPrecip.dat", 
                               col.names = c("Temp", "Precip"))
Prec <- read.table(
  "C:\\Users\\sz424\\Desktop\\Precip_test_011950_062015.dat",
  col.names = month_index$month)
Temp <- read.table(
  "C:\\Users\\sz424\\Desktop\\Temp_test_011950_062015.dat", 
  col.names = month_index$month
)
SST <- read_table(
  "C:\\Users\\sz424\\Desktop\\SST_test_011950_062015.dat", 
  col_names = month_index$month
)

save(list = ls(), file = "./rawData.RData")

tt <- prcomp(t(Temp))
scores.Temp <- tt$x[,1:3]
tt <- prcomp(t(Prec))
scores.Prec <- tt$x[,1:3]
tt <- prcomp(t(SST))
scores.SST <- tt$x[,1:3]


NewT <- as.data.frame(matrix(nrow=450+54,ncol=13+72+36))
for(i in 1:56){
  for(j in 1:9){
    NewT[9*(i-1)+j,1] <- Yield[i+10,j+1]    
    NewT[9*(i-1)+j,2] <- j
    NewT[9*(i-1)+j,3] <- 1959+i 
    
    for(k in 1:10){
      NewT[9*(i-1)+j,3+k] <- Yield[i+10-k,j+1]
    }
    for(k in 1:12){
      NewT[9*(i-1)+j,13+k] <- scores.Temp[111+(i-1)*12+k,1]
    }
    for(k in 1:12){
      NewT[9*(i-1)+j,25+k] <- scores.Temp[111+(i-1)*12+k,2]
    }
    for(k in 1:12){
      NewT[9*(i-1)+j,37+k] <- scores.Temp[111+(i-1)*12+k,3]
    }
    for(k in 1:12){
      NewT[9*(i-1)+j,49+k] <- scores.Prec[111+(i-1)*12+k,1]
    }
    for(k in 1:12){
      NewT[9*(i-1)+j,61+k] <- scores.Prec[111+(i-1)*12+k,2]
    }
    for(k in 1:12){
      NewT[9*(i-1)+j,73+k] <- scores.Prec[111+(i-1)*12+k,3]
    }
    for(k in 1:12){
      NewT[9*(i-1)+j,85+k] <- scores.SST[111+(i-1)*12+k,1]
    }
    for(k in 1:12){
      NewT[9*(i-1)+j,97+k] <- scores.SST[111+(i-1)*12+k,2]
    }
    for(k in 1:12){
      NewT[9*(i-1)+j,109+k] <- scores.SST[111+(i-1)*12+k,3]
    }
  }
}

NewT$V2 <- as.factor(NewT$V2)
#NewT$V3 <- as.factor(NewT$V3)
load("C:/Users/sz424/Desktop/nino.RData")
V122 <- rep(0,56)
for(i in 1:56){
  for(j in 1:9){
    V122[9*(i-1)+j] <- nino[,2][nino[,1]==1959+i]
  }
}
NewT <- cbind(NewT,V122)
h2o.init(nthreads=-1,max_mem_size='6G')

MSE.list <- rep(0,46)
Test.MSE.list <- rep(0,46)
Test.absE.list <- rep(0, 46)
features<-names(NewT)
for(i in 1970:2015){
  #rm(c(TryT,TestT,TryT.h2o,TestT.h2o))
  TrainT <- NewT[NewT$V3 < i & NewT$V3 >= (i-10),]
  TestT <- NewT[NewT$V3 == i,]
  TrainT.h2o<-as.h2o(TrainT)
  TestT.h2o<-as.h2o(TestT)
  rfHex.p2 <- h2o.randomForest(x=features[2:85],
                               y="V1", 
                               ntrees = 500,
                               max_depth = 40,
                               mtries = 45,
                               nbins = 100,
                               #nbins_cats = 1000, 
                               nfolds = 5,
                               training_frame=TrainT.h2o)
  MSE.list[i-1969] <- rfHex.p2@model$training_metrics@metrics$MSE
  predict_test<-as.data.frame(h2o.predict(rfHex.p2,TestT.h2o))
  Test.MSE.list[i-1969] <-mean((predict_test-TestT$V1)^2)
  Test.absE.list[i-1969] <- mean(unlist(abs(predict_test-TestT$V1)))
  print(i)
}

Test.MSE.list
mean(Test.MSE.list)
plot(1970:2015,Test.MSE.list,type="p",col="red")
mean(Test.absE.list)
