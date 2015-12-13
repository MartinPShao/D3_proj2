setwd("~/Downloads")
rm(list = ls())
library(readr)
library(plyr)
Iowa_CRD_Centroid_Long_Lat <- file(description = "./Iowa_CRD_Centroid_Long_Lat")
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
month_index <- expand.grid(month.abb, 1950:2008)
month_index <- mdply(month_index, 'paste', sep = "")
month_index <- data.frame(ind = 1:(nrow(month_index) + 6),
                          month = c(month_index[, 3], 
                                    paste(month.abb[1:6], 
                                          "2009", 
                                          sep = "")), 
                          stringsAsFactors = FALSE
)
IowaCornYieldAnom19502009train <- 
  read.table("./IowaCornYieldAnom19502009train.dat", 
             col.names = c("Year",as.character(Iowa_CRD_Centroid_Long_Lat$Location)[order(Iowa_CRD_Centroid_Long_Lat$Location)]))


lonlatSST <- read.table(
  "./lonlatSST.dat", 
  col.names = c("lon", "lat")
)

lonlatTempPrecip <- read.table("./lonlatTempPrecip.dat", 
                               col.names = c("Temp", "Precip"))
Precip_train_011950_062009 <- read.table(
  "./Precip_train_011950_062009.dat", 
  col.names = month_index$month)
Temp_train_011950_062009 <- read.table(
  "./Temp_train_011950_062009.dat", 
  col.names = month_index$month
)
SST_train_011950_062009 <- read_table(
  "./SST_train_011950_062009.dat", 
  col_names = month_index$month
)

save(list = ls(), file = "./rawData.RData")

yield <- IowaCornYieldAnom19502009train
Temp <- Temp_train_011950_062009
Prec <- Precip_train_011950_062009
SST <- SST_train_011950_062009

tt <- prcomp(t(Temp))
scores.Temp <- tt$x[,1:3]
tt <- prcomp(t(Prec))
scores.Prec <- tt$x[,1:3]
tt <- prcomp(t(SST))
scores.SST <- tt$x[,1:3]

rm(NewT)
NewT <- as.data.frame(matrix(nrow=450,ncol=13+72+36))
for(i in 1:50){
    for(j in 1:9){
      NewT[9*(i-1)+j,1] <- yield[i+10,j+1]    
      NewT[9*(i-1)+j,2] <- j
      NewT[9*(i-1)+j,3] <- 1959+i 
      
      for(k in 1:10){
        NewT[9*(i-1)+j,3+k] <- yield[i+10-k,j+1]
      }
      for(k in 1:12){
        NewT[9*(i-1)+j,13+k] <- scores.Temp[120+(i-2)*12+k,1]
      }
      for(k in 1:12){
        NewT[9*(i-1)+j,25+k] <- scores.Temp[120+(i-2)*12+k,2]
      }
      for(k in 1:12){
        NewT[9*(i-1)+j,37+k] <- scores.Temp[120+(i-2)*12+k,3]
      }
      for(k in 1:12){
        NewT[9*(i-1)+j,49+k] <- scores.Prec[120+(i-2)*12+k,1]
      }
      for(k in 1:12){
        NewT[9*(i-1)+j,61+k] <- scores.Prec[120+(i-2)*12+k,2]
      }
      for(k in 1:12){
        NewT[9*(i-1)+j,73+k] <- scores.Prec[120+(i-2)*12+k,3]
      }
      for(k in 1:12){
        NewT[9*(i-1)+j,85+k] <- scores.SST[120+(i-2)*12+k,1]
      }
      for(k in 1:12){
        NewT[9*(i-1)+j,97+k] <- scores.SST[120+(i-2)*12+k,2]
      }
      for(k in 1:12){
        NewT[9*(i-1)+j,109+k] <- scores.SST[120+(i-2)*12+k,3]
      }
    }
}
  
NewT$V2 <- as.factor(NewT$V2)
NewT$V3 <- as.numeric(NewT$V3)
library(h2o)
h2o.init(nthreads=-1,max_mem_size='6G')
## Load data into cluster from R
NewT.h2o<-as.h2o(NewT)
## Set up variable to use all features other than those specified here
features<-names(NewT)
## Train a random forest using all default parameters
rfHex <- h2o.randomForest(x=features[2:121],
                          y="V1", 
                          ntrees = 1000,
                          max_depth = 65,
                          nbins_cats = 9, 
                          nfolds = 5,
                          training_frame=NewT.h2o)

predict_train<-as.data.frame(h2o.predict(rfHex,NewT.h2o))
mean((predict_train-NewT$V1)^2)


par(mfrow=c(3,1),mar=c(2,2,2,2))
plot(NewT$V1)
plot(predict_train$predict)
plot(NewT$V1-predict_train$predict)




