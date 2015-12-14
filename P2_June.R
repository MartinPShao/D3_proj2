setwd("~/Downloads")
rm(list = ls())
library(readr)
library(plyr)
Iowa_CRD_Centroid_Long_Lat <- file(description = "~/Downloads/Iowa_CRD_Centroid_Long_Lat")
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
  read.table("./IowaCornYieldAnom19502015test.dat", 
             col.names = c("Year",as.character(Iowa_CRD_Centroid_Long_Lat$Location)[order(Iowa_CRD_Centroid_Long_Lat$Location)]))


lonlatSST <- read.table(
  "./lonlatSST.dat", 
  col.names = c("lon", "lat")
)

lonlatTempPrecip <- read.table("./lonlatTempPrecip.dat", 
                               col.names = c("Temp", "Precip"))
Prec <- read.table(
  "./Precip_test_011950_062015.dat",
  col.names = month_index$month)
Temp <- read.table(
  "./Temp_test_011950_062015.dat", 
  col.names = month_index$month
)
SST <- read_table(
  "./SST_test_011950_062015.dat", 
  col_names = month_index$month
)

# save(list = ls(), file = "./rawData.RData")

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
      NewT[9*(i-1)+j,13+k] <- scores.Temp[114+(i-1)*12+k,1]
    }
    for(k in 1:12){
      NewT[9*(i-1)+j,25+k] <- scores.Temp[114+(i-1)*12+k,2]
    }
    for(k in 1:12){
      NewT[9*(i-1)+j,37+k] <- scores.Temp[114+(i-1)*12+k,3]
    }
    for(k in 1:12){
      NewT[9*(i-1)+j,49+k] <- scores.Prec[114+(i-1)*12+k,1]
    }
    for(k in 1:12){
      NewT[9*(i-1)+j,61+k] <- scores.Prec[114+(i-1)*12+k,2]
    }
    for(k in 1:12){
      NewT[9*(i-1)+j,73+k] <- scores.Prec[114+(i-1)*12+k,3]
    }
    for(k in 1:12){
      NewT[9*(i-1)+j,85+k] <- scores.SST[114+(i-1)*12+k,1]
    }
    for(k in 1:12){
      NewT[9*(i-1)+j,97+k] <- scores.SST[114+(i-1)*12+k,2]
    }
    for(k in 1:12){
      NewT[9*(i-1)+j,109+k] <- scores.SST[114+(i-1)*12+k,3]
    }
  }
}

NewT$V2 <- as.factor(NewT$V2)
#NewT$V3 <- as.factor(NewT$V3)

V122 <- rep(0,56)
for(i in 1:56){
  for(j in 1:9){
     V122[9*(i-1)+j] <- nino[,2][nino[,1]==1959+i]
  }
}

NewT <- cbind(NewT,V122)

h2o.init(nthreads=-1,max_mem_size='6G')
features<-names(NewT)
MSE.list <- rep(0,46)
Test.MSE.list <- rep(0,46)
for(i in 1970:2015){
  #rm(c(TryT,TestT,TryT.h2o,TestT.h2o))
  TrainT <- NewT[NewT$V3 < i,]
  TestT <- NewT[NewT$V3 == i,]
  TrainT.h2o<-as.h2o(TrainT)
  TestT.h2o<-as.h2o(TestT)
  rfHex.p2 <- h2o.randomForest(x=features[2:122],
                               y="V1", 
                               ntrees = 300,
                               max_depth = 40,
                               mtries = 45,
                               #nbins = 100,
                               nbins_cats = 9, 
                               nfolds = 5,
                               training_frame=TrainT.h2o)
  MSE.list[i-1969] <- rfHex.p2@model$training_metrics@metrics$MSE
  predict_test<-as.data.frame(h2o.predict(rfHex.p2,TestT.h2o))
  Test.MSE.list[i-1969] <-mean((predict_test-TestT$V1)^2)
  print(i)
}


JuneMSE <- Test.MSE.list
JuneMSE.nino <- Test.MSE.list
h2o.shutdown()






# start from 1971 as test, training sample from 1960~1970
# whether there is improve along time



























dlHex <- h2o.deeplearning(
  x=features[2:121],
  y="V1", 
  training_frame=TryT.h2o,
  #activation = 
  hidden = c(100,100),
  rho = 0.99,
  epsilon = 0.00001,
  rate = 0.005
)
dlHex@model$training_metrics@metrics$MSE
pred.test.dl <- as.data.frame(h2o.predict(dlHex,TestT.h2o))
pred.test.dl
mean((pred.test.dl-TestT$V1)^2)

predict_test<-as.data.frame(h2o.predict(rfHex.p2,TestT.h2o))
predict_test
mean((predict_test-TestT$V1)^2)

predict_train<-as.data.frame(h2o.predict(rfHex.p2,TryT.h2o))
mean((predict_train-TryT$V1)^2)



h2o.shutdown()












par(mfrow=c(3,1),mar=c(2,2,2,2))
plot(NewT$V1)
plot(predict_train$predict)
plot(NewT$V1-predict_train$predict)






h2o.init(nthreads=-1,max_mem_size='6G')
full.list <- seq(1:450)

train.list <- sort(sample(full.list,360,replace=FALSE))
test.list <- setdiff(full.list,train.list) 
train <- NewT[train.list,]
test <- NewT[test.list,]
train.h2o <- as.h2o(train)
test.h2o <- as.h2o(test)
features<-names(NewT)
#for(i in )
gbm.NewT <- h2o.gbm(x=features[2:121],
                    y="V1",
                    distribution = "gaussian",
                    learn_rate = 0.2,
                    ntrees = 32,
                    max_depth = 7,
                    nbins_cats = 9, 
                    validation_frame = test.h2o,
                    training_frame=train.h2o)
predict.test.gbm<-as.data.frame(h2o.predict(gbm.NewT,test.h2o))
mean((predict.test.gbm-test$V1)^2)

par(mfrow=c(3,1),mar=c(2,2,2,2))
plot(NewT$V1)
plot(predict.train.gbm$predict)
plot(NewT$V1-predict.train.gbm$predict)


library(caret)

gbm.NewT <- train(
  V1~., data=NewT,
#  nrounds=500,
#  interaction.depth=20,
#  shinkage=0.01,
#  trControl = trainControl(method = "cv"),
  method="xgbTree"
)
pred.gbm <- predict(gbm.NewT,NewT)
mean((pred.gbm-NewT$V1)^2)

dim(NewT)

rfN <- randomForest(V1~.,
                    data = NewT,
                    importance=TRUE)



TRAIN <- NewT[1:405,]
TEST <- NewT[406:450,]
TRAIN.h2o<-as.h2o(TRAIN)
TEST.h2o <- as.h2o(TEST)
## Set up variable to use all features other than those specified here
features<-names(NewT)
## Train a random forest using all default parameters
rfHex <- h2o.randomForest(x=features[2:121],
                          y="V1", 
                          ntrees = 800,
                          max_depth = 50,
                          #nbins_cats = 9, 
                          #nfolds = 5,
                          mtries = 35,
                          training_frame=TRAIN.h2o
                          #validation_frame = TEST.h2o
                          )
#rfHex@model$validation_metrics@metrics$MSE
#rfHex@model$training_metrics@metrics$MSE

predict_test<-as.data.frame(h2o.predict(rfHex,TEST.h2o))
#plot(predict_train,TEST$V1)
mean((predict_test-TEST$V1)^2)

