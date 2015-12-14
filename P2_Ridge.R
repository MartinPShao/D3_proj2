rm(list = ls())
library(glmnet)
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
load("C:/Users/sz424/Desktop/nino.RData")
V122 <- rep(0,56)
for(i in 1:56){
  for(j in 1:9){
    V122[9*(i-1)+j] <- nino[,2][nino[,1]==1959+i]
  }
}
NewT <- cbind(NewT,V122)



lasso.mod.full <- glmnet(x = model.matrix(Apps ~ ., College)[,-2], 
                         y = College$Apps, 
                         alpha = 1, 
                         lambda = lasso.bestlam)
lasso.coef <- predict(lasso.mod.full,
                      type="coefficients",
                      s=lasso.bestlam)[1:18,]
lasso.coef <- lasso.coef[lasso.coef!=0]

Test.MSE.list <- numeric(46)
for(i in 1970:2015){
  TrainT <- NewT[NewT$V3 < i & NewT$V3 >= (i-10),]
  TestT <- NewT[NewT$V3 == i,]

  lasso.cv.out <- cv.glmnet(x = model.matrix(V1 ~ ., TrainT)[,-1], 
                            y = TrainT$V1, 
                            alpha = 1)
  lasso.bestlam <- lasso.cv.out$lambda.min
  lasso.mod <- glmnet(x = model.matrix(V1 ~ ., TrainT)[,-1], 
                      y = TrainT$V1,
                      alpha = 1, 
                      lambda = lasso.bestlam)
  lasso.pred <- predict(lasso.mod,
                        s = lasso.bestlam,
                        newx = model.matrix(V1 ~ ., TestT)[,-1])
  Test.MSE.list[i-1969] <- mean((lasso.pred - TestT$V1)^2)
  print(i)
}






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
N1 <- NewT
NewT <- N1[,-c(86:121)]

Test.MSE.list.ridge <- numeric(46)
Test.absE.list <- rep(0, 46)
for(i in 1970:2015){
  TrainT <- NewT[NewT$V3 < i & NewT$V3 >= (i-10),]
  TestT <- NewT[NewT$V3 == i,]
  
  lasso.cv.out <- cv.glmnet(x = model.matrix(V1 ~ ., TrainT)[,-1], 
                            y = TrainT$V1, 
                            alpha = 0)
  lasso.bestlam <- lasso.cv.out$lambda.min
  lasso.mod <- glmnet(x = model.matrix(V1 ~ ., TrainT)[,-1], 
                      y = TrainT$V1,
                      alpha = 0, 
                      lambda = lasso.bestlam)
  lasso.pred <- predict(lasso.mod,
                        s = lasso.bestlam,
                        newx = model.matrix(V1 ~ ., TestT)[,-1])
  Test.MSE.list.ridge[i-1969] <- mean((lasso.pred - TestT$V1)^2)
  Test.absE.list[i-1969] <- mean(unlist(abs(lasso.pred-TestT$V1)))
  print(i)
}

mean(Test.MSE.list.ridge)
mean(Test.absE.list)

#March 430
#June 452.41

