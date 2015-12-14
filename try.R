setwd("~/Documents/git/D3_proj2")
load("./rawData.RData")
baseline_2 <- IowaCornYieldAnom19502009train[1:59, -1]
diff_2 <- IowaCornYieldAnom19502009train[2:60, -1] - baseline_2
mean(apply(diff_2^2, 1, mean))
basline_1 <- apply(IowaCornYieldAnom19502009train, 2, mean)
for (i in 1:60){
        diff_1 <- IowaCornYieldAnom19502009train[i, ] - basline_1
}
mean(apply(diff_1^2, 1, mean))
smooth_corn <- matrix(NA, nrow = 58, ncol = 9)
for (i in 1:58){
        smooth_corn[i, ] <- apply(IowaCornYieldAnom19502009train[i:(i+2), 
                                                                 -1], 
                                  2, 
                                  mean)
}


par(mfrow=c(2,1),mar=c(3,3,1,1))
rm(ttt)
ttt <- numeric(59)
for (i in 0:58){
        ttt[i+1] <- mean(as.numeric(Precip_train_011950_062009[34, (12*i+1):(12*(i+1))]))
}
plot(1950:2008, ttt, type = "l")
abline(v = c(1983, 1988, 1993, 1994, 1998))

rm(ttt)
ttt <- numeric(59)
for (i in 0:58){
        ttt[i+1] <- mean(as.numeric(Temp_train_011950_062009[34,  (12*i+1):(12*(i+1))]))
}
plot(1950:2008, ttt, type = "l")
abline(v = c(1983, 1988, 1993, 1994, 1998))


nino <- read.table(file = "3mth.nino34.81-10.ascii.txt", header = FALSE, 
                   col.names = c("mons", "year", "sst"))
library(dplyr)
nino <- nino %>%
        group_by(year) %>%
        summarise(sst = mean(sst))
nino <- as.data.frame(nino)


tp <- rbind(as.matrix(Iowa_CRD_Centroid_Long_Lat[,2:3]), as.matrix(lonlatTempPrecip))
tp <- as.matrix(dist(tp, diag = TRUE, upper = TRUE))
distance <- tp[1:9, -(1:9)]
for (i in 1:9){
        distance[i, distance[i, ]<quantile(distance[i, ], probs = 0.21)] <- 1
        distance[i, !(distance[i, ]<quantile(distance[i, ], probs = 0.21))] <- 0
}
dimnames(distance) <- NULL

area_ave_temp <- matrix(NA, nrow = 9, ncol = 786)
for (i in 1:9){
        area_ave_temp[i, ] <- apply(Temp[as.logical(distance[i, ]), ], 2, mean)
}

area_ave_prec <- matrix(NA, nrow = 9, ncol = 786)
for (i in 1:9){
        area_ave_prec[i, ] <- apply(Prec[as.logical(distance[i, ]), ], 2, mean)
}
save(area_ave_temp, area_ave_prec, file = "area_monthly_data.RData")

pna <- read.table(file = "./pna.txt", header = TRUE)
pna <- pna %>%
        group_by(YEAR) %>%
        summarise(pna = mean(INDEX))
pna <- as.data.frame(pna)

solar <- read.table(file = "solar.data.txt", header = FALSE)
solar <- reshape(solar, direction = "long", varying = list(names(solar)[-1]), 
                 idvar = "V1")
solar <- solar %>%
        group_by(V1) %>%
        summarize(solar_flux = mean(V2))


Long_yield <- reshape(Yield, idvar = "Year", direction = "long", times = 1:9, timevar = "area", 
                      varying = list(names(Yield)[-1]))





rownames(Long_yield) <- NULL
colnames(Long_yield) <- c("year", "district", "yield")
cut1 <- quantile(Long_yield$yield, probs = 1/3)
cut2 <- quantile(Long_yield$yield, probs = 2/3)
Long_yield$yield[Long_yield$yield<=cut2 & Long_yield$yield>=cut1] <- 0
Long_yield$yield[Long_yield$yield<cut1] <- -1
Long_yield$yield[Long_yield$yield>cut2] <- 1

Long_yield$yield <- as.factor(Long_yield$yield)




library(caret)
cbaseline1 <- Long_yield$yield
for (i in 0:8){
        cbaseline1[(66*i+2):(66*i+66)] <- cbaseline1[(66*i+1):(66*i+65)]
}
confusionMatrix(cbaseline1, Long_yield$yield)
cbaseline2 <- factor(rep(0, 594), levels = levels(Long_yield$yield))
confusionMatrix(cbaseline2, Long_yield$yield)
save(Long_yield, file = "./Class_Resp.RData")
