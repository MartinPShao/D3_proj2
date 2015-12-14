
# Before it, you need read the hold data using your code, the datasets are named
# as Temp and Prec


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