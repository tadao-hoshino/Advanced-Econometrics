rm(list = ls())
setwd("C:/R/dataset/S_SAR/crime")

 data <- read.csv("data.csv")

 # Dependent Variable #

 crime  <- data$あき巣ねらい23 + data$忍び込み23 + data$居あき23
 nhouse <- data$世帯数
 crate  <- 1000*crime/nhouse; crate[crate=="NaN"] <- 0

　# Exogenous Variables #

 detach <- data$一戸建/nhouse
　high   <- data$階数11以上/nhouse
　area   <- data$面積150以上/nhouse
 owner  <- data$持ち家/nhouse
 single <- data$単独世帯/nhouse
 elder  <- data$高齢者のみ世帯数/nhouse
 dnsty  <- nhouse/data$AREA
 nhmem  <- data$世帯当たり人員
 commt  <- data$自宅外就業者数/data$労働力人口; commt[commt=="NaN"] <- 0
 retail <- data$売り場面積.小売./data$AREA
 manag  <- data$管理的職業従事者/data$労働力人口; manag[manag =="NaN"] <- 0

 datt <- cbind(crate,dnsty,owner,elder,single,manag,retail,high,nhmem,commt,detach)

 # Spatial weight matrix #

 n <- length(crate)
 A <- matrix(0,n,n)
 
 for(i in 1:n){

	for(j in 1:data$Number_Neighbors[i]){

		A[i,data[i,54+j]] <- 1

		}

	}

 out1 <- ifelse(nhouse < 10, NA, 0)
 out2 <- ifelse(dnsty  < 1/10000, NA, 0)
 dd <- complete.cases(cbind(datt,out1,out2))
 
 DATA <- datt[dd,]
 W    <- A[dd,dd]
 rW   <- ifelse(rowSums(W) > 0, rowSums(W), 1)
 W    <- W/rW

 write.csv(DATA, "TokyoCrime.csv")
 write.csv(W, "TokyoCrime_W.csv")
 write.csv(data$ward[dd], "ku.csv")
 write.csv(data$aza[dd], "aza.csv")