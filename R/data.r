rm(list = ls())
setwd("C:/R/dataset/S_SAR/crime")

 data <- read.csv("data.csv")

 # Dependent Variable #

 crime  <- data$�������˂炢23 + data$�E�э���23 + data$������23
 nhouse <- data$���ѐ�
 crate  <- 1000*crime/nhouse; crate[crate=="NaN"] <- 0

�@# Exogenous Variables #

 detach <- data$��ˌ�/nhouse
�@high   <- data$�K��11�ȏ�/nhouse
�@area   <- data$�ʐ�150�ȏ�/nhouse
 owner  <- data$������/nhouse
 single <- data$�P�Ɛ���/nhouse
 elder  <- data$����҂̂ݐ��ѐ�/nhouse
 dnsty  <- nhouse/data$AREA
 nhmem  <- data$���ѓ�����l��
 commt  <- data$����O�A�ƎҐ�/data$�J���͐l��; commt[commt=="NaN"] <- 0
 retail <- data$�����ʐ�.����./data$AREA
 manag  <- data$�Ǘ��I�E�Ə]����/data$�J���͐l��; manag[manag =="NaN"] <- 0

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