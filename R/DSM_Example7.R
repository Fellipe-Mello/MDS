getwd()
setwd("DIRETORIO/SUBPASTA") # Adjusting directory

# Adding packages ----

if(!require(randomForest)) {
  install.packages("randomForest"); require(randomForest)}

if(!require(gstat)) {
  install.packages("gstat"); require(gstat)}

if(!require(automap)) {
  install.packages("automap"); require(automap)}

if(!require(fBasics)) {
  install.packages("fBasics"); require(fBasics)}

if(!require(ggplot2)) {
  install.packages("ggplot2"); require(ggplot2)}

if(!require(nortest)) {
  install.packages("nortest"); require(nortest)}

if(!require(caret)) {
  install.packages("caret"); require(caret)}

if(!require(sp)) {
  install.packages("sp"); require(sp)}

if(!require(raster)) {
  install.packages("raster"); require(raster)}

if(!require(rgdal)) {
  install.packages("rgdal"); require(rgdal)}

if(!require(MASS)) {
  install.packages("MASS"); require(MASS)}

if (!require(corrplot)){
  install.packages("corrplot"); require(corrplot)
}

if (!require(dplyr)){
  install.packages("dplyr"); require(dplyr) # for "glimpse"
}

if (!require(magrittr)){
  install.packages("magrittr"); require(magrittr)
}

if (!require(scales)){
  install.packages("scales"); require(scales) # for "comma"
}

if (!require(Cubist)){
  install.packages("Cubist"); require(Cubist) # for "comma"
}

if (!require(e1071)){
  install.packages("e1071"); require(e1071) # for "comma"
}


# Funcao para R2_RMSE_RPD_RPIQ
goof <- function(observed,predicted, plot.it = FALSE, type="DSM"){
  # Coefficient of determination
  rLM <- lm(predicted ~ observed)
  R2 <- as.matrix(summary(rLM)$adj.r.squared)
  
  # Standard error of prediction ^2
  SEP2 <- mean((observed - predicted)^2)
  
  # Standard error of prediction
  SEP <- sqrt(SEP2)
  
  #Bias
  bias <- mean(predicted) - mean(observed)
  
  # residual  variance
  SEP2c <- sum(((predicted - bias - observed)^2) / length(observed))
  SEPc <- sqrt(SEP2c)
  
  # ratio of performance to deviation
  RPD <- sd(observed) / SEP
  
  # Ratio of performance to interquartile distance
  IQ <- c(quantile(observed))[3] - c(quantile(observed))[2]
  RPIQ <- IQ / SEP
  
  # Concordance
  mx <- mean(observed)
  my <- mean(predicted)
  s2x <- var(observed)
  s2y <- var(predicted)
  sxy <- mean((observed-mx) * (predicted-my))
  ccc <- 2 * sxy / (s2x + s2y + (mx - my)^2)
  
  if (plot.it==TRUE){eqscplot(observed, predicted)
    abline(a = 0, b = 1, col = "brown4")}
  
  if (type == "DSM"){ gf <- data.frame(R2=R2, concordance=ccc, MSE=SEP2, RMSE=SEP, bias=bias, row.names=NULL)}
  else if (type == "spec"){ gf <- data.frame(R2=R2, concordance=ccc, MSE=SEP2, RMSE=SEP, bias=bias, 
                                             MSEc=SEP2c,RMSEc=SEPc, RPD=RPD, RPIQ=RPIQ, row.names=NULL)}
  else {stop("ERROR: Revise the type of output you require. Select from either DSM or spec")} 
  
  return(gf)
} 


#--Open Database and Initial stats--------------------------------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~ Tabelas de calibracao~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Importing data
#Layer A
PI <- read.csv("modelagem.csv", head=T,sep=";")
sapply(PI, class) 
PI$Mat_Orig <- as.factor(as.integer(PI$Mat_Orig))  # Convert one variable to character
sapply(PI, class)
head(PI)
PI.Camada_A = subset(PI, Camada == "A")
str(PI)
summary(PI)
row.has.na <- apply(PI, 1, function(x){any(is.na(x))}) # Verifica linhas c/ NAs
sum(row.has.na)
PI <- PI[complete.cases(PI),]
colnames(PI)
PI_PRO <- PI
colnames(PI_PRO)
names(PI_PRO)[4:14] <- c("SYSI_1", "SYSI_2", "SYSI_3", "SYSI_4", 
                         "SYSI_5" ,"SYSI_7", "DEM", "VDTCN", "LSFAC", 
                         "SLOPE", "TWI")
colnames(PI_PRO)
#Descriptive stats
summary(PI_PRO)
sd(PI_PRO$Silte.gkg)
sd(PI_PRO$Argila.gkg)
sd(PI_PRO$Areia.gkg)
sd(PI_PRO$MO.gkg)
sd(PI_PRO$CTC.mmolkg)
sd(PI_PRO$SYSI_1)
sd(PI_PRO$SYSI_2)
sd(PI_PRO$SYSI_3)
sd(PI_PRO$SYSI_4)
sd(PI_PRO$SYSI_5)
sd(PI_PRO$SYSI_7)
sd(PI_PRO$SF)

# Exporting BD
write.csv(PI_PRO, "./PI_SemNaN_Okay.csv", sep = ",")

# Importing RASTERs
setwd("F:/Doutorado/Producoes/pereira barreto/Santa_Adelia_novo/raster")

mosaico = list.files(paste(getwd()), pattern = "SYSI_STADELIA_32722.tif")
mosaico
mosaico[[1]]; summary(mosaico[[1]])
str(mosaico)
class(mosaico)
colnames(PI_PRO)

#Spliting band of SYSI (Synthetic Soil Image)
SYSI_1 = raster(mosaico[[1]], band = 1)
SYSI_2 = raster(mosaico[[1]], band = 2)
SYSI_3 = raster(mosaico[[1]], band = 3)
SYSI_4 = raster(mosaico[[1]], band = 4)
SYSI_5 = raster(mosaico[[1]], band = 5)
SYSI_7 = raster(mosaico[[1]], band = 6)

#Export splited rasters

#writeRaster(SYSI_1, "./SYSI_1", format = "GTiff", datatype = "FLT4S", overwrite = T)
#writeRaster(SYSI_2, "./SYSI_2", format = "GTiff", datatype = "FLT4S", overwrite = T)
#writeRaster(SYSI_3, "./SYSI_3", format = "GTiff", datatype = "FLT4S", overwrite = T)
#writeRaster(SYSI_4, "./SYSI_4", format = "GTiff", datatype = "FLT4S", overwrite = T)
#writeRaster(SYSI_5, "./SYSI_5", format = "GTiff", datatype = "FLT4S", overwrite = T)
#writeRaster(SYSI_7, "./SYSI_7", format = "GTiff", datatype = "FLT4S", overwrite = T)

plot(SYSI_7)
NAvalue(SYSI_1) <- 0
NAvalue(SYSI_2) <- 0
NAvalue(SYSI_3) <- 0
NAvalue(SYSI_4) <- 0
NAvalue(SYSI_5) <- 0
NAvalue(SYSI_7) <- 0
plot(SYSI_7)

#Leitura do DEM.tif
DEM = list.files(paste(getwd()), pattern = "SRTM_ELEVACAO.tif$")
DEM <- raster(DEM)
str(DEM)
class(DEM)
extent(DEM)
extent(SYSI_7)
DEM <- resample(DEM, SYSI_7, method = "ngb")
par(mfrow = c(1,1))
plot(DEM)
dev.off()
colnames(PI_PRO)

#Leitura do VDTCN.tif
VDTC = list.files(paste(getwd()), pattern = "SRTM_VDTCN.tif$")
VDTC <- raster(VDTC)
str(VDTC)
class(VDTC)
extent(VDTC)
extent(SYSI_7)
VDTC <- resample(VDTC, SYSI_7, method = "ngb")
par(mfrow = c(1,1))
plot(VDTC)
dev.off()

#Leitura do LSF.tif
LSF = list.files(paste(getwd()), pattern = "SRTM_LSFACTOR.tif$")
LSF <- raster(LSF)
str(LSF)
class(LSF)
extent(LSF)
extent(SYSI_7)
LSF <- resample(LSF, SYSI_7, method = "ngb")
par(mfrow = c(1,1))
plot(LSF)
dev.off()

#Leitura do SLOPE.tif
SLOPE = list.files(paste(getwd()), pattern = "SRTM_SLOPEPERC.tif$")
SLOPE <- raster(SLOPE)
str(SLOPE)
class(SLOPE)
extent(SLOPE)
extent(SYSI_7)
SLOPE <- resample(SLOPE, SYSI_7, method = "ngb")
par(mfrow = c(1,1))
plot(SLOPE)
dev.off()

#Leitura do TWI.tif
TWI = list.files(paste(getwd()), pattern = "SRTM_TWI.tif$")
TWI <- raster(TWI)
str(TWI)
class(TWI)
extent(TWI)
extent(SYSI_7)
TWI <- resample(TWI, SYSI_7, method = "ngb")
par(mfrow = c(1,1))
plot(TWI)
dev.off()

covs <- stack(SYSI_1, SYSI_2, SYSI_3, SYSI_4, SYSI_5, SYSI_7, 
              DEM, VDTC, LSF, SLOPE,TWI)
names(covs) <- c("SYSI_1", "SYSI_2", "SYSI_3", "SYSI_4", "SYSI_5" ,"SYSI_7",
                        "DEM", "VDTCN", "LSFAC", "SLOPE","TWI")
covs
colnames(PI_PRO)
CovStack = stack(SYSI_1, SYSI_2, SYSI_3, SYSI_4, SYSI_5, SYSI_7, 
                 DEM, VDTC, LSF, SLOPE,TWI)
tempD <- data.frame(cellNos = seq(1:ncell(CovStack)))
vals <- as.data.frame(getValues(CovStack))
tempD <- cbind(tempD, vals)
tempD <- tempD[complete.cases(tempD), ]
cellNos <- c(tempD$cellNos)
gXY <- data.frame(xyFromCell(CovStack, cellNos, spatial = F))
tempD <- cbind(gXY, tempD)
str(tempD)
names(tempD)[4:14] <- c("SYSI_1", "SYSI_2", "SYSI_3", "SYSI_4", "SYSI_5" ,"SYSI_7",
                        "DEM", "VDTCN", "LSFAC", "SLOPE","TWI")
crs(CovStack)
str(tempD)
summary(tempD)
tempD <- subset(tempD, SYSI_1>0 & SYSI_2>0 & SYSI_3>0 & SYSI_4>0 & SYSI_5>0 & SYSI_7>0
                & DEM>0 & VDTC>0 & LSF>0 & SLOPE>0 & TWI>0)
summary(tempD)

##----- Exploratory Analysis 
# Layer A
colnames(PI_PRO)
str(PI_PRO)
corrplot(cor(PI_PRO[3:15]), method = "num", type = "upper")
# Significance test with correlogram
PI.cor <- cor(PI_PRO[3:15])
res1 <- cor.mtest(PI_PRO[3:15], conf.level = .95, method = "pearson") # cor.mtest - Produces p-values and confidence intervals for each pair of input features
res2 <- cor.mtest(PI_PRO[3:15], conf.level = .99)
corrplot(PI.cor, p.mat = res1$p, sig.level = .05)
corrplot(PI.cor, p.mat = res1$p, insig = "label_sig", pch.col = "gray", sig.level = .05,
         method = "number", type = "upper")

# Data-splitting (Total of 946 points)
str(PI_PRO)
colnames(PI_PRO)
PI_PRO.ind <- createDataPartition(PI_PRO$Mat_Orig, p = .75, list = F, times = 1)
head(PI_PRO.ind)
PI.train <- PI_PRO[PI_PRO.ind,]
str(PI.train)
View(PI.train)
PI.val <- PI_PRO[-PI_PRO.ind,]
str(PI.val)
View(PI.val)
# Exporting BD. Training and validation
write.csv(PI.train, "./PI.Train.csv")
write.csv(PI.val, "./PI.Val.csv")

# Stats more precise
# Layer A
# Argila ----
colnames(PI.train)
hist(PI.train$Mat_Orig)
skewness(PI.train$Argila.gkg) # Okay! Bcuz the skewness is -0.015. This value implies that the distribution of the data is "slightly" skewed or negatively skewed. Skewed to left(negative) and slightly(close to zero)
kurtosis(PI.train$Argila.gkg)
boxplot(PI.train$Argila.gkg)
str(PI.train)
#PI.train$Log.Argila <- log(PI.train$Argila.gkg)
hist(PI.train$Log.Argila)
skewness(PI.train$Log.Argila)
kurtosis(PI.train$Log.Argila)

#--Digital Mapping ~~~~~~~~~~~~~
ctrl <- trainControl(method = "cv", number = 10)

###########################Clay ##########################
# Gerando Mapa - RF (Random Forest)
colnames(PI.train)
Mat_Orig_RF <- train(x = PI.train[, c(4:14)], y = PI.train$Mat_Orig, method = "rf", 
                      trControl = ctrl)
Mat_Orig_RF
print(Mat_Orig_RF)
varImpPlot(Mat_Orig_RF$finalModel, scale=T)
importance(Mat_Orig_RF$finalModel)

map_Mat_Orig_RF <- predict(Mat_Orig_RF, newdata = tempD)
map_Mat_Orig_RF<- raster::predict(covs, Mat_Orig_RF)
plot(map_Mat_Orig_RF)
map_Mat_Orig_RF <- cbind(data.frame(tempD[, c("x", "y")]), map_Mat_Orig_RF)
writeRaster(map_Mat_Orig_RF, "Mat_Orig_RF.tif", format = "GTiff", 
            datatype = "FLT4S", overwrite = T)

summary(map_Mat_Orig_RF)
map_Mat_Orig_RF.Raster <- rasterFromXYZ(as.data.frame(map_Mat_Orig_RF[, 1:3]))
plot(map_Mat_Orig_RF.Raster, main = "XXX")
crs(map_Mat_Orig_RF.Raster) <- "+init=epsg:4326"
writeRaster(map_Mat_Orig_RF.Raster, "Clay_RF_proj.tif", format = "GTiff", 
            datatype = "FLT4S", overwrite = T)

# Gerando Mapa - Cubist
library(kernlab)
ArgilaFit.Cubist.A <- train(x=PI.train[,c(8:13)], y=PI.train$Argila.gkg, method = "svmLinear3", 
                          trControl = ctrl)
print(ArgilaFit.Cubist.A)
map.Argila.Cubist.A <- predict(ArgilaFit.Cubist.A, newdata = tempD)
map.Argila.Cubist.A <- cbind(data.frame(tempD[, c("x", "y")]), map.Argila.Cubist.A)
summary(map.Argila.Cubist.A)
map.Argila.Cubist.A <- subset(map.Argila.Cubist.A, map.Argila.Cubist.A <= 1000)
map.Argila.Cubist.Raster.A <- rasterFromXYZ(as.data.frame(map.Argila.Cubist.A[, 1:3]))
#plot(map.Argila.Cubist.Raster.A, main = "Argila map")
crs(map.Argila.Cubist.Raster.A) <- "+init=epsg:4326"
writeRaster(map.Argila.Cubist.Raster.A, "Clay_cubist_Proj.tif", format = "GTiff", 
            datatype = "FLT4S", overwrite = T)
memory.limit()
memory.limit(size=56000)
# Gerando Mapa - SVM (Support Vector Machine)

ArgilaFit.svm2.A <- train(x=PI.train[,c(8:13)], y=PI.train$Argila.gkg, method = "svmLinear", 
                            trControl = ctrl)

colnames(PI.train)
tuneResult <- tune(svm, Argila.gkg~SYSI_1 + SYSI_2 + SYSI_3 + SYSI_4 + SYSI_5 + SYSI_7, data = PI.train,
                   ranges = list(epsilon = seq(0.1, 0.2, 0.02), cost = c(5, 7, 15, 20)))
plot(tuneResult)
ArgilaFit.SVM.A <- tuneResult$best.model
print(ArgilaFit.SVM.A)
map.Argila.SVM.A <- predict(ArgilaFit.SVM.A, newdata = tempD) 
map.Argila.SVM.A <- cbind(data.frame(tempD[, c("x", "y")]), map.Argila.SVM.A)
summary(map.Argila.SVM.A)
map.Argila.SVM.A <- subset(map.Argila.SVM.A, map.Argila.SVM.A >= 0)
map.Argila.SVM.Raster.A <- rasterFromXYZ(as.data.frame(map.Argila.SVM.A[, 1:3]))
#plot(map.Argila.SVM.Raster.A, main = "Argila map")
crs(map.Argila.SVM.Raster.A) <- "+init=epsg:4326"
writeRaster(map.Argila.SVM.Raster.A, "Clay_SVM_Proj.tif", format = "GTiff", 
            datatype = "FLT4S", overwrite = T)

## Validation of the model
PI.Val <- read.csv("Pts_Val.csv", header = T, sep = ";")
str(PI.Val)



PI.Val$ARG.PE.RF  <- PI.Val$Argila_gkg - PI.Val$Clay_RF_pr
PI.Val$ARG.PE.Cub <- PI.Val$Argila_gkg - PI.Val$Clay_cubis
PI.Val$ARG.PE.SVM <- PI.Val$Argila_gkg - PI.Val$Clay_SVM_P
summary(PI.val)

PI.VAL.NA <- apply(PI.Val, 1, function(x){any(is.na(x))}) # Verifica linhas c/ NAs
sum(PI.VAL.NA)
PI.val.ok <- PI.Val[complete.cases(PI.Val),]
summary(PI.val.ok)

sd(PI.val.ok$ARG.PE.SVM)
sd(PI.val.ok$ARG.PE.RF)
sd(PI.val.ok$ARG.PE.Cub)

PI.val.ok$

par(mfrow=c(1,1))
plot(PI.val.ok$Clay_RF_pr, PI.val.ok$Argila_gkg, main = "RF", xlab="Predicted (g/kg)", ylab="Observed (g/kg)",
     xlim=c(0,700), ylim=c(0,700))
abline(0,1, lty=2, col="black")
abline(lm(PI.val.ok$Clay_RF_pr ~ PI.val.ok$Argila_gkg), col="red", lty=2)
goof(observed = PI.val.ok$Argila_gkg, predicted = PI.val.ok$Clay_RF_pr, type = "DSM")

plot(PI.val.ok$Clay_SVM_P, PI.val.ok$Argila_gkg, main = "SVM", xlab="Predicted (g/kg)", ylab="Observed (g/kg)",
     xlim=c(0,700), ylim=c(0,700))
abline(0,1, lty=2, col="black")
abline(lm(PI.val.ok$Clay_SVM_P ~ PI.val.ok$Argila_gkg), col="red", lty=2)
goof(observed = PI.val.ok$Argila_gkg, predicted = PI.val.ok$Clay_SVM_P, type = "DSM")

plot(PI.val.ok$Clay_cubis, PI.val.ok$Argila_gkg, main = "Cubist", xlab="Predicted (g/kg)", ylab="Observed (g/kg)",
     xlim=c(0,700), ylim=c(0,700))
abline(0,1, lty=2, col="black")
abline(lm(PI.val.ok$Clay_cubis ~ PI.val.ok$Argila_gkg), col="red", lty=2)
goof(observed = PI.val.ok$Argila_gkg, predicted = PI.val.ok$Clay_cubis, type = "DSM")

dev.off()
########################## Silt #####################################
# Gerando Mapa - RF (Random Forest)
colnames(PI.train)
SiltFit.RF.A <- train(x = PI.train[, c(8:13)], y = PI.train$Silte.gkg, method = "rf", 
                      trControl = ctrl)
SiltFit.RF.A
print(SiltFit.RF.A)
varImpPlot(SiltFit.RF.A$finalModel, scale=T)
importance(SiltFit.RF.A$finalModel)
map.Silt.RF.A <- predict(SiltFit.RF.A, newdata = tempD)
map.Silt.RF.A <- cbind(data.frame(tempD[, c("x", "y")]), map.Silt.RF.A)
summary(map.Silt.RF.A)
map.Silt.RF.Raster.A <- rasterFromXYZ(as.data.frame(map.Silt.RF.A[, 1:3]))
#plot(map.Silt.RF.Raster.A, main = "Silt map")
crs(map.Silt.RF.Raster.A) <- "+init=epsg:4326"
setwd("F:/Doutorado/Producoes/Artigo_Erosao/DSM/Silt")
writeRaster(map.Silt.RF.Raster.A, "Silt_RF_proj.tif", format = "GTiff", 
            datatype = "FLT4S", overwrite = T)

# Gerando Mapa - Cubist
#library(kernlab)
SiltFit.Cubist.A <- train(x=PI.train[,c(8:13)], y=PI.train$Silte.gkg, method = "cubist", 
                          trControl = ctrl)
print(SiltFit.Cubist.A)
map.Silt.Cubist.A <- predict(SiltFit.Cubist.A, newdata = tempD)
map.Silt.Cubist.A <- cbind(data.frame(tempD[, c("x", "y")]), map.Silt.Cubist.A)
summary(map.Silt.Cubist.A)
map.Silt.Cubist.A <- subset(map.Silt.Cubist.A, map.Silt.Cubist.A <= 1000)
map.Silt.Cubist.Raster.A <- rasterFromXYZ(as.data.frame(map.Silt.Cubist.A[, 1:3]))
plot(map.Silt.Cubist.Raster.A, main = "Silt map")
crs(map.Silt.Cubist.Raster.A) <- "+init=epsg:4326"
writeRaster(map.Silt.Cubist.Raster.A, "Silt_cubist_proj.tif", format = "GTiff", 
            datatype = "FLT4S", overwrite = T)
memory.limit()
memory.limit(size=56000)
# Gerando Mapa - SVM (Support Vector Machine)

#SiltFit.svm.A2 <- train(x=PI.train[,c(8:13)], y=PI.train$Silte.gkg, method = "svmLinear", trControl = ctrl)

colnames(PI.train)
tuneResult <- tune(svm, Silte.gkg~SYSI_1 + SYSI_2 + SYSI_3 + SYSI_4 + SYSI_5 + SYSI_7, data = PI.train,
                   ranges = list(epsilon = seq(0.1, 0.2, 0.02), cost = c(5, 7, 15, 20)))
plot(tuneResult)
SiltFit.SVM.A <- tuneResult$best.model
print(SiltFit.SVM.A)
map.Silt.SVM.A <- predict(SiltFit.SVM.A, newdata = tempD) 
map.Silt.SVM.A <- cbind(data.frame(tempD[, c("x", "y")]), map.Silt.SVM.A)
summary(map.Silt.SVM.A)
map.Silt.SVM.A <- subset(map.Silt.SVM.A, map.Silt.SVM.A >= 0)
map.Silt.SVM.Raster.A <- rasterFromXYZ(as.data.frame(map.Silt.SVM.A[, 1:3]))
plot(map.Silt.SVM.Raster.A, main = "Silt map")
crs(map.Silt.SVM.Raster.A) <- "+init=epsg:4326"
writeRaster(map.Silt.SVM.Raster.A, "Silt_SVM_proj.tif", format = "GTiff", 
            datatype = "FLT4S", overwrite = T)

## Validation of the model
PI.Val2 <- read.csv("PI_Val_silt.csv", header = T, sep = ";")
str(PI.Val2)
colnames(PI.Val2)

PI.Val2$SILT.PE.RF  <- PI.Val2$Silte_gkg - PI.Val2$Silt_RF_pr
PI.Val2$SILT.PE.Cub <- PI.Val2$Silte_gkg - PI.Val2$Silt_cubis
PI.Val2$SILT.PE.SVM <- PI.Val2$Silte_gkg - PI.Val2$Silt_SVM_p
summary(PI.Val2)

PI.VAL.NA <- apply(PI.Val2, 1, function(x){any(is.na(x))}) # Verifica linhas c/ NAs
sum(PI.VAL.NA)
PI.val.ok <- PI.Val2[complete.cases(PI.Val2),]
summary(PI.val.ok)

sd(PI.val.ok$ARG.PE.SVM)
sd(PI.val.ok$ARG.PE.RF)
sd(PI.val.ok$ARG.PE.Cub)



par(mfrow=c(1,1))
plot(PI.val.ok$Silt_RF_pr, PI.val.ok$Silte_gkg, main = "RF", xlab="Predicted (g/kg)", ylab="Observed (g/kg)",
     xlim=c(0,700), ylim=c(0,700))
abline(0,1, lty=2, col="black")
abline(lm(PI.val.ok$Silt_RF_pr ~ PI.val.ok$Silte_gkg), col="red", lty=2)
goof(observed = PI.val.ok$Silte_gkg, predicted = PI.val.ok$Silt_RF_pr, type = "DSM")

plot(PI.val.ok$Silt_SVM_p, PI.val.ok$Silte_gkg, main = "SVM", xlab="Predicted (g/kg)", ylab="Observed (g/kg)",
     xlim=c(0,700), ylim=c(0,700))
abline(0,1, lty=2, col="black")
abline(lm(PI.val.ok$Silt_SVM_p ~ PI.val.ok$Silte_gkg), col="red", lty=2)
goof(observed = PI.val.ok$Silte_gkg, predicted = PI.val.ok$Silt_SVM_p, type = "DSM")

plot(PI.val.ok$Sand_cubis, PI.val.ok$Silte_gkg, main = "Cubist", xlab="Predicted (g/kg)", ylab="Observed (g/kg)",
     xlim=c(0,700), ylim=c(0,700))
abline(0,1, lty=2, col="black")
abline(lm(PI.val.ok$Sand_cubis ~ PI.val.ok$Silte_gkg), col="red", lty=2)
goof(observed = PI.val.ok$Silte_gkg, predicted = PI.val.ok$Sand_cubis, type = "DSM")

dev.off()

####################################################################
########################## SAND #####################################
# Gerando Mapa - RF (Random Forest)
colnames(PI.train)
SandFit.RF.A <- train(x = PI.train[, c(8:13)], y = PI.train$Areia.gkg, method = "rf", 
                        trControl = ctrl)
SandFit.RF.A
print(SandFit.RF.A)
varImpPlot(SandFit.RF.A$finalModel, scale=T)
importance(SandFit.RF.A$finalModel)
map.Sand.RF.A <- predict(SandFit.RF.A, newdata = tempD)
map.Sand.RF.A <- cbind(data.frame(tempD[, c("x", "y")]), map.Sand.RF.A)
summary(map.Sand.RF.A)
map.Sand.RF.Raster.A <- rasterFromXYZ(as.data.frame(map.Sand.RF.A[, 1:3]))
#plot(map.Sand.RF.Raster.A, main = "Sand map")
crs(map.Sand.RF.Raster.A) <- "+init=epsg:4326"
setwd("F:/Doutorado/Producoes/Artigo_Erosao/DSM/Sand")
writeRaster(map.Sand.RF.Raster.A, "Sand_RF_Proj.tif", format = "GTiff", 
            datatype = "FLT4S", overwrite = T)

# Gerando Mapa - Cubist
#library(kernlab)
SandFit.Cubist.A <- train(x=PI.train[,c(8:13)], y=PI.train$Areia.gkg, method = "cubist", 
                            trControl = ctrl)
print(SandFit.Cubist.A)
map.Sand.Cubist.A <- predict(SandFit.Cubist.A, newdata = tempD)
map.Sand.Cubist.A <- cbind(data.frame(tempD[, c("x", "y")]), map.Sand.Cubist.A)
summary(map.Sand.Cubist.A)
map.Sand.Cubist.A <- subset(map.Sand.Cubist.A, map.Sand.Cubist.A <= 1000)
map.Sand.Cubist.Raster.A <- rasterFromXYZ(as.data.frame(map.Sand.Cubist.A[, 1:3]))
#plot(map.Sand.Cubist.Raster.A, main = "Sand map")
crs(map.Sand.Cubist.Raster.A) <- "+init=epsg:4326"
writeRaster(map.Sand.Cubist.Raster.A, "Sand_cubist_Proj.tif", format = "GTiff", 
            datatype = "FLT4S", overwrite = T)
memory.limit()
memory.limit(size=56000)
# Gerando Mapa - SVM (Support Vector Machine)

#SandFit.svm.A2 <- train(x=PI.train[,c(8:13)], y=PI.train$Areia.gkg, method = "svmLinear", trControl = ctrl)

colnames(PI.train)
tuneResult <- tune(svm, Areia.gkg~SYSI_1 + SYSI_2 + SYSI_3 + SYSI_4 + SYSI_5 + SYSI_7, data = PI.train,
                   ranges = list(epsilon = seq(0.1, 0.2, 0.02), cost = c(5, 7, 15, 20)))
plot(tuneResult)
SandFit.SVM.A <- tuneResult$best.model
print(SandFit.SVM.A)
map.Sand.SVM.A <- predict(SandFit.SVM.A, newdata = tempD) 
map.Sand.SVM.A <- cbind(data.frame(tempD[, c("x", "y")]), map.Sand.SVM.A)
summary(map.Sand.SVM.A)
map.Sand.SVM.A <- subset(map.Sand.SVM.A, map.Sand.SVM.A >= 0)
map.Sand.SVM.Raster.A <- rasterFromXYZ(as.data.frame(map.Sand.SVM.A[, 1:3]))
#plot(map.Sand.SVM.Raster.A, main = "Sand map")
crs(map.Sand.SVM.Raster.A) <- "+init=epsg:4326"
writeRaster(map.Sand.SVM.Raster.A, "Sand_SVM_Proj.tif", format = "GTiff", 
            datatype = "FLT4S", overwrite = T)

## Validation of the model
PI.Val2 <- read.csv("PI_Val_sand.csv", header = T, sep = ";")
str(PI.Val2)
colnames(PI.Val2)

PI.Val2$ARE.PE.RF  <- PI.Val2$Areia_gkg - PI.Val2$Sand_RF_Pr
PI.Val2$ARE.PE.Cub <- PI.Val2$Areia_gkg - PI.Val2$Sand_cubis
PI.Val2$ARE.PE.SVM <- PI.Val2$Areia_gkg - PI.Val2$Sand_SVM_P
summary(PI.Val2)

PI.VAL.NA <- apply(PI.Val2, 1, function(x){any(is.na(x))}) # Verifica linhas c/ NAs
sum(PI.VAL.NA)
PI.val.ok <- PI.Val2[complete.cases(PI.Val2),]
summary(PI.val.ok)

sd(PI.val.ok$ARG.PE.SVM)
sd(PI.val.ok$ARG.PE.RF)
sd(PI.val.ok$ARG.PE.Cub)


  
    par(mfrow=c(1,1))
plot(PI.val.ok$Sand_RF_Pr, PI.val.ok$Areia_gkg, main = "RF", xlab="Predicted (g/kg)", ylab="Observed (g/kg)",
     xlim=c(0,700), ylim=c(0,700))
abline(0,1, lty=2, col="black")
abline(lm(PI.val.ok$Sand_RF_Pr ~ PI.val.ok$Areia_gkg), col="red", lty=2)
goof(observed = PI.val.ok$Areia_gkg, predicted = PI.val.ok$Sand_RF_Pr, type = "DSM")

plot(PI.val.ok$Sand_SVM_P, PI.val.ok$Areia_gkg, main = "SVM", xlab="Predicted (g/kg)", ylab="Observed (g/kg)",
     xlim=c(0,700), ylim=c(0,700))
abline(0,1, lty=2, col="black")
abline(lm(PI.val.ok$Sand_SVM_P ~ PI.val.ok$Areia_gkg), col="red", lty=2)
goof(observed = PI.val.ok$Areia_gkg, predicted = PI.val.ok$Sand_SVM_P, type = "DSM")

plot(PI.val.ok$Sand_cubis, PI.val.ok$Areia_gkg, main = "Cubist", xlab="Predicted (g/kg)", ylab="Observed (g/kg)",
     xlim=c(0,700), ylim=c(0,700))
abline(0,1, lty=2, col="black")
abline(lm(PI.val.ok$Sand_cubis ~ PI.val.ok$Areia_gkg), col="red", lty=2)
goof(observed = PI.val.ok$Areia_gkg, predicted = PI.val.ok$Sand_cubis, type = "DSM")

dev.off()

####################################################################
#Additional model evaluation
#DB.val <- read.csv("DB.Val_Okay_Preditos.csv", header = T, sep = ",")
str(PI.val.ok)
#Creating a new data.frame
modRF <- data.frame(obs = PI.val.ok$Argila.gkg, mod = PI.val.ok$Arg.RF, model = "RF")
modCubist <- data.frame(obs = PI.val.ok$Argila.gkg, mod = PI.val.ok$Arg.Cub, model = "Cubist")
modSVM <- data.frame(obs = PI.val.ok$Argila.gkg, mod = PI.val.ok$Arg.SVM, model = "SVM")
modData <- rbind(modRF, modCubist, modSVM)
summary(modData)

if (!require(openair)){install.packages("openair");require(openair)}
modsts <- modStats(modData, obs = "obs", mod = "mod", type = "model")
write.table(modsts, file="ModelEvaluation.txt", sep = ",", quote = FALSE, row.names = F)

TaylorDiagram(modData, obs = "obs", mod = "mod", group = "model", cols = c("green", "yellow", "red"),
              cor.col = 'darkblue', rms.col = 'black')

conditionalQuantile(modData, obs = "obs", mod = "mod", type = "model")

########################## CTC ###############################
colnames(PI.train)
hist(PI.train$CTC.mmolkg)
skewness(PI.train$CTC.mmolkg) # Okay! Bcuz the skewness is -0.015. This value implies that the distribution of the data is "slightly" skewed or negatively skewed. Skewed to left(negative) and slightly(close to zero)
kurtosis(PI.train$CTC.mmolkg)
boxplot(PI.train$CTC.mmolkg)
str(PI.train)
PI.train$Log.CTC <- log(PI.train$CTC.mmolkg)
hist(PI.train$Log.CTC)
skewness(PI.train$Log.CTC)
kurtosis(PI.train$Log.CTC)

#--Digital Mapping ~~~~~~~~~~~~~
ctrl <- trainControl(method = "repeatedcv", number = 3, repeats = 3)

# Gerando Mapa - RF (Random Forest)
colnames(PI.train)
CTCFit.RF.A <- train(x = PI.train[, c(8:13)], y = PI.train$Log.CTC, method = "rf", 
                        trControl = ctrl)
CTCFit.RF.A
print(CTCFit.RF.A)
varImpPlot(CTCFit.RF.A$finalModel, scale=T)
importance(CTCFit.RF.A$finalModel)
map.CTC.RF.A <- predict(CTCFit.RF.A, newdata = tempD)
map.CTC.RF.A <- expm1(map.CTC.RF.A)   #é o antilog
map.CTC.RF.A <- cbind(data.frame(tempD[, c("x", "y")]), map.CTC.RF.A)
summary(map.CTC.RF.A)
map.CTC.RF.Raster.A <- rasterFromXYZ(as.data.frame(map.CTC.RF.A[, 1:3]))
plot(map.CTC.RF.Raster.A, main = "CTC map")
crs(map.CTC.RF.Raster.A) <- "+init=epsg:4326"
writeRaster(map.CTC.RF.Raster.A, "Mapa_CTC_RF.tif", format = "GTiff", 
            datatype = "FLT4S", overwrite = T)

# Gerando Mapa - Cubist
CTCFit.Cubist.A <- train(x=PI.train[,c(8:13)], y=PI.train$Log.CTC, method = "cubist", 
                            trControl = ctrl)
print(CTCFit.Cubist.A)
map.CTC.Cubist.A <- predict(CTCFit.Cubist.A, newdata = tempD)
map.CTC.Cubist.A <- expm1(map.CTC.Cubist.A)
map.CTC.Cubist.A <- cbind(data.frame(tempD[, c("x", "y")]), map.CTC.Cubist.A)
summary(map.CTC.Cubist.A)
map.CTC.Cubist.A <- subset(map.CTC.Cubist.A, map.CTC.Cubist.A <= 1000)
map.CTC.Cubist.Raster.A <- rasterFromXYZ(as.data.frame(map.CTC.Cubist.A[, 1:3]))
plot(map.CTC.Cubist.Raster.A, main = "CTC map")
crs(map.CTC.Cubist.Raster.A) <- "+init=epsg:4326"
writeRaster(map.CTC.Cubist.Raster.A, "Map_CTC_mmolkg_Cubist.tif", format = "GTiff", 
            datatype = "FLT4S", overwrite = T)

# Gerando Mapa - SVM (Support Vector Machine)
colnames(PI.train)
tuneResult <- tune(svm, Log.CTC~SYSI_1 + SYSI_2 + SYSI_3 + SYSI_4 + SYSI_5 + SYSI_7, data = PI.train,
                   ranges = list(epsilon = seq(0.1, 0.2, 0.02), cost = c(5, 7, 15, 20)))
plot(tuneResult)
CTCFit.SVM.A <- tuneResult$best.model
print(CTCFit.SVM.A)
map.CTC.SVM.A <- predict(CTCFit.SVM.A, newdata = tempD) 
map.CTC.SVM.A <- expm1(map.CTC.SVM.A)
map.CTC.SVM.A <- cbind(data.frame(tempD[, c("x", "y")]), map.CTC.SVM.A)
summary(map.CTC.SVM.A)
map.CTC.SVM.A <- subset(map.CTC.SVM.A, map.CTC.SVM.A >= 0)
map.CTC.SVM.Raster.A <- rasterFromXYZ(as.data.frame(map.CTC.SVM.A[, 1:3]))
plot(map.CTC.SVM.Raster.A, main = "CTC map")
crs(map.CTC.SVM.Raster.A) <- "+init=epsg:4326"
writeRaster(map.CTC.SVM.Raster.A, "Mapa_CTC_mmolkg_SVM.tif", format = "GTiff", 
            datatype = "FLT4S", overwrite = T)

## Validation of the model
PI.val <- read.csv("PI.val_preditos_CTC.csv", header = T, sep = ";")
str(PI.val)

PI.val$CTC.PE.RF <- PI.val$CTC.mmolkg - PI.val$CTC.RF 
PI.val$CTC.PE.Cub <- PI.val$CTC.mmolkg - PI.val$CTC.Cub 
PI.val$CTC.PE.SVM <- PI.val$CTC.mmolkg - PI.val$CTC.SVM 
summary(PI.val)

sd(PI.val$CTC.PE.SVM)
sd(PI.val$CTC.PE.RF)
sd(PI.val$CTC.PE.Cub)

par(mfrow=c(1,1))
plot(PI.val$CTC.RF, PI.val$CTC.mmolkg, main = "RF", xlab="Predicted (g/kg)", ylab="Observed (g/kg)",
     xlim=c(0,200), ylim=c(0,350))
abline(0,1, lty=2, col="black")
abline(lm(PI.val$CTC.RF ~ PI.val$CTC.mmolkg), col="red", lty=2)
goof(observed = PI.val$CTC.mmolkg, predicted = PI.val$CTC.RF, type = "DSM")

plot(PI.val$CTC.SVM, PI.val$CTC.mmolkg, main = "SVM", xlab="Predicted (g/kg)", ylab="Observed (g/kg)",
     xlim=c(0,200), ylim=c(0,350))
abline(0,1, lty=2, col="black")
abline(lm(PI.val$CTC.SVM ~ PI.val$CTC.mmolkg), col="red", lty=2)
goof(observed = PI.val$CTC.mmolkg, predicted = PI.val$CTC.SVM, type = "DSM")

plot(PI.val$CTC.Cub, PI.val$CTC.mmolkg, main = "Cubist", xlab="Predicted (g/kg)", ylab="Observed (g/kg)",
     xlim=c(0,200), ylim=c(0,350))
abline(0,1, lty=2, col="black")
abline(lm(PI.val$CTC.Cub ~ PI.val$CTC.mmolkg), col="red", lty=2)
goof(observed = PI.val$CTC.mmolkg, predicted = PI.val$CTC.Cub, type = "DSM")

dev.off()

#Additional model evaluation
#DB.val <- read.csv("DB.Val_Okay_Preditos.csv", header = T, sep = ",")
str(PI.val)
#Creating a new data.frame
modRF <- data.frame(obs = PI.val$CTC.mmolkg, mod = PI.val$CTC.RF, model = "RF")
modCubist <- data.frame(obs = PI.val$CTC.mmolkg, mod = PI.val$CTC.Cub, model = "Cubist")
modSVM <- data.frame(obs = PI.val$CTC.mmolkg, mod = PI.val$CTC.SVM, model = "SVM")
modData <- rbind(modRF, modCubist, modSVM)
summary(modData)

if (!require(openair)){install.packages("openair");require(openair)}
modsts <- modStats(modData, obs = "obs", mod = "mod", type = "model")
write.table(modsts, file="ModelEvaluation.txt", sep = ",", quote = FALSE, row.names = F)

TaylorDiagram(modData, obs = "obs", mod = "mod", group = "model", cols = c("green", "yellow", "red"),
              cor.col = 'darkblue', rms.col = 'black')

conditionalQuantile(modData, obs = "obs", mod = "mod", type = "model")


############################## OM ####################################
colnames(PI.train)
hist(PI.train$MO.gkg)
skewness(PI.train$MO.gkg) # Okay! Bcuz the skewness is -0.015. This value implies that the distribution of the data is "slightly" skewed or negatively skewed. Skewed to left(negative) and slightly(close to zero)
kurtosis(PI.train$MO.gkg)
boxplot(PI.train$MO.gkg)
str(PI.train)
PI.train$Log.MO <- log(PI.train$MO.gkg)
hist(PI.train$Log.MO)
skewness(PI.train$Log.MO)
kurtosis(PI.train$Log.MO)

#--Digital Mapping ~~~~~~~~~~~~~
ctrl <- trainControl(method = "repeatedcv", number = 3, repeats = 3)

# Gerando Mapa - RF (Random Forest)
colnames(PI.train)
MOFit.RF.A <- train(x=PI.train[,c(8:15)], y=PI.train$MO.gkg, method = "rf", 
                    trControl = ctrl)
MOFit.RF.A
print(MOFit.RF.A)
varImpPlot(MOFit.RF.A$finalModel, scale=T)
importance(MOFit.RF.A$finalModel)
map.MO.RF.A <- predict(MOFit.RF.A, newdata = tempD)
map.MO.RF.A <- cbind(data.frame(tempD[, c("x", "y")]), map.MO.RF.A)
summary(map.MO.RF.A)
map.MO.RF.Raster.A <- rasterFromXYZ(as.data.frame(map.MO.RF.A[, 1:3]))
plot(map.MO.RF.Raster.A, main = "MO map")
crs(map.MO.RF.Raster.A) <- "+init=epsg:4326"
setwd("F:/Doutorado/Producoes/Artigo_Erosao/DSM/OM")
writeRaster(map.MO.RF.Raster.A, "Map_MO_RF_Proj.tif", format = "GTiff", 
            datatype = "FLT4S", overwrite = T)

# Gerando Mapa - Cubist
MOFit.Cubist.A <- train(x=PI.train[,c(8:13)], y=PI.train$MO.gkg, method = "cubist", 
                            trControl = ctrl)
print(MOFit.Cubist.A)
map.MO.Cubist.A <- predict(MOFit.Cubist.A, newdata = tempD)
map.MO.Cubist.A <- cbind(data.frame(tempD[, c("x", "y")]), map.MO.Cubist.A)
summary(map.MO.Cubist.A)
map.MO.Cubist.A <- subset(map.MO.Cubist.A, map.MO.Cubist.A <= 1000)
map.MO.Cubist.Raster.A <- rasterFromXYZ(as.data.frame(map.MO.Cubist.A[, 1:3]))
plot(map.MO.Cubist.Raster.A, main = "MO map")
crs(map.MO.Cubist.Raster.A) <- "+init=epsg:4326"
writeRaster(map.MO.Cubist.Raster.A, "Map_MO_Cubist_Proj.tif", format = "GTiff", 
            datatype = "FLT4S", overwrite = T)

# Gerando Mapa - SVM (Support Vector Machine)

#SiltFit.svm.A2 <- train(x=PI.train[,c(8:13)], y=PI.train$MO.gkg, method = "svmLinear", trControl = ctrl)

colnames(PI.train)
tuneResult <- tune(svm, MO.gkg~SYSI_1 + SYSI_2 + SYSI_3 + SYSI_4 + SYSI_5 + SYSI_7, data = PI.train,
                   ranges = list(epsilon = seq(0.1, 0.2, 0.02), cost = c(5, 7, 15, 20)))
plot(tuneResult)
MOFit.SVM.A <- tuneResult$best.model
print(MOFit.SVM.A)
map.MO.SVM.A <- predict(MOFit.SVM.A, newdata = tempD) 
map.MO.SVM.A <- cbind(data.frame(tempD[, c("x", "y")]), map.MO.SVM.A)
summary(map.MO.SVM.A)
map.MO.SVM.A <- subset(map.MO.SVM.A, map.MO.SVM.A >= 0)
map.MO.SVM.Raster.A <- rasterFromXYZ(as.data.frame(map.MO.SVM.A[, 1:3]))
#plot(map.MO.SVM.Raster.A, main = "MO map")
crs(map.MO.SVM.Raster.A) <- "+init=epsg:4326"
writeRaster(map.MO.SVM.Raster.A, "Mapa_MO_SVM_Proj.tif", format = "GTiff", 
            datatype = "FLT4S", overwrite = T)

## Validation of the model
PI.val <- read.csv("PI_Val_MO.csv", header = T, sep = ";")
str(PI.val)

PI.val$MO.PE.RF <- PI.val$MO_gkg - PI.val$Map_MO_RF_ 
PI.val$MO.PE.Cub <- PI.val$MO_gkg - PI.val$Map_MO_Cub 
PI.val$MO.PE.SVM <- PI.val$MO_gkg - PI.val$Mapa_MO_SV 
summary(PI.val)

PI.VAL.NA <- apply(PI.val, 1, function(x){any(is.na(x))}) # Verifica linhas c/ NAs
sum(PI.VAL.NA)
PI.val.ok <- PI.val[complete.cases(PI.val),]
summary(PI.val.ok)

sd(PI.val.ok$MO.PE.SVM)
sd(PI.val.ok$MO.PE.RF)
sd(PI.val.ok$MO.PE.Cub)

par(mfrow=c(1,1))
plot(PI.val.ok$Map_MO_RF_, PI.val.ok$MO_gkg, main = "RF", xlab="Predicted (g/kg)", ylab="Observed (g/kg)",
     xlim=c(0,40), ylim=c(0,60))
abline(0,1, lty=2, col="black")
abline(lm(PI.val.ok$Map_MO_RF_ ~ PI.val.ok$MO_gkg), col="red", lty=2)
goof(observed = PI.val.ok$MO_gkg, predicted = PI.val.ok$Map_MO_RF_, type = "DSM")

plot(PI.val.ok$Mapa_MO_SV, PI.val.ok$MO_gkg, main = "SVM", xlab="Predicted (g/kg)", ylab="Observed (g/kg)",
     xlim=c(0,40), ylim=c(0,60))
abline(0,1, lty=2, col="black")
abline(lm(PI.val.ok$Mapa_MO_SV ~ PI.val.ok$MO_gkg), col="red", lty=2)
goof(observed = PI.val.ok$MO_gkg, predicted = PI.val.ok$Mapa_MO_SV, type = "DSM")

plot(PI.val.ok$Map_MO_Cub, PI.val.ok$MO_gkg, main = "Cubist", xlab="Predicted (g/kg)", ylab="Observed (g/kg)",
     xlim=c(0,40), ylim=c(0,60))
abline(0,1, lty=2, col="black")
abline(lm(PI.val.ok$Map_MO_Cub ~ PI.val.ok$MO_gkg), col="red", lty=2)
goof(observed = PI.val.ok$MO_gkg, predicted = PI.val.ok$Map_MO_Cub, type = "DSM")

dev.off()

#Additional model evaluation
#DB.val <- read.csv("DB.Val_Okay_Preditos.csv", header = T, sep = ",")
str(PI.val.ok)
#Creating a new data.frame
modRF <- data.frame(obs = PI.val.ok$MO.gkg, mod = PI.val.ok$MO.RF, model = "RF")
modCubist <- data.frame(obs = PI.val.ok$MO.gkg, mod = PI.val.ok$MO.Cub, model = "Cubist")
modSVM <- data.frame(obs = PI.val.ok$MO.gkg, mod = PI.val.ok$MO.SVM, model = "SVM")
modData <- rbind(modRF, modCubist, modSVM)
summary(modData)

if (!require(openair)){install.packages("openair");require(openair)}
modsts <- modStats(modData, obs = "obs", mod = "mod", type = "model")
write.table(modsts, file="ModelEvaluation.txt", sep = ",", quote = FALSE, row.names = F)

TaylorDiagram(modData, obs = "obs", mod = "mod", group = "model", cols = c("green", "yellow", "red"),
              cor.col = 'darkblue', rms.col = 'black')

conditionalQuantile(modData, obs = "obs", mod = "mod", type = "model")



# Correlação MO e SF --------------------------------
PI.pred <- read.csv("MO_SF_cor.csv", header = T, sep = ";")
colnames(PI.pred)
str(PI.pred)

PI.pred.na <- apply(PI.pred, 1, function(x){any(is.na(x))}) # Verifica linhas c/ NAs
sum(PI.pred.na)
PI.pred.ok <- PI.pred[complete.cases(PI.pred),]
str(PI.pred.ok)
View(PI.pred)

corrplot(cor(PI.pred.ok[3:5]), method = "num", type = "upper")
# Significance test with correlogram
PI.cor <- cor(PI.pred[3:5])
res1 <- cor.mtest(PI.pred[3:5], conf.level = .95, method = "pearson") # cor.mtest - Produces p-values and confidence intervals for each pair of input features
res2 <- cor.mtest(PI.pred[3:5], conf.level = .99)
corrplot(PI.cor, p.mat = res1$p, sig.level = .05)
corrplot(PI.cor, p.mat = res1$p, insig = "label_sig", pch.col = "gray", sig.level = .05,
         method = "number", type = "upper")

#Validação
PI.graf <- read.csv("MO_SF_cor.csv", header = T, sep = ";")
str(PI.graf)
PI.graf$MO.PE.RF <- PI.graf$MO.gkg - PI.graf$MO.RF 

PI.graf.NA <- apply(PI.graf, 1, function(x){any(is.na(x))}) # Verifica linhas c/ NAs
sum(PI.graf.NA)
PI.graf.ok <- PI.graf[complete.cases(PI.graf),]

summary(PI.graf.ok)
sd(PI.graf.ok$MO.PE.RF)

par(mfrow=c(1,1))
plot(PI.graf.ok$SF, PI.graf.ok$MO.RF, main = "RF", xlab="Soil Frequency", ylab="Predicted OM (g/kg)",
     xlim=c(0,2500), ylim=c(0,70))
abline(0,1, lty=2, col="black")
abline(lm(PI.graf.ok$MO.RF ~ PI.graf.ok$MO.gkg), col="red", lty=2)
goof(observed = PI.graf.ok$MO.gkg, predicted = PI.graf.ok$MO.RF, type = "DSM")

