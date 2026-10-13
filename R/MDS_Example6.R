# Modelo digital de solos Referência Piracicaba
library(raster)
library(caret)

# Abrir SYSI
setwd("C:/Fellipe/BENRI/DSM/Covariates")
SYSI <- stack("SYSI.tif") ##before the / is the folder name
SYSI[SYSI == 0] <- NA  ##set 0 values as NA
SYSI[SYSI < 0] <- NA  ##set 0 values as NA
names(SYSI) <- c('SYSI.1', 'SYSI.2', 'SYSI.3', 'SYSI.4', 'SYSI.5', 'SYSI.6')
plot(SYSI)

#Leitura do DEM
DEM <- list.files(paste(getwd()), pattern = "DEM.tif$")
DEM <- raster(DEM)
DEM <- resample(DEM, SYSI, method = "ngb")

#Leitura do  Slope
SLP <- list.files(paste(getwd()), pattern = "SLP.tif$")
SLP <- raster(SLP)
SLP <- resample(SLP, SYSI, method = "ngb")

#Leitura do  TWI
TWI <- list.files(paste(getwd()), pattern = "TWI.tif$")
TWI <- raster(TWI)
TWI <- resample(TWI, SYSI, method = "ngb")

#Leitura do  AHS
AHS <- list.files(paste(getwd()), pattern = "AHS.tif$")
AHS <- raster(AHS)
AHS <- resample(AHS, SYSI, method = "ngb")

#Leitura do  LSF
LSF <- list.files(paste(getwd()), pattern = "LSF.tif$")
LSF <- raster(LSF)
LSF <- resample(LSF, SYSI, method = "ngb")

#Leitura do  CNBL
CNBL <- list.files(paste(getwd()), pattern = "CNBL.tif$")
CNBL <- raster(CNBL)
CNBL <- resample(CNBL, SYSI, method = "ngb")

#Leitura do  RSP
RSP <- list.files(paste(getwd()), pattern = "RSP.tif$")
RSP <- raster(RSP)
RSP <- resample(RSP, SYSI, method = "ngb")

raster_stack <- stack(DEM,SLP,TWI,AHS,LSF,CNBL,RSP,SYSI)
plot(raster_stack$DEM)
