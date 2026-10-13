library(raster)
library(caret)
library(Cubist)
library(terra)
list.files()

# Abrir SYSI
setwd("C:/Fellipe/BENRI/DSM/Covariates")
SYSI <- stack("SYSI.tif") ##before the / is the folder name
SYSI[SYSI == 0] <- NA  ##set 0 values as NA
SYSI[SYSI < 0] <- NA  ##set 0 values as NA
names(SYSI) <- c('SYSI.1', 'SYSI.2', 'SYSI.3', 'SYSI.4', 'SYSI.5', 'SYSI.6')
plot(SYSI)

"DEM.tif"
"SLP.tif"
"TWI.tif" 
"AHS.tif"
"LSF.tif"
"CNBL.tif"
"RSP.tif"

setwd("C:/Fellipe/BD_Solos/Mod_Pira/Covariates")
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

#x <- rast(raster_stack)
#terra::writeRaster(x, "Cov_Stack.tif")

setwd("C:/Fellipe/BENRI/DSM")
list.files()
bd <- read.csv("BD_GEOCIS_Publico.csv", header = TRUE, sep = ";")
colnames(bd)

bd <- bd[,c(8,9,33,34,35,38)]

write.csv(bd, "BD_GEOCIS_MDS.csv")

setwd("C:/Fellipe/BD_Solos/Mod_Pira/Covariates")
list.files()





