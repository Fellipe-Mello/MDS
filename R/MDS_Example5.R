library(raster)
library(caret)
library(dplyr)
library(ggcorrplot)
library(Cubist)
library(sf)

setwd('C:/Users/fellipe.alcantara/OneDrive - GATEC S A GESTAO AGROINDUSTRIAL/Fellipe OneDriver/BD_Solos/Mod_Pira')
list.files()

bd <- read.csv("bd_Modelagem_2.csv", sep = ";")

str(bd)
colnames(bd)
# Change format of DEM to numeric
bd$DEM                = as.numeric(as.integer(bd$DEM))
bd$SYSI_Pira_UTM_1         = as.numeric(as.integer(bd$SYSI_Pira_UTM_1))
bd$SYSI_Pira_UTM_2         = as.numeric(as.integer(bd$SYSI_Pira_UTM_2))
bd$SYSI_Pira_UTM_3         = as.numeric(as.integer(bd$SYSI_Pira_UTM_3))
bd$SYSI_Pira_UTM_4         = as.numeric(as.integer(bd$SYSI_Pira_UTM_4))
bd$SYSI_Pira_UTM_5         = as.numeric(as.integer(bd$SYSI_Pira_UTM_5))
bd$SYSI_Pira_UTM_6         = as.numeric(as.integer(bd$SYSI_Pira_UTM_6))

# Assuming your dataframe is named df and the specific column is "your_column"
bda <- bd %>% filter(Layer == "A")
bdb <- bd %>% filter(Layer == "B")
bdc <- bd %>% filter(Layer == "C")

colnames(bda)
# Create the databases
bd_sand <- bda[,c(10,36:39,41:43,45,46,48,49:57)]
bd_silt <- bda[,c(11,36:39,41:43,45,46,48,49:57)]
bd_clay <- bda[,c(12,36:39,41:43,45,46,48,49:57)]
bd_om   <- bda[,c(13,36:39,41:43,45,46,48,49:57)]
bd_ctc  <- bda[,c(24,36:39,41:43,45,46,48,49:57)]
bd_V    <- bda[,c(27,36:39,41:43,45,46,48,49:57)]


## Remove NAs 
sand_bd_na <- na.omit(bd_sand)
bd_sand    <- sand_bd_na

silt_bd_na <- na.omit(bd_silt)
bd_silt    <- silt_bd_na

clay_bd_na <- na.omit(bd_clay)
bd_clay    <- clay_bd_na

om_bd_na   <- na.omit(bd_om)
bd_om      <- om_bd_na

bd_ctc_na   <- na.omit(bd_ctc)
bd_ctc      <- bd_ctc_na

bd_V_na   <- na.omit(bd_V)
bd_V      <- bd_V_na

'# Scale the values
bd_sand = as.data.frame(scale(bd_sand, center = TRUE, scale = TRUE))
bd_silt = as.data.frame(scale(bd_silt, center = TRUE, scale = TRUE))
bd_clay = as.data.frame(scale(bd_clay, center = TRUE, scale = TRUE))'

# Data-splitting
bd_sand_ref <- createDataPartition(bd_sand$Sand_gkg, p = .70, list = F, times = 1)
bd_sand_cal <- bd_sand[bd_sand_ref,]
bd_sand_val <- bd_sand[-bd_sand_ref,]

bd_silt_ref <- createDataPartition(bd_silt$Silt_gkg, p = .70, list = F, times = 1)
bd_silt_cal <- bd_silt[bd_silt_ref,]
bd_silt_val <- bd_silt[-bd_silt_ref,]  

bd_clay_ref <- createDataPartition(bd_clay$Clay_gkg, p = .70, list = F, times = 1)
bd_clay_cal <- bd_clay[bd_clay_ref,]
bd_clay_val <- bd_clay[-bd_clay_ref,]  

bd_om_ref <- createDataPartition(bd_om$OM_gkg, p = .70, list = F, times = 1)
bd_om_cal <- bd_om[bd_om_ref,]
bd_om_val <- bd_om[-bd_om_ref,]  

bd_ctc_ref <- createDataPartition(bd_ctc$CEC_Ph7_mm, p = .70, list = F, times = 1)
bd_ctc_cal <- bd_ctc[bd_ctc_ref,]
bd_ctc_val <- bd_ctc[-bd_ctc_ref,] 

bd_V_ref <- createDataPartition(bd_V$V, p = .70, list = F, times = 1)
bd_V_cal <- bd_V[bd_V_ref,]
bd_V_val <- bd_V[-bd_V_ref,] 


colnames(bd_sand_cal)
# Modeling
Sand_RF <- train(x = bd_sand_cal[,c(2:20)],
                 y = bd_sand_cal$Sand_gkg, 
                 method = "rf", 
                 trControl = trainControl(method = "cv", number = 10))

Silt_RF <- train(x = bd_silt_cal[,c(2:20)], 
                 y = bd_silt_cal$Silt_gkg, 
                 method = "rf", 
                 trControl = trainControl(method = "cv", number = 10))

Clay_RF <- train(x = bd_clay[,c(2:20)], 
                 y = bd_clay$Clay_gkg, 
                 method = "rf", 
                 trControl = trainControl(method = "cv", number = 10))


ctc_RF <- train(x = bd_ctc_cal[,c(2:20)], 
                 y = bd_ctc_cal$CEC_Ph7_mm, 
                 method = "rf", 
                 trControl = trainControl(method = "cv", number = 10))


V_RF <- train(x = bd_V_cal[,c(2:20)], 
                 y = bd_V_cal$V, 
                 method = "rf", 
                 trControl = trainControl(method = "cv", number = 10))



'# Correlation test 
x = scale(bd_sand, center = TRUE, scale = TRUE)

mydata.cor = cor(x, method = c("spearman"))


tiff("Correlacao_Clay.tif", width = 1000, height = 1000, res = 100) 
ggcorrplot(mydata.cor,
           hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           lab_size = 6,
           outline.color = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))
dev.off() 
'
# Opening the rasters
# Abrir SYSI
setwd('C:/Users/fellipe.alcantara/OneDrive - GATEC S A GESTAO AGROINDUSTRIAL/Fellipe OneDriver/BD_Solos/Mod_Pira/Covariates')
SYSI <- stack("SYSI_Pira_UTM.tif") ##before the / is the folder name
SYSI[SYSI == 0] <- NA  ##set 0 values as NA
SYSI[SYSI < 0] <- NA  ##set 0 values as NA


setwd('C:/Users/fellipe.alcantara/OneDrive - GATEC S A GESTAO AGROINDUSTRIAL/Fellipe OneDriver/BD_Solos/Mod_Pira/Covariates')
list.files()
#Leitura do DEM
DEM <- list.files(paste(getwd()), pattern = "DEM.tif$")
DEM <- raster(DEM)
DEM <- resample(DEM, SYSI, method = "ngb")

#Leitura do  Slope
SLP <- list.files(paste(getwd()), pattern = "SLP.tif$")
SLP <- raster(SLP)
SLP <- resample(SLP, SYSI, method = "ngb")
plot(SLP)
#Leitura do  TWI
TWI <- list.files(paste(getwd()), pattern = "TWI.tif$")
TWI <- raster(TWI)
TWI <- resample(TWI, SYSI, method = "ngb")
plot(TWI)
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

'#Leitura do  VOFD
VOFD <- list.files(paste(getwd()), pattern = "VOFD.tif$")
VOFD <- raster(VOFD)
VOFD <- resample(VOFD, SYSI, method = "ngb")

#Leitura do  VDCN
VDCN <- list.files(paste(getwd()), pattern = "VDCN.tif$")
VDCN <- raster(VDCN)
VDCN <- resample(VDCN, SYSI, method = "ngb")

#Leitura do  UCV
UCV <- list.files(paste(getwd()), pattern = "UCV.tif$")
UCV <- raster(UCV)
UCV <- resample(UCV, SYSI, method = "ngb")'

#Leitura do  TPI
TPI <- list.files(paste(getwd()), pattern = "TPI.tif$")
TPI <- raster(TPI)
TPI <- resample(TPI, SYSI, method = "ngb")

'#Leitura do  OFD
OFD <- list.files(paste(getwd()), pattern = "OFD_.tif")
OFD <- raster(OFD)
OFD <- resample(OFD, SYSI, method = "ngb")'

#Leitura do  MRI
MRI <- list.files(paste(getwd()), pattern = "MRI.tif$")
MRI <- raster(MRI)
MRI <- resample(MRI, SYSI, method = "ngb")

#Leitura do  LCV
LCV <- list.files(paste(getwd()), pattern = "LCV.tif$")
LCV <- raster(LCV)
LCV <- resample(LCV, SYSI, method = "ngb")

'#Leitura do  HOFD
HOFD <- list.files(paste(getwd()), pattern = "HOFD.tif$")
HOFD <- raster(HOFD)
HOFD <- resample(HOFD, SYSI, method = "ngb")'

#Leitura do  FAC
FAC <- list.files(paste(getwd()), pattern = "FAC.tif$")
FAC <- raster(FAC)
FAC <- resample(FAC, SYSI, method = "ngb")

'#Leitura do  DCV
DCV <- list.files(paste(getwd()), pattern = "DCV.tif$")
DCV <- raster(DCV)
DCV <- resample(DCV, SYSI, method = "ngb")'

#Leitura do  CVI
CVI <- list.files(paste(getwd()), pattern = "CVI.tif$")
CVI <- raster(CVI)
CVI <- resample(CVI, SYSI, method = "ngb")

#Leitura do  CSC
CSC <- list.files(paste(getwd()), pattern = "CSC.tif$")
CSC <- raster(CSC)
CSC <- resample(CSC, SYSI, method = "ngb")

# Raster Stack
raster_stack <- stack(DEM,LSF,TWI,TPI,SLP,RSP,MRI,LCV,FAC,CVI,CSC,CNBL,AHS,SYSI)
                      DEM LSF TWI TPI SLP RSP MRI LCV FAC CVI CSC CNBL AHS
                      
                      SYSI_UTM_1" "SYSI_UTM_2" "SYSI_UTM_3" "SYSI_UTM_4" "SYSI_UTM_5"
[19] "SYSI_UTM_6

plot(raster_stack)



'DEM
LSF
TWI
TPI
SLP
RSP
MRI
LCV
FAC
DCV
CVI
CSC
CNBL
AHS
SYSI'

raster_stack$SYSI_Pira_UTM_1
  
# Predict the Raster                 
Sand_raster  <- raster::predict(raster_stack, Sand_RF)
Silt_raster  <- raster::predict(raster_stack, Silt_RF)
Clay_raster  <- raster::predict(raster_stack, Clay_RF)
ctc_raster   <- raster::predict(raster_stack, ctc_RF)
V_raster     <- raster::predict(raster_stack, V_RF)

colnames(bd_sand_cal[,c(2:20)])


# Plot the Raster 
plot(Sand_raster)
plot(Silt_raster)
plot(Clay_raster)
plot(ctc_raster)
plot(V_raster)

##Save the predicted map##
setwd("C:/Users/fellipe.alcantara/OneDrive - GATEC S A GESTAO AGROINDUSTRIAL/Fellipe OneDriver/Granelli/Pred_Maps")
getwd()
writeRaster(Sand_raster,  "Sand.tif",   overwrite=TRUE)
writeRaster(Silt_raster,  "Silt.tif",   overwrite=TRUE)
writeRaster(Clay_raster,  "Clay.tif",  overwrite=TRUE)
writeRaster(ctc_raster,  "CTC.tif",  overwrite=TRUE)
writeRaster(V_raster,  "V.tif",  overwrite=TRUE)


'
Covariáveis
AHS  - Analytical Hillshading -
CNBL - Channel Network Base Level -
DEM  - Digital Elevation Model -
LSF  - LS Factor -
RSP  - Relative Slope Position -
SLP  - Slope -
TWI  - Topographic Wetness Index - 
VDCN - Vertical Distance to Channel Network
CVI  - Convergence Index -
FAC  - Flow accumulation -
VDCN - Vertical distance to channel network
TPI  - Topographic Position index - 
MRI  - Melton Ruggedness Index - 
CSC  - Cross Sectional Curvature - 
LCV  - Longitudinal Curvature - 
UCV  - Upslope curvature
DCV  - Downslope curvature
OFD  - Overland Flow Distance
VOFD - Vertical Overland Flow Distance
HOFD - Horizontal Overland Flow Distance'


