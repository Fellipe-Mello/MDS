library(raster)
library(caret)
library(dplyr)
library(Cubist)
library(sf)
library(corrplot)
library(GGally)
library(ggcorrplot)

# Abrir Banco de Dados do Henrique
setwd('C:/Users/fellipe.alcantara/OneDrive - GATEC S A GESTAO AGROINDUSTRIAL/Fellipe_OneDriver/BD_Solos/Mod_Pira')
list.files()

bd <- read.csv("BD_Henrique.csv", sep = ";")

colnames(bd)

bd <- bd[,c(5:7,10:15,17,18,20,21,26,32,35:38)]

colnames(bd)    <- c("Sand","Silt","Clay","SYSI_1","SYSI_2","SYSI_3","SYSI_4","SYSI_5","SYSI_6",
                         "DEM","LSF","SLP","TWI","GCV","TRI","CVX","CVI","TPI","PCV")


str(bd)

## Retirada dos NAs 
PTS_Model_NA <- na.omit(bd)
bd <- PTS_Model_NA

# CORRELAÇÃO E PLOT
mydata.cor = cor(bd_cor[,c(1:7,11:13,27:39)], method = c("spearman"))

setwd('C:/Users/fellipe.alcantara/OneDrive - GATEC S A GESTAO AGROINDUSTRIAL/Fellipe_OneDriver/ASCANA')
tiff("Correlacao.tif", width = 1000, height = 1000, res = 100) 
ggcorrplot(mydata.cor,
           hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           lab_size = 3.5,
           outline.color = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))
dev.off()    

############################################################################################
############################################################################################
colnames(bd)
# Modeling
Sand_RF          <- train(x = bd[,c(4:19)],
                         y = bd$Sand, 
                         method = "rf", 
                         trControl = trainControl(method = "cv", number = 10))

Clay_RF          <- train(x = bd[,c(4:19)],
                       y = bd$Clay, 
                       method = "rf", 
                       trControl = trainControl(method = "cv", number = 10))

Silt_RF          <- train(x = bd[,c(4:19)],
                       y = bd$Silt, 
                       method = "rf", 
                       trControl = trainControl(method = "cv", number = 10))


# Abrir SYSI
setwd("C:/Users/fellipe.alcantara/OneDrive - GATEC S A GESTAO AGROINDUSTRIAL/Fellipe_OneDriver/ESTER/Raster")
list.files(pattern = "\\.tif$")
SYSI <- stack("SYSI.tif") ##before the / is the folder name
SYSI[SYSI == 0] <- NA  
SYSI[SYSI < 0] <- NA  

DEM = list.files(paste(getwd()), pattern = "DEM.tif$")
DEM <- raster(DEM)
DEM <- resample(DEM, SYSI, method = "ngb")

LSF = list.files(paste(getwd()), pattern = "LSF.tif$")
LSF <- raster(LSF)
LSF <- resample(LSF, SYSI, method = "ngb")

SLP = list.files(paste(getwd()), pattern = "SLP.tif$")
SLP <- raster(SLP)
SLP <- resample(SLP, SYSI, method = "ngb")

TWI = list.files(paste(getwd()), pattern = "TWI.tif$")
TWI <- raster(TWI)
TWI <- resample(TWI, SYSI, method = "ngb")

PCV = list.files(paste(getwd()), pattern = "PCV.tif$")
PCV <- raster(PCV)
PCV <- resample(PCV, SYSI, method = "ngb")

CVI = list.files(paste(getwd()), pattern = "CVI.tif$")
CVI <- raster(CVI)
CVI <- resample(CVI, SYSI, method = "ngb")

CVX = list.files(paste(getwd()), pattern = "CVX.tif$")
CVX <- raster(CVX)
CVX <- resample(CVX, SYSI, method = "ngb")

GCV = list.files(paste(getwd()), pattern = "GCV.tif$")
GCV <- raster(GCV)
GCV <- resample(GCV, SYSI, method = "ngb")

TPI = list.files(paste(getwd()), pattern = "TPI.tif$")
TPI <- raster(TPI)
TPI <- resample(TPI, SYSI, method = "ngb")

TRI = list.files(paste(getwd()), pattern = "TRI.tif$")
TRI <- raster(TRI)
TRI <- resample(TRI, SYSI, method = "ngb")

# Raster Stack
raster_stack <-     stack(DEM,  CVI,  CVX,  GCV,  LSF,  PCV,  SLP,  TPI,  TRI,  TWI,  SYSI)
s <- dropLayer(raster_stack, c(17))
raster_stack <- s
names(raster_stack) <- c("DEM","CVI","CVX","GCV","LSF","PCV","SLP","TPI","TRI","TWI","SYSI_1","SYSI_2","SYSI_3","SYSI_4","SYSI_5","SYSI_6")
raster_stack

# Predict the Raster
Sand_raster  <- raster::predict(raster_stack, Sand_RF)

Sand_RF
Clay_RF
Silt_RF

Sand_RF_pred         <- raster::predict(raster_stack, Sand_RF)
Clay_RF_pred         <- raster::predict(raster_stack, Clay_RF)
Silt_RF_pred         <- raster::predict(raster_stack, Silt_RF)

# Plot the Raster 
plot(Sand_RF_pred)
plot(Clay_RF_pred)
plot(Silt_RF_pred)

##Save the predicted map##
setwd("C:/Users/fellipe.alcantara/OneDrive - GATEC S A GESTAO AGROINDUSTRIAL/Fellipe_OneDriver/ESTER/Pred_Maps")

writeRaster(Sand_RF_pred,    "Sand.tif",     overwrite=TRUE)
writeRaster(Clay_RF_pred,    "Clay.tif",    overwrite=TRUE)
writeRaster(Silt_RF_pred,    "Silt.tif",     overwrite=TRUE)





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


#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
# Abrindo os Dados do GeoCIS
setwd('C:/Users/fellipe.alcantara/OneDrive - GATEC S A GESTAO AGROINDUSTRIAL/Fellipe_OneDriver/BD_Solos/Mod_Pira')
list.files()

bd <- read.csv("bd_Modelagem_2.csv", sep = ";")

str(bd)
colnames(bd)
# Change format of DEM to numeric
bd$DEM                     = as.numeric(as.integer(bd$DEM))
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
bd_sand <- bda[,c(10,36,38,39,42,46,52:57)]
bd_silt <- bda[,c(11,36,38,39,42,46,52:57)]
bd_clay <- bda[,c(12,36,38,39,42,46,52:57)]
bd_om   <- bda[,c(13,36,38,39,42,46,52:57)]
bd_ctc  <- bda[,c(24,36,38,39,42,46,52:57)]
bd_V    <- bda[,c(27,36,38,39,42,46,52:57)]

colnames(bd_sand)   =   c("Sand","TWI","SLP","RSP","LSF","DEM","SYSI1","SYSI2","SYSI3","SYSI4","SYSI5","SYSI6")
colnames(bd_silt)   =   c("Silt","TWI","SLP","RSP","LSF","DEM","SYSI1","SYSI2","SYSI3","SYSI4","SYSI5","SYSI6")
colnames(bd_clay)   =   c("Clay","TWI","SLP","RSP","LSF","DEM","SYSI1","SYSI2","SYSI3","SYSI4","SYSI5","SYSI6")
colnames(bd_om)     =   c("OM","TWI","SLP","RSP","LSF","DEM","SYSI1","SYSI2","SYSI3","SYSI4","SYSI5","SYSI6")
colnames(bd_ctc)    =   c("CTC","TWI","SLP","RSP","LSF","DEM","SYSI1","SYSI2","SYSI3","SYSI4","SYSI5","SYSI6")
colnames(bd_V)      =   c("V","TWI","SLP","RSP","LSF","DEM","SYSI1","SYSI2","SYSI3","SYSI4","SYSI5","SYSI6")




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



#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
setwd('C:/Users/fellipe.alcantara/OneDrive - GATEC S A GESTAO AGROINDUSTRIAL/Fellipe_OneDriver/ASCANA')
list.files()

bd <- read.csv("Recomendacao.csv", sep = ";")
bd

bd$CTC           = as.numeric(as.integer(bd$CTC))
bd$V             = as.numeric(as.integer(bd$V))
bd$CTC_b         = as.numeric(as.integer(bd$CTC_b))
bd$V_b           = as.numeric(as.integer(bd$V_b))
bd$DEM_4326         = as.numeric(as.integer(bd$DEM_4326))
bd$SYSI_SP_4326_1   = as.numeric(as.integer(bd$SYSI_SP_4326_1))
bd$SYSI_SP_4326_2   = as.numeric(as.integer(bd$SYSI_SP_4326_2))
bd$SYSI_SP_4326_3   = as.numeric(as.integer(bd$SYSI_SP_4326_3))
bd$SYSI_SP_4326_4   = as.numeric(as.integer(bd$SYSI_SP_4326_4))
bd$SYSI_SP_4326_5   = as.numeric(as.integer(bd$SYSI_SP_4326_5))
bd$SYSI_SP_4326_6   = as.numeric(as.integer(bd$SYSI_SP_4326_6))

str(bd)

# Horizonte B
bd_cor <- bd[,c(3:18)]

colnames(bd_cor)    <- c("CTC","V","CTC_B","V_B",
                         "DEM","TWI","SLP","RSP","PCV","LSF","SYSI_SP_4326_1","SYSI_SP_4326_2",
                         "SYSI_SP_4326_3","SYSI_SP_4326_4","SYSI_SP_4326_5","SYSI_SP_4326_6")



colnames(bd_cor[,c(5:16)])
CTC_RF        <- train(x = bd_cor[,c(5:16)],
                       y = bd_cor$CTC_B, 
                       method = "rf", 
                       trControl = trainControl(method = "cv"))

V_RF        <- train(x = bd_cor[,c(5:16)],
                     y = bd_cor$V_B, 
                     method = "rf", 
                     trControl = trainControl(method = "cv"))

Sand_raster   <- raster::predict(raster_stack, CTC_RF)
V_raster      <- raster::predict(raster_stack, V_RF)

plot(Sand_raster)
plot(V_raster)


#Save the predicted map##
setwd("C:/Users/fellipe.alcantara/OneDrive - GATEC S A GESTAO AGROINDUSTRIAL/Fellipe_OneDriver/ASCANA/Pred_Maps_2")

writeRaster(Sand_raster, "CTC_B.tif",     overwrite=TRUE)
writeRaster(V_raster,    "V_B.tif",    overwrite=TRUE)




