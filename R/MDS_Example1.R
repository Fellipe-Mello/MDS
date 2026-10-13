library(raster)
library(caret)
library(dplyr)
library(Cubist)
library(sf)
library(corrplot)
library(GGally)
library(ggcorrplot)

# Abrir Banco de Dados do Henrique
setwd('DIRETORIO/SUBPASTA')
list.files()

bd <- read.csv("BD_Ascana.csv", sep = ",")

str(bd)

bd$P                = as.numeric(as.integer(bd$P))
bd$M.O.             = as.numeric(as.integer(bd$M.O.))
bd$C                = as.numeric(as.integer(bd$C))
bd$Mg               = as.numeric(as.integer(bd$Mg))
bd$H.AL             = as.numeric(as.integer(bd$H.AL))
bd$AL               = as.numeric(as.integer(bd$AL))
bd$S                = as.numeric(as.integer(bd$S))
bd$Fe               = as.numeric(as.integer(bd$Fe))
bd$SYSI_SP_4326_1   = as.numeric(as.integer(bd$SYSI_SP_4326_1))
bd$SYSI_SP_4326_2   = as.numeric(as.integer(bd$SYSI_SP_4326_2))
bd$SYSI_SP_4326_3   = as.numeric(as.integer(bd$SYSI_SP_4326_3))
bd$SYSI_SP_4326_4   = as.numeric(as.integer(bd$SYSI_SP_4326_4))
bd$SYSI_SP_4326_5   = as.numeric(as.integer(bd$SYSI_SP_4326_5))
bd$SYSI_SP_4326_6   = as.numeric(as.integer(bd$SYSI_SP_4326_6))

bd_cor <- bd[,c(5:18,26:30,32:43)]

colnames(bd_cor)    <- c("P","MO","C","PH","K","Ca","Mg","H_AL","AL","SB","CTC","V","m","S",
                         "Cu","Zn","Fe","Mn","B","DEM","TWI","SLP","RSP","PCV","LSF","SYSI_SP_4326_1",
                         "SYSI_SP_4326_2","SYSI_SP_4326_3","SYSI_SP_4326_4","SYSI_SP_4326_5","SYSI_SP_4326_6")

## Retirada dos NAs 
PTS_Model_NA <- na.omit(bd_cor)                            # Apply na.omit in R
bd_cor <- PTS_Model_NA


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
############################################################################################
############################################################################################
############################################################################################
############################################################################################
colnames(bd_cor)
# Modeling
P_RF          <- train(x = bd_cor[,c(20:31)],
                 y = bd_cor$P, 
                 method = "rf", 
                 trControl = trainControl(method = "cv", number = 5))

MO_RF          <- train(x = bd_cor[,c(20:31)],
                       y = bd_cor$MO, 
                       method = "rf", 
                       trControl = trainControl(method = "cv", number = 5))

C_RF          <- train(x = bd_cor[,c(20:31)],
                       y = bd_cor$C, 
                       method = "rf", 
                       trControl = trainControl(method = "cv", number = 5))

PH_RF          <- train(x = bd_cor[,c(20:31)],
                       y = bd_cor$PH, 
                       method = "rf", 
                       trControl = trainControl(method = "cv", number = 5))

K_RF          <- train(x = bd_cor[,c(20:31)],
                        y = bd_cor$K, 
                        method = "rf", 
                        trControl = trainControl(method = "cv", number = 5))

Ca_RF          <- train(x = bd_cor[,c(20:31)],
                        y = bd_cor$Ca, 
                        method = "rf", 
                        trControl = trainControl(method = "cv", number = 5))

Mg_RF          <- train(x = bd_cor[,c(20:31)],
                        y = bd_cor$Mg, 
                        method = "rf", 
                        trControl = trainControl(method = "cv", number = 5))

H_AL_RF          <- train(x = bd_cor[,c(20:31)],
                        y = bd_cor$H_AL, 
                        method = "rf", 
                        trControl = trainControl(method = "cv", number = 5))

AL_RF          <- train(x = bd_cor[,c(20:31)],
                        y = bd_cor$AL, 
                        method = "rf", 
                        trControl = trainControl(method = "cv", number = 5))

SB_RF          <- train(x = bd_cor[,c(20:31)],
                        y = bd_cor$SB, 
                        method = "rf", 
                        trControl = trainControl(method = "cv", number = 5))

CTC_RF          <- train(x = bd_cor[,c(20:31)],
                        y = bd_cor$CTC, 
                        method = "rf", 
                        trControl = trainControl(method = "cv", number = 5))

V_RF          <- train(x = bd_cor[,c(20:31)],
                        y = bd_cor$V, 
                        method = "rf", 
                        trControl = trainControl(method = "cv", number = 5))

m_RF          <- train(x = bd_cor[,c(20:31)],
                        y = bd_cor$m, 
                        method = "rf", 
                        trControl = trainControl(method = "cv", number = 5))

'S_RF          <- train(x = bd_cor[,c(20:31)],
                       y = bd_cor$S, 
                       method = "rf", 
                       trControl = trainControl(method = "cv", number = 5))

Cu_RF          <- train(x = bd_cor[,c(20:31)],
                       y = bd_cor$Cu, 
                       method = "rf", 
                       trControl = trainControl(method = "cv", number = 5))

Zn_RF          <- train(x = bd_cor[,c(20:31)],
                       y = bd_cor$Zn, 
                       method = "rf", 
                       trControl = trainControl(method = "cv", number = 5))

Fe_RF          <- train(x = bd_cor[,c(20:31)],
                       y = bd_cor$Fe, 
                       method = "rf", 
                       trControl = trainControl(method = "cv", number = 5))

Mn_RF          <- train(x = bd_cor[,c(20:31)],
                       y = bd_cor$Mn, 
                       method = "rf", 
                       trControl = trainControl(method = "cv", number = 5))

B_RF          <- train(x = bd_cor[,c(20:31)],
                       y = bd_cor$B, 
                       method = "rf", 
                       trControl = trainControl(method = "cv", number = 5))'


# Abrir SYSI
setwd("C:/Users/fellipe.alcantara/OneDrive - GATEC S A GESTAO AGROINDUSTRIAL/Fellipe_OneDriver/ASCANA/Relevo")
SYSI <- stack("SYSI_4326_ASCANA.tif") ##before the / is the folder name
SYSI[SYSI == 0] <- NA  
SYSI[SYSI < 0] <- NA  

setwd("C:/Users/fellipe.alcantara/OneDrive - GATEC S A GESTAO AGROINDUSTRIAL/Fellipe_OneDriver/ASCANA/Relevo")
list.files(pattern = "\\.tif$")

DEM = list.files(paste(getwd()), pattern = "DEM_4326.tif$")
DEM <- raster(DEM)
DEM <- resample(DEM, SYSI, method = "ngb")

LSF = list.files(paste(getwd()), pattern = "LSF_4326.tif$")
LSF <- raster(LSF)
LSF <- resample(LSF, SYSI, method = "ngb")

RSP = list.files(paste(getwd()), pattern = "RSP_4326.tif$")
RSP <- raster(RSP)
RSP <- resample(RSP, SYSI, method = "ngb")

SLP = list.files(paste(getwd()), pattern = "SLP_4326.tif$")
SLP <- raster(SLP)
SLP <- resample(SLP, SYSI, method = "ngb")

TWI = list.files(paste(getwd()), pattern = "TWI_4326.tif$")
TWI <- raster(TWI)
TWI <- resample(TWI, SYSI, method = "ngb")

PCV = list.files(paste(getwd()), pattern = "PCV_4326.tif$")
PCV <- raster(PCV)
PCV <- resample(PCV, SYSI, method = "ngb")

# Raster Stack
raster_stack <-     stack(DEM,  LSF,  RSP,  SLP,  TWI,  PCV,  SYSI)
names(raster_stack) <- c("DEM","LSF","RSP","SLP","TWI","PCV","SYSI_SP_4326_1",
                         "SYSI_SP_4326_2","SYSI_SP_4326_3","SYSI_SP_4326_4",
                         "SYSI_SP_4326_5","SYSI_SP_4326_6","SYSI_SP_4326_7")

plot(s)

s <- dropLayer(raster_stack, c(13))

# Predict the Raster
Sand_raster  <- raster::predict(raster_stack, Sand_RF)


P_RF_pred         <- raster::predict(s, P_RF)
MO_RF_pred        <- raster::predict(s, MO_RF)
C_RF_pred         <- raster::predict(s, C_RF)
PH_RF_pred        <- raster::predict(s, PH_RF)
K_RF_pred         <- raster::predict(s, K_RF)
Ca_RF_pred        <- raster::predict(s, Ca_RF)
Mg_RF_pred        <- raster::predict(s, Mg_RF)
H_AL_RF_pred      <- raster::predict(s, H_AL_RF)
AL_RF_pred        <- raster::predict(s, AL_RF)
SB_RF_pred        <- raster::predict(s, SB_RF)
CTC_RF_pred       <- raster::predict(s, CTC_RF)
V_RF_pred         <- raster::predict(s, V_RF)
m_RF_pred         <- raster::predict(s, m_RF)


# Plot the Raster 
plot(Sand_raster)
plot(Silt_raster)
plot(Clay_raster)


##Save the predicted map##
setwd("C:/Users/fellipe.alcantara/OneDrive - GATEC S A GESTAO AGROINDUSTRIAL/Fellipe_OneDriver/ASCANA/Pred_Maps_2")

writeRaster(P_RF_pred,     "P.tif",     overwrite=TRUE)
writeRaster(MO_RF_pred,    "MO.tif",    overwrite=TRUE)
writeRaster(C_RF_pred,     "C.tif",     overwrite=TRUE)
writeRaster(PH_RF_pred,    "PH.tif",    overwrite=TRUE)
writeRaster(K_RF_pred,     "K.tif",     overwrite=TRUE)
writeRaster(Ca_RF_pred,    "Ca.tif",    overwrite=TRUE)
writeRaster(Mg_RF_pred,    "Mg.tif",    overwrite=TRUE)
writeRaster(H_AL_RF_pred,  "H_AL.tif",  overwrite=TRUE)
writeRaster(AL_RF_pred,    "Al.tif",    overwrite=TRUE)
writeRaster(SB_RF_pred,    "SB.tif",    overwrite=TRUE)
writeRaster(CTC_RF_pred,   "CTC.tif",   overwrite=TRUE)
writeRaster(V_RF_pred,     "V.tif",     overwrite=TRUE)
writeRaster(m_RF_pred,     "M.tif",     overwrite=TRUE)






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
setwd('DIRETORIO/SUBPASTA')
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
setwd("DIRETORIO/SUBPASTA")

writeRaster(Sand_raster, "CTC_B.tif",     overwrite=TRUE)
writeRaster(V_raster,    "V_B.tif",    overwrite=TRUE)




