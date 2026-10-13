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

bd <- read.csv("bd_modelagem_completo.csv", sep = ";")
colnames(bd)

# Assuming your dataframe is named df and the specific column is "your_column"
bda <- bd %>% filter(Layer == "A")
bd <- bda

bd <- bd[,c(16,17,25,34,35,39:41,59,60,62,63:70)]

colnames(bd)    <- c("C","OM","CEC","DEM","LSF","TWI","TPI","SLP","CVI","TRI","PCV","GCV","CVX",
                     "SYSI_1","SYSI_2","SYSI_3","SYSI_4","SYSI_5","SYSI_6")

bd

## Retirada dos NAs 
PTS_Model_NA <- na.omit(bd)
bd <- PTS_Model_NA

# CORRELAÇÃO E PLOT
mydata.cor = cor(bd_cor[,c(1:7,11:13,27:39)], method = c("spearman"))

setwd('DIRETORIO/SUBPASTA')
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
C_RF          <- train(x = bd[,c(4:19)],
                          y = bd$C, 
                          method = "rf", 
                          trControl = trainControl(method = "cv", number = 10))

OM_RF          <- train(x = bd[,c(4:19)],
                          y = bd$OM, 
                          method = "rf", 
                          trControl = trainControl(method = "cv", number = 10))

CTC_RF          <- train(x = bd[,c(4:19)],
                          y = bd$CEC, 
                          method = "rf", 
                          trControl = trainControl(method = "cv", number = 10))

# Abrir SYSI
setwd("DIRETORIO/SUBPASTA")
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

# Predict the Raster
C_RF
OM_RF
CTC_RF

C_RF_pred         <- raster::predict(raster_stack, C_RF)
OM_RF_pred        <- raster::predict(raster_stack, OM_RF)
CTC_RF_pred       <- raster::predict(raster_stack, CTC_RF)

# Plot the Raster 
plot(C_RF_pred)
plot(OM_RF_pred)
plot(CTC_RF_pred)

##Save the predicted map##
setwd("DIRETORIO/SUBPASTA")

writeRaster(C_RF_pred,    "C.tif",     overwrite=TRUE)
writeRaster(OM_RF_pred,    "MO.tif",    overwrite=TRUE)
writeRaster(CTC_RF_pred,    "CTC.tif",     overwrite=TRUE)
