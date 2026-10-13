#Import Shapefile to R

install.packages("rgdal")
library(rgdal)
# Read this shape file with the rgdal library. 
setwd("DIRETORIO/SUBPASTA")
getwd()

shapefile <- readOGR(dsn = "DIRETORIO/SUBPASTA", 
                     layer = "Angulos_Terreno")


write.table(shapefile, file = "Angulos_Terreno.csv")
Sinuo_Geologia <- read.csv("sinuo_geologia_Benito_reclass.csv", header = TRUE, sep = ";")
head(Sinuo_Geologia)
summary(Sinuo_Geologia)

write.csv(shapefile@data, file="DIRETORIO/SUBPASTA.csv")
