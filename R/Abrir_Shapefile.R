#Import Shapefile to R

install.packages("rgdal")
library(rgdal)
# Read this shape file with the rgdal library. 
setwd("F:/Doutorado/Dados_Doc/Shapefiles/Drenagem/Dre_Pira")
getwd()

shapefile <- readOGR(dsn = "F:/Doutorado/Dados_Doc/Shapefiles/Drenagem/Dre_Pira", 
                     layer = "Angulos_Terreno")


write.table(shapefile, file = "Angulos_Terreno.csv")
Sinuo_Geologia <- read.csv("sinuo_geologia_Benito_reclass.csv", header = TRUE, sep = ";")
head(Sinuo_Geologia)
summary(Sinuo_Geologia)

write.csv(shapefile@data, file="F:/Doutorado/Dados_Doc/Shapefiles/Drenagem/Dre_Pira/sinuo_geologia_Benito_reclass.csv")
