#############################################################################
#                           Maximilian Wesemeyer                            #
# Getting phenocam locations; write csv to use with GEE; Prepare data       #
# for modelling                                                             #
#############################################################################
# loading the packages

library(phenocamapi)
library(dplyr)
#############################################################################
#

phenos <- get_phenos()
phenos <- phenos[which(phenos$active == TRUE),]
phenos <- phenos[,]
phenos <- phenos[!is.na(phenos$MAP_daymet),]
phenos <- phenos[phenos$site_meteorology==TRUE,]
phenos <- phenos[which(phenos$ecoregion==8 |
                         phenos$ecoregion==10),]

phenos$ecoregion <- as.factor(phenos$ecoregion)
out <- data.frame()

for(i in c(8,10)) {
  s <- phenos[which(phenos$ecoregion==i),]
  out <- rbind(out, sample_n(s,10))
  }

out1 = out[1:5,c(1:3)]
out2 = out[5:10, 1:3]
write.csv(out1, "C:/Users/Maximus/Documents/R_Projekt/my_data/phenocam_locations_1.csv")
write.csv(out2, "C:/Users/Maximus/Documents/R_Projekt/my_data/phenocam_locations_2.csv")


#############################################################################
#load ee landsat data

data_path <- "C:/Users/Maximus/Documents/R_Projekt/my_data/"
Landsat_data_1_5 <- read.csv(paste0(data_path, "Landsat_1_5.csv"))
Landsat_data_11_15 <- read.csv(paste0(data_path, "Landsat_11_15.csv"))

# remove system index and .geo

#Landsat_data_1_5 <- Landsat_data_1_5[,-1]
#Landsat_data_1_5 <- Landsat_data_1_5[,-16]
#Landsat_data_11_15 <- Landsat_data_11_15[,-1]
#Landsat_data_11_15 <- Landsat_data_11_15[,-16]

Landsat_data_1_5 <- merge(Landsat_data_1_5, phenos, by.x = "site", by.y = "site")
Landsat_data_11_15 <- merge(Landsat_data_11_15, phenos, by.x = "site", by.y = "site")

#############################################################################

Landsat_SR = rbind(Landsat_data_1_5, Landsat_data_11_15)
library(stringr)
Landsat_SR$EVI <- 2.5*((Landsat_SR$nir-Landsat_SR$red)/(Landsat_SR$nir+2.4*Landsat_SR$red+1))
Landsat_SR$scene_id <- as.character(Landsat_SR$scene_id)
Landsat_SR$scene_id <- substr(Landsat_SR$scene_id,1,nchar(Landsat_SR$scene_id)-15)
Landsat_SR$date <- str_extract(Landsat_SR$scene_id, pattern = "[:digit:]{8}$" )
Landsat_SR$date <- as.POSIXct(Landsat_SR$date, format = ("%Y%m%d"))
Landsat_SR$year <- format(as.Date(Landsat_SR$date, format="%d/%m/%Y"),"%Y")
#Landsat_SR$year

# count qa 

Landsat_SR %>% group_by(pixel_qa) %>% count(pixel_qa)
Landsat_SR %>% group_by(pixel_qa) %>% group_by(sensor_id) %>% count(pixel_qa)

# 898, 834 = high confidence cirrus -> will be removed
# 130 will be removed as well 
Landsat_SR <- Landsat_SR %>% dplyr::filter(pixel_qa != 130 & pixel_qa != 898 & 
                                             pixel_qa != 834 & pixel_qa != 68 & pixel_qa != 324 & pixel_qa != 386)
Landsat_SR %>% group_by(pixel_qa) %>% group_by(sensor_id) %>% count(pixel_qa)

#observations per year
Landsat_SR %>% group_by(year) %>% count(year)

#create a dataset for modelling with only those variables that will be used

data_prep <- Landsat_SR[,c("year", "EVI", "doy", "site", "elev",
                           "MAP_site", "MAT_site", "primary_veg_type")]
data_prep$pixel <- as.integer(data_prep$site)
stations <- data.frame(pixel = unique(data_prep$pixel), site = levels(data_prep$site))
data_prep <- data_prep[,-4]
colnames(data_prep) <- c("year", "vi", "doy", "elevation", "MAP_site",
                         "MAT_site", "primary_veg_type", "pixel")
data_prep$year <- as.integer(data_prep$year)

#data_prep <- subset(data_prep, data_prep$doy <=200)

Landsat_SR[which(Landsat_SR$EVI < -0.1),"pixel_qa"]

data_prep$primary_veg_type <- as.factor(data_prep$primary_veg_type)
subset(data_prep, pixel <= 5) %>% 
ggplot(aes(x = doy, y = vi, col = primary_veg_type)) + geom_point() + 
  facet_wrap(facets = "elevation")
# AG = Agriculture DB = Deciduous Broadleaf EN = Evergreen Needleleaf 

range(data_prep[which(data_prep$primary_veg_type == "DB"),"pixel"])




                                                