library(tidyverse) 
library(viridisLite)
library(terra)
library(sf)
library(exactextractr)
library(fixest)
library(conleyreg)
library(raster)
library(spatialreg)
library(spdep)

# INKAR -> Zentrale Orte Monitoring -> ... 

# read municipality polygons
saxony <- st_read("data/raw/vwg_sn/gem.shp")
#saxony <- st_read("data/raw/gadm41_DEU_shp/gadm41_DEU_3.shp") %>% filter(NAME_1 == "Sachsen") 
#second one needed, if raster should be cropped

INKAR_data <- read.csv2("data/raw/INKAR.csv")
# read municipality details

# function to clean the data
transform_str_num <- function(number){
  number = substr(number, 1, nchar(number) - 3)
  number_str = sub(".","", number, fixed=TRUE)
  as.integer(number_str)
}

# data cleaning, renaming bulky variable names
#INKAR_data %>% mutate( 
#  pop_2016 = transform_str_num(Bevölkerung.gesamt), 
#  pop_2021 = transform_str_num(Bevölkerung.gesamt.1),   
#  unemployment = transform_str_num(Arbeitslose),
#  labour_force = transform_str_num(Erwerbsfähige.Bevölkerung..15.bis.unter.65.Jahre.),
#  employed = transform_str_num(Sozialversicherungspflichtig.Beschäftigte.am.Wohnort),
#  purchasing_power = transform_str_num(Kaufkraft),
#  distance_supermarkets = transform_str_num(Nahversorgung.Supermärkte.Durchschnittsdistanz),
#  distance_pharmacy = transform_str_num(Nahversorgung.Apotheken.Durchschnittsdistanz), 
#  Raumeinheit = str_split_i(Raumeinheit,", ",1), # correct for ", Stadt"
#  Raumeinheit = str_split_i(Raumeinheit," /",1), # correct for listing of sorbisch city names
#  aging = Einwohner.65.Jahre.und.älter / (Einwohner.unter.6.Jahre + Einwohner.von.6.bis.unter.18.Jahren)
#)

INKAR_data <- INKAR_data %>% mutate( 
  employed = transform_str_num(Beschäftigtenquote),
  purchasing_power = transform_str_num(Kaufkraft),
  distance_supermarkets = transform_str_num(Nahversorgung.Supermärkte.Durchschnittsdistanz),
  distance_pharmacy = transform_str_num(Nahversorgung.Apotheken.Durchschnittsdistanz), 
  Raumeinheit = str_split_i(Raumeinheit,", ",1), # correct for ", Stadt"
  Raumeinheit = str_split_i(Raumeinheit," /",1), # correct for listing of sorbisch city names
)

INKAR_data <- INKAR_data %>% 
  dplyr::select(employed, purchasing_power, distance_supermarkets, distance_pharmacy, Raumeinheit)


# merge data sets
saxony <- left_join(saxony, INKAR_data, by=c("ORTSNAME"="Raumeinheit"))

# first check, if it worked
ggplot() + geom_sf(data=saxony, aes(fill=log(pop_2016))) +
  scale_fill_viridis_c() + theme_minimal() 



# read population raster information
pop_2015 <- raster("data/raw/gpw_v4_population_count_rev11_2015_30_sec.tif")
pop_2020 <- raster("data/raw/gpw_v4_population_count_rev11_2020_30_sec.tif")

#cropped_2015 <- crop(pop_2015, saxony)
#cropped_2015 <- mask(cropped_2015, saxony)

saxony$pop_2015 <- exact_extract(pop_2015, saxony, fun="sum")
saxony$pop_2020 <- exact_extract(pop_2020, saxony, fun="sum")

saxony$degurba <- exact_extract(pop_2015, saxony, fun="mean")
saxony <- saxony %>% mutate(degurba = log(degurba))

# check extraction results
ggplot() + geom_sf(data=saxony, aes(fill=log(pop_2015))) + 
  scale_fill_viridis_c() + theme_minimal() 


saxony %>% mutate(pop_growth = log(pop_2020/pop_2015)) %>% 
  filter(!ORTSNAME %in% c("Dresden","Chemnitz","Leipzig")) %>%
  feols(data=., fml=pop_growth ~ log(pop_2015) + labour_force + log(employed) + 
        log(purchasing_power) + distance_supermarkets + 
       distance_pharmacy + Erreichbarkeit.von.Mittelzentren + Erreichbarkeit.von.Oberzentren + 
         Siedlungs..und.Verkehrsfläche + Erholungsfläche + degurba) %>% 
  etable()

saxony %>% mutate(pop_growth = log(pop_2020/pop_2015)) %>% 
  filter(!ORTSNAME %in% c("Dresden","Chemnitz","Leipzig")) %>%
  feols(data=., fml=pop_growth ~ log(pop_2015) + labour_force + log(employed) + 
          log(purchasing_power) + distance_supermarkets + 
          distance_pharmacy + Erreichbarkeit.von.Mittelzentren + 
           degurba) %>% 
  etable()



saxony %>% mutate(pop_growth = pop_2020/pop_2015) %>% 
  filter(!ORTSNAME %in% c("Dresden","Chemnitz","Leipzig", "Zwickau")) %>%
conleyreg(data=., formula=pop_growth ~ pop_2015 + log(employed*pop_2015) + 
            log(purchasing_power) + distance_supermarkets + 
            distance_pharmacy + Erreichbarkeit.von.Mittelzentren + 
            Siedlungs..und.Verkehrsfläche + Erholungsfläche + degurba, 25) %>% 
  print()



saxony_spatial <- saxony %>% mutate(pop_growth = log(pop_2020/pop_2015)) %>% 
  filter(!ORTSNAME %in% c("Dresden","Chemnitz","Leipzig", "Zwickau"))

list.queen<-poly2nb(saxony_spatial, queen=TRUE)
W<-nb2listw(list.queen, style="W", zero.policy=TRUE)
W

lagsarlm(data=saxony_spatial, formula=pop_growth ~ log(pop_2015) + log(employed) + 
           log(purchasing_power) + distance_supermarkets + 
           distance_pharmacy + Erreichbarkeit.von.Mittelzentren + 
           Siedlungs..und.Verkehrsfläche + Erholungsfläche + aging + degurba, W) %>% summary()
