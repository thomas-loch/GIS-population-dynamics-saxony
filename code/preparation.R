


# read municipality polygons
saxony <- st_read(here("data/raw/vwg_sn/gem.shp"), quiet=TRUE)

# read municipality details
INKAR_data <- read.csv2(here("data/raw/INKAR.csv"))

# read population raster information
pop_2015 <- raster(here("data/raw/gpw_v4_population_count_rev11_2015_30_sec.tif"))
pop_2020 <- raster(here("data/raw/gpw_v4_population_count_rev11_2020_30_sec.tif"))


# data cleaning
INKAR_data <- INKAR_data %>% mutate( 
  unemployed = transform_str_num(Arbeitslose),
  purchasing_power = transform_str_num(Kaufkraft),
  purchasing_power_retail = transform_str_num(Einzelhandelsrelevante.Kaufkraft),
  distance_supermarkets = transform_str_num(Nahversorgung.Supermärkte.Durchschnittsdistanz),
  distance_pharmacy = transform_str_num(Nahversorgung.Apotheken.Durchschnittsdistanz), 
  reachability_middle_city = Erreichbarkeit.von.Mittelzentren, 
  reachability_metropolitan = Erreichbarkeit.von.Oberzentren,
  Raumeinheit = str_split_i(Raumeinheit,", ",1), # correct for ", Stadt"
  Raumeinheit = str_split_i(Raumeinheit," /",1), # correct for listing of Sorbian city names
)

INKAR_data <- INKAR_data %>% mutate( 
  settlement_space_share = Siedlungs..und.Verkehrsfläche,
#  settlement_space_share_2017 = Siedlungs..und.Verkehrsfläche.1,  
#  settlement_space_share_2018 = Siedlungs..und.Verkehrsfläche.2,
#  settlement_space_share_2019 = Siedlungs..und.Verkehrsfläche.3,  
#  settlement_space_share_2020 = Siedlungs..und.Verkehrsfläche.4,  
#  settlement_space_share_2021 = Siedlungs..und.Verkehrsfläche.5,
#  settlement_space_share_2022 = Siedlungs..und.Verkehrsfläche.6,  
  recreations_space_share = Erholungsfläche
#  recreations_space_share_2017 = Erholungsfläche.1,
#  recreations_space_share_2018 = Erholungsfläche.2,
#  recreations_space_share_2019 = Erholungsfläche.3,
#  recreations_space_share_2020 = Erholungsfläche.4,
#  recreations_space_share_2021 = Erholungsfläche.5,
#  recreations_space_share_2022 = Erholungsfläche.6,
)



INKAR_data <- INKAR_data %>% na.omit()

ggplot(INKAR_data) + 
  geom_point(aes(y=settlement_space_share_2016, x=1)) +
  geom_point(aes(y=settlement_space_share_2017, x=2)) +
  geom_point(aes(y=settlement_space_share_2018, x=3)) +
  geom_point(aes(y=settlement_space_share_2018, x=4)) +
  geom_point(aes(y=settlement_space_share_2019, x=5)) +
  geom_point(aes(y=settlement_space_share_2020, x=6)) +
  geom_point(aes(y=settlement_space_share_2021, x=7)) +
  geom_point(aes(y=settlement_space_share_2022, x=8)) 
  

# select only relevant variables
INKAR_data <- INKAR_data %>% 
  dplyr::select(unemployed, purchasing_power, purchasing_power_retail, distance_supermarkets, 
                distance_pharmacy, Raumeinheit, reachability_middle_city, reachability_metropolitan, 
                settlement_space_share, 
          #      settlement_space_share_2017, settlement_space_share_2018, 
          #      settlement_space_share_2019, settlement_space_share_2020, 
          #      settlement_space_share_2021, settlement_space_share_2022, 
                recreations_space_share
          #      recreations_space_share_2017, recreations_space_share_2018, 
          #      recreations_space_share_2019, recreations_space_share_2020, 
          #      recreations_space_share_2021, recreations_space_share_2022
  )



# merge data sets
saxony <- left_join(saxony, INKAR_data, by=c("ORTSNAME"="Raumeinheit"))



# first check, if it worked
#ggplot() + geom_sf(data=saxony, aes(fill=purchasing_power)) +
#  scale_fill_viridis_c() + theme_minimal() 



# extract population raster data
saxony$pop_2015 <- exact_extract(pop_2015, saxony, fun="sum", progress=FALSE)
saxony$pop_2020 <- exact_extract(pop_2020, saxony, fun="sum", progress=FALSE)



# check extraction results
#ggplot() + geom_sf(data=saxony, aes(fill=log(pop_2015))) + 
#  scale_fill_viridis_c() + theme_minimal() 


saxony <- saxony %>% mutate(
  pop_growth = log(pop_2020/pop_2015),
  unemployed_log = log(unemployed), 
  pop_2015_log = log(pop_2015),
  pop_2020_log = log(pop_2020)
)
