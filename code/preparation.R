


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
  distance_supermarkets = transform_str_num(Nahversorgung.Supermärkte.Durchschnittsdistanz),
  distance_pharmacy = transform_str_num(Nahversorgung.Apotheken.Durchschnittsdistanz), 
  Raumeinheit = str_split_i(Raumeinheit,", ",1), # correct for ", Stadt"
  Raumeinheit = str_split_i(Raumeinheit," /",1), # correct for listing of sorbisch city names
)

# select only relevant variables
INKAR_data <- INKAR_data %>% 
  dplyr::select(unemployed, purchasing_power, distance_supermarkets, distance_pharmacy, Raumeinheit)



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
