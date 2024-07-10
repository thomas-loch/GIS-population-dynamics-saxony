library(tidyverse) 
library(viridisLite)
library(terra)
library(sf)
library(raster)
library(exactextractr)
library(here)
library(modelsummary)
library(fixest)
library(conleyreg)
library(spatialreg)
library(spdep)
library(modelsummary)




# function to clean the data -> see preparation.R
transform_str_num <- function(number){
  number = substr(number, 1, nchar(number) - 3)
  number_str = sub(".","", number, fixed=TRUE)
  as.integer(number_str)
}
