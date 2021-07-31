library(tidyverse)
library(ggspatial)
library(rasterVis)
library(patchwork)
library(cptcity)
library(raster)
library(ggplot2)
library(rgee)
library(sf)

ee_Initialize(email = 'antonybarja8@gmail.com',drive = T)

# 1.Study area ------------------------------------------------------------

list_names <- paste0('./crudo/',list.files(path = 'crudo/',pattern = '.gpkg$'))
study_area <- lapply(list_names,st_read)
study_area_ee <- list()

for (i in 1:length(study_area)) {
  sf_to_ee <- study_area[[i]] %>% 
    st_bbox() %>% 
    st_as_sfc() %>% 
    sf_as_ee() 
  study_area_ee[[i]] <- sf_to_ee
}

# 02- Get SO2 information -------------------------------------------------
# Edite only start_* and end_* date
start_year <- 2020
end_year <- 2020
start_month <- 8
end_month <- 11

months <- ee$List$sequence(start_month, end_month)
years <- ee$List$sequence(start_year, end_year)
so2db <- ee$ImageCollection("COPERNICUS/S5P/NRTI/L3_SO2")$
  select("SO2_column_number_density")

so2_list <- ee$
  ImageCollection$
  fromImages(years$map(
    ee_utils_pyfunc(function(y) {
      months$map(ee_utils_pyfunc(
        function(m) {
          so2db$
            filter(ee$Filter$calendarRange(y, y, "year"))$
            filter(ee$Filter$calendarRange(m, m, "month"))$
            max()$
            rename("so2")$
            set("year", y)$
            set("month", m)
        }
      ))
    })
  )$flatten())

so2_get_img <- list()

for(i in 1:length(study_area_ee)){
  so2_bands <- so2_list$
    filter(ee$Filter$inList("month", c(1:12)))$
    map(function(x){x$clip(study_area_ee[[i]])})$
    mean() 
  so2_get_img[[i]] <- so2_bands 
}

for (i in 1:length(so2_get_img)) {
  ee_as_raster(
    image = so2_get_img[[i]],
    maxPixels = 10e12, 
    region = study_area_ee[[i]],
    scale = 5500,
    dsn = paste0(
      'SO2_',
      substr(list.files(path = 'crudo/',pattern = '.gpkg$'),1,2)[i],
      '_',
      start_month,
      '_to_',
      end_month,
      '_',
      start_year,
      '.tif')
    )
}
  

