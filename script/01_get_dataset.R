library(tidyverse) # pkg for data science
library(rgee)      # pkg for spatial analysis with Earth Engine
library(sf)        # pkg for spatial data - vector
ee_Initialize()    # User of Earth Engine


# 1.Reading spatial data - study area -------------------------------------

list_names <- paste0(
  '../study_area/',
  list.files(
    path = '../study_area/',
    pattern = '.gpkg$'
    )
  )

study_area <- lapply(list_names,st_read)
study_area_ee <- list()
# sf to featurecollection 
for (i in 1:length(study_area)) {
  sf_to_ee <- study_area[[i]] %>% 
    st_bbox() %>% 
    st_as_sfc() %>% 
    sf_as_ee() 
  study_area_ee[[i]] <- sf_to_ee
}


# 2.Get NO2 information ---------------------------------------------------
# Edited only start_* and end_* date
start_year <- 2021
end_year <- 2021
start_month <- 08
end_month <- 08

months <- ee$List$sequence(start_month, end_month)
years <- ee$List$sequence(start_year, end_year)
so2db <- ee$ImageCollection("COPERNICUS/S5P/NRTI/L3_NO2")$
  select("NO2_column_number_density")

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
  if(0 == dir.exists("../Download_data")) {
    dir.create("../Download_data")
  } 
  ee_as_raster(
    image = so2_get_img[[i]],
    maxPixels = 10e12, 
    region = study_area_ee[[i]],
    scale = 5500,
    dsn = paste0(
      '../Download_data/',
      substr(list.files(path = '../study_area/',pattern = '.gpkg$'),1,2)[i],
      '_',
      start_month,
      '_to_',
      end_month,
      '_',
      start_year,
      '.tif')
    )
}
  

