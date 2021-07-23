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
study_area <- st_read('crudo/piura.gpkg')
study_area_ee <- study_area %>% 
  st_bbox() %>% 
  st_as_sfc() %>% 
  sf_as_ee()

# 02- Get SO2 information -------------------------------------------------
# Edite only start_* and end_* date
start_year <- 2020
end_year <- 2020
start_month <- 04
end_month <- 04

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

so2_bands <- so2_list$
  filter(ee$Filter$inList("month", c(1:12)))$
  map(function(x){x$clip(study_area_ee)})$toBands()

so2_to_raster <- ee_as_raster(
  image = so2_bands,
  maxPixels = 10e12, 
  region = study_area_ee,
  scale = 5000,
  dsn = paste0('SO2_piura',start_year,'_',start_month,'.tif'))
