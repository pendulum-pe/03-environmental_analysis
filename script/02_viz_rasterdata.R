library(tidyverse)
library(ggplot2)
library(raster)
library(cptcity)
library(extrafont)
library(cowplot)
library(sf)
source("viz_no2.R")
# 1.Data reading  ---------------------------------------------------------

# Raster data
all_raster <- paste0("../Download_data/", list.files(path = "../Download_data/", pattern = ".tif$"))
index_lima <- which(str_detect(all_raster, pattern = "li"))
index_ica <- which(str_detect(all_raster, pattern = "ic"))
index_arequipa <- which(str_detect(all_raster, pattern = "ar"))
index_iquitos <- which(str_detect(all_raster, pattern = "iq"))
index_piura <- which(str_detect(all_raster, pattern = "pi"))


limadb <- stack(all_raster[index_lima])
icadb <- stack(all_raster[index_ica])
arequipadb <- stack(all_raster[index_arequipa])
iquitosdb <- stack(all_raster[index_iquitos])
piuradb <- stack(all_raster[index_piura])

# Vector data
all_vector <- paste0("../study_area/", list.files(path = "../study_area/", pattern = ".gpkg$"))
index_lima <- which(str_detect(all_vector, pattern = "lima"))
index_ica <- which(str_detect(all_vector, pattern = "ica"))
index_arequipa <- which(str_detect(all_vector, pattern = "arequipa"))
index_iquitos <- which(str_detect(all_vector, pattern = "iquitos"))
index_piura <- which(str_detect(all_vector, pattern = "piura"))

# districts
lima_sf <- st_read(all_vector[index_lima])
ica_sf <- st_read(all_vector[index_ica])
arequipa_sf <- st_read(all_vector[index_arequipa])
iquitos_sf <- st_read(all_vector[index_iquitos])
piura_sf <- st_read(all_vector[index_piura])

# Border of region
lima_reg <- lima_sf %>% summarise()
ica_reg <- ica_sf %>% summarise()
arequipa_reg <- arequipa_sf %>% summarise()
iquitos_reg <- iquitos_sf %>% summarise()
piura_reg <- piura_sf %>% summarise()

# 2.Smothing --------------------------------------------------------------

smothing_df <- function(x, y) {
  # reproject and resample
  new_smooth <- x %>%
    projectRaster(
      crs = "+proj=utm +zone=18 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
      res = 100
    )

  # raster to dataframe
  new_df <- new_smooth %>%
    mask(y %>% st_transform(32718)) %>%
    as.data.frame(xy = T) %>%
    drop_na()
  names(new_df) <- c(
    "Este", "Norte", "NO2_2020_2", "NO2_2021_2",
    "NO2_2020_8", "NO2_2021_8"
  )
  return(new_df)
}

arequipadb <- smothing_df(arequipadb, arequipa_sf)
icadb <- smothing_df(icadb, ica_sf)
limadb <- smothing_df(limadb, lima_sf)
iquitosdb <- smothing_df(iquitosdb, iquitos_sf)
piuradb <- smothing_df(piuradb, piura_sf)


# 3.Arequipa --------------------------------------------------------------
are_2020_2 <- viz_no2(
  data = arequipadb,
  sf = arequipa_sf,
  reg = arequipa_reg,
  col = "NO2_2020_2",
  pal = "inferno",
  title = "Feb-2020",
  subtitle = "Pendulum"
)

are_2020_8 <- viz_no2(
  data = arequipadb,
  sf = arequipa_sf,
  reg = arequipa_reg,
  col = "NO2_2020_8",
  pal = "inferno",
  title = "Ago-2020",
  subtitle = "Pendulum"
)

are_2021_2 <- viz_no2(
  data = arequipadb,
  sf = arequipa_sf,
  reg = arequipa_reg,
  col = "NO2_2021_2",
  pal = "inferno",
  title = "Feb-2021",
  subtitle = "Pendulum"
)

are_2021_8 <- viz_no2(
  data = arequipadb,
  sf = arequipa_sf,
  reg = arequipa_reg,
  col = "NO2_2021_8",
  pal = "inferno",
  title = "Ago-2021",
  subtitle = "Pendulum"
)

# Dif
newdataset <- arequipadb %>%
  mutate(
    d_2120_2 = NO2_2021_2 - NO2_2020_2,
    d_2120_8 = NO2_2021_8 - NO2_2020_8,
    d_2020_8_2 = NO2_2020_8 - NO2_2020_2,
    d_2020_8_2 = NO2_2021_8 - NO2_2021_2
  )

delta_2120_2 <- viz_no2_di(
  data = newdataset,
  sf = arequipa_sf,
  reg = arequipa_reg,
  col = "d_2120_2",
  title = "ΔFeb | 2021-2020",
  subtitle = "Pendulum"
)

delta_2120_8 <- viz_no2_di(
  data = newdataset,
  sf = arequipa_sf,
  reg = arequipa_reg,
  col = "d_2120_8",
  title = "Δ Ago | 2021-2020",
  subtitle = "Pendulum"
)

delta_2020_8_2 <- viz_no2_di(
  data = newdataset,
  sf = arequipa_sf,
  reg = arequipa_reg,
  col = "d_2020_8_2",
  title = "Δ Ago - Feb | 2020",
  subtitle = "Pendulum"
)

delta_2021_8_2 <- viz_no2_di(
  data = newdataset,
  sf = arequipa_sf,
  reg = arequipa_reg,
  col = "d_2020_8_2",
  title = "Δ Ago - Feb | 2021",
  subtitle = "Pendulum"
)

plot_grid(are_2020_2,are_2020_8,delta_2020_8_2,
          are_2020_2,are_2021_2,delta_2120_2,
          are_2020_8,are_2021_8,delta_2120_8)

ggsave(
  filename = "plots/arequipa.png",
  plot = last_plot(),
  width = 17, height = 12,bg = "white"
)

dir.create("arequipa")

plots <- list(are_2020_2,are_2020_8,delta_2020_8_2,
  are_2020_2,are_2021_2,delta_2120_2,
  are_2020_8,are_2021_8,delta_2120_8)

names <- c("are_2020_2","are_2020_8","delta_2020_8_2",
           "are_2020_2","are_2021_2","delta_2120_2",
           'are_2020_8','are_2021_8','delta_2120_8')

for (i in 1:length(plots)) {
  g1 <- plots[[i]]
  ggsave(
    filename = paste0("./arequipa/",names[i],".png"),
    plot = g1,
    width = 7, height = 5,bg = "white"
  )
  }



# 4.Ica -------------------------------------------------------------------

ica_2020_2 <- viz_no2(
  data = icadb,
  sf = ica_sf,
  reg = ica_reg,
  col = "NO2_2020_2",
  pal = "inferno",
  title = "Feb-2020",
  subtitle = "Pendulum"
)

ica_2020_8 <- viz_no2(
  data = icadb,
  sf = ica_sf,
  reg = ica_reg,
  col = "NO2_2020_8",
  pal = "inferno",
  title = "Ago-2020",
  subtitle = "Pendulum"
)

ica_2021_2 <- viz_no2(
  data = icadb,
  sf = ica_sf,
  reg = ica_reg,
  col = "NO2_2021_2",
  pal = "inferno",
  title = "Feb-2021",
  subtitle = "Pendulum"
)

ica_2021_8 <- viz_no2(
  data = icadb,
  sf = ica_sf,
  reg = ica_reg,
  col = "NO2_2021_8",
  pal = "inferno",
  title = "Ago-2021",
  subtitle = "Pendulum"
)

# Dif

newdataset <- icadb %>%
  mutate(
    d_2120_2 = NO2_2021_2 - NO2_2020_2,
    d_2120_8 = NO2_2021_8 - NO2_2020_8,
    d_2020_8_2 = NO2_2020_8 - NO2_2020_2,
    d_2020_8_2 = NO2_2021_8 - NO2_2021_2
  )

ica_delta_2120_2 <- viz_no2_di(
  data = newdataset,
  sf = ica_sf,
  reg = ica_reg,
  col = "d_2120_2",
  title = "ΔFeb | 2021-2020",
  subtitle = "Pendulum"
)

ica_delta_2120_8 <- viz_no2_di(
  data = newdataset,
  sf = ica_sf,
  reg = ica_reg,
  col = "d_2120_8",
  title = "Δ Ago | 2021-2020",
  subtitle = "Pendulum"
)

ica_delta_2020_8_2 <- viz_no2_di(
  data = newdataset,
  sf = ica_sf,
  reg = ica_reg,
  col = "d_2020_8_2",
  title = "Δ Ago - Feb | 2020",
  subtitle = "Pendulum"
)

ica_delta_2021_8_2 <- viz_no2_di(
  data = newdataset,
  sf = ica_sf,
  reg = ica_reg,
  col = "d_2020_8_2",
  title = "Δ Ago - Feb | 2021",
  subtitle = "Pendulum"
)


plot_grid(ica_2020_2,ica_2020_8,ica_delta_2020_8_2,
          ica_2020_2,ica_2021_2,ica_delta_2120_2,
          ica_2020_8,ica_2021_8,ica_delta_2120_8)

ggsave(
  filename = "plots/ica.png",
  plot = last_plot(),
  width = 15, height = 15,bg = "white"
)

dir.create("ica")

plots <- list(ica_2020_2,ica_2020_8,ica_delta_2020_8_2,
              ica_2020_2,ica_2021_2,ica_delta_2120_2,
              ica_2020_8,ica_2021_8,ica_delta_2120_8)

names <- c("ica_2020_2","ica_2020_8","ica_2020_8_2",
           "ica_2020_2","ica_2021_2","ica_2120_2",
           'ica_2020_8','ica_2021_8','ica_2120_8')

for (i in 1:length(plots)) {
  g1 <- plots[[i]]
  ggsave(
    filename = paste0("./ica/",names[i],".png"),
    plot = g1,
    width = 5, height = 6,bg = "white"
  )
}


# 5. Lima -----------------------------------------------------------------

lima_2020_2 <- viz_no2(
  data = limadb,
  sf = lima_sf,
  reg = lima_reg,
  col = "NO2_2020_2",
  pal = "inferno",
  title = "Feb-2020",
  subtitle = "Pendulum"
)

lima_2020_8 <- viz_no2(
  data = limadb,
  sf = lima_sf,
  reg = lima_reg,
  col = "NO2_2020_8",
  pal = "inferno",
  title = "Ago-2020",
  subtitle = "Pendulum"
)

lima_2021_2 <- viz_no2(
  data = limadb,
  sf = lima_sf,
  reg = lima_reg,
  col = "NO2_2021_2",
  pal = "inferno",
  title = "Feb-2021",
  subtitle = "Pendulum"
)

lima_2021_8 <- viz_no2(
  data = limadb,
  sf = lima_sf,
  reg = lima_reg,
  col = "NO2_2021_8",
  pal = "inferno",
  title = "Ago-2021",
  subtitle = "Pendulum"
)

# Dif

newdataset <- limadb %>%
  mutate(
    d_2120_2 = NO2_2021_2 - NO2_2020_2,
    d_2120_8 = NO2_2021_8 - NO2_2020_8,
    d_2020_8_2 = NO2_2020_8 - NO2_2020_2,
    d_2020_8_2 = NO2_2021_8 - NO2_2021_2
  )


lima_delta_2120_2 <- viz_no2_di(
  data = newdataset,
  sf = lima_sf,
  reg = lima_reg,
  col = "d_2120_2",
  title = "ΔFeb | 2021-2020",
  subtitle = "Pendulum"
)

lima_delta_2120_8 <- viz_no2_di(
  data = newdataset,
  sf = lima_sf,
  reg = lima_reg,
  col = "d_2120_8",
  title = "Δ Ago | 2021-2020",
  subtitle = "Pendulum"
)

lima_delta_2020_8_2 <- viz_no2_di(
  data = newdataset,
  sf = lima_sf,
  reg = lima_reg,
  col = "d_2020_8_2",
  title = "Δ Ago - Feb | 2020",
  subtitle = "Pendulum"
)

lima_delta_2021_8_2 <- viz_no2_di(
  data = newdataset,
  sf = lima_sf,
  reg = lima_reg,
  col = "d_2020_8_2",
  title = "Δ Ago - Feb | 2021",
  subtitle = "Pendulum"
)


plot_grid(lima_2020_2,lima_2020_8,lima_delta_2020_8_2,
          lima_2020_2,lima_2021_2,lima_delta_2120_2,
          lima_2020_8,lima_2021_8,lima_delta_2120_8)

ggsave(
  filename = "plots/lima.png",
  plot = last_plot(),
  width = 16, height = 13,bg = "white"
)


dir.create("lima")
plots <- list(lima_2020_2,lima_2020_8,lima_delta_2020_8_2,
              lima_2020_2,lima_2021_2,lima_delta_2120_2,
              lima_2020_8,lima_2021_8,lima_delta_2120_8)

names <- c("lima_2020_2","lima_2020_8","lima_2020_8_2",
           "lima_2020_2","lima_2021_2","lima_2120_2",
           'lima_2020_8','lima_2021_8','lima_2120_8')

for (i in 1:length(plots)) {
  g1 <- plots[[i]]
  ggsave(
    filename = paste0("./lima/",names[i],".png"),
    plot = g1,
    width = 5, height = 6,bg = "white"
  )
}

# 6. Iquitos --------------------------------------------------------------
iqui_2020_2 <- viz_no2(
  data = iquitosdb,
  sf = iquitos_sf,
  reg = iquitos_reg,
  col = "NO2_2020_2",
  pal = "inferno",
  title = "Feb-2020",
  subtitle = "Pendulum"
)

iqui_2020_8 <- viz_no2(
  data = iquitosdb,
  sf = iquitos_sf,
  reg = iquitos_reg,
  col = "NO2_2020_8",
  pal = "inferno",
  title = "Ago-2020",
  subtitle = "Pendulum"
)

iqui_2021_2 <- viz_no2(
  data = iquitosdb,
  sf = iquitos_sf,
  reg = iquitos_reg,
  col = "NO2_2021_2",
  pal = "inferno",
  title = "Feb-2021",
  subtitle = "Pendulum"
)

iqui_2021_8 <- viz_no2(
  data = iquitosdb,
  sf = iquitos_sf,
  reg = iquitos_reg,
  col = "NO2_2021_8",
  pal = "inferno",
  title = "Ago-2021",
  subtitle = "Pendulum"
)

# Dif

newdataset <- iquitosdb %>%
  mutate(
    d_2120_2 = NO2_2021_2 - NO2_2020_2,
    d_2120_8 = NO2_2021_8 - NO2_2020_8,
    d_2020_8_2 = NO2_2020_8 - NO2_2020_2,
    d_2020_8_2 = NO2_2021_8 - NO2_2021_2
  )


iqui_delta_2120_2 <- viz_no2_di(
  data = newdataset,
  sf = iquitos_sf,
  reg = iquitos_reg,
  col = "d_2120_2",
  title = "ΔFeb | 2021-2020",
  subtitle = "Pendulum"
)

iqui_delta_2120_8 <- viz_no2_di(
  data = newdataset,
  sf = iquitos_sf,
  reg = iquitos_reg,
  col = "d_2120_8",
  title = "Δ Ago | 2021-2020",
  subtitle = "Pendulum"
)

iqui_delta_2020_8_2 <- viz_no2_di(
  data = newdataset,
  sf = iquitos_sf,
  reg = iquitos_reg,
  col = "d_2020_8_2",
  title = "Δ Ago - Feb | 2020",
  subtitle = "Pendulum"
)

iqui_delta_2021_8_2 <- viz_no2_di(
  data = newdataset,
  sf = iquitos_sf,
  reg = iquitos_reg,
  col = "d_2020_8_2",
  title = "Δ Ago - Feb | 2021",
  subtitle = "Pendulum"
)


plot_grid(iqui_2020_2,iqui_2020_8,iqui_delta_2020_8_2,
          iqui_2020_2,iqui_2021_2,iqui_delta_2120_2,
          iqui_2020_8,iqui_2021_8,iqui_delta_2120_8)

ggsave(
  filename = "plots/iquitos.png",
  plot = last_plot(),
  width = 16, height = 13,bg = "white"
)

dir.create("iquitos")
plots <- list(iqui_2020_2,iqui_2020_8,iqui_delta_2020_8_2,
              iqui_2020_2,iqui_2021_2,iqui_delta_2120_2,
              iqui_2020_8,iqui_2021_8,iqui_delta_2120_8)

names <- c("iqui_2020_2","iqui_2020_8","iqui_2020_8_2",
           "iqui_2020_2","iqui_2021_2","iqui_2120_2",
           'iqui_2020_8','iqui_2021_8','iqui_2120_8')

for (i in 1:length(plots)) {
  g1 <- plots[[i]]
  ggsave(
    filename = paste0("./iquitos/",names[i],".png"),
    plot = g1,
    width = 5, height = 6,bg = "white"
  )
}

# 7.Piura -----------------------------------------------------------------
piura_2020_2 <- viz_no2(
  data = piuradb,
  sf = piura_sf,
  reg = piura_reg,
  col = "NO2_2020_2",
  pal = "inferno",
  title = "Feb-2020",
  subtitle = "Pendulum"
)

piura_2020_8 <- viz_no2(
  data = piuradb,
  sf = piura_sf,
  reg = piura_reg,
  col = "NO2_2020_8",
  pal = "inferno",
  title = "Ago-2020",
  subtitle = "Pendulum"
)

piura_2021_2 <- viz_no2(
  data = piuradb,
  sf = piura_sf,
  reg = piura_reg,
  col = "NO2_2021_2",
  pal = "inferno",
  title = "Feb-2021",
  subtitle = "Pendulum"
)

piura_2021_8 <- viz_no2(
  data = piuradb,
  sf = piura_sf,
  reg = piura_reg,
  col = "NO2_2021_8",
  pal = "inferno",
  title = "Ago-2021",
  subtitle = "Pendulum"
)

# Dif

newdataset <- piuradb %>%
  mutate(
    d_2120_2 = NO2_2021_2 - NO2_2020_2,
    d_2120_8 = NO2_2021_8 - NO2_2020_8,
    d_2020_8_2 = NO2_2020_8 - NO2_2020_2,
    d_2020_8_2 = NO2_2021_8 - NO2_2021_2
  )


piura_delta_2120_2 <- viz_no2_di(
  data = newdataset,
  sf = piura_sf,
  reg = piura_reg,
  col = "d_2120_2",
  title = "ΔFeb | 2021-2020",
  subtitle = "Pendulum"
)

piura_delta_2120_8 <- viz_no2_di(
  data = newdataset,
  sf = piura_sf,
  reg = piura_reg,
  col = "d_2120_8",
  title = "Δ Ago | 2021-2020",
  subtitle = "Pendulum"
)

piura_delta_2020_8_2 <- viz_no2_di(
  data = newdataset,
  sf = piura_sf,
  reg = piura_reg,
  col = "d_2020_8_2",
  title = "Δ Ago - Feb | 2020",
  subtitle = "Pendulum"
)

piura_delta_2021_8_2 <- viz_no2_di(
  data = newdataset,
  sf = piura_sf,
  reg = piura_reg,
  col = "d_2020_8_2",
  title = "Δ Ago - Feb | 2021",
  subtitle = "Pendulum"
)


plot_grid(piura_2020_2,piura_2020_8,piura_delta_2020_8_2,
          piura_2020_2,piura_2021_2,piura_delta_2120_2,
          piura_2020_8,piura_2021_8,piura_delta_2120_8)

ggsave(
  filename = "plots/piura.png",
  plot = last_plot(),
  width = 15, height = 13,bg = "white"
)


dir.create("piura")
plots <- list(piura_2020_2,piura_2020_8,piura_delta_2020_8_2,
              piura_2020_2,piura_2021_2,piura_delta_2120_2,
              piura_2020_8,piura_2021_8,piura_delta_2120_8)

names <- c("piura_2020_2","piura_2020_8","piura_2020_8_2",
           "piura_2020_2","piura_2021_2","piura_2120_2",
           'piura_2020_8','piura_2021_8','piura_2120_8')

for (i in 1:length(plots)) {
  g1 <- plots[[i]]
  ggsave(
    filename = paste0("./piura/",names[i],".png"),
    plot = g1,
    width = 5, height = 6,bg = "white"
  )
}

# 8.Poster ----------------------------------------------------------------

peru <- st_read("crudo/peru.gpkg")
peru %>%
  ggplot() +
  geom_sf(fill = NA) +
  theme(
    element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  )

ggsave(
  filename = "svg/peru.svg", plot = last_plot(),
  width = 12, height = 12
)
