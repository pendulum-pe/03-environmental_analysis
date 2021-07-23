library(ggplot2)
library(raster)
library(cptcity)
library(extrafont)
library(patchwork)


# 1.Data reading  ---------------------------------------------------------

# Raster data 
all_raster <- paste0('SO2/',list.files(path = './SO2/',pattern = '.tif$'))
index_lima <- which(str_detect(all_raster,pattern = 'lima'))
index_ica  <- which(str_detect(all_raster,pattern = 'ica'))
index_arequipa <- which(str_detect(all_raster,pattern = 'arequipa'))
index_iquitos  <- which(str_detect(all_raster,pattern = 'iquitos'))
index_piura  <- which(str_detect(all_raster,pattern = 'piura'))


limadb <- stack(all_raster[index_lima])
icadb <- stack(all_raster[index_ica])
arequipadb <- stack(all_raster[index_arequipa])
iquitosdb <- stack(all_raster[index_iquitos])
piuradb <- stack(all_raster[index_piura])

# Vector data
all_vector <- paste0('crudo/',list.files(path = './crudo/',pattern = '.gpkg$'))
index_lima <- which(str_detect(all_vector,pattern = 'lima'))
index_ica <- which(str_detect(all_vector,pattern = 'ica'))
index_arequipa <- which(str_detect(all_vector,pattern = 'arequipa'))
index_iquitos <- which(str_detect(all_vector,pattern = 'iquitos'))
index_piura <- which(str_detect(all_vector,pattern = 'piura'))


lima_sf <- st_read(all_vector[index_lima])
ica_sf  <- st_read(all_vector[index_ica])
arequipa_sf <- st_read(all_vector[index_arequipa])
iquitos_sf  <- st_read(all_vector[index_iquitos])
piura_sf  <- st_read(all_vector[index_piura])


# 2.Smothing --------------------------------------------------------------

smothing_df <- function(x,y){
  # reproject and resample
  new_smooth <- x %>% 
    projectRaster(
      crs = '+proj=utm +zone=18 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs',
      res = 100) 
  
  # raster to dataframe  
  new_df <- new_smooth %>% 
    mask(y %>% st_transform(32718)) %>% 
    as.data.frame(xy = T) %>% 
    drop_na()
  names(new_df) <- c("Este","Norte","so2_2020","so2_2021")
  return(new_df)
}

arequipadb <- smothing_df(arequipadb, arequipa_sf)
icadb <- smothing_df(icadb,ica_sf)
limadb <- smothing_df(limadb,lima_sf)
iquitosdb <- smothing_df(iquitosdb,iquitos_sf)
piuradb <- smothing_df(piuradb,piura_sf)


# 3.Arequipa --------------------------------------------------------------

arequipa_2020 <- arequipadb %>% 
  ggplot() + geom_raster(aes(Este,Norte, fill = so2_2020)) +  
  geom_sf(data = arequipa_sf %>% st_transform(32718),
          fill = 'transparent',color = 'white',lwd=0.4) + 
  scale_fill_viridis_c(
    name = expression(paste(S0[2],group("(",paste(mol/m^2), ")"))),
    option = 'inferno',
    limits = c(0,0.017),
    breaks = seq(0,0.017,length.out = 4)) + 
  labs(x = '', y = '') + 
  theme_minimal(12) + 
  labs(
    title = 'SO2 Emission in Arequipa-Peru {2020-04}',
    subtitle = 'Source: Sentinel-5P NRTI SO2 (working with rgee)',
    caption = '@antony_barja, Antony Barja, 2021-07-22'
  ) + 
  theme(
    plot.margin = unit(rep(.5, 4), "cm"),
    plot.title.position = "plot",
    plot.title = element_text(size = 28,family = 'Roboto',face = 'bold'),
    plot.subtitle = element_text(color = "gray40"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 24),
    plot.caption = element_text(family = "Inconsolata")
  )


arequipa_2021 <- arequipadb %>% 
  ggplot() + geom_raster(aes(Este,Norte, fill = so2_2021)) +  
  geom_sf(data = arequipa_sf %>% st_transform(32718),
          fill = 'transparent',color = 'white',lwd=0.4) + 
  scale_fill_viridis_c(
    name = expression(paste(S0[2],group("(",paste(mol/m^2), ")"))),
    option = 'inferno',
    limits = c(0,0.017),
    breaks = seq(0,0.017,length.out = 4)) + 
  labs(x = '', y = '') + 
  theme_minimal(12) + 
  labs(
    title = 'SO2 Emission in Arequipa-Peru {2021-04}',
    subtitle = 'Source: Sentinel-5P NRTI SO2 (working with rgee)',
    caption = '@antony_barja, Antony Barja, 2021-07-22'
  ) + 
  theme(
    plot.margin = unit(rep(.5, 4), "cm"),
    plot.title.position = "plot",
    plot.title = element_text(size = 28,family = 'Roboto',face = 'bold'),
    plot.subtitle = element_text(color = "gray40"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 24),
    plot.caption = element_text(family = "Inconsolata")
  )

plot_grid(arequipa_2020,arequipa_2021,ncol = 1)
ggsave(filename = 'svg/arequipa.svg',plot = last_plot(),
       width = 12,height = 12)

# 4.Ica -------------------------------------------------------------------

ica_2020 <- icadb %>% 
  ggplot() + geom_raster(aes(Este,Norte, fill = so2_2020)) +  
  geom_sf(data = ica_sf %>% st_transform(32718),
          fill = 'transparent',color = 'white',lwd=0.4) + 
  scale_fill_viridis_c(
    name = expression(paste(S0[2],group("(",paste(mol/m^2), ")"))),
    option = 'inferno',
    limits = c(0,0.0073),
    breaks = seq(0,0.0073,length.out = 5)) +  
  labs(x = '', y = '') + 
  theme_minimal(12) +
  labs(
    title = 'SO2 Emission in Ica-Peru {2020-04}',
    subtitle = 'Source: Sentinel-5P NRTI SO2 (working with rgee)',
    caption = '@antony_barja, Antony Barja, 2021-07-22'
  ) + 
  theme(
    plot.margin = unit(rep(.5, 4), "cm"),
    plot.title.position = "plot",
    plot.title = element_text(size = 28,family = 'Roboto',face = 'bold'),
    plot.subtitle = element_text(color = "gray40"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 24),
    plot.caption = element_text(family = "Inconsolata")
  )

ica_2021 <- icadb %>% 
  ggplot() + geom_raster(aes(Este,Norte, fill = so2_2021)) +  
  geom_sf(data = ica_sf %>% st_transform(32718),
          fill = 'transparent',color = 'white',lwd=0.4) + 
  scale_fill_viridis_c(
    name = expression(paste(S0[2],group("(",paste(mol/m^2), ")"))),
    option = 'inferno',
    limits = c(0,0.0073),
    breaks = seq(0,0.0073,length.out = 5)) + 
  labs(x = '', y = '') + 
  theme_minimal(12) +
  labs(
    title = 'SO2 Emission in Ica-Peru {2021-04}',
    subtitle = 'Source: Sentinel-5P NRTI SO2 (working with rgee)',
    caption = '@antony_barja, Antony Barja, 2021-07-22'
  ) + 
  theme(
    plot.margin = unit(rep(.5, 4), "cm"),
    plot.title.position = "plot",
    plot.title = element_text(size = 28,family = 'Roboto',face = 'bold'),
    plot.subtitle = element_text(color = "gray40"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 24),
    plot.caption = element_text(family = "Inconsolata")
  )

plot_grid(ica_2020,ica_2021,ncol = 1)
ggsave(filename = 'svg/ica.svg',plot = last_plot(),
       width = 12,height = 12)


# 5. Lima -----------------------------------------------------------------

lima_2020 <- limadb %>% 
  ggplot() + geom_raster(aes(Este,Norte, fill = so2_2020)) +  
  geom_sf(data = lima_sf %>% st_transform(32718),
          fill = 'transparent',color = 'white',lwd=0.4) + 
  scale_fill_viridis_c(
    name = expression(paste(S0[2],group("(",paste(mol/m^2), ")"))),
    option = 'inferno',
    limits = c(0,0.0022),
    breaks = seq(0,0.0022,length.out = 4)) + 
  labs(x = '', y = '') + 
  theme_minimal(12) + 
  labs(
    title = 'SO2 Emission in Lima-Peru {2020-04}',
    subtitle = 'Source: Sentinel-5P NRTI SO2 (working with rgee)',
    caption = '@antony_barja, Antony Barja, 2021-07-22'
  ) + 
  theme(
    plot.margin = unit(rep(.5, 4), "cm"),
    plot.title.position = "plot",
    plot.title = element_text(size = 28,family = 'Roboto',face = 'bold'),
    plot.subtitle = element_text(color = "gray40"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 24),
    plot.caption = element_text(family = "Inconsolata")
  )

lima_2021 <- limadb %>% 
  ggplot() + geom_raster(aes(Este,Norte, fill = so2_2021)) +  
  geom_sf(data = lima_sf %>% st_transform(32718),
          fill = 'transparent',color = 'white',lwd=0.4) + 
  scale_fill_viridis_c(
    name = expression(paste(S0[2],group("(",paste(mol/m^2), ")"))),
    option = 'inferno',
    limits = c(0,0.0022),
    breaks = seq(0,0.0022,length.out = 4)) + 
  labs(x = '', y = '') + 
  theme_minimal(12) + 
  labs(
    title = 'SO2 Emission in Lima-Peru {2021-04}',
    subtitle = 'Source: Sentinel-5P NRTI SO2 (working with rgee)',
    caption = '@antony_barja, Antony Barja, 2021-07-22'
  ) + 
  theme(
    plot.margin = unit(rep(.5, 4), "cm"),
    plot.title.position = "plot",
    plot.title = element_text(size = 28,family = 'Roboto',face = 'bold'),
    plot.subtitle = element_text(color = "gray40"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 24),
    plot.caption = element_text(family = "Inconsolata")
  )

plot_grid(lima_2020,lima_2021,ncol = 1)
ggsave(filename = 'svg/lima.svg',plot = last_plot(),
       width = 12,height = 12)

# 6. Iquitos --------------------------------------------------------------

iquitos_2020 <- iquitosdb %>% 
  ggplot() + geom_raster(aes(Este,Norte, fill = so2_2020)) +  
  geom_sf(data = iquitos_sf %>% st_transform(32718),
          fill = 'transparent',color = 'white',lwd=0.4) + 
  scale_fill_viridis_c(
    name = expression(paste(S0[2],group("(",paste(mol/m^2), ")"))),
    option = 'inferno',
    limits = c(0,0.0016),
    breaks = seq(0,0.0016,length.out = 4)) + 
  labs(x = '', y = '') + 
  theme_minimal(12) + 
  labs(
    title = 'SO2 Emission in Iquitos-Peru {2020-04}',
    subtitle = 'Source: Sentinel-5P NRTI SO2 (working with rgee)',
    caption = '@antony_barja, Antony Barja, 2021-07-22'
  ) + 
  theme(
    plot.margin = unit(rep(.5, 4), "cm"),
    plot.title.position = "plot",
    plot.title = element_text(size = 28,family = 'Roboto',face = 'bold'),
    plot.subtitle = element_text(color = "gray40"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 24),
    plot.caption = element_text(family = "Inconsolata")
  )


iquitos_2021 <- iquitosdb %>% 
  ggplot() + geom_raster(aes(Este,Norte, fill = so2_2021)) +  
  geom_sf(data = iquitos_sf %>% st_transform(32718),
          fill = 'transparent',color = 'white',lwd=0.4) + 
  scale_fill_viridis_c(
    name = expression(paste(S0[2],group("(",paste(mol/m^2), ")"))),
    option = 'inferno',
    limits = c(0,0.0016),
    breaks = seq(0,0.0016,length.out = 4)) + 
  labs(x = '', y = '') + 
  theme_minimal(12) + 
  labs(
    title = 'SO2 Emission in Iquitos-Peru {2021-04}',
    subtitle = 'Source: Sentinel-5P NRTI SO2 (working with rgee)',
    caption = '@antony_barja, Antony Barja, 2021-07-22'
  ) + 
  theme(
    plot.margin = unit(rep(.5, 4), "cm"),
    plot.title.position = "plot",
    plot.title = element_text(size = 28,family = 'Roboto',face = 'bold'),
    plot.subtitle = element_text(color = "gray40"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 24),
    plot.caption = element_text(family = "Inconsolata")
  )

plot_grid(iquitos_2020,iquitos_2021,ncol = 1)
ggsave(filename = 'svg/iquitos.svg',plot = last_plot(),
       width = 12,height = 12)

# 7.Piura -----------------------------------------------------------------

piura_2020 <- piuradb %>% 
  ggplot() + geom_raster(aes(Este,Norte, fill = so2_2021)) +  
  geom_sf(data = piura_sf %>% st_transform(32718),
          fill = 'transparent',color = 'white',lwd=0.4) + 
  scale_fill_viridis_c(
    name = expression(paste(S0[2],group("(",paste(mol/m^2), ")"))),
    option = 'inferno',
    limits = c(0,0.0027),
    breaks = seq(0,0.0027,length.out = 4)) + 
  labs(x = '', y = '') + 
  theme_minimal(12) + 
  labs(
    title = 'SO2 Emission in Piura-Peru {2020-04}',
    subtitle = 'Source: Sentinel-5P NRTI SO2 (working with rgee)',
    caption = '@antony_barja, Antony Barja, 2021-07-22'
  ) + 
  theme(
    plot.margin = unit(rep(.5, 4), "cm"),
    plot.title.position = "plot",
    plot.title = element_text(size = 28,family = 'Roboto',face = 'bold'),
    plot.subtitle = element_text(color = "gray40"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 24),
    plot.caption = element_text(family = "Inconsolata")
  )


piura_2021 <- piuradb %>% 
  ggplot() + geom_raster(aes(Este,Norte, fill = so2_2021)) +  
  geom_sf(data = piura_sf %>% st_transform(32718),
          fill = 'transparent',color = 'white',lwd=0.4) + 
  scale_fill_viridis_c(
    name = expression(paste(S0[2],group("(",paste(mol/m^2), ")"))),
    option = 'inferno',
    limits = c(0,0.0027),
    breaks = seq(0,0.0027,length.out = 4)) + 
  labs(x = '', y = '') + 
  theme_minimal(12) + 
  labs(
    title = 'SO2 Emission in Piura-Peru {2021-04}',
    subtitle = 'Source: Sentinel-5P NRTI SO2 (working with rgee)',
    caption = '@antony_barja, Antony Barja, 2021-07-22'
  ) + 
  theme(
    plot.margin = unit(rep(.5, 4), "cm"),
    plot.title.position = "plot",
    plot.title = element_text(size = 28,family = 'Roboto',face = 'bold'),
    plot.subtitle = element_text(color = "gray40"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 24),
    plot.caption = element_text(family = "Inconsolata")
  )

plot_grid(piura_2020,piura_2021,ncol = 1)
ggsave(filename = 'svg/piura.svg',plot = last_plot(),
       width = 12,height = 12)

# 8.Poster ----------------------------------------------------------------

peru <- st_read('crudo/peru.gpkg')
peru %>% 
  ggplot() +
  geom_sf(fill = NA) + 
  theme(
    element_blank(),
    panel.background=element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank())

ggsave(filename = 'svg/peru.svg',plot = last_plot(),
       width = 12,height = 12)
