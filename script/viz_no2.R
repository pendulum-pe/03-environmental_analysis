viz_no2 <- function(data, sf, reg, col, pal, title, subtitle) {
  val_min <- data[,-c(1:2)] %>% summarise_all(list(min)) %>% 
    as.matrix() %>% as.vector() %>% min()
  val_max <- data[,-c(1:2)] %>% summarise_all(list(max)) %>% 
    as.matrix() %>% as.vector() %>% max()
  
  g1 <- data %>%
    ggplot() +
    geom_tile(aes(Este, Norte, fill = (.data[[col]] * 1000))) +
    geom_sf(
      data = sf %>% st_transform(32718),
      fill = "transparent", color = "gray", lwd = 0.2
    ) +
    geom_sf(
      data = reg %>% st_transform(32718),
      fill = NA, color = "black", lwd = 1
    ) +
    scale_fill_viridis_c(
      name = expression(paste(N0[2], group("(", paste(MiliMol / m^2), ")"))),
      option = pal,
      limits = c(val_min*1000, val_max*1000),
      breaks = round(seq(val_min*1000, val_max*1000, length.out = 5), 3)
    ) +
    labs(x = "", y = "") +
    theme_void() +
    labs(
      title = title,
      subtitle = subtitle
    ) +
    theme(
      legend.position = "top",
      plot.margin = unit(rep(.5, 4), "cm"),
      plot.title.position = "plot",
      plot.title = element_text(size = 14, family = "Roboto", face = "bold"),
      plot.subtitle = element_text(color = "gray40"),
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8),
      axis.title.y = element_text(size = 12),
      plot.caption = element_text(family = "Inconsolata")
    )
  g2 <- g1 + guides(
    fill = guide_colourbar(
      title.position = "top",
      barwidth = 10,
      barheight = .5
    )
  )

  return(g2)
}


viz_no2_di <- function(data, sf, reg, pal = "RdBu", col, title, subtitle) {
  val_min <- data[,-c(1:6)] %>% summarise_all(list(min)) %>% 
    as.matrix() %>% as.vector() %>% min()
  val_max <- data[,-c(1:6)] %>% summarise_all(list(max)) %>% 
    as.matrix() %>% as.vector() %>% max()
  g1 <- data %>%
    ggplot() +
    geom_tile(aes(Este, Norte, fill = (.data[[col]]) * 1000)) +
    geom_sf(
      data = sf %>% st_transform(32718),
      fill = "transparent", color = "gray", lwd = 0.2
    ) +
    geom_sf(
      data = reg %>% st_transform(32718),
      fill = NA, color = "black", lwd = 1
    ) +
    scale_fill_distiller(
      palette = pal,
      limits = c(val_min*1000,val_max*1000),
      name = expression(paste(N0[2], group("(", paste(MiliMol / m^2), ")")))
    ) +
    labs(x = "", y = "") +
    theme_void() +
    labs(
      title = title,
      subtitle = subtitle
    ) +
    theme(
      legend.position = "top",
      plot.margin = unit(rep(.5, 4), "cm"),
      plot.title.position = "plot",
      plot.title = element_text(size = 14, family = "Roboto", face = "bold"),
      plot.subtitle = element_text(color = "gray40"),
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8),
      axis.title.y = element_text(size = 12),
      plot.caption = element_text(family = "Inconsolata")
    )
  g2 <- g1 + guides(
    fill = guide_colourbar(
      title.position = "top",
      barwidth = 10,
      barheight = .5
    )
  )
  return(g2)
}
