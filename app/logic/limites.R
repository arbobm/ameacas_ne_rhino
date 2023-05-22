# app/logic/01_limites.R

box::use(
  sf[
    read_sf, 
  ],
  leaflet[
    colorFactor
  ],
  dplyr[
    as_tibble, select, mutate, bind_rows
  ],
  stringr[str_split]
)

box::use(
  app/logic/funcoes[project_wgs]
)



# estados -----------------------------------------------------------------


ufs_ne <- sf::read_sf("app/shapes/ufs_ne.shp")
#' @export
ufs_ne <- project_wgs(ufs_ne)


#' @export
# limites PANs ------------------------------------------------------------

# limite_PANs <- sf::read_sf("app/shapes/limites_pans.shp")
limite_panne <- sf::read_sf("app/shapes/limite_panne.shp")
limite_panne <- project_wgs(limite_panne)


# biomas ------------------------------------------------------------------

biomas <- sf::read_sf("app/shapes/biomas_ne.shp")
#' @export
biomas <- project_wgs(biomas)

## paleta de cores dos biomas

biomas$Bioma <- factor(biomas$Bioma)

cores_biomas <- c(
  "#B3964F", # Caatinga
  "#B7DB6E", # Cerrado
  "#86A686" # Mata Atlântica
)

#' @export
pal_biomas <- leaflet::colorFactor(cores_biomas, domain = biomas$Bioma)

#' @export
# bacias_ana --------------------------------------------------------------

# bacias_ana <- st_read("C:/Users/bruna/OneDrive/01_BaseSIG/Brasil/MapBiomas/MapBiomas7/dashboard_river-basins-static-layer/dashboard_river-basins-static-layer.shp",
#                       options = "ENCODING=WINDOWS-1252")

bacias_ana <- sf::read_sf("app/shapes/bacias_ana_ne.shp")
bacias_ana <- project_wgs(bacias_ana)
# bacias_ana_alb <- sf::read_sf("app/shapes/bacias_ana_alb.shp")


bacias_area <- bacias_ana |> 
  dplyr::as_tibble() |> 
  dplyr::select(nome = name, area_ha)


# areas estratégicas - PAN ------------------------------------------------

aes_panne <- read_sf("app/shapes/pan_ran_nordeste_c2_area_estrategica_022023_a.shp",
                     options = "ENCODING=WINDOWS-1252")
#' @export
aes_panne <- project_wgs(aes_panne)



# areas estratégicas - pat ------------------------------------------------


aes_pat_files <- list.files("app/shapes/", pattern = "PAT.shp", full.names = TRUE)

aes_pat_list <- list()

for (i in 1:length(aes_pat_files)) {
  
  shp <- read_sf(aes_pat_files[i])
  nome <- unlist(str_split(unlist(str_split(aes_pat_files[i], "/"))[2], "_"))[2]
  
  shp <- shp |> 
    mutate(finalidade = nome) |> 
    select(-geometry, geometry)
  
  aes_pat_list[[i]] <- shp
  
}

#' @export
aes_pat <- aes_pat_list |> 
  bind_rows()

aes_pat$finalidade <- factor(aes_pat$finalidade, levels = c("conservacao", 
                                                            "restauracao"),
                             labels = c("Conservação", "Restauração"))

colors <- c("#8B0A50", "#8B7B8B")

#' @export
factpal_pats <- colorFactor(colors, domain = aes_pat$finalidade)
