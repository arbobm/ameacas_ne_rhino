# app/logic/atividades.R

box::use(
  raster[raster],
  readxl[
    read_xlsx
  ],
  sf[read_sf]
)


## agricultura

#' @export
areas_agric <- raster("app/rasters/agric_alb_1k.tif")

#' @export
freq_area <- readxl::read_xlsx("app/planilhas/freq_area_mapbiomas71_ne.xlsx")

## urbano

#' @export
areas_urb <- raster("app/rasters/urbano_alb_1k.tif")

## pastagem

#' @export
pastagem <- raster("app/rasters/pastagem_alb_1k.tif")

## parques eolicos

#' @export
aerogeradores <- read_sf("app/shapes/aerog_ne.shp")

#' @export
eols_pol <- read_sf("app/shapes/eol_pol_ne.shp")

## parque de energia solar

#' @export
ufv <- read_sf("app/shapes/ufv_ne.shp")


## mineração substancias

# mined_subs <- raster("app/rasters/mined_subst_ne_alb_1k.tif")
# cores_mined <- read_xlsx("C:/Users/bruna/OneDrive/01_BaseSIG/Brasil/MapBiomas/MapBiomas7/mapbiomas-brazil-collection-70-mined-substance-brasil-2021/cores.xlsx")
# freq <- data.frame(raster::freq(mined_subs))
# 
# # cores_freq <- left_join(freq, cores_mined, by = c("value" = "id")) %>% 
# #   select(-c(count), count) %>% 
# #   mutate(area_m2 = count * (30*30))
# 
# # visualizar com paleta
# cores <- tibble(id = unique(mined_subs)) %>% 
#   left_join(cores_mined) %>% 
#   # filter(id != 0) %>% 
#   pull(cor)
# labels <- tibble(id = unique(mined_subs)) %>% 
#   left_join(cores_mined) %>% 
#   # filter(id != 0) %>% 
#   pull(substancia)
# 
# 
# l <- str_split(labels, "/")
# subst <- c()
# for (i in 2:length(l)) {
#   
#   t <- l[[i]][3]
#   message(t)
#   subst[i-1] <- t
#   
# }
## mineracao

#' @export
mineracao <- raster("app/rasters/mineracao_alb_1k.tif")

## hidreletricas

#' @export
ahes <- read_sf("app/shapes/ahe_ne.shp")

#' @export
ahes_pol <- read_sf("app/shapes/ahes_pol.shp")

## reservatorio_recreacao/turismo?

#' @export
reserv_recrea <- read_sf("app/shapes/reserv_recreacao.shp")

## silvicultura

#' @export
silvicultura <- raster("app/rasters/silvicultura_alb_1k.tif")

## estradas

#' @export
estradas_federais <- read_sf("app/shapes/estradas_federais.shp")

## ferrovias

#' @export
ferrovias <- read_sf("app/shapes/ferrivias.shp")



