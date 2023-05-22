# app/logic/00_funcoes.R

box::use(
  sf[
    st_transform, st_filter
  ],
  raster[
    projectRaster
  ]
)

#' @export

# filtra pra região nordeste ----------------------------------------------


filtrar_ne <- function(shp) {
  v_proj <- sf::st_transform(shp, st_crs(ufs_ne)) |>
    sf::st_filter(st_union(ufs_ne))
  
  return(v_proj)
}

#' @export
# projeta pra wgs84 -------------------------------------------------------

project_wgs <- function(shp) {
  return(sf::st_transform(shp, crs = 4326))
}

projectraster_wgs <- function(raster, method = 'ngb') {
  return(raster::projectRaster(raster, crs = 4326, method = method))
}


#' @export
# formata numeros com vígula e n casas decimais ---------------------------


formata_numero <- function(numero, casas_decimais = 2) {
  n <- format(round(numero, digits = casas_decimais),
              scientific = FALSE, decimal.mark = ",")
  return(n)
}

