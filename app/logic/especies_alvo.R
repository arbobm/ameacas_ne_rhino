# app/logic/especies_alvo.R

box::use(
  readxl[
    read_xlsx
    ],
  dplyr[
    filter, select
  ],
  stringr[
    str_replace_all
    ],
  sf[
    st_as_sf, st_transform
  ],
  dplyr[
    arrange, desc, pull
    ]
)


# species data ------------------------------------------------------------

occ <- readxl::read_xlsx("app/planilhas/ocorrencias_sdupli.xlsx")

#' @export
occ <- occ |> 
  filter(ameacada == "sim") |>
  dplyr::select(nome_cientifico, grupo, classe, familia, 
                cat_validada = categoria_validada, 
                lat = latitude, long = longitude,
                ameacada, ameaca, tendencia_populacional, descricao_amecas)

occ$ameaca <- occ$ameaca |>
  str_replace_all("\n", "</br>")


# info
#' @export
occ_sf <- sf::st_as_sf(occ, coords = c("long", "lat"), 
                       crs = 4674, remove = FALSE) |>
  sf::st_transform(crs = 4326)

# write_sf(occ_sf, "shapes/occ_sf.shp")

#' @export
combined_distribution <- occ_sf 
# |>
# dplyr::select(taxon, lat, lon) |>
# dplyr::bind_rows(combined_distribution_benef)

#' @export
species_list <- occ |>
  arrange(desc(ameacada), nome_cientifico) |>
  # select(ameacada, nome_cientifico)
  pull(nome_cientifico) |>
  unique() 

# species_list_nameac <- occ |>
#   filter(ameacada == "nao") |>
#   pull(nome_cientifico) |>
#   unique() |>
#   sort()