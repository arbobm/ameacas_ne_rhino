# app/logic/oportunidades.R

box::use(
  sf[
    read_sf, st_transform
  ],
  leaflet[
    colorFactor
  ],
)

box::use(
  app/logic/funcoes[project_wgs]
)

# UCs ---------------------------------------------------------------------

ucs <- sf::read_sf("app/shapes/ucs_ne.shp")

#' @export
ucs <- project_wgs(ucs)

ucs$grupo <- factor(ucs$grupo, levels = c("US", "PI"),
                    labels = c("Uso sustentável", 
                               "Proteção integral"))

ucs$categoria <- factor(ucs$categoria, levels = c("APA", "FLONA", "RDS",
                                                  "ARIE", "RESEX", "RPPN", 
                                                  "PARNA", "Parque",
                                                  "REVIS", "REBIO", "MONA",
                                                  "ESEC"))





cores_ucs <- c("#ffa237", # US
               "#9dce45"  #PI
)

#' @export
pal_ucs <- colorFactor(cores_ucs, domain = ucs$grupo)

# cores_ucs <- c(
#   
#   # Uso sustentavel
#   "#FFC685", # APA
#   "#FFA237", # FLONA
#   "#CC822D", # Floresta Estadual
#   "#806342", # RDS
#   "#BF7A2A", # ARIE
#   "#80511C", # RESEX
#   "#40290E", # RPPN  
#   
#   # Proteção Integral
#   "#3D4F1A", # PARNA
#   "#454F32", # Parque Estadual
#   "#9DCE45", # REVIS
#   "#BBD787", # REBIO
#   "#779C33", # MONA
#   "#6D8F2F"  # ESEC
#   )




# pal_ucs <- colorFactor(cores_ucs, domain = ucs$categoria)


# Terras Indigenas --------------------------------------------------------

# tis <- st_read("C:/Users/bruna/OneDrive/01_BaseSIG/Brasil/funai/GEOFT_TERRA_INDIGENA/GEOFT_TERRA_INDIGENA_valid.shp") |> 
#   janitor::clean_names()
# 
# tis <- tis |> 
#   st_transform(crs = 4326) |> 
#   st_filter(st_as_sf(st_union(dplyr::filter(ufs, NM_REGIAO == "Sudeste")))) |> 
#   st_simplify()

#' @export
tis <- sf::read_sf("app/shapes/tis_ne.shp", options = "ENCODING=WINDOWS-1252")

# # kba ---------------------------------------------------------------------
# 
kba_ne <- read_sf("app/shapes/kba_ne.shp")

#' @export
kba_ne <- kba_ne |>
  st_transform(crs = 4326)

# kba_se_t <- kba_se |>
#   tibble |>
#   select(-geometry)
# lapply(kba_se_t, unique)

# writexl::write_xlsx(kba_se_t, "C:/Users/bruna/OneDrive/01_BaseSIG/Brasil/kba/Brazil_KBA/kba_se.xlsx")



