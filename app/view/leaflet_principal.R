# app/view/leaflet_principal.R


box::use(
  leaflet[
    leafletOutput, renderLeaflet, leaflet, addProviderTiles, providers,
    pathOptions, addMapPane, addLayersControl, hideGroup, addScaleBar, 
    scaleBarOptions, layersControlOptions, addPolygons, addLegend
  ],
  shiny[
    div, moduleServer, NS
    ],
  htmltools[
    htmlEscape
  ]
)

box::use(
  app/logic/limites[aes_pat, factpal_pats, aes_panne, bacias_ana, biomas,
                    pal_biomas, ufs_ne],
  app/logic/oportunidades[tis, ucs, pal_ucs, kba_ne],
)


#' @export
ui <- function(id) {
  ns <- NS(id)

  leafletOutput(ns("map"), width = "100%", height = "100%")
  
}


#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$map <- renderLeaflet({
      
      leaflet() |> 
        addMapPane("background", zIndex = 0) |>        # Level 1: bottom
        addMapPane("rasters", zIndex = 1) |>           # Level 2: middle
        addMapPane("polygons", zIndex = 200) |>        # Level 3: middle
        addMapPane("polylines", zIndex = 300) |>        # Level 3: middle
        # addMapPane("rasters", zIndex = 100000) |>    # Level 3: middle
        addMapPane("points", zIndex = 440) |>          # Level 4: middle
        addMapPane("labels", zIndex = 450) |>          # Level 5: top
        addProviderTiles(providers$Esri.WorldImagery, group="Satellite", 
                         options = pathOptions(pane = "background")
        ) |>
        addProviderTiles(providers$Esri.WorldStreetMap, group = "Open Street Map",
                         options = pathOptions(pane = "background")) |> 
        addProviderTiles(providers$Stamen.TonerLabels, group="Labels", 
                         options = pathOptions(pane = "labels")) |> 
        addPolygons(
          group = "KBAs",
          data = kba_ne,
          color = c("orange"),
          fillOpacity = 0.01,
          weight = 2,
          smoothFactor = 1,
          opacity = 0.6,
          label = ~ htmlEscape(NatName),
          popup = paste0(
            "<strong>", kba_ne$NatName, "</strong>",
            "</br>",
            "<strong>KBA_Qual: </strong>",
            kba_ne$KBA_Qual,
            "</br>",
            "<strong>Fonte: </strong>",
            kba_ne$Source,
            "</br>",
            "<strong>Adicionada em: </strong>",
            kba_ne$AddedDate,
            "</br>",
            "<strong>Detalhes: </strong>",
            kba_ne$DelTxt)
        ) |>
        addPolygons(group = "Estados do NE",
                    data = ufs_ne,
                    color = "black",
                    # fillOpacity = 0.01,
                    fill = FALSE,
                    weight = 2,
                    smoothFactor = 1,
                    # opacity = 0.6,
                    # label = ~ htmlEscape(NM_UF),
                    options = pathOptions(pane = "polygons")
        ) |>
        addPolygons(group = "Biomas",
                    data = biomas,
                    color = "black",
                    fillOpacity = 0.5,
                    fillColor = ~ pal_biomas(Bioma),
                    weight = 2,
                    smoothFactor = 1,
                    opacity = 0.5,
                    label = ~ htmlEscape(Bioma),
                    options = pathOptions(pane = "polygons")
        ) |>
        addPolygons(group = "Bacias",
                    data = bacias_ana,
                    fill = TRUE,
                    stroke = TRUE,
                    fillColor = "lightblue",
                    color = "lightblue",
                    weight = 2,
                    smoothFactor = 1,
                    fillOpacity = 0.0000005,
                    label = ~ htmlEscape(name),
                    options = pathOptions(pane = "polygons")
        ) |> 
        
        
        addPolygons(group = "Unidades de Conservação",
                    data = ucs,
                    color = "black",
                    fillColor = ~ pal_ucs(grupo),
                    fillOpacity = 0.4,
                    # fill = FALSE,
                    weight = 2,
                    smoothFactor = 1,
                    opacity = 0.5,
                    label = ~ htmlEscape(nome_uc),
                    
                    options = pathOptions(pane = "polygons"),
                    popup = paste(
                      "<strong>",
                      ucs$nome_uc,
                      "</strong>",
                      "</br>
                <strong>Grupo:</strong>",
                      ucs$grupo,
                      "</br>
                <strong>Esfera:</strong>",
                      ucs$esfera,
                      #         "</br>
                      # <strong>Estados:</strong>",
                      #         ucs$estados,
                      "</br>
                <strong>Fonte:</strong>",
                      ucs$fonte,
                      "</br>"
                    )
                    
        ) |>
        addLegend(
          data = ucs,
          values = ~levels(grupo),
          pal = pal_ucs,
          opacity = 0.6,
          title = "Unidades de Conservação",
          position = "topright",
          group = "Unidades de Conservação"
        ) |>
        addPolygons(group = "Terras Indígenas",
                    data = tis,
                    color = "black",
                    fillColor = "yellow",
                    fillOpacity = 0.4,
                    weight = 2,
                    smoothFactor = 1,
                    opacity = 0.5,
                    label = ~ htmlEscape(terrai_nom),
                    options = pathOptions(pane = "polygons"),
                    popup = paste(
                      "<strong>",
                      tis$terrai_nom,
                      "</strong>",
                      "</br>
                <strong>Nome da etnia:</strong>",
                      tis$etnia_nome,
                      "</br>
                <strong>Modalidade:</strong>",
                      tis$modalidade,
                      "</br>
                <strong>Fase:</strong>",
                      tis$fase_ti,
                      "</br>
                <strong>Estados:</strong>",
                      tis$uf_sigla,
                      "</br>
                <strong>Município:</strong>",
                      tis$municipio_
                    )
        ) |> 
        addLegend(
          data = tis,
          colors = "yellow",
          opacity = 0.6,
          labels = "Terras indígenas",
          # title = "Áreas protegidas",
          position = "topright",
          group = "Terras Indígenas"
        ) |>
        addPolygons(group = "Áreas estratégicas - PAN",
                    data = aes_panne,
                    fill = FALSE,
                    stroke = TRUE,
                    # fillColor = "lightblue",
                    color = "#af1a19",
                    weight = 2,
                    smoothFactor = 1,
                    # fillOpacity = 0.0000005,
                    label = ~ htmlEscape(NmArea),
                    options = pathOptions(pane = "polygons")
        ) |> 
        addPolygons(group = "Áreas - PAT",
                    data = aes_pat,
                    fill = TRUE,
                    stroke = TRUE,
                    color = "black",
                    fillColor = ~ factpal_pats(finalidade),
                    # color = "orange",
                    weight = 2,
                    smoothFactor = 1,
                    fillOpacity = 0.3,
                    label = ~ htmlEscape(AEs),
                    options = pathOptions(pane = "polygons")
        ) |>
        addLegend(
          data = aes_pat,
          values = ~levels(finalidade),
          pal = factpal_pats,
          opacity = 0.6,
          title = "Áreas - PAT",
          position = "topright",
          group = "Áreas - PAT"
        ) |>
        addLayersControl(
          baseGroups = c(
            # "Topography",
            "Satellite",
            "Open Street Map"
            # "Terrain",
            
          ),
          overlayGroups = c(
            "Estados do NE",
            "Unidades de Conservação",
            "Terras Indígenas",
            "Áreas estratégicas - PAN",
            "Bacias",
            "Biomas",
            "Áreas - PAT",
            "KBAs",
            "Labels"
          ),
          options = layersControlOptions(collapsed = FALSE, position = "bottomright")
        ) |> 
        ## layers que começam desmarcados
        hideGroup(c(
          "KBAs",
          "Labels",
          "Biomas",
          "Bacias",
          "Unidades de Conservação",
          "Áreas estratégicas - PAN",
          "Áreas - PAT",
          "Terras Indígenas"
        )) |>
        addScaleBar(position = c("bottomleft"), options = scaleBarOptions()) 
      
    })
    
  })
}