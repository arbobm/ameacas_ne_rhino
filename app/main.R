# app/main.R

box::use(
  shiny[
    navbarPage, tabPanel, div, moduleServer, NS, renderUI, tags, uiOutput,
    absolutePanel, reactive, observe, observeEvent, h3, h4, h5, strong, 
    checkboxInput, actionButton, p, br, req, tagList, HTML, helpText, em
  ],
  leaflet[
    leafletOutput, renderLeaflet, colorFactor, addMapPane, addProviderTiles,
    addPolygons, leafletProxy, clearGroup, clearMarkerClusters, removeControl,
    clearImages, clearControls, pathOptions, popupOptions,
    addLegend, addLayersControl, hideGroup, layersControlOptions, addScaleBar,
    addRasterImage, scaleBarOptions, leaflet, providers, addCircleMarkers, 
    addPolylines, addMeasure
  ],
  
  shinyWidgets[
    pickerInput, updatePickerInput
  ],
  dplyr[
    select, arrange, distinct, pull, filter, mutate, case_when
  ],
  shinybusy[
    show_modal_spinner, remove_modal_spinner
  ],
  sf[
    st_bbox
  ],
  methods[
    as
  ],
  leaflet.extras[
    clearHeatmap
  ],
  htmltools[
    htmlEscape
  ],
  raster[
    unique
  ],
  stringr[
    word
  ],
  reactable[reactable, renderReactable, reactableOutput, colDef]
)

box::use(
  # app/view/especies_pts,
  app/logic/especies_alvo[species_list, occ, combined_distribution],
  app/logic/limites[biomas, aes_pat, factpal_pats, aes_panne, bacias_ana,
                    ufs_ne],
  app/logic/oportunidades[tis, ucs, pal_ucs, kba_ne],
  app/logic/atividades[freq_area, areas_agric, areas_urb, pastagem, 
                       aerogeradores, eols_pol, ufv, mineracao, ahes, ahes_pol,
                       reserv_recrea, silvicultura, estradas_federais, ferrovias,
                       barragens_anm],
  
  
  
  app/logic/funcoes[formata_numero]
  
  
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  navbarPage("PAN Herpetofauna do Nordeste",
             id = "nav",
             
             
             tabPanel("Base de dados",
                      
                      tags$head(
                        
                      ),
                      div(class = "outer",
                          # aqui vai o modulo com o mapa grande
                          tagList(
                            leafletOutput(ns("map"), width = "100%", height = "100%"),
                            
                            absolutePanel(id = "controls", class="panel panel-default", 
                                          fixed = TRUE, draggable = TRUE, top = 60, 
                                          left = 20, right = "auto", bottom = 50,
                                          width = 400, style = "overflow-y: scroll;",
                                          
                                          ## Selecionar espécies ameacadas
                                          
                                          ## Selecionar espécies ameacadas
                                          
                                          pickerInput(ns("especies"),
                                                      label = h5("Espécies:"), 
                                                      choices = species_list, 
                                                      multiple = TRUE,
                                                      options = list(`actions-box` = TRUE,
                                                                     # `deselect-all-text` = "None...",
                                                                     # `select-all-text` = "Yeah, all !",
                                                                     `none-selected-text` = "Selecione uma ou mais espécies"
                                                      )
                                          ),
                                          
                                          
                                          
                                          
                                          actionButton(ns("update"), "Atualizar"),
                                          
                                          # h4("Áreas protegidas:"),
                                          # checkboxInput(inputId = "ucs", 
                                          #               label = h5("Unidades de Conservação"),
                                          #               value = FALSE),
                                          # 
                                          # checkboxInput(inputId = "tis", 
                                          #               label = h5("Terras Indígenas"),
                                          #               value = FALSE),
                                          # 
                                          # br(),
                                          
                                          h4("Ameaças potenciais:"),
                                          checkboxInput(ns("agricultura"),
                                                        label = h5("Agricultura"),
                                                        value = FALSE),
                                          checkboxInput(ns("areasurbanas"),
                                                        label = h5("Áreas urbanas"),
                                                        value = FALSE),
                                          checkboxInput(ns("pastagem"),
                                                        label = h5("Pastagem"),
                                                        value = FALSE),
                                          checkboxInput(ns("silvicultura"),
                                                        label = h5("Silvicultura"),
                                                        value = FALSE),
                                          checkboxInput(ns("eol"), 
                                                        label = h5("Parques eólicos"),
                                                        value = FALSE),
                                          checkboxInput(ns("aerogeradores"), 
                                                        label = h5("Aerogeradores"),
                                                        value = FALSE),
                                          checkboxInput(ns("ufv"), 
                                                        label = h5("Parques de energia solar"),
                                                        value = FALSE),
                                          checkboxInput(ns("mineracao"),
                                                        label = h5("Mineração"),
                                                        value = FALSE),
                                          checkboxInput(ns("mineracaoanm"),
                                                        label = h5("Mineração - barragens"),
                                                        value = FALSE),
                                          
                                          checkboxInput(ns("ahe"), 
                                                        label = h5("Hidrelétricas - pontos"),
                                                        value = FALSE),
                                          checkboxInput(ns("ahepol"), 
                                                        label = h5("Hidrelétricas - reservatórios"),
                                                        value = FALSE),
                                          checkboxInput(ns("reservrecreac"), 
                                                        label = h5("Recreação - reservatórios"),
                                                        value = FALSE),
                                          checkboxInput(ns("estradas"), 
                                                        label = h5("Rodovias"),
                                                        value = FALSE),
                                          checkboxInput(ns("ferrovias"), 
                                                        label = h5("Ferrovias"),
                                                        value = FALSE)
                                          
                            )
                          ))
             ),
             # aba 2 ------------------------------------------------------------
             
             tabPanel("Modo de Usar",
                      
                      
                      h3("Instruções gerais:",
                         br(),
                         br()),
                      
                      
                      p(
                        
                        "- No menu da barra lateral, a esquerda, é possível 
                        selecionar as espécies e ameaças potenciais que 
                        ocorrem na região a ser visualizada. Cada vez que uma 
                        nova camada for selecionada, é necessário pressionar o botão ", 
                        strong("Atualizar"), " para que a(s) camada(s) sejam adicionadas ao mapa;",
                        
                        helpText("Obs: Camadas relacionadas a cobertura do solo e camadas que 
                                 ocupam uma grande extensão do território podem demorar um pouquinho mais 
                                 para carregar."),
                        
                        "- No canto inferior direito estão camadas relacionadas a limites geográficos, áreas protegidas,
                        projetos interessantes localizados no território que podem ser adicionados para referência
                        simultâneamente às camadas relacionadas às ameaças;",
                        br(),

                     
                        "- Procure não deixar o ", em("site"), "aberto por muito tempo esquecido no navegador 
                        ou ele pode desconectar e será preciso entrar novamente.",
                        br(),
                        
                      )
             ),
             # aba 3 ------------------------------------------------------------
             
             tabPanel("Ameaças por grupos", id = "titulo-ameacas",
                      h3(strong("Frequência das ameaças citadas por grupos avaliados")),
                      
                      
                      tags$div(
                      
                      HTML('
                     <div class="flourish-embed flourish-hierarchy" data-src="visualisation/13869226"><script src="https://public.flourish.studio/resources/embed.js"></script></div>
                        ')
                      )
                      
                      
                      
                      
                      ),
             
             # aba 3 ------------------------------------------------------------
             
             tabPanel("Fonte dos dados",
                      # h3(strong("Em construção!!",
                      #           br(),
                      #           br())),
                      
                      reactable::reactableOutput(ns("fontes"))
                      
             )
             
             
             
             
             
             
             
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # aba 1 - mapa ------------------------------------------------------------
    
    
    
    output$map <- renderLeaflet({
      
      cores_biomas <- c(
        "#B3964F", # Caatinga
        "#B7DB6E", # Cerrado
        "#86A686"  # Mata Atlântica
      )
      
      pal_biomas <- colorFactor(cores_biomas, domain = biomas$Bioma)
      
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
        # addProviderTiles(providers$Esri.WorldTerrain, group="Terrain", options = pathOptions(pane = "background")) |>
        # addProviderTiles(providers$Stamen.TonerLines, group="Boundaries", options = pathOptions(pane = "labels")) |>
        # addWMSTiles('http://ows.mundialis.de/services/service?', layers='TOPO-WMS', group="Topography", options = pathOptions(pane = "background")) |>
        
        
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
        )|>
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
                    fill = TRUE,
                    stroke = TRUE,
                    fillColor = "#af1a19",
                    color = "#af1a19",
                    weight = 2,
                    smoothFactor = 1,
                    fillOpacity = 0.2,
                    label = ~ htmlEscape(NmArea),
                    options = pathOptions(pane = "polygons")
        ) |> 
        addPolygons(group = "Áreas - PAT",
                    data = aes_pat,
                    fill = TRUE,
                    stroke = TRUE,
                    color = "black",
                    fillColor = factpal_pats(aes_pat$finalidade),
                    # color = "orange",
                    weight = 2,
                    smoothFactor = 1,
                    fillOpacity = 0.6,
                    label = ~ htmlEscape(AEs),
                    popup = paste(
                      "<strong>",
                      aes_pat$AEs,
                      "</strong>",
                      "</br>
                <strong>Finalidade:</strong>",
                      aes_pat$finalidade),
                    
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
        addScaleBar(position = c("bottomleft"), options = scaleBarOptions()) |> 
        addMeasure(
          position = "bottomleft",
          primaryLengthUnit = "meters",
          primaryAreaUnit = "sqmeters",
          activeColor = "#3D535D",
          completedColor = "#7D4479",
          localization = "pt_BR",
          decPoint = ",",
          thousandsSep = ".",
          secondaryLengthUnit = "kilometers",
          secondaryAreaUnit = "hectares")
      
    })
    
    
    
    ### lista de espécies
    
    
    # update species list
    
    new_species_list <- reactive({
      sp_list <- occ |>
        dplyr::select(nome_cientifico) |>
        dplyr::arrange(nome_cientifico) |>
        dplyr::distinct() |>
        dplyr::pull(nome_cientifico)
      
    })
    
    observe({
      # updateSelectizeInput(session,
      updatePickerInput(session,
                        "especies",
                        choices = new_species_list()
                        # selected = head(new_species_list(), 1)
      )
    })
    
    
    
    # Filter distributions
    distribution <- reactive({
      
      
      combined_distribution |>
        filter(nome_cientifico %in% input$especies)
      
      
    })
    
    observeEvent(
      
      input$update,
      
      {
        show_modal_spinner()
        distribution <- distribution()
        # bbox <- sf::st_bbox(as(distribution, "sf")) |>
        #   as.vector()
        
        
        # sp_factpal <- colorFactor(sp_pal, domain = distribution$taxon)
        
        colors <- c("#cce226", # Quase Ameaçada (NT)
                    "#fae813",  # Vulnerável (VU)
                    "#fc7f40", # Em Perigo (EN)
                    "#d81e06", # Criticamente em Perigo (CR)
                    "#000000" # Extinta (EX)
                    
                    
        )
        
        occ$cat_validada <- factor(occ$cat_validada,
                                   levels = c("Quase Ameaçada (NT)",
                                              "Vulnerável (VU)",
                                              "Em Perigo (EN)",
                                              "Criticamente em Perigo (CR)",
                                              "Extinta (EX)"))
        
        occ <- occ |>
          dplyr::mutate(group =
                          case_when(
                            cat_validada == "Quase Ameaçada (NT)" ~ "nt",
                            cat_validada == "Vulnerável (VU)" ~ "vu",
                            cat_validada == "Em Perigo (EN)" ~ "en",
                            cat_validada == "Criticamente em Perigo (CR)" ~ "cr",
                            cat_validada == "Extinta (EX)" ~ "ex"
                          ))
        
        factpal <- colorFactor(colors, domain = occ$cat_validada)
        
        
        leafletProxy("map", data = distribution) |>
          clearGroup("distribution") |>
          clearGroup("occpoints") |>
          clearGroup("pas") |>
          clearGroup("atividade") |>
          clearHeatmap() |>
          clearMarkerClusters() |>
          removeControl("distribution") |>
          clearImages() |>
          clearControls() |>
          
          # distribution <- occ
          # leaflet() |>
          # addTiles() |>
          
          addCircleMarkers(data = distribution, group = "occpoints",
                           lng =  ~ as.numeric(long),
                           lat =  ~ as.numeric(lat),
                           radius = 4,
                           fill = TRUE,
                           # fillColor = "black",
                           fillColor = ~ factpal(cat_validada),
                           fillOpacity = 1, weight = 1,
                           stroke = TRUE, color = "black", opacity = 1,
                           options = pathOptions(pane = "points"),
                           popup = paste0(
                             "</br>",
                             "<strong><em>", distribution$nome_cientifico, "</em></strong>",
                             "</br>",
                             "<strong>Grupo:</strong> ",
                             distribution$grupo,
                             "</br>",
                             "<strong>Família:</strong> ",
                             distribution$familia,
                             "</br>",
                             "<strong>Tendência populacional:</strong> ",
                             distribution$tendencia_populacional,
                             "</br>
                           <strong>Categoria de risco de extinção:</strong> ",
                             distribution$cat_validada,
                             "</br>",
                             "<strong>Ameaça:</strong> </br>",
                             distribution$ameaca,
                             "</br>
                           <strong>Descrição - ameaças:</strong> ",
                             distribution$descricao_amecas
                           ),
                           popupOptions = popupOptions(maxWidth = 500)
                           
          ) |>
          addLegend(data = distribution,
                    position = "topright",
                    pal = factpal,
                    values = ~ cat_validada,
                    layerId = "distribution",
                    opacity = 0.6,
                    title = "Espécies")
        # |>
        #   fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
        
        ### termina mapa espécie alvo ------------
        
        # ### começa mapa de agricultura ------------------------------------------------------
        
        
        if (input$agricultura) {
          
          cores_agricultura <- freq_area |>
            dplyr::filter(new_id %in% as.numeric(unique(areas_agric))) |>
            dplyr::select(new_id, color_number) |>
            arrange(new_id) |>
            pull(color_number)
          
          labels <- freq_area |>
            dplyr::filter(new_id %in% as.numeric(unique(areas_agric))) |>
            dplyr::filter(new_id != 0) |>
            dplyr::mutate(class_t = 
                            word(colecao_7_classes, start = 2, end = -1)) |> 
            pull(class_t)
          
          
          
          leafletProxy("map", data = distribution) |>
            # leaflet() |>
            #   addTiles() |>
            addRasterImage(
              areas_agric,
              colors = c(cores_agricultura,
                         "transparent"),
              opacity = 0.6,
              method = 'ngb',
              group = "atividade"
            ) |>
            addLegend(
              data = areas_agric,
              opacity = 0.6,
              labels = labels,
              colors = cores_agricultura[-1],
              title = "Atividade",
              position = "topright"
            )
          
        }
        
        
        # ### termina mapa de agricultura ------------------------------------------------------
        
        # ### começa mapa de areas urbanas ------------------------------------------------------
        if (input$areasurbanas) {
          
          leafletProxy("map", data = distribution) |>
            # leaflet() |>
            #   addTiles() |>
            addRasterImage(
              areas_urb,
              colors = c("transparent", "#af2a2a", "transparent"),
              opacity = 1,
              method = 'ngb',
              group = "atividade"
            ) |>
            addLegend(
              data = areas_urb,
              opacity = 1,
              labels = "Áreas urbanas",
              colors = c("#af2a2a"),
              title = "Atividade",
              position = "topright"
            )
          
        }
        
        # ### termina mapa de areas urbanas ------------------------------------------------------
        
        # ### começa mapa de pastagem ------------------------------------------------------
        if (input$pastagem) {
          
          cores_pastagem <- freq_area |>
            dplyr::filter(new_id %in% as.numeric(unique(pastagem))) |>
            dplyr::select(new_id, color_number) |>
            dplyr::arrange(new_id) |>
            dplyr::pull(color_number)
          
          leafletProxy("map", data = distribution) |>
            # leaflet() |>
            #   addTiles() |>
            addRasterImage(
              pastagem,
              colors = c(cores_pastagem),
              opacity = 1,
              method = 'ngb',
              group = "atividade"
            ) |>
            addLegend(
              data = pastagem,
              opacity = 1,
              labels = "Pastagem",
              colors = cores_pastagem[-1],
              title = "Atividade",
              position = "topright"
            )
          
        }
        
        # ### termina mapa de pastagem ------------------------------------------------------
        
        # ### começa mapa de silvicultura ------------------------------------------------------
        if (input$silvicultura) {
          
          cores_silvicultura <- freq_area |>
            dplyr::filter(new_id %in% as.numeric(unique(silvicultura))) |>
            dplyr::select(new_id, color_number) |>
            dplyr::arrange(new_id) |>
            dplyr::pull(color_number)
          
          leafletProxy("map", data = distribution) |>
            # leaflet() |>
            #   addTiles() |>
            addRasterImage(
              silvicultura,
              colors = c(cores_silvicultura),
              opacity = 1,
              method = 'ngb',
              group = "atividade"
            ) |>
            addLegend(
              data = silvicultura,
              opacity = 1,
              labels = "Silvicultura",
              colors = cores_silvicultura[-1],
              title = "Atividade",
              position = "topright"
            )
          
        }
        
        # ### termina mapa de silvicultura ------------------------------------------------------
        
        # ### começa mapa de parques eolicos ------------------------------------------------------
        if (input$eol) {
          
          eols_pol <- eols_pol |> 
            filter(FASE %in% c("Operação", "Construção"))
          
          eols_pol$FASE <- factor(eols_pol$FASE, levels = c("Operação", "Construção"))
          
          cores_eols_pol <- c("#CD3333", "#CDC8B1")
          
          pal_eols_pol <- colorFactor(cores_eols_pol, domain = eols_pol$FASE)
          
          leafletProxy("map", data = distribution) |>
            # leaflet() |>
            #   addTiles() |>
            addPolygons(
              data = eols_pol,
              group = "atividade",
              color = "black",
              fillColor = ~ pal_eols_pol(FASE),
              weight = 2,
              smoothFactor = 1,
              opacity = 0.6,
              options = pathOptions(pane = "polygons"),
              popup = paste(
                "<strong>", eols_pol$NOME_EOL, "</strong>", "</br>",
                "<strong>Pot_MW:</strong>",
                formata_numero(eols_pol$POT_MW),
                "</br>
              <strong>Última atualização:</strong> ", 
                eols_pol$DATA_ATUAL,
                "</br>
              <strong>Fase:</strong> ",
                eols_pol$FASE
              )
            ) |>
            addLegend(
              data = eols_pol,
              opacity = 0.6,
              values = ~ FASE,
              labels = levels(eols_pol$FASE),
              colors = cores_eols_pol,
              title = "Parques eólicos",
              position = "topright"
            ) 
          
        }
        
        ### termina mapa de parques eólicos ------------------------------------------------------
        # ### começa mapa de aerogeradores ------------------------------------------------------
        if (input$aerogeradores) {
          
          aerogeradores <- aerogeradores |> 
            filter(OPERACAO %in% c("Sim", "Não"))
          
          aerogeradores$OPERACAO <- factor(aerogeradores$OPERACAO, 
                                           levels = c("Sim", "Não"))
          
          cores_aerogeradores <- c("#CD3333", "#CDC8B1")
          
          pal_aerogeradores <- colorFactor(cores_aerogeradores, 
                                           domain = aerogeradores$OPERACAO)
          
          leafletProxy("map", data = distribution) |>
            # leaflet() |>
            #   addTiles() |>
            addCircleMarkers(
              data = aerogeradores,
              fillOpacity = 1, weight = 1,
              stroke = TRUE,
              radius = 2,
              color = "black",
              popup = paste0(
                "<strong>", aerogeradores$NOME_EOL, "</strong> </br>",
                "<strong>Pot_MW:</strong> ", 
                formata_numero(aerogeradores$POT_MW),
                "</br>",
                "<strong>Operação:</strong> ", aerogeradores$OPERACAO, "</br>",
                "<strong>UF:</strong> ", aerogeradores$UF, "</br>",
                "<strong>Proprietário:</strong> ", aerogeradores$PROPRIETAR, "</br>",
                "<strong>Última atualização:</strong> ", aerogeradores$DATA_ATUAL
              ),
              group = "atividade",
              fillColor = ~ pal_aerogeradores(OPERACAO),
              opacity = 0.6,
              options = pathOptions(pane = "points")
            ) |>
            addLegend(
              data = aerogeradores,
              opacity = 0.6,
              values = ~ OPERACAO,
              labels = levels(aerogeradores$OPERACAO),
              colors = cores_aerogeradores,
              title = "Aerogeradores - Operação",
              position = "topright")
          
          
        }
        
        ### termina mapa de aerogeradores ------------------------------------------------------
        
        # ### começa mapa de parques energia solar ------------------------------------------------------
        if (input$ufv) {
          
          ufv <- ufv |>
            filter(FASE_USINA %in% c("Operação", "Construção"))
          
          ufv$FASE_USINA <- factor(ufv$FASE_USINA, levels = c("Operação", "Construção"))
          
          cores_ufv <- c("#EE7600", "#F0F8FF")
          
          pal_ufv <- colorFactor(cores_ufv, domain = ufv$FASE_USINA)
          
          
          
          
          
          
          leafletProxy("map", data = distribution) |>
            
            
            addCircleMarkers(
              data = ufv,
              group = "atividade",
              color = "black",
              fillColor = ~ pal_ufv(FASE_USINA),
              radius = 4,
              weight = 2,
              opacity = 0.6,
              options = pathOptions(pane = "points"),
              popup = paste(
                "<strong>", ufv$NOME, "</strong>", "</br>",
                "<strong>Pot_MW:</strong>",
                format(ufv$POT_KW, decimal.mark = ","),
                "</br>
            <strong>Última atualização:</strong> ",
                ufv$DATA_ATUAL,
                "</br>
            <strong>Fase:</strong> ",
                ufv$FASE_USINA, "</br>
            <strong>Município:</strong> ",
                ufv$MUNIC, "</br>
            <strong>UF:</strong> ",
                ufv$UF, "</br>
            <strong>Proprietário:</strong> ",
                ufv$PROPRIETAR
                
              )
            ) |>
            addLegend(
              data = ufv,
              opacity = 0.6,
              values = ~ FASE_USINA,
              labels = levels(ufv$FASE_USINA),
              colors = cores_ufv,
              title = "Parques de energia solar",
              position = "topright"
            )
          
        }
        
        ### termina mapa de parques energia solar ------------------------------------------------------
        
        ### começa mapa de mineração ------------------------------------------------------
        if (input$mineracao) {
          
          cores_mineracao <- freq_area |>
            dplyr::filter(new_id %in% as.numeric(unique(mineracao))) |>
            dplyr::select(new_id, color_number) |>
            dplyr::arrange(new_id) |>
            dplyr::pull(color_number)
          
          leafletProxy("map", data = distribution) |>
            # leaflet() |>
            #   addTiles() |>
            addRasterImage(
              mineracao,
              colors = c(cores_mineracao),
              opacity = 0.6,
              method = 'ngb',
              group = "atividade"
            ) |>
            addLegend(
              data = mineracao,
              opacity = 0.6,
              labels = "Mineração",
              colors = cores_mineracao,
              title = "Atividade",
              position = "topright"
            )
          
        }
        
        # ### termina mapa de mineracao ------------------------------------------------------
        
        ### começa mapa de mineraçãobarragens ------------------------------------------------------
        if (input$mineracaoanm) {
          
          cores_barragensanm <- c(
            "#67dddd", # Baixo
            "#ff9900", # Médio
            "#e661ac", # Alto
            "gray"     # N/A
          )
          
          barragens_anm$ctgr_d_ <- factor(barragens_anm$ctgr_d_,
                                          levels = c("Baixo", "Médio", 
                                                     "Alto", "N/A"))
          
          pal_barraganm <- colorFactor(cores_barragensanm, 
                                       domain = barragens_anm$ctgr_d_)
          
          leafletProxy("map", data = distribution) |>
            # leaflet() |> 
            #   addTiles() |> 
            addCircleMarkers(
              group = "atividade",
              data = barragens_anm,
              lng = ~long_dd,
              lat = ~lat_dd,
              radius = 4,
              fillOpacity = 1, weight = 1,
              stroke = TRUE, color = "black", opacity = 1,
              fillColor = ~pal_barraganm(barragens_anm$ctgr_d_),
              options = pathOptions(pane = "points"),
              popup = paste0(
                "<strong>",
                barragens_anm$nm_d_br,
                "</strong>",
                "</br><strong>Município:</strong> ",
                barragens_anm$municip,
                "</br><strong>Estado:</strong> ",
                barragens_anm$uf,
                "</br><strong>Minério principal:</strong> ",
                barragens_anm$mnr_prn,
                "</br><strong>Método construtivo:</strong> ", 
                barragens_anm$mtd_cns,
                "</br><strong>Categoria de risco:</strong> ",
                barragens_anm$ctgr_d_,
                "</br><strong>Dano Potencial Associado:</strong> ",
                barragens_anm$dn_ptn_,
                "</br><strong>Nível de emergência:</strong> ",
                barragens_anm$nvl_d_m,
                "</br><strong>Status DCE atual:</strong> ",
                barragens_anm$dce_atu,
                "</br><strong>Status DCO atual:</strong> ",
                barragens_anm$dco_atu
              )) |>  
            addLegend(position = "topright",
                      group = "atividade",
                      data = barragens_anm,
                      values = ~ ctgr_d_,
                      colors = cores_barragensanm,
                      labels = levels(barragens_anm$ctgr_d_),
                      opacity = 0.6,
                      title = "Categorias de risco")
          
        }
        
        # ### termina mapa de mineracao barragens------------------------------------------------------
        
        ### começa mapa de ahes ------------------------------------------------------
        if (input$ahe) {
          
          
          ahes <- ahes |> 
            filter(FASE %in% c("Operação", "Construção"))
          
          cores_ahe <- c(
            "#009ACD", # CGH
            "#00698C", # PCH
            "#00394D" # UHE
          )
          
          pal_ahe <- colorFactor(cores_ahe,
                                 domain = ahes$TIPO_AHE)
          
          leafletProxy("map", data = distribution) |>
            # leaflet() |>
            #   addTiles() |>
            addCircleMarkers(
              # addMarkers(
              data = ahes,
              group = "atividade",
              # icon = ~aheicons[tipo_ahe],
              # lng =  ~ long,
              # lat =  ~ lat,
              radius = 4,
              fill = TRUE,
              fillColor = ~ pal_ahe(TIPO_AHE),
              fillOpacity = 1,
              weight = 1,
              stroke = TRUE,
              color = "black",
              opacity = 1,
              options = pathOptions(pane = "points"),
              
              popup = paste(
                "<strong>",
                ahes$NOME,
                "</strong>",
                "</br>
              <strong>Tipo de AHE:</strong> ",
                ahes$TIPO_AHE,
                "</br> 
              <strong>Fase:</strong> ",
                ahes$FASE,
                "</br> 
              <strong>Última atualização:</strong> ",
                ahes$DATA_ATUAL
              )
            ) |>
            addLegend(
              data = ahes,
              title = "Atividade",
              values = ~ TIPO_AHE,
              pal = pal_ahe,
              opacity = 0.6
            )
          
        }
        
        # ### termina mapa de ahes ------------------------------------------------------
        
        # ### começa mapa de ahes reservatorios ------------------------------------------------------
        if (input$ahepol) {
          
          cores_ahe <- c(
            "#009ACD", # CGH
            "#00698C", # PCH
            "#00394D" # UHE
          )
          
          ahes_pol$detipoapr <- factor(ahes_pol$detipoapr, levels = c("CGH",
                                                                      "PCH",
                                                                      "UHE"))
          
          cores_ahe_pol <- colorFactor(cores_ahe, domain = levels(ahes_pol$detipoapr))
          
          
          
          
          
          leafletProxy("map", data = distribution) |>
            # leaflet() |>
            #   addTiles() |>
            addPolygons(
              data = ahes_pol,
              group = "atividade",
              color = "black",
              fillColor = ~ cores_ahe_pol(detipoapr),
              weight = 2,
              smoothFactor = 1,
              opacity = 0.6,
              options = pathOptions(pane = "polygons"),
              popup = paste(
                "<strong>", ahes_pol$nmoriginal, "</strong>", "</br>",
                "<strong>Domínio:</strong>",
                ahes_pol$dedominial, "</br>",
                ahes_pol$dedominio,
                "</br>
              <strong>Fiscalização:</strong> ",
                ahes_pol$defiscaliz, "</br>
              <strong>Empresa:</strong> ",
                ahes_pol$nmemp,
                "</br>
              <strong>Data do Reservatório:</strong> ",
                ahes_pol$dtreserv,
                "</br>
              <strong>Volume (m³):</strong> ",
                formata_numero(ahes_pol$nuvolumhm3),
                "</br>
              <strong>Área (ha):</strong> ",
                formata_numero(ahes_pol$nuareaha)
              )
            ) |>
            addLegend(
              data = ahes_pol,
              opacity = 0.6,
              values = ~ detipoapr,
              labels = levels(ahes_pol$detipoapr),
              colors = cores_ahe,
              title = "Hidrelétricas",
              position = "topright"
            ) 
          
        }
        
        ### termina mapa de ahes reservatorios ------------------------------------------------------
        
        # ### começa mapa de recreacao reservatorios ------------------------------------------------------
        if (input$reservrecreac) {
          
          leafletProxy("map", data = distribution) |>
            # leaflet() |>
            #   addTiles() |>
            addPolygons(
              data = reserv_recrea,
              group = "atividade",
              color = "black",
              fillColor = "blue",
              weight = 2,
              smoothFactor = 1,
              opacity = 0.6,
              options = pathOptions(pane = "polygons"),
              popup = paste(
                "<strong>", reserv_recrea$nmoriginal, "</strong>", "</br>",
                "<strong>Domínio:</strong>",
                reserv_recrea$dedominial, "</br>",
                reserv_recrea$dedominio,
                "</br>
              <strong>Fiscalização:</strong> ",
                reserv_recrea$defiscaliz, "</br>
              <strong>Empresa:</strong> ",
                reserv_recrea$nmemp,
                "</br>
              <strong>Data do Reservatório:</strong> ",
                reserv_recrea$dtreserv,
                "</br>
              <strong>Volume (m³):</strong> ",
                formata_numero(reserv_recrea$nuvolumhm3),
                "</br>
              <strong>Área (ha):</strong> ",
                formata_numero(reserv_recrea$nuareaha)
              )
            ) |>
            addLegend(
              data = reserv_recrea,
              opacity = 0.6,
              # values = ~ detipoapr,
              labels = "Recreação",
              colors = "blue",
              title = "Recreação",
              position = "topright"
            ) 
          
        }
        
        ### termina mapa de recreacao reservatorios ------------------------------------------------------
        # ### começa mapa de estradas ------------------------------------------------------
        
        
        
        if (input$estradas) {
          
          leafletProxy("map", data = distribution) |>
            # leaflet() |>
            #   addTiles() |>
            addPolylines(
              data = estradas_federais,
              group = "atividade",
              color = c("#CD3333"),
              dashArray = 5,
              # fillColor = "blue",
              weight = 3,
              smoothFactor = 1,
              opacity = 0.6,
              label = ~ htmlEscape(paste("BR", estradas_federais$vl_br)),
              # options = pathOptions(pane = "polylines"),
              popup = paste(
                "<strong>", 
                paste("BR", estradas_federais$vl_br), 
                "</strong>", "</br>",
                "<strong>Superfície:</strong>",
                estradas_federais$ds_superfi,
                "</br>
              <strong>Situação:</strong> ",
                estradas_federais$ds_legenda, "</br>
              <strong>Fonte:</strong> ",
                estradas_federais$b_Fonte
              )
            ) |>
            addLegend(
              data = estradas_federais,
              opacity = 0.6,
              # values = ~ detipoapr,
              labels = "Estradas federais",
              colors = "#CD3333",
              title = "Atividades",
              position = "topright"
            )
          
        }
        
        ### termina mapa de estradas ------------------------------------------------------
        # ### começa mapa de ferrovias ------------------------------------------------------
        
        
        
        if (input$ferrovias) {
          
          leafletProxy("map", data = distribution) |>
            # leaflet() |>
            #   addTiles() |>
            addPolylines(
              data = ferrovias,
              group = "atividade",
              color = c("black"),
              # dashArray = 5,
              # fillColor = "blue",
              weight = 3,
              smoothFactor = 1,
              opacity = 0.6,
              label = ~ htmlEscape(name),
              options = pathOptions(pane = "polylines"),
              popup = paste(
                "<strong>", 
                ferrovias$name, 
                "</strong>", "</br>",
                "<strong>Classe:</strong> ",
                ferrovias$fclass
              )
            ) |>
            addLegend(
              data = ferrovias,
              opacity = 0.6,
              # values = ~ detipoapr,
              labels = "Ferrovias",
              colors = "black",
              title = "Atividades",
              position = "topright"
            )
          
        }
        
        ### termina mapa de ferrovias ------------------------------------------------------
        
        remove_modal_spinner()
        
      }
      
      
    )
    
    output$fontes <- renderReactable({
      
      fontes <- readxl::read_xlsx("app/planilhas/fonte_dados.xlsx") |> 
        janitor::clean_names()
      
      fontes$data_download <- lubridate::ymd(fontes$data_download)
      
      
      reactable(fontes,
                searchable = TRUE,
                style = list(fontFamily = 'sans-serif'),
                defaultPageSize = 23,
                showPageSizeOptions = TRUE,
                
                columns = list(
                  camada = colDef(name = "Camada"),
                  tipo_de_dado = colDef(name = "Formato"),
                  categoria = colDef(name = "Categoria"),
                  fonte = colDef(name = "Fonte"),
                  resolucao_original = colDef(name = "Resulução original"),
                  resolucao_utilizada = colDef(name = "Resulução utilizada"),
                  ano_do_dado = colDef(name = "Ano do dado"),
                  site = colDef(name = "Site"),
                  data_download = colDef(name = "Acessado em")
                )
      )
    })
    
    
    
    
    
    
    
  })
}