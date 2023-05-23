# app/view/especies_pts.R

box::use(
  shiny[
    NS, h5, reactive, observe, observeEvent, moduleServer
  ],
  shinyWidgets[
    pickerInput, updatePickerInput
  ],
  dplyr[
    select, arrange, distinct, pull, filter, mutate
  ],
  shinybusy[
    show_modal_spinner
  ],

  
  leaflet[
    colorFactor, leafletProxy, clearGroup, clearMarkerClusters, removeControl, 
    clearImages, clearControls, addCircleMarkers, pathOptions,
    popupOptions, addLegend
    
  ],
  
  leaflet.extras[
    clearHeatmap
    
    
  ]
)

box::use(
  app/logic/especies_alvo[
    species_list, occ, combined_distribution
  ]
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  
  ## Selecionar espécies ameacadas
  
  pickerInput(ns("especies"),
              label = h5("Espécies:"), 
              choices = species_list, 
              multiple = TRUE,
              options = list(`actions-box` = TRUE)
  )
  
  
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    ### lista de especies
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
        dplyr::filter(nome_cientifico %in% input$especies)


    })
    
    
    observeEvent(
      input$update,
      {
        show_modal_spinner()
        
        #' @export
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
        
        
        
        
        
        
        
        
        
        
        
        
      }
    )
            
            
            
            
    
    
  }
  )}






    
    
    
    
    
    
    

  
  