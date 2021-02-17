library(shiny)
library(shinydashboard)
library(ggplot2)
library(shinyWidgets)
library(data.table)
library(shinyjs)



ui <- dashboardPage(
  
  dashboardHeader(title = "Anotador de Penales"),
  
  dashboardSidebar(collapsed = TRUE,
                   sidebarMenu(
                     menuItem("Selección Mayor", tabName = "dashboard", icon = icon("people-carry")),
                     menuItem('Escogencia de Jugadores: Mayor', tabName = 'escogencia_M'),
                     menuItem("Selección Juvenil", tabName = "widgets", icon = icon("child")),
                     menuItem('Escogencia de Jugadores: Juvenil', tabName = 'escogencia_J')
                   )
  ),
  dashboardBody(
    tabItems(
      
      tabItem(tabName = 'escogencia_M',
              column(width = 5,
                     checkboxGroupButtons(inputId = "escogencia_mayor", "Escogencia Jugadores Entrenamiento Mayor",
                                          c(sort(c("Yamal", "Santiago", "Johann", 'Emmanuel', 'Rafael', 'Justin', 'David Jimenez', 
                                                   "Daniel Meza", "David Molina", "Erick", 'Julio', 'Óscar Solórzano', 'Viggo', 'Manuel',
                                                   "Manfred", "Freddy", "Jorge", 'Daniel Villegas', 'Valery')), 
                                            sort(c("Bryan R.", "Christopher", "David Herrera", 'Esteban', 'Shadir', 'Noé',
                                                   'Óscar Lira', 'David Jiménez', 'David Vindas', 'Jeremy')), 
                                            c('Invitado 1', 'Invitado 2')), 
                                          selected = sort(c("Yamal", "Santiago", "Johann", 'Emmanuel', 'Rafael', 'Justin', 'David Jimenez', 
                                                            "Daniel Meza", "David Molina", "Erick", 'Julio', 'Óscar Solórzano', 'Viggo', 'Manuel',
                                                            "Manfred", "Freddy", "Jorge", 'Daniel Villegas', 'Valery')),
                                          status = "primary", 
                                          direction = 'vertical',
                                          checkIcon = list(
                                            yes = icon("ok", 
                                                       lib = "glyphicon"),
                                            no = icon("remove",
                                                      lib = "glyphicon")))),
              
              
              column(width = 5,
                     checkboxGroupButtons(inputId = "escogencia_mayor_porteros", "Escogencia Porteros Entrenamiento Mayor",
                                          c(sort(c("Adrian", "Bryan", 'Jepherson')), sort(c("Isaac", "Alejandro", 'Mario', 'Andrés')), 'Invitado1'), 
                                          selected = sort(c("Adrian", "Bryan", 'Jepherson')),
                                          status = "primary", 
                                          direction = 'vertical',
                                          checkIcon = list(
                                            yes = icon("ok", 
                                                       lib = "glyphicon"),
                                            no = icon("remove",
                                                      
                                            ))))),
      tabItem(tabName = 'escogencia_J',
              
              column(width = 5, checkboxGroupButtons(inputId = "escogencia_juvenil", "Escogencia Jugadores Entrenamiento Juvenil",
                                                     c(sort(c("Bryan R.", "Christopher", "David Herrera", 'Emmanuel', 'Esteban', 'Shadir', 'Noé',
                                                              'Óscar Lira', 'Viggo', 'Johann', 'David Jiménez', 'David Vindas', 'Jeremy')),
                                                       sort(c("Yamal", "Santiago", 'Rafael', 'Justin', 'David Jimenez', 
                                                              "Daniel Meza", "David Molina", "Erick", 'Julio', 'Óscar Solórzano', 'Manuel',
                                                              "Manfred", "Freddy", "Jorge", 'Daniel Villegas', 'Valery')),
                                                       c('Invitado 1', 'Invitado 2')), 
                                                     selected = sort(c("Bryan R.", "Christopher", "David Herrera", 'Emmanuel', 'Esteban', 'Shadir', 'Noé',
                                                                       'Óscar Lira', 'Viggo', 'Johann', 'David Jiménez', 'David Vindas', 'Jeremy')),
                                                     status = "primary", 
                                                     direction = 'vertical',
                                                     checkIcon = list(
                                                       yes = icon("ok", 
                                                                  lib = "glyphicon"),
                                                       no = icon("remove",
                                                                 lib = "glyphicon")))),
              
              column(width = 5,
                     checkboxGroupButtons(inputId = "escogencia_juvenil_porteros", "Escogencia Jugadores Entrenamiento Juvenil",
                                          c(sort(c("Isaac", "Alejandro", 'Mario', 'Andrés')), sort(c("Adrian", "Bryan", 'Jepherson')), 'Invitado1'), 
                                          selected = sort(c("Isaac", "Alejandro", 'Mario', 'Andrés')),
                                          status = "primary", 
                                          direction = 'vertical',
                                          checkIcon = list(
                                            yes = icon("ok", 
                                                       lib = "glyphicon"),
                                            no = icon("remove",
                                                      lib = "glyphicon"))))
              
      ),
      # First tab content
      tabItem(tabName = "dashboard",
              
              
              fluidRow(
                shinyjs::useShinyjs(),
                column(width = 2, radioGroupButtons(
                  inputId = "tirador",
                  label = "Tirador ",
                  choices = sort(c("Yamal", "Santiago", "Johann", 'Emmanuel', 'Rafael', 'Justin', 'David Jimenez', 
                                   "Daniel Meza", "David Molina", "Erick", 'Julio', 'Óscar Solórzano', 'Viggo', 'Manuel',
                                   "Manfred", "Freddy", "Jorge", 'Daniel Villegas', 'Valery')),
                  # size = 'lg',
                  direction = 'vertical',
                  individual = FALSE,
                  status = 'primary', 
                  checkIcon = list(
                    yes = icon("ok", 
                               lib = "glyphicon"))
                )),
                column(width = 2, radioGroupButtons(
                  inputId = "portero",
                  label = "Portero ",
                  choices = c("Adrian", "Bryan", 'Jepherson'),
                  # size = 'lg',
                  direction = 'horizontal',
                  individual = FALSE,
                  status = 'primary', 
                  checkIcon = list(
                    yes = icon("ok", 
                               lib = "glyphicon"))
                ),radioGroupButtons(inputId = "color",label = "Gol", choices =  c("Gol", "No Gol"), size = 'lg'),
                sliderTextInput(
                  inputId = "fatiga",
                  label = "Cansancio", 
                  choices = seq(0L, 10L, 1L),
                  grid = TRUE
                ),
                checkboxGroupButtons(
                  inputId = "Adicionales", 
                  label = "Tipo de tiro adicional",
                  size = 'lg',
                  choices = c("Finta", 
                              "Pique", "Bañito", "Rosca"),
                  checkIcon = list(
                    yes = tags$i(class = "fa fa-check-square", 
                                 style = "color: steelblue"),
                    no = tags$i(class = "fa fa-square-o", 
                                style = "color: steelblue"))
                ), 
                materialSwitch(
                  inputId = "material_finta",
                  label = "Tiro al mismo lugar de la finta", 
                  value = FALSE,
                  status = "primary"
                ),
                
                sliderInput("posicion_portero", "Salida del portero en metros",
                            min = 0, max = 4,
                            value = 0, step = 0.25),
                
                # sliderTextInput(
                #   inputId = "posicion_portero",
                #   label = "Salida del portero en metros", 
                #   choices = seq(0, 4, 0.25),
                #   grid = TRUE
                # ),
                
                checkboxGroupButtons(
                  inputId = "infraccion", 
                  label = "Infraccion",
                  size = 'lg', 
                  choices = c("Desliza/Maja", 
                              "Levanta Pie", 
                              "Tiempo",
                              "Se pasa"),
                  checkIcon = list(
                    yes = tags$i(class = "fa fa-check-square", 
                                 style = "color: steelblue"),
                    no = tags$i(class = "fa fa-square-o", 
                                style = "color: steelblue"))
                ),   
                textInput("comentarios", "Comentario", "")), 
                column(width = 6,
                       h4("Click en el gráfico para agregar el tiro"),
                       actionButton("rem_point", "Quite el último dato"),
                       plotOutput("plot1", click = "plot_click")),
                column(width = 6,
                       h4("Tiros realizados"),
                       tableOutput("table")),    downloadButton("download", "Descargar Sesión", size = 'lg'))
              
              
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              fluidRow(
                shinyjs::useShinyjs(),
                column(width = 2, radioGroupButtons(
                  inputId = "tirador2",
                  label = "Tirador ",
                  choices = c(sort(c("Bryan R.", "Christopher", "David Herrera", 'Emmanuel', 'Esteban', 'Shadir', 'Noé',
                                     'Óscar Lira', 'Viggo', 'Johann', 'David Jiménez', 'David Vindas', 'Jeremy'))),
                  # size = 'lg',
                  direction = 'vertical',
                  individual = FALSE,
                  status = 'primary', 
                  checkIcon = list(
                    yes = icon("ok", 
                               lib = "glyphicon"))
                )),
                column(width = 2, radioGroupButtons(
                  inputId = "portero2",
                  label = "Portero ",
                  choices = c("Isaac", "Alejandro", 'Mario', 'Andrés'),
                  # size = 'lg',
                  direction = 'horizontal',
                  individual = FALSE,
                  status = 'primary', 
                  checkIcon = list(
                    yes = icon("ok", 
                               lib = "glyphicon"))
                ),radioGroupButtons(inputId = "color2",label = "Gol", choices =  c("Gol", "No Gol"), size = 'lg'),
                sliderTextInput(
                  inputId = "fatiga2",
                  label = "Cansancio", 
                  choices = seq(0L, 10L, 1L),
                  grid = TRUE
                ),
                checkboxGroupButtons(
                  inputId = "Adicionales2", 
                  label = "Tipo de tiro adicional",
                  size = 'lg',
                  choices = c("Finta", 
                              "Pique", "Bañito", "Rosca"),
                  checkIcon = list(
                    yes = tags$i(class = "fa fa-check-square", 
                                 style = "color: steelblue"),
                    no = tags$i(class = "fa fa-square-o", 
                                style = "color: steelblue"))
                ), 
                materialSwitch(
                  inputId = "material_finta2",
                  label = "Tiro al mismo lugar de la finta", 
                  value = FALSE,
                  status = "primary"
                ),
                
                sliderInput("posicion_portero2", "Salida del portero en metros",
                            min = 0, max = 4,
                            value = 0, step = 0.25),
                
                # sliderTextInput(
                #   inputId = "posicion_portero",
                #   label = "Salida del portero en metros", 
                #   choices = seq(0, 4, 0.25),
                #   grid = TRUE
                # ),
                
                checkboxGroupButtons(
                  inputId = "infraccion2", 
                  label = "Infraccion",
                  size = 'lg', 
                  choices = c("Desliza/Maja", 
                              "Levanta Pie", 
                              "Tiempo",
                              "Se pasa"),
                  checkIcon = list(
                    yes = tags$i(class = "fa fa-check-square", 
                                 style = "color: steelblue"),
                    no = tags$i(class = "fa fa-square-o", 
                                style = "color: steelblue"))
                ),   
                textInput("comentarios2", "Comentario", "")), 
                column(width = 6,
                       h4("Click en el gráfico para agregar el tiro"),
                       actionButton("rem_point2", "Quite el último dato"),
                       plotOutput("plot2", click = "plot_click2")),
                column(width = 6,
                       h4("Tiros realizados"),
                       tableOutput("table2")),    downloadButton("download2", "Descargar Sesión", size = 'lg'))
      )
    )
  )
)

server <- function(input, output, session) {
  
  
  jugadores_mayor <- sort(c("Yamal", "Santiago", "Johann", 'Emmanuel', 'Rafael', 'Justin', 'David Jimenez', 
                            "Daniel Meza", "David Molina", "Erick", 'Julio', 'Óscar Solórzano', 'Viggo', 'Manuel',
                            "Manfred", "Freddy", "Jorge", 'Daniel Villegas', 'Valery'))
  
  jugadores_juvenil <- sort(c("Bryan R.", "Christopher", "David Herrera", 'Emmanuel', 'Esteban', 'Shadir', 'Noé',
                              'Óscar Lira', 'Viggo', 'Johann', 'David Jiménez', 'David Vindas', 'Jeremy'))
  
  jugadores <- unique(c(jugadores_mayor, jugadores_juvenil, 'Invitado1', 'Invitado2', 'Invitado3'))
  
  
  porteros_mayor <- sort(c("Adrian", "Bryan", 'Jepherson'))
  porteros_juvenil <- sort(c("Isaac", "Alejandro", 'Mario', 'Andrés'))
  porteros <- unique(c(porteros_mayor, porteros_juvenil, 'Invitado1'))
  
  
  observeEvent(input$escogencia_mayor, {
    jugadores_escogidos <- jugadores[jugadores %in% input$escogencia_mayor]
    
    # Method 1
    updateRadioGroupButtons(session = session, inputId = "tirador",
                            choices = jugadores_escogidos, selected = jugadores_mayor, status = 'primary', checkIcon = list(
                              yes = icon("ok", 
                                         lib = "glyphicon")))
    
    
  }, ignoreInit = TRUE)
  
  
  observeEvent(input$escogencia_juvenil, {
    jugadores_escogidos <- jugadores[jugadores %in% input$escogencia_juvenil]
    
    # Method 1
    updateRadioGroupButtons(session = session, inputId = "tirador2",
                            choices = jugadores_escogidos, selected = jugadores_juvenil, status = 'primary', checkIcon = list(
                              yes = icon("ok", 
                                         lib = "glyphicon")))
    
    
  }, ignoreInit = TRUE)
  
  observeEvent(input$escogencia_juvenil_porteros, {
    porteros_escogidos <- porteros[porteros %in% input$escogencia_juvenil_porteros]
    
    # Method 1
    updateRadioGroupButtons(session = session, inputId = "portero2",
                            choices = porteros_escogidos, selected = porteros_juvenil, status = 'primary', checkIcon = list(
                              yes = icon("ok", 
                                         lib = "glyphicon")))
    
    
  }, ignoreInit = TRUE)
  
  
  observeEvent(input$escogencia_mayor_porteros, {
    porteros_escogidos <- porteros[porteros %in% input$escogencia_mayor_porteros]
    
    # Method 1
    updateRadioGroupButtons(session = session, inputId = "portero",
                            choices = porteros_escogidos, selected = porteros_mayor, status = 'primary', checkIcon = list(
                              yes = icon("ok", 
                                         lib = "glyphicon")))
    
    
  }, ignoreInit = TRUE)
  
  
  
  
  
  
  values <- reactiveValues()
  values$DT2 <- data.table(x = numeric(),
                           y = numeric(),
                           gol = character(),
                           tirador = character(),
                           portero = character(),
                           hora = character(),
                           adicional = character(),
                           infraccion = character(),
                           posicion_portero = numeric(),
                           tiro_igual_finta = logical(),
                           fatiga = integer(),
                           comentario = character())
  
  # Create a plot
  output$plot2 = renderPlot({
    ggplot(values$DT2, aes(x = x, y = y)) +
      geom_point(color = data.table::fifelse(values$DT2$gol == 'Gol', 'Green', 'Red'), size = 6) +
      lims(x = c(-3, 3), y = c(-1, 2)) +
      theme(legend.position = "none") +
      # include so that colors don't change as more colors are chosen
      scale_color_discrete(drop = FALSE) +
      theme_void() +
      geom_segment(aes(x = -1.5, xend = 1.5, y = 1, yend = 1), colour = "#7A1F1F", size = 3) + 
      geom_segment(aes(x = -1.5, xend = -1.5, y = -1, yend = 1), colour = "#7A1F1F", size = 3) + 
      geom_segment(aes(x = 1.5, xend = 1.5, y = -1, yend = 1), colour = "#7A1F1F", size = 3) +
      coord_fixed()
  })
  
  # Add a new row, as a reaction to a click
  observeEvent(input$plot_click2, {
    add_row <- data.table(x = input$plot_click2$x,
                          y = input$plot_click2$y,
                          gol = ifelse(input$color2 == 'Gol', 'Gol', 'NO Gol'),
                          tirador = input$tirador2,
                          portero = input$portero2,
                          hora = as.character(Sys.time()),
                          adicional = paste0(sort(input$Adicionales2), collapse = '_'),
                          infraccion = paste0(sort(input$infraccion2), collapse = '_'),
                          posicion_portero = paste0(sort(input$posicion_portero2), collapse = '_'),
                          tiro_igual_finta = input$material_finta2,
                          fatiga = input$fatiga2,
                          comentario = input$comentarios2)
    values$DT2 <- rbind(values$DT2, add_row)
  })
  
  # Action button in case a row should be removed.
  observeEvent(input$rem_point2, {
    rem_row <- values$DT[-nrow(values$DT2), ]
    values$DT2 <- rem_row
  })
  
  # Render the table
  output$table2 <- renderTable({
    data <- data.table::setDT(tail(values$DT2, 10))
    a_mostrar <- nrow(data[!is.na(x)])
    data[min(10,a_mostrar):1]
  })
  
  # Add a download button
  output$download2 <- downloadHandler(
    filename = function() {
      paste0('penales_juvenil', Sys.Date(),'.csv')
    },
    content = function(file) {
      data.table::fwrite(values$DT2, file)
    }
  )
  
  observeEvent(input$plot_click2, {
    shinyjs::reset(id = 'comentarios2')
    shinyjs::reset(id = 'posicion_portero2')
  })
  
  
  
  
  
  
  
  
  # Ahora el de la mayor
  
  values$DT <- data.table(x = numeric(),
                          y = numeric(),
                          gol = character(),
                          tirador = character(),
                          portero = character(),
                          hora = character(),
                          adicional = character(),
                          infraccion = character(),
                          posicion_portero = numeric(),
                          tiro_igual_finta = logical(),
                          fatiga = integer(),
                          comentario = character())
  
  # Create a plot
  output$plot1 = renderPlot({
    ggplot(values$DT, aes(x = x, y = y)) +
      geom_point(color = data.table::fifelse(values$DT$gol == 'Gol', 'Green', 'Red'), size = 6) +
      lims(x = c(-3, 3), y = c(-1, 2)) +
      theme(legend.position = "none") +
      # include so that colors don't change as more colors are chosen
      scale_color_discrete(drop = FALSE) +
      theme_void() +
      geom_segment(aes(x = -1.5, xend = 1.5, y = 1, yend = 1), colour = "#7A1F1F", size = 3) + 
      geom_segment(aes(x = -1.5, xend = -1.5, y = -1, yend = 1), colour = "#7A1F1F", size = 3) + 
      geom_segment(aes(x = 1.5, xend = 1.5, y = -1, yend = 1), colour = "#7A1F1F", size = 3) +
      coord_fixed()
  })
  
  # Add a new row, as a reaction to a click
  observeEvent(input$plot_click, {
    add_row <- data.table(x = input$plot_click$x,
                          y = input$plot_click$y,
                          gol = ifelse(input$color == 'Gol', 'Gol', 'NO Gol'),
                          tirador = input$tirador,
                          portero = input$portero,
                          hora = as.character(Sys.time()),
                          adicional = paste0(sort(input$Adicionales), collapse = '_'),
                          infraccion = paste0(sort(input$infraccion), collapse = '_'),
                          posicion_portero = paste0(sort(input$posicion_portero), collapse = '_'),
                          tiro_igual_finta = input$material_finta,
                          fatiga = input$fatiga,
                          comentario = input$comentarios)
    values$DT <- rbind(values$DT, add_row)
  })
  
  # Action button in case a row should be removed.
  observeEvent(input$rem_point, {
    rem_row <- values$DT[-nrow(values$DT), ]
    values$DT <- rem_row
  })
  
  # Render the table
  output$table <- renderTable({
    data <- data.table::setDT(tail(values$DT, 10))
    a_mostrar <- nrow(data[!is.na(x)])
    data[min(10,a_mostrar):1]
  })
  
  # Add a download button
  output$download <- downloadHandler(
    filename = function() {
      paste0('penales_mayor', Sys.Date(),'.csv')
    },
    content = function(file) {
      data.table::fwrite(values$DT, file)
    }
  )
  
  observeEvent(input$plot_click, {
    shinyjs::reset(id = 'comentarios')
    shinyjs::reset(id = 'posicion_portero')
  })
  
}



shinyApp(ui, server)