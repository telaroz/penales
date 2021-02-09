library(shiny)
library(shinydashboard)
library(ggplot2)
library(shinyWidgets)
library(data.table)

ui <- dashboardPage(
  dashboardHeader(title = 'Anotador de Penales'),
  dashboardSidebar(collapsed = TRUE),
  dashboardBody(
    
    fluidRow(column(width = 2, radioGroupButtons(
      inputId = "tirador",
      label = "Tirador ",
      choices = c(sort(c("Yamal", "Santiago", "Johann", 'Emmanuel', 'Rafael', 'Justin', 'David Jimenez', 
                  "Daniel Meza", "David Molina", "Erick", 'Julio', 'Óscar Solórzano', 'Viggo', 'Manuel',
                  "Manfred", "Freddy", "Jorge", 'Daniel Villegas', 'Valery')),c('Bryan', 'Adrian',
                  'Jepherson')),
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
    
    sliderTextInput(
      inputId = "posicion_portero",
      label = "Salida del portero en metros", 
      choices = seq(0,4,0.25),
      grid = TRUE
    ),
    
    checkboxGroupButtons(
      inputId = "infraccion", 
      label = "Infraccion",
      size = 'lg', 
      choices = c("Levanta Pie", 
                  "Tiempo"),
      checkIcon = list(
        yes = tags$i(class = "fa fa-check-square", 
                     style = "color: steelblue"),
        no = tags$i(class = "fa fa-square-o", 
                    style = "color: steelblue"))
    )), 
    column(width = 6,
           h4("Click en el gráfico para agregar el tiro"),
           actionButton("rem_point", "Quite el último dato"),
           plotOutput("plot1", click = "plot_click")),
    column(width = 6,
           h4("Tiros realizados"),
           tableOutput("table"))),
    downloadButton("download", "Descargar Sesión", size = 'lg'))
  
  
  
  
  
  
  
)


server <- function(input, output) {
 # Create a reactive data.table
  values <- reactiveValues()
  values$DT <- data.table(x = numeric(),
                          y = numeric(),
                          gol = character(),
                          tirador = character(),
                          portero = character(),
                          hora = character(),
                          adicional = character(),
                          infraccion = character(),
                          posicion_portero = character(),
                          tiro_igual_finta = logical())
  
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
                          tiro_igual_finta = input$material_finta)
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
      paste0(input$dataset, ".csv")
    },
    content = function(file) {
      data.table::fwrite(values$DT, file)
    }
  )
  
  
  
}

shinyApp(ui, server)