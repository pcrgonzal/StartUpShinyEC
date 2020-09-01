library(shiny)
library(shinydashboard)
library(modeest)
library(DT)
library(RColorBrewer)
library(ggplot2)
library(party)
library(nortest)


dropMenu <- dropdownMenu(type = "messages",
                         messageItem(
                           from = "Actualización",
                           message = "Se encuentra disponible la opción de Análisis conjunto",
                           icon = icon("life-ring")
                         )
)

header <- dashboardHeader(title = "Análisis de divorcios", dropMenu)


sidebarMenu <- sidebarMenu(
  menuItem(
    "Principal",
    tabName = "main",
    icon = icon("dashboard")
  ),
  menuItem(
    "Gráficas",
    icon = icon("th"),
    tabName = "graphics",
    badgeLabel = "recomendado",
    badgeColor = "red"
  ),
  menuItem(
    "Análisis bivariante",
    icon = icon("medrt"),
    tabName = "relation",
    badgeLabel = "nuevo",
    badgeColor = "green"
  ),
  menuItem(
    "Ejemplos",
    icon = icon("question-circle"),
    tabName = "examples"
  )
)

sidebar <- dashboardSidebar(sidebarMenu)

variablesTab <- tabItem(
  tabName = "main",
  h2("Divorcios en Ecuador"),
  h3("Variables escogidas"),
  fluidRow(
    box(
      title = "Variables cuantitativas",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      "Año de divorcio",
      br(),
      "Año de matrimonio",
      br(),
      "Duración de matrimonio",
      br(),
      "Edad del hombre",
      br(),
      "Edad de la mujer"
    ),
    
    box(
      title = "Variables cualitativas",
      status = "warning",
      solidHeader = TRUE,
      collapsible = TRUE,
      "Provincia",
      br(),
      "Causa del divorcio",
      br(),
      "Nivel de educación del hombre",
      br(),
      "Nivel de educación de la mujer"
    )
  ),
  fluidRow(
    tabBox(
      title = "Todas las variables",
      id = "allVarsTabs",
      width = 700,
      tabPanel("Año de divorcio",
               fluidRow(
                 box(width = 6, DTOutput('table_tab1')),
                 column(6, fluidRow(
                   column(12, valueBoxOutput(width = 5 , "value_box_tab1")),
                   box(
                     width = 12,
                     verbatimTextOutput("summary_box_tab1"),
                     title = "Resumen de años"
                   ),
                   column(12, imageOutput("image1"))
                 ))
               )),
      
      tabPanel("Año de matrimonio",
               fluidRow(
                 box(width = 6, DTOutput('table_tab2')),
                 column(6, fluidRow(
                   column(12, valueBoxOutput(width = 5 , "value_box_tab2")),
                   box(
                     width = 12,
                     verbatimTextOutput("summary_box_tab2"),
                     title = "Resumen de años"
                   ),
                   column(12, imageOutput("image2"))
                 ))
               )),
      
      tabPanel("Duración de matrimonio",
               fluidRow(
                 box(width = 6, DTOutput('table_tab3')),
                 column(6, fluidRow(
                   column(12, valueBoxOutput(width = 5 , "value_box_tab3")),
                   box(
                     width = 12,
                     verbatimTextOutput("summary_box_tab3"),
                     title = "Resumen de duración"
                   ),
                   column(12, imageOutput("image3"))
                 ))
               )),
      
      tabPanel("Edad del hombre",
               fluidRow(
                 box(width = 6, DTOutput('table_tab4')),
                 column(6, fluidRow(
                   column(12, valueBoxOutput(width = 5 , "value_box_tab4")),
                   box(
                     width = 12,
                     verbatimTextOutput("summary_box_tab4"),
                     title = "Resumen de edades"
                   ),
                   column(12, imageOutput("image4"))
                 ))
               )),
      
      tabPanel("Edad de la mujer",
               fluidRow(
                 box(width = 6, DTOutput('table_tab5')),
                 column(6, fluidRow(
                   column(12, valueBoxOutput(width = 5 , "value_box_tab5")),
                   box(
                     width = 12,
                     verbatimTextOutput("summary_box_tab5"),
                     title = "Resumen de edades"
                   ),
                   column(12, imageOutput("image5"))
                 ))
               )),
      
      tabPanel("Provincia",
               fluidRow(
                 box(width = 6, DTOutput('table_tab6')),
                 column(6, fluidRow(
                   column(12, valueBoxOutput(width = 5 , "value_box_tab6")),
                   box(
                     width = 12,
                     verbatimTextOutput("summary_box_tab6"),
                     title = "Resumen de provincias"
                   ),
                   column(12, imageOutput("image6"))
                 ))
               )),
      
      tabPanel("Causa del divorcio",
               fluidRow(
                 box(width = 6, DTOutput('table_tab7')),
                 column(6, fluidRow(
                   column(12, valueBoxOutput(width = 5 , "value_box_tab7")),
                   box(
                     width = 12,
                     verbatimTextOutput("summary_box_tab7"),
                     title = "Resumen de causas"
                   ),
                   column(12, imageOutput("image7"))
                 ))
               )),
      
      tabPanel("Nivel de educación del hombre",
               fluidRow(
                 box(width = 6, DTOutput('table_tab8')),
                 column(6, fluidRow(
                   column(12, valueBoxOutput(width = 5 , "value_box_tab8")),
                   box(
                     width = 12,
                     verbatimTextOutput("summary_box_tab8"),
                     title = "Resumen de nivel"
                   ),
                   column(12, imageOutput("image8"))
                 ))
               )),
      
      tabPanel("Nivel de educación de la mujer",
               fluidRow(
                 box(width = 6, DTOutput('table_tab9')),
                 column(6, fluidRow(
                   column(12, valueBoxOutput(width = 5 , "value_box_tab9")),
                   box(
                     width = 12,
                     verbatimTextOutput("summary_box_tab9"),
                     title = "Resumen de nivel"
                   ),
                   column(12, imageOutput("image9"))
                 ))
               ))
    )
  )
)


graficasTab <- tabItem(tabName = "graphics",
                       h2("Gráficas"),
                       fluidRow(
                         column(
                         4, selectInput(
                           "selectVar",
                           label = h3("Variables"),
                           choices = list(
                             "Año de divorcio" = 1,
                             "Año de matrimonio" = 2,
                             "Duración de matrimonio" = 3,
                             "Edad del hombre" = 4,
                             "Edad de la mujer" = 5,
                             "Provincia" = 6,
                             "Causa del divorcio" = 7,
                             "Nivel de educación del hombre" = 8,
                             "Nivel de educación de la mujer" = 9
                           ),
                           selected = 1
                         )
                       ),
                       box(
                         width = 4, 
                         sliderInput(
                           "slider1", 
                           label = h3("Número de divisiones"), 
                           min = 0, 
                           max = 50, 
                           value = 5, 
                           animate = TRUE)
                        ),
                       column(
                         12,
                         box(title = "Gráfica de barras", plotOutput("barOut")),
                         box(title = "Gráfico Pie", plotOutput("pieOut")),
                         box(id = "b1" , title = "Histograma", plotOutput("histOut")),
                         box(title = "Diagrama de cajas", plotOutput("boxOut"))
                         )
                  )
            )


ejemplosTab <- tabItem(tabName = "examples",
                       fluidPage(
                         titlePanel("Ejemplo Contenedores"),
                         
                         sidebarLayout( 
                           
                           sidebarPanel(
                             width = 4,
                                 sliderInput(
                                   inputId = "contenedores",
                                   label = "Numero de contenedores",  #aconpana al input
                                   min = 1,
                                   max = 30,
                                   value = 25
                                 ), 
                                 verbatimTextOutput("info"),
                                 imageOutput("image30")
                             
                             
                           ),
                           
                           mainPanel(
                             h1("Panel de selección"),
                             selectInput("Color", "Seleccione un color", 
                                         choices = c("Rojo", "Verde", "Azul"),
                                         selected="Verde"),
                             
                             
                             #grafica de histograma
                             plotOutput(outputId = 'plot', click = "plot_click")
                           )
                         )
                       ))



conjuntasTab <- tabItem(tabName = 'relation', 
                        fluidRow(
                          column(width = 6,
                                 box(width = 12, 
                                   title = "Análisis en pares", solidHeader = TRUE, status = "primary",
                                   h4("Escoja las variables que desea comparar"),
                                   column(width = 6,
                                          selectInput(
                                            "svar1", selectize=TRUE,
                                            label = h6("Variable 1 (x)"),
                                            choices = list(
                                              "Año de divorcio" = 1,
                                              "Año de matrimonio" = 2,
                                              "Duración de matrimonio" = 3,
                                              "Edad del hombre" = 4,
                                              "Edad de la mujer" = 5
                                            ),
                                            selected = 1
                                          )
                                   ),
                                   column(width = 6,
                                     selectInput(
                                       "svar2", selectize=TRUE,
                                       label = h6("Variable 2 (y)"),
                                       choices = list(
                                         "Año de divorcio" = 1,
                                         "Año de matrimonio" = 2,
                                         "Duración de matrimonio" = 3,
                                         "Edad del hombre" = 4,
                                         "Edad de la mujer" = 5
                                       ),
                                       selected = 2
                                     )
                                   )
                                 ), 
                                 box(width = 12, plotOutput("varPlot1")),
                                 box(width = 12,
                                     checkboxInput("checkbox2", label = "Cambiar de variable", value = FALSE) ,
                                     plotOutput("varPlot2")),
                                 box(width = 12, plotOutput("varPlot3"))
                              )
                          ,
                          column(width = 6,
                                 box(width = 12,
                                   title = "Relación estadística de las variables", 
                                   background = "yellow",
                                   "Matriz de correlación",
                                   verbatimTextOutput("varcor"),
                                   "Resumen de cada variable",
                                   verbatimTextOutput("varsummary"),
                                   "Prueba Kolmogorov-Smirnov Lilliefors",
                                   verbatimTextOutput("vartest"),
                                   verbatimTextOutput("vartest2"),
                                   "Modelo de regresión lineal",
                                   verbatimTextOutput("varlinear"),
                                   verbatimTextOutput("vardetail"),
                                   checkboxInput("checkbox", label = "Ver estimadores y errores (primeros 50)", value = FALSE)
                                 ),
                                 box(width = 12, plotOutput("varPlot4"))
                                 
                            
                          )
                          
                        )
  
)




body <-
  dashboardBody(tabItems(variablesTab, graficasTab , ejemplosTab, conjuntasTab))


ui <- dashboardPage(header, sidebar, body,  skin = "purple")

server <- function(input, output) {
  setwd("C:/Users/paula/OneDrive - Escuela Superior Politécnica del Litoral/Estadística/2020/rstudio/shiny/proyecto/proyecto")
  datos <- read.csv("EDV.csv", sep = ";", header = TRUE, nrows = 15000)
  attach(datos)
  
  ceiling_dec <-
    function(x, level = 1)
      round(x + 5 * 10 ^ (-level - 1), level)
  
  #tab de anio divorcio
  ft_anioDiv <- data.frame(table(anio_div))
  colnames(ft_anioDiv) <- c('Año', 'Divorcios')
  output$table_tab1 <- renderDT(ft_anioDiv, selection = 'single')
  
  
  output$value_box_tab1 <- renderValueBox({
    rowSelected1 <- input$table_tab1_rows_selected
    anio <- ft_anioDiv$Año[rowSelected1]
    value <- ft_anioDiv$Divorcios[rowSelected1] / length(anio_div)
    porcen <- ceiling_dec(value * 100, 2)
    text <- paste0(porcen, "%")
    if (length(porcen) == 0) {
      text <- "- %"
      anio <- "Año"
    }
    print(porcen)
    valueBox(text,
             anio,
             icon = icon("list"),
             color = "yellow")
  })
  
  output$summary_box_tab1 <- renderPrint({
    summary(anio_div)
  })
  
  output$image1 <- renderImage({
    return(
      list(
        src = "divorce.jpg",
        contentType = "image/png",
        alt = "ring",
        width = 200,
        height = 200
      )
    )
    
    
  }, deleteFile = FALSE)
  
  #tab de anio matri
  ft_anioDiv2 <- data.frame(table(anio_mat))
  colnames(ft_anioDiv2) <- c('Año', 'Matrimonios')
  output$table_tab2 <- renderDT(ft_anioDiv2, selection = 'single')
  
  
  output$value_box_tab2 <- renderValueBox({
    rowSelected1 <- input$table_tab2_rows_selected
    anio <- ft_anioDiv2$Año[rowSelected1]
    value <- ft_anioDiv2$Matrimonios[rowSelected1] / length(anio_mat)
    porcen <- ceiling_dec(value * 100, 2)
    text <- paste0(porcen, "%")
    if (length(porcen) == 0) {
      text <- "- %"
      anio <- "Año"
    }
    print(porcen)
    valueBox(text,
             anio,
             icon = icon("list"),
             color = "purple")
  })
  
  output$summary_box_tab2 <- renderPrint({
    summary(anio_mat)
  })
  
  output$image2 <- renderImage({
    return(
      list(
        src = "wed.png",
        contentType = "image/png",
        alt = "wed",
        width = 200,
        height = 200
      )
    )
    
    
  }, deleteFile = FALSE)
  
  
  #tab de duracion matri
  ft_anioDiv3 <- data.frame(table(dur_mat))
  colnames(ft_anioDiv3) <- c('Años', 'Duración')
  output$table_tab3 <- renderDT(ft_anioDiv3, selection = 'single')
  
  
  output$value_box_tab3 <- renderValueBox({
    rowSelected1 <- input$table_tab3_rows_selected
    anio <- ft_anioDiv3$Años[rowSelected1]
    value <- ft_anioDiv3$Duración[rowSelected1] / length(dur_mat)
    porcen <- ceiling_dec(value * 100, 2)
    text <- paste0(porcen, "%")
    if (length(porcen) == 0) {
      text <- "- %"
      anio <- "Duración"
    }
    print(porcen)
    valueBox(text,
             anio,
             icon = icon("list"),
             color = "blue")
  })
  
  output$summary_box_tab3 <- renderPrint({
    summary(dur_mat)
  })
  
  output$image3 <- renderImage({
    return(
      list(
        src = "time.png",
        contentType = "image/png",
        alt = "wed",
        width = 200,
        height = 200
      )
    )
    
    
  }, deleteFile = FALSE)
  
  #tab de edad hombre
  ft_anioDiv4 <- data.frame(table(edad_hom))
  colnames(ft_anioDiv4) <- c('Años', 'Cantidad')
  output$table_tab4 <- renderDT(ft_anioDiv4, selection = 'single')
  
  
  output$value_box_tab4 <- renderValueBox({
    rowSelected1 <- input$table_tab4_rows_selected
    anio <- ft_anioDiv4$Años[rowSelected1]
    value <- ft_anioDiv4$Cantidad[rowSelected1] / length(edad_hom)
    porcen <- ceiling_dec(value * 100, 2)
    text <- paste0(porcen, "%")
    if (length(porcen) == 0) {
      text <- "- %"
      anio <- "Edad"
    }
    print(porcen)
    valueBox(text,
             anio,
             icon = icon("list"),
             color = "blue")
  })
  
  output$summary_box_tab4 <- renderPrint({
    summary(edad_hom)
  })
  
  output$image4 <- renderImage({
    return(
      list(
        src = "man.png",
        contentType = "image/png",
        alt = "wed",
        width = 200,
        height = 200
      )
    )
    
    
  }, deleteFile = FALSE)
  
  
  #tab de edad mujer
  ft_anioDiv5 <- data.frame(table(edad_muj))
  colnames(ft_anioDiv5) <- c('Años', 'Cantidad')
  output$table_tab5 <- renderDT(ft_anioDiv5, selection = 'single')
  
  
  output$value_box_tab5 <- renderValueBox({
    rowSelected1 <- input$table_tab5_rows_selected
    anio <- ft_anioDiv5$Años[rowSelected1]
    value <- ft_anioDiv5$Cantidad[rowSelected1] / length(edad_muj)
    porcen <- ceiling_dec(value * 100, 2)
    text <- paste0(porcen, "%")
    if (length(porcen) == 0) {
      text <- "- %"
      anio <- "Edad"
    }
    print(porcen)
    valueBox(text,
             anio,
             icon = icon("list"),
             color = "red")
  })
  
  output$summary_box_tab5 <- renderPrint({
    summary(edad_muj)
  })
  
  output$image5 <- renderImage({
    return(
      list(
        src = "woman.png",
        contentType = "image/png",
        alt = "wed",
        width = 100,
        height = 200
      )
    )
    
    
  }, deleteFile = FALSE)
  
  
  #tab de provincia
  ft_anioDiv6 <- data.frame(table(prov_insc))
  colnames(ft_anioDiv6) <- c('Provincia', 'Cantidad')
  output$table_tab6 <- renderDT(ft_anioDiv6, selection = 'single')
  
  
  output$value_box_tab6 <- renderValueBox({
    rowSelected1 <- input$table_tab6_rows_selected
    anio <- ft_anioDiv6$Provincia[rowSelected1]
    value <- ft_anioDiv6$Cantidad[rowSelected1] / length(prov_insc)
    porcen <- ceiling_dec(value * 100, 2)
    text <- paste0(porcen, "%")
    if (length(porcen) == 0) {
      text <- "- %"
      anio <- "Provincia"
    }
    print(porcen)
    valueBox(text,
             anio,
             icon = icon("list"),
             color = "red")
  })
  
  output$summary_box_tab6 <- renderPrint({
    summary(prov_insc)
  })
  
  output$image6 <- renderImage({
    return(
      list(
        src = "loc.png",
        contentType = "image/png",
        alt = "wed",
        width = 200,
        height = 200
      )
    )
    
    
  }, deleteFile = FALSE)
  
  
  #tab de causas
  ft_anioDiv7 <- data.frame(table(cau_div))
  colnames(ft_anioDiv7) <- c('Causa', 'Cantidad')
  output$table_tab7 <- renderDT(ft_anioDiv7, selection = 'single')
  
  
  output$value_box_tab7 <- renderValueBox({
    rowSelected1 <- input$table_tab7_rows_selected
    anio <- ft_anioDiv7$Causa[rowSelected1]
    value <- ft_anioDiv7$Cantidad[rowSelected1] / length(cau_div)
    porcen <- ceiling_dec(value * 100, 2)
    text <- paste0(porcen, "%")
    if (length(porcen) == 0) {
      text <- "- %"
      anio <- "Causa"
    }
    print(porcen)
    valueBox(text,
             anio,
             icon = icon("list"),
             color = "yellow")
  })
  
  output$summary_box_tab7 <- renderPrint({
    summary(cau_div)
  })
  
  output$image7 <- renderImage({
    return(
      list(
        src = "causes.png",
        contentType = "image/png",
        alt = "wed",
        width = 200,
        height = 200
      )
    )
    
    
  }, deleteFile = FALSE)
  
  
  #tab de niv hombre
  ft_anioDiv8 <- data.frame(table(niv_insth))
  colnames(ft_anioDiv8) <- c('Nivel', 'Cantidad')
  output$table_tab8 <- renderDT(ft_anioDiv8, selection = 'single')
  
  
  output$value_box_tab8 <- renderValueBox({
    rowSelected1 <- input$table_tab8_rows_selected
    anio <- ft_anioDiv8$Nivel[rowSelected1]
    value <- ft_anioDiv8$Cantidad[rowSelected1] / length(niv_insth)
    porcen <- ceiling_dec(value * 100, 2)
    text <- paste0(porcen, "%")
    if (length(porcen) == 0) {
      text <- "- %"
      anio <- "Nivel académico"
    }
    print(porcen)
    valueBox(text,
             anio,
             icon = icon("list"),
             color = "purple")
  })
  
  output$summary_box_tab8 <- renderPrint({
    summary(niv_insth)
  })
  
  output$image8 <- renderImage({
    return(
      list(
        src = "meng.png",
        contentType = "image/png",
        alt = "wed",
        width = 200,
        height = 200
      )
    )
    
    
  }, deleteFile = FALSE)
  
  
  #tab de niv mujer
  ft_anioDiv9 <- data.frame(table(niv_instm))
  colnames(ft_anioDiv9) <- c('Nivel', 'Cantidad')
  output$table_tab9 <- renderDT(ft_anioDiv9, selection = 'single')
  
  
  output$value_box_tab9 <- renderValueBox({
    rowSelected1 <- input$table_tab8_rows_selected
    anio <- ft_anioDiv9$Nivel[rowSelected1]
    value <- ft_anioDiv9$Cantidad[rowSelected1] / length(niv_instm)
    porcen <- ceiling_dec(value * 100, 2)
    text <- paste0(porcen, "%")
    if (length(porcen) == 0) {
      text <- "- %"
      anio <- "Nivel académico"
    }
    print(porcen)
    valueBox(text,
             anio,
             icon = icon("list"),
             color = "green")
  })
  
  output$summary_box_tab9 <- renderPrint({
    summary(niv_instm)
  })
  
  output$image9 <- renderImage({
    return(
      list(
        src = "womang.png",
        contentType = "image/png",
        alt = "wed",
        width = 200,
        height = 200
      )
    )
    
    
  }, deleteFile = FALSE)
  
  
  #plotstab
  
  
  output$histOut <- renderPlot({
    varSelected<- switch(as.integer(input$selectVar),anio_div,anio_mat,dur_mat,edad_hom,edad_muj)
    if(input$selectVar > 5){
      
      h3("None")
      
    }else{
      len<- input$slider1+1
      bins <- seq(min(varSelected), max(varSelected), length.out = len )
      hist(varSelected, breaks = bins, col = "#75AADB", border = "white",
           xlab = "Tiempo", main="Histogram") 
    }
    
  })
  
  output$barOut <- renderPlot({
    varSelected<- switch(as.integer(input$selectVar),anio_div,anio_mat,dur_mat,edad_hom,edad_muj, prov_insc, cau_div, niv_insth, niv_instm)
    len<- input$slider1+1
    barplot(table(varSelected),
            xlab = "Descripción",
            ylab = "Cantidad",
            border="red",
            col="blue",
            density=10, main= "Barplot")
  })
  
  output$pieOut <- renderPlot({
    varSelected<- switch(as.integer(input$selectVar),anio_div,anio_mat,dur_mat,edad_hom,edad_muj, prov_insc, cau_div, niv_insth, niv_instm)
    mytable <- table(varSelected)
    lbls <- paste(names(mytable), "\n", mytable, sep="")
    pie(mytable,
        labels=NA,
        clockwise=TRUE,
        col=brewer.pal(7,"Set1"),
        border="white",
        radius=0.7,
        cex=0.8,
        main="Pie Chart")
    legend("bottomright",legend=lbls,bty="n",
           fill=brewer.pal(7,"Set1"), bg = 'pink', ncol = 2)
  })
  
  output$boxOut <- renderPlot({
    if(input$selectVar > 5){
      
      h3("None")
      
    }else{
      varSelected<- switch(as.integer(input$selectVar),anio_div,anio_mat,dur_mat,edad_hom,edad_muj)
      boxplot(varSelected,
              main = "Boxplot",
              xlab="Tiempo",
              col = "orange",
              border = "brown",
              horizontal = TRUE,
              notch = TRUE
      )
    }
    
  })
  
  
  
  #examplestab
  
  output$plot <- renderPlot(
    # Expresion que genera el plot
    {
      x <- faithful$waiting
      contenedores <- seq(min(x), max(x), length.out = input$contenedores + 1)
      selectedColor <- input$Color
      if(selectedColor == "Rojo"){
        hist(x, 
             breaks = contenedores, 
             col = "#E85757",
             border = "white",
             xlab = "Tiempo de espera entre erupciones",
             ylab = "Frecuencia",
             main = "HISTOGRAMA")
      } 
      else if(selectedColor == "Verde"){
        hist(x, 
             breaks = contenedores, 
             col = "#B6F39F",
             border = "white",
             xlab = "Tiempo de espera entre erupciones",
             ylab = "Frecuencia",
             main = "HISTOGRAMA")
      }  
      else if(selectedColor == "Azul"){
        hist(x, 
             breaks = contenedores, 
             col = "#87AFF6",
             border = "white",
             xlab = "Tiempo de espera entre erupciones",
             ylab = "Frecuencia",
             main = "HISTOGRAMA")
      }
    }
  )
  
  output$info <- renderText({
    paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
  })
  
  output$image30 <- renderImage({
    return(
      list(
        src = "cont.png",
        contentType = "image/png",
        alt = "wed",
        width = 200,
        height = 200
      )
    )
    
    
  }, deleteFile = FALSE)
  
  
  #relationTab
  
  #resumen de dos variables
  
  
  output$varsummary <- renderPrint({
    var1<- switch(as.integer(input$svar1),anio_div,anio_mat,dur_mat,edad_hom,edad_muj)
    name1<- switch(as.integer(input$svar1),"Año de divorcio","Año de matrimonio","Duración en años","Edad del hombre", "Edad de la mujer")
    var2<- switch(as.integer(input$svar2),anio_div,anio_mat,dur_mat,edad_hom,edad_muj)
    name2<- switch(as.integer(input$svar2),"Año de divorcio","Año de matrimonio","Duración en años","Edad del hombre", "Edad de la mujer")
    #agrupar datos y realizar comando
    #tapply(var1,var2,mean)
    mix <- cbind(var1,var2)
    colnames(mix) <- c(name1,name2)
    summary(mix)
  })
  
  output$varcor <- renderPrint({
    var1<- switch(as.integer(input$svar1),anio_div,anio_mat,dur_mat,edad_hom,edad_muj)
    name1<- switch(as.integer(input$svar1),"Año de divorcio","Año de matrimonio","Duración en años","Edad del hombre", "Edad de la mujer")
    var2<- switch(as.integer(input$svar2),anio_div,anio_mat,dur_mat,edad_hom,edad_muj)
    name2<- switch(as.integer(input$svar2),"Año de divorcio","Año de matrimonio","Duración en años","Edad del hombre", "Edad de la mujer")
    #agrupar datos y realizar comando
    #tapply(var1,var2,mean)
    mix <- cbind(var1,var2)
    colnames(mix) <- c(name1,name2)
    cor(mix)
  })
  
  output$vartest <- renderPrint({
    var1<- switch(as.integer(input$svar1),anio_div,anio_mat,dur_mat,edad_hom,edad_muj)
    #res <- ks.test(var1, pnorm, mean(var1), sd(var1))
    res <- lillie.test(x = var1)
    res
  })
  
  output$vartest2 <- renderPrint({
    var2<- switch(as.integer(input$svar2),anio_div,anio_mat,dur_mat,edad_hom,edad_muj)
    #res <- ks.test(var2, pnorm, mean(var2), sd(var2))
    res <- lillie.test(x = var2)
    res
  })
  
  output$varlinear <- renderPrint({
    var1<- switch(as.integer(input$svar1),anio_div,anio_mat,dur_mat,edad_hom,edad_muj)
    name1<- switch(as.integer(input$svar1),"Año de divorcio","Año de matrimonio","Duración en años","Edad del hombre", "Edad de la mujer")
    var2<- switch(as.integer(input$svar2),anio_div,anio_mat,dur_mat,edad_hom,edad_muj)
    name2<- switch(as.integer(input$svar2),"Año de divorcio","Año de matrimonio","Duración en años","Edad del hombre", "Edad de la mujer")
    #agrupar datos y realizar comando
    #tapply(var1,var2,mean)
    mix <- cbind(var1,var2)
    colnames(mix) <- c(name1,name2)
    dataf <- data.frame(mix)
    modelo <- lm(var2 ~ var1, data = dataf)
    summary(modelo)
  })
  
  output$vardetail <- renderPrint({
    if(input$checkbox == TRUE){
      var1<- switch(as.integer(input$svar1),anio_div,anio_mat,dur_mat,edad_hom,edad_muj)
      name1<- switch(as.integer(input$svar1),"Año de divorcio","Año de matrimonio","Duración en años","Edad del hombre", "Edad de la mujer")
      var2<- switch(as.integer(input$svar2),anio_div,anio_mat,dur_mat,edad_hom,edad_muj)
      name2<- switch(as.integer(input$svar2),"Año de divorcio","Año de matrimonio","Duración en años","Edad del hombre", "Edad de la mujer")
      #agrupar datos y realizar comando
      #tapply(var1,var2,mean)
      mix <- cbind(var1,var2)
      dataf <- data.frame(mix)
      modelA<-lm(dataf$var2~dataf$var1)
      d<- cbind(dataf$var2, dataf$var1,modelA$fitted.values, resid(modelA))
      colnames(d) <- c(paste0(name2," (Y)"), paste0(name1," (X)"), paste0(name2," (Y^)"), 'Residuo/Err')
      (d[1:50,])
    }
  })
  
  
  #grafica de dos variables
  output$varPlot1 <- renderPlot({
    var1<- switch(as.integer(input$svar1),anio_div,anio_mat,dur_mat,edad_hom,edad_muj)
    name1<- switch(as.integer(input$svar1),"Año de divorcio","Año de matrimonio","Duración en años","Edad del hombre", "Edad de la mujer")
    var2<- switch(as.integer(input$svar2),anio_div,anio_mat,dur_mat,edad_hom,edad_muj)
    name2<- switch(as.integer(input$svar2),"Año de divorcio","Año de matrimonio","Duración en años","Edad del hombre", "Edad de la mujer")
    mix <- cbind(var1,var2)
    colnames(mix) <- c(name1,name2)
    pairs(mix, main="Diagrama de dispersión", col="blue")
  })
  
  output$varPlot2 <- renderPlot({
    var1<- switch(as.integer(input$svar1),anio_div,anio_mat,dur_mat,edad_hom,edad_muj)
    name1<- switch(as.integer(input$svar1),"Año de divorcio","Año de matrimonio","Duración en años","Edad del hombre", "Edad de la mujer")
    var2<- switch(as.integer(input$svar2),anio_div,anio_mat,dur_mat,edad_hom,edad_muj)
    name2<- switch(as.integer(input$svar2),"Año de divorcio","Año de matrimonio","Duración en años","Edad del hombre", "Edad de la mujer")
    mix <- cbind(var1,var2)
    dataf <- data.frame(mix)
    #ggplot(data = dataf, aes(x=var1,y=var2)) + geom_point()+ labs(y= name2, x = name1)
    if(input$checkbox2 == TRUE){
      ggplot(data = dataf, aes(x = dataf$var2)) +
        geom_histogram(aes(y = ..density.., fill = ..count..)) +
        scale_fill_gradient(low = "#DCDCDC", high = "#7C7C7C") +
        stat_function(fun = dnorm, colour = "firebrick",
                      args = list(mean = mean(dataf$var2),
                                  sd = sd(dataf$var2))) +
        ggtitle(paste0("Histograma + curva normal teórica de ",name2)) + labs(x = name2, y= "Densidad")+
        theme_bw()
    }else{
      ggplot(data = dataf, aes(x = dataf$var1)) +
        geom_histogram(aes(y = ..density.., fill = ..count..)) +
        scale_fill_gradient(low = "#DCDCDC", high = "#7C7C7C") +
        stat_function(fun = dnorm, colour = "firebrick",
                      args = list(mean = mean(dataf$var1),
                                  sd = sd(dataf$var1))) +
        ggtitle(paste0("Histograma + curva normal teórica de ",name1)) + labs(x = name1, y= "Densidad")+
        theme_bw()
    }
    
})
  
  output$varPlot3 <- renderPlot({
    var1<- switch(as.integer(input$svar1),anio_div,anio_mat,dur_mat,edad_hom,edad_muj)
    name1<- switch(as.integer(input$svar1),"Año de divorcio","Año de matrimonio","Duración en años","Edad del hombre", "Edad de la mujer")
    var2<- switch(as.integer(input$svar2),anio_div,anio_mat,dur_mat,edad_hom,edad_muj)
    name2<- switch(as.integer(input$svar2),"Año de divorcio","Año de matrimonio","Duración en años","Edad del hombre", "Edad de la mujer")
    mix <- cbind(var1,var2)
    dataf <- data.frame(mix)
    boxplot(dataf$var2 ~ dataf$var1, col = "gray", xlab = name1 , ylab = name2)
  })
  
  output$varPlot4 <- renderPlot({
    var1<- switch(as.integer(input$svar1),anio_div,anio_mat,dur_mat,edad_hom,edad_muj)
    name1<- switch(as.integer(input$svar1),"Año de divorcio","Año de matrimonio","Duración en años","Edad del hombre", "Edad de la mujer")
    var2<- switch(as.integer(input$svar2),anio_div,anio_mat,dur_mat,edad_hom,edad_muj)
    name2<- switch(as.integer(input$svar2),"Año de divorcio","Año de matrimonio","Duración en años","Edad del hombre", "Edad de la mujer")
    mix <- cbind(var1,var2)
    dataf <- data.frame(mix)
    ggplot(data = dataf, aes(x=var1,y=var2)) + geom_point()+stat_smooth(method = lm)+ labs(y= name2, x = name1)
    #plot(y=var2, x=var1,
    #     main = "Modelo de regresión",
    #     xlab = name1, ylab = name2, pch=20)
    #modelo.00 <- lm(dataf$var2 ~ dataf$var1, data = dataf)
    #abline(modelo.00, col = "red")
  })
  
  
}

shinyApp(ui = ui, server = server)
