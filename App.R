#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#setwd("C:/Users/cpardom/Corona/Pricing - Mansfield/20. Lowes/Datos Robot")

library(shiny)
library(shinydashboard)
library(readxl)
library(ggplot2)
library(dplyr)
library(plotly)
library("RColorBrewer")
library(lubridate)


##Lectura de datos------------------------------------
Data <- read_excel("Datos_Robot_Lowes.xlsx", 
                   col_types = c("date", "text", "text", 
                                 "text", "text", "text", "text", "text", 
                                 "text", "text", "text", "text", "numeric", 
                                 "numeric", "numeric", "text", "text", 
                                 "text", "text", "text", "text","text", "text", 
                                 "numeric"))

##------------------------------------------------------------------------------------------------
product_Manfield <- unique(Data$Descripcion_H_Manfield)
##----------------------------------------------------------
##-------------------------------------------------------------------------------------------------

fecha_filtro <- max(Data$Fecha)
fecha_filtro_min <- min(Data$Fecha)


body <-   dashboardBody(
    fluidRow(
      tabBox(
        title = "",
        id = "tabset1", height = 450, width = 450,
        tabPanel("Consumer Price Index - Only Lowes",
                 
        plotlyOutput("plot1", height = 550, width = 850)
        ),
        
        tabPanel("Consumer Price Trends - Only Lowes",
                 
        plotlyOutput("plot2", height = 550, width = 850)
        ),
        
        tabPanel("Consumer Price Index Trends - Only Lowes",
                 
                 fluidRow(
                   box(title = "Graph", solidHeader = T,
                       width = 10,
                       collapsible = T,
                       plotlyOutput("plot3", height = 550, width = 850 )
                       ),
                   box(title = "Image", solidHeader = T,
                        width = 2,
                        collapsible = T,
                        imageOutput("image1")
                   ),
                   box(title = "Click",
                       solidHeader = T,
                       width = 2,
                       collapsible = T,
                       verbatimTextOutput("click"))
                 )
                 
        )

      )
      
  )
)

ui <- dashboardPage(
  dashboardHeader(title = "Price Monitoring"),
  dashboardSidebar(
    title = "Controls",
    dateInput("x", "Select Date(Price Index):", value = fecha_filtro, min = fecha_filtro_min, max = fecha_filtro,
              format = "dd-mm-yyyy", startview = "month", weekstart = 0,
              language = "en", width = NULL),
    selectInput("type", "Select Product(Price Trends):", choices = product_Manfield)
  ),
body
)



server <- function(input, output) {
  
  
  #------------------------------------------------
    
  #------------------------------------------------
  output$plot1 <- renderPlotly({
    
    ##Data Price Index-------------------
    Data_Filtro_P_Index <- Data %>%
      filter(Fecha == ymd(input$x)) %>%
      mutate(Price_Index = Precio_Consumidor/Precio_Max_Mansfield)
    ##-----------------------------------
    
    fig <- plot_ly()
    fig <- fig %>%
      add_trace(
        type = 'scatter',
        mode = 'markers',
        x = Data_Filtro_P_Index$Descripcion_H_Manfield,
        y = round(Data_Filtro_P_Index$Price_Index*100,0),
        color = Data_Filtro_P_Index$Fabricante,
        text = Data_Filtro_P_Index$Producto,
        hovertemplate = paste('<b>Price Index</b>: %{y:.0f}%',
                              '<br><b>P_Mansfield</b>: %{x}<br>',
                              '<b>Product:</b> %{text}'),
        showlegend = TRUE,
        size = 0.8,
        colors = "Set1"
      ) %>%  layout(title = paste('Price Index Lowes - ', input$x), plot_bgcolor='#e5ecf6',xaxis = list(title = 'Products'), 
                    yaxis = list(title = '% Price Index'), legend = list(title=list(text='<b> Brands </b>')))
    
   
     fig
  }
  )
  
  output$plot2 <- renderPlotly({
    
    #-----------------------------------------------
    Data_Product <- Data %>%
      filter(Descripcion_H_Manfield == input$type)
    
    #-----------------------------------------------
    
    fig <- plot_ly()
    fig <- fig %>%
      add_trace(
        type = 'scatter',
        mode = 'lines + markers', 
        color = Data_Product$Producto,
        text = Data_Product$Fabricante,
        x = Data_Product$Fecha,
        y = Data_Product$Precio_Consumidor,
        showlegend = TRUE,
        colors = "Set1"
      ) %>%  layout(title = paste('Price Trends Lowes - ', input$type), plot_bgcolor='#e5ecf6',xaxis = list(title = 'Date'), 
                    yaxis = list(title = 'Price Trend'), legend = list(title=list(text='<b> Products </b>')))
    
    fig
    
    
    
  })
  
  output$plot3 <- renderPlotly({
    
    #-----------------------------------------------
    Data_P_Index <- Data %>%
      mutate(Price_Index = round(Precio_Consumidor*100/Precio_Max_Mansfield,1)) %>%
      filter(Descripcion_H_Manfield == input$type)
    
    #-----------------------------------------------
    
    fig <- plot_ly()
    fig <- fig %>%
      add_trace(
        type = 'scatter',
        mode = 'lines + markers', 
        color = Data_P_Index$Producto,
        text = Data_P_Index$Fabricante,
        x = Data_P_Index$Fecha,
        y = Data_P_Index$Price_Index,
        showlegend = TRUE,
        colors = "Set1"
      ) %>%  layout(title = paste('% Price Index Trends Lowes - ', input$type), plot_bgcolor='#e5ecf6',xaxis = list(title = 'Date'), 
                    yaxis = list(title = '% Price Index Trend'), legend = list(title=list(text='<b> Products </b>')))
    
    fig
    
    
    
  })
  
  output$image1 <- renderImage({
    ImgTxt <- "./sanitario.jpg"
    width<- "100%"
    height<- "100%"
    list(src = ImgTxt,
         contentType = "image/jpg",
         width = width,
         height = "auto"
    )
  }, deleteFile = FALSE)
  
  output$click <- renderPrint({
       event_data("plotly_click")
     })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
