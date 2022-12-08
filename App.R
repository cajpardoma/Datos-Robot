#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(readxl)
library(ggplot2)
library(dplyr)
library(plotly)
library("RColorBrewer")
library(lubridate)



#Data <- read_excel("C:/Users/cpardom/Corona/Pricing - Mansfield/20. Lowes/Datos Robot/Datos_Robot_Lowes.xlsx", 
#                   col_types = c("date", "text", "text", 
#                                 "text", "text", "text", "text", "text", 
#                                 "text", "text", "text", "text", "numeric", 
#                                 "numeric", "numeric", "text", "text", 
#                                 "text", "text", "text", "text","text", "text", 
#                                 "numeric"))

Data <- read_excel("Datos_Robot_Lowes.xlsx", 
                   col_types = c("date", "text", "text", 
                                 "text", "text", "text", "text", "text", 
                                 "text", "text", "text", "text", "numeric", 
                                 "numeric", "numeric", "text", "text", 
                                 "text", "text", "text", "text","text", "text", 
                                 "numeric"))

##------------------------------------------------------------------------------------------------
product_Manfield <- unique(Data$Descripcion_H_Manfield)
Data_Product <- Data %>%
  filter(Descripcion_H_Manfield == product_Manfield[2])

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
  ) %>%  layout(title = paste('Price Trends Lowes', ''), plot_bgcolor='#e5ecf6',xaxis = list(title = 'Date'), 
                yaxis = list(title = 'Price Trend'), legend = list(title=list(text='<b> Products </b>')))

fig


##-------------------------------------------------------------------------------------------------

fecha_filtro <- max(Data$Fecha)
fecha_filtro_min <- min(Data$Fecha)

ui <- dashboardPage(
  dashboardHeader(title = "Price Monitoring"),
  dashboardSidebar(
    title = "Controls",
    dateInput("x", "Select Date:", value = fecha_filtro, min = fecha_filtro_min, max = fecha_filtro,
              format = "dd-mm-yyyy", startview = "month", weekstart = 0,
              language = "en", width = NULL),
    selectInput("type", "Select Product:", choices = product_Manfield)
  ),
  dashboardBody(
    fluidRow(
      box(plotlyOutput("plot1", height = 500, width = 800)),
      
      box(plotlyOutput("plot2", height = 500, width = 800))
  )
)
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
  
}

# Run the application 
shinyApp(ui = ui, server = server)
