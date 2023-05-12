library(shiny)
library(bslib)
library(ggthemes)
library(RColorBrewer)
library(sf)
library(shinythemes)
library(lubridate)
library(jsonlite)
library(stringr)
library(readr)
library(dplyr)
library(tidyverse)
library(shinyjs)
library(plotly)
library(ggplot2)
library(DT)
library(shinyWidgets)
library(shinydashboard)
library(bslib)
library(shinycssloaders)
library(ggmap)
library(ggspatial)
library(rmarkdown)
library(rgdal)
# Define UI
ui <- fluidPage(
   
  # Define o tema
  theme = shinytheme("flatly"),
  
  # Define a barra de navegação
  navbarPage(
    # Define o título da barra de navegação
    title = "SUSAMATI" ,
   # header = "Meu Cabeçalho",
   # tags$head(tags$style(HTML('.navbar-default {background-color: #a742f5;}'))),
    #title = "ASSOCIAÇÃO MUVA",
    
   ##VISAO GERAL
  navbarMenu("VISAO GERAL",
    tabPanel("Overview",
           #  uiOutput("rmd_page")
           # Elementos do painel principal
           h2("Susamati - Soluções Inovadoras para o Saneamento em Moçambique"),
           wellPanel(p("Susamati é um negócio social que traz soluções inovadoras para o saneamento em Moçambique. Seu objetivo é melhorar o saneamento das famílias, principalmente as que vivem em zonas suburbanas e rurais."),
           p("O negócio social desenvolve projetos em diferentes províncias do país, implementando soluções de saneamento sustentáveis e acessíveis."),
           # Adicione outros componentes e elementos conforme necessário)
          
   ),
   h2("Projecto Sanear Pemba"),
   wellPanel("No Projecto Sanear Pemba Mapeamos")
   
   )),
  
     #BASELINE 
    ##CHUIBA
    navbarMenu("CHUIBA", icon=icon("exchange-alt"),
               tabPanel("MAPA-Tipo de saneamento",
                        column(12,
                               wellPanel(h4("Mapa de variação em baixo mostra 
                               tipo de latrina mais frequente em Chuiba 
                                            Quanto mais carregado for 
                                            a cor do mapa mais familias usam esse tipo de latrina.
                                            Deslize o mouse nos mapas para verificar a percentagem 
                                            das familias que usa determinado tipo de latrina
                                            ")),
                               wellPanel("BASELINE"),
                               withSpinner(plotlyOutput("chuiba_mapa", height = 500) ,color="purple")
                               
                               )),
               tabPanel("Cobertura de saneamento",tabname="Chuiba_saneamento", icon=icon("chart-line"),
                        # Define as colunas do layout de grade
                        column(12,
                               wellPanel(radioButtons("chuiba_filtro_saneamento", "Selecione uma opcção:",
                                                       c("Tem Latrina?","Latrina esta sempre disponivel","tem casota"))
                                          
                               )
                        ),
                        
                        column(6,wellPanel("BASELINE"),
                               withSpinner(plotlyOutput("chuiba_saneamento_baseline"),color="purple"),
                               wellPanel(""),
                        ),  
                        column(6,wellPanel("ENDLINE"),
                               plotlyOutput("chuiba_saneamento_endline"),
                               wellPanel(""),
                        ), 
                        
               ), 
               tabPanel("Vontade de contribuir",tabname="Chuiba_vontade", icon=icon("chart-line"),
                        # Define as colunas do layout de grade
                        # Define as colunas do layout de grade
                        column(12,
                               wellPanel(radioButtons("chuiba_filtro_vontade", "Selecione uma opcção:",
                                                       c("Podes contribuir para melhorar a sua sanita?","Com Quanto gostaria de constribuir?")),
                               )
                        ),
                        
                        
                        column(6,wellPanel("BASELINE"),
                               withSpinner(plotlyOutput("chuiba_vontade_baseline"),color="purple"),p(""),
                        ),  
                        column(6,wellPanel("ENDLINE"),
                               withSpinner(plotlyOutput("chuiba_vontade_endline")),p(""),
                        ),                
                        
               ),
               tabPanel("Situação económica",tabname="Chuiba_situacao", icon=icon("chart-line"),
                        # Define as colunas do layout de grade
                        column(12, 
                               wellPanel( radioButtons("chuiba_filtro_economica", "Selecione uma opcção:",
                                                       c("Tipo de Rendimento","Rendimento Mensal")),
                               )
                        ), 
                        
                        
                        column(6,wellPanel("BASELINE"),
                               withSpinner(plotlyOutput("chuiba_economica_baseline"),color="purple"),p(""),
                        ),  
                        column(6,wellPanel("ENDLINE"),
                               withSpinner(plotlyOutput("chuiba_economica_endline")),p(""), 
                        ),               
                        
               ),
               
    ), # Chuiba
  
    
    
    
  )
  
)
#)


# Define o servidor
server <- function(input, output) {
  
#______________mapas______________________#
 
  
  #__________________cobertura DE SANEAME____________________________

   output$chuiba_saneamento_baseline <- renderPlotly({
    if(input$chuiba_filtro_saneamento=="Tem Latrina?") {
      tem_latrina(pemba,1200, 400)
    } else 
      if(input$chuiba_filtro_saneamento=="Latrina esta sempre disponivel") {
        disponivel_casa_banho(pemba,1200,200)
        
      }else
        if(input$chuiba_filtro_saneamento=="tem casota"){
          tem_casota(pemba,1000,300)}
    })
  
  
  #______________Vontade de contribuir___________________________________
 
  ##Pemba
  output$chuiba_vontade_baseline <- renderPlotly({
    if(input$chuiba_filtro_vontade=="Podes contribuir para melhorar a sua sanita?") {
      tem_condicoes(pemba,1500,200)
    }else 
      if(input$chuiba_filtro_vontade=="Com Quanto gostaria de constribuir?") {
        pemba<-pemba %>%  filter(tem_condicoes %in% "SIM" | is.na(tem_condicoes))
        valor_contribuir(pemba)+
          ggthemes::theme_stata() +
          theme(
           legend.position = "bottom",
            plot.caption = element_text(size = 14L),
            axis.text.x = element_blank(),
            axis.title.x = element_blank()
          )+
          geom_text(stat = "count", aes(label = paste0(round(..count../sum(..count..) * 100), "%")),
          )
      } 
  })
  
  #_______________Enconomia____________________________________________________
 
  ## Pemba
  output$chuiba_economica_baseline <- renderPlotly({
    
    if(input$chuiba_filtro_economica=="Tipo de Rendimento") {
      tipo_redimento(pemba,800,200)
       
    } else 
      if(input$chuiba_filtro_economica=="Rendimento Mensal") {
        rendimento_faixa(pemba,1550,250)
      } })
  
  ####PDF
  #output$rmd_page <- renderUI({
   # includeMarkdown("visaogeral.pdf")
  #})
}
# Execute o aplicativo Shiny
shinyApp(ui, server)

