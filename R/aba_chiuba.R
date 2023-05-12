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
library(readxl)
library(readr)


dir <- "data"
pemba <- read_excel(paste(dir, "/pemba_clear.xls", sep=""))

 
pemba$tipo_casa_banho1<- ifelse(is.na(pemba$tipo_casa_banho1), "Sem Latrina", pemba$tipo_casa_banho1)
## shapfile de distritos
#monapo_sf <- readRDS(paste(dir, "/sf_districts.rds", sep="")) %>% filter(District_ID %in% "MZ0715")


###JUNTAR BASE DE DADOS E SHAPFILE


########MAPA DE MONAPO###################

#### GRAFICO de subapa economia
#filtro_economia_monapo<- radioButtons("monapo_filtro_economica", "Selecione uma opcção:",
 #                                     c("Tipo de Rendimento","Rendimento Mensal"))
