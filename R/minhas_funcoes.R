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
 

create_summary_table <- function(data, group_vars, count_var) {
   summary_table <- data %>%
      group_by(across(all_of(group_vars))) %>%
     summarise(Total= n()) %>%
      mutate(percentagem = round(Total / sum(Total)*100, digits = 2 ))
     
    return(summary_table)
  }

  ## funcao que cria um grafico
  grafico <- function(dados, x, y, fill, dados_total, string_familias, limsup, quebra){
    library(ggplot2)
    ggplot(dados, aes(text =paste("Numero de familias:", {{y}}, "de", nrow(dados_total), string_familias),x = {{x}}, y = {{y}}, fill = {{fill}})) +
      geom_col() +geom_text(aes(label = paste0(round({{y}} / sum({{y}}) * 100), "%")), position = position_stack(vjust = 0.5)) +
      scale_fill_hue(direction = 1) +
      ggthemes::theme_stata()+labs(subtitle = "Linha preta representa o numero total de familias entrevistadas",
                                                                  x="",
                                                                  y="Numero de Familias"
      )+theme(legend.position = "top")+
      scale_y_continuous(limits = c(0,limsup), breaks = seq(0,limsup,by=quebra))
  }
 
  
  
  ###funcao para mapa
  funcao_mapa <- function(data_sf, dado_dist, tam_letra, num_caract, filename) {
    library(stringr)
    dados_mapa <- left_join(data_sf, dado_dist %>% 
                              group_by(District_ID, distrito, tipo_casa_banho1) %>% 
                              summarise(Total = n()) %>% 
                              mutate(percentagem = round(Total / sum(Total) * 100, digits = 2)),
                            by = c("District_ID" = "District_ID"), multiple = "all")
    
    centro_mapa <- st_centroid(data_sf) # obter as coordenadas do centro do mapa
    
    mapa <- ggplot(dados_mapa) +
      aes(text = paste("Tipo de latrina:", tipo_casa_banho1), fill = percentagem) +
      geom_sf(size = 1.5) + 
      scale_fill_distiller(palette = "Purples", direction = 1) +
      theme_bw() +
      facet_wrap(vars(tipo_casa_banho1), nrow = 1L, 
                 labeller = labeller(tipo_casa_banho1 = function(x) str_wrap(x, width = num_caract))) +
      theme(
        strip.text = element_text(
          size = tam_letra, # Tamanho do texto aumentado para 10
          face = "bold",
          color = "black",
          margin = margin(12, 12, 12, 12, "pt")
        ),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank()
      ) +
      labs(x = "Longitude", y = "Latitude") +
      geom_text(x = centro_mapa$x, y = centro_mapa$y, label = paste0("Percentagem: ", dados_mapa$percentagem, "%"), 
                size = tam_letra, color = "black")
    
    #ggsave(filename, plot = mapa, device = "png", width = 10, height = 7, dpi = 300)
    
    return(mapa)
  }
  
  
  
  
 #funcao_mapa<-function(data_sf, dado_dist, tam_letra,num_caract){
  # library(stringr)
   #dados_mapa <- left_join(data_sf, dado_dist %>% group_by(District_ID, distrito, tipo_casa_banho1) %>% 
    #                         summarise(Total = n()) %>% mutate(percentagem = round(Total / sum(Total) * 100, digits = 2)),
     #                      by = c("District_ID" = "District_ID"), multiple = "all")
   
   
   #mapa <- ggplot(dados_mapa) +
    # aes(text = paste("Tipo de latrina:", tipo_casa_banho1), fill = percentagem) +
     #geom_sf(size = 1.5) + 
     #scale_fill_distiller(palette = "Purples", direction = 1) +
     #theme_bw() +
     #facet_wrap(vars(tipo_casa_banho1), nrow = 1L, labeller = labeller(tipo_casa_banho1 = function(x) str_wrap(x, width = num_caract))) +
     #theme(
      # strip.text = element_text(
       #  size = tam_letra, # Tamanho do texto aumentado para 10
        # face = "bold",
         #color = "black",
         #margin = margin(12, 12, 12, 12, "pt")
       #),
   #axis.text.x = element_blank(), 
   #axis.text.y = element_blank()
    # )+
     #labs(x = "Longitude", y = "Latitude")
   #return(mapa)
# }
   
 ############Rendimento mensal 
 tipo_redimento<-function(dado,limsup, quebra) {
   visitas <- as.name("famílias visitadas")
   
   grafico(create_summary_table(dado, "rendimento_tipo"),x=rendimento_tipo,y=Total, fill=rendimento_tipo, dado,visitas,limsup, quebra)+
     labs(subtitle = "Linha preta representa o numero total de familias entrevistadas",
          x="",
          y="Numero de Familias")+
     theme(axis.text.x = element_blank(), # remove os valores do eixo x
           axis.title.x = element_blank() # , axis.line.x = element_blank()remove o título do eixo x
     )+ # remove a linha do eixo x
     theme(legend.position = "top") 
 }
 
############## 
 faixa_rendimento  <- function(dados,limsup, quebra) {
   tem_latrina <- as.name("famílias visitadas")
   grafico(create_summary_table(dado, "rendimento_faixa"), x = rendimento_faixa, y = Total, fill = rendimento_faixa,dados,tem_latrina,limsup, quebra) +
     labs(subtitle = "Linha preta representa o numero total de familias entrevistadas",
          x = "",
          y = "Numero de Familias") +
     theme(axis.text.x = element_blank(), # remove os valores do eixo x
           axis.title.x = element_blank() # , axis.line.x = element_blank()remove o título do eixo x
           )+ # remove a linha do eixo x
     theme(legend.position = "top")
   #return(grafico(monapo, tem_latrina, 250, 50))
 }
 
 ##_____________________Cobertura de saneamento______________________
 tem_latrina<-function(dados,limsup, quebra){
   visitas <- as.name("famílias visitadas")
   
   grafico(create_summary_table(dados, "Tem_latrina"),x=Tem_latrina,y=Total, fill=Tem_latrina, dados,visitas,limsup, quebra)+
     labs(subtitle = "Linha preta representa o numero total de familias entrevistadas",
          x="",
          y="Numero de Familias")+theme(legend.position = "top")
 }
 
 
 
 disponivel_casa_banho<- function(dados,limsup, quebra){
   dado1<- filter(dados, Tem_latrina=="SIM")
   dado1<-filter(dados,disponivel_casa_banho %in% c("SIM","NAO")) 
     
   tem_latrina <- as.name("famílias quem tem latrina")
   grafico(create_summary_table(dado1, "disponivel_casa_banho"),x=disponivel_casa_banho,y=Total, fill=disponivel_casa_banho, dado1,tem_latrina,limsup, quebra)+
     labs(subtitle = "Linha preta representa o numero total de familias entrevistadas",
          x="",
          y="Numero de Familias")
 }
 tem_casota <- function(dados, limsup, quebra) {
   dados<-filter(dados, tem_casota %in% c("SIM","NAO"))
   graf_casota <- dados %>%  
     filter(Tem_latrina %in% "SIM") %>%
     ggplot() +
     aes(x = tem_casota, fill = tem_casota) +
     geom_bar() +
     scale_fill_hue(direction = 1) +
     ggthemes::theme_stata() + labs(subtitle = " ",
                                    x = "",
                                    y = "Numero de Familias") +
     theme(legend.position = "top") +
     scale_y_continuous(limits = c(0, limsup), breaks = seq(0, limsup, by = quebra)) +
     geom_text(stat = "count", aes(label = paste0(round(..count../sum(..count..) * 100), "%")),
               vjust = 0.5)
   
   return(graf_casota)
 }
 
 #tem_casota<-function(dados, limsup, quebra){
  # graf_casota=dados %>%
   #  filter(Tem_latrina %in% "SIM") %>%
    # ggplot() +
     #aes(x = tem_casota, fill = tem_casota) +
  #   geom_bar() +
     
   #  scale_fill_hue(direction = 1)+
     #ggthemes::theme_stata()+labs(subtitle = " ",
      #                            x="",
       #                           y="Numero de Familias")+theme(legend.position = "top")+
     #scale_y_continuous(limits = c(0,limsup), breaks = seq(0,limsup,by=quebra))
   #return(graf_casota)
# }

#______________Vontade de contribuir___________________________________________
 tem_condicoes<-function(dados,limsup, quebra){
   visitas <- as.name("famílias visitadas")
 #  table(nacala$tem_condicoes)
 monapo1 <- dados%>%
   filter(!(tem_condicoes %in% ".a"))
 # table(nacala$quanto_contribuir)
 grafico(create_summary_table(monapo1, "tem_condicoes"),x=tem_condicoes,y=Total, fill=tem_condicoes, monapo1,visitas,limsup, quebra)+
   labs(subtitle = "Linha preta representa o numero total de familias entrevistadas",
        x="Pode contribuir com melhoria da sua sanita",
        y="Numero de Familias")+theme(legend.position = "top")}
 
 valor_contribuir<-function(dado){
   dado1<-filter(dado, tem_condicoes=="SIM") 
   ggplot(dado1) +
     aes(x = faixa_contribuir, fill=faixa_contribuir) +
     geom_bar() +
     theme(axis.title.y = element_text(face = "bold"), axis.title.x = element_text(face = "bold")) +
     labs(x = "valor contribuir", y = "Numero de familias", 
          fill = "valor contribuir") +
     ggthemes::theme_stata() 
   
 }
 # library(esquisse)
 #esquisser(pemba)
 #_________________________________ECONOMIA_____________________________________________________________________
 rendimento_tipo<-function(dados,limsup, quebra){
   visitas <- as.name("famílias visitadas")
   
   grafico(create_summary_table(dados, "rendimento_tipo"),x=rendimento_tipo,y=Total, fill=rendimento_tipo, dados,visitas,limsup, quebra)+
     labs(subtitle = "Linha preta representa o numero total de familias entrevistadas",
          x="",
          y="Numero de Familias")+theme(legend.position = "top")+
     theme(axis.text.x = element_blank(), # remove os valores do eixo x
           axis.title.x = element_blank())
           #, # remove o título do eixo x
           #axis.line.x = element_blank())# remove a linha do eixo x
   
 }

   rendimento_faixa<-function(dados,limsup, quebra){ 
   tem_latrina <- as.name("famílias visitadas")
   grafico(create_summary_table(dados, "rendimento_faixa"),x=rendimento_faixa,y=Total, fill=rendimento_faixa, dados,tem_latrina,limsup, quebra)+
     labs(subtitle = "Linha preta representa o numero total de familias entrevistadas",
          x="",
          y="Numero de Familias") +
     theme(axis.text.x = element_blank(), # remove os valores do eixo x
           axis.title.x = element_blank())
          # , # remove o título do eixo x
           #axis.line.x = element_blank()) # remove a linha do eixo x
   }
 

 