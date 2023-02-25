## Análise do SAEB 2019 5º Ano Ensino Fundamental - Aglomerados Subnormais
## João Paulo Andrade 



# Carregando bibliotecas:
library(magrittr)
library(sf)
library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(RColorBrewer)
library(curl)

#path <-""
#setwd(path)
# Carregandoa Base de Dados

url_escolas <- "https://onedrive.live.com/download?cid=B84B577199D77872&resid=B84B577199D77872%212018&authkey=AJo6TQY4c1w2xMw"
url_shape <- "https://onedrive.live.com/download?cid=B84B577199D77872&resid=B84B577199D77872%212017&authkey=ACeOJ5ER2mvB48Q"
url_uf <- "https://onedrive.live.com/embed?cid=B84B577199D77872&resid=B84B577199D77872%212016&authkey=ALDpqAM3qgFTYCc" 



curl::curl_download(url_escolas, "data/escolas.RDS")
curl::curl_download(url_shape, "data/shape.RDS")
curl::curl_download(url_escolas, "data/UF.csv")




escolas_total <- read_rds("data/escolas.RDS")

ufs <- escolas_total %>% transmute(
  UF,
  UF_COD = as.numeric(uf)
) %>% unique() %>% filter(UF_COD >= 0)

escolas <- escolas_total


escolas <-escolas %>% filter(MEDIA_5EF_LP > 0) %>%
  transmute(escola = as.factor(Escola),
            UF = UF,
            latitude = Latitude,
            longitude = Longitude,
            nome_aglomerado = as.factor(NM_AGSN),
            ME5F_LP = MEDIA_5EF_LP,
            ME5F_MT = MEDIA_5EF_MT)
escolas$NM_AGSN %>% replace_na(0)

escolas$aglomerado = is.na(escolas$nome_aglomerado)
escolas <- escolas %>% mutate(aglomerado = as_factor(aglomerado),
                              aglomerado = str_replace(aglomerado,"TRUE","NÃO"),
                              aglomerado = str_replace(aglomerado,"FALSE","SIM"),
                              aglomerado = as_factor(aglomerado))%>%
  mutate(escola = escola,
         nome_aglomerado = nome_aglomerado,
         aglomerado = aglomerado,
         ME5F_LP = ME5F_LP,
         ME5F_MT = ME5F_MT)
# Tabela para centrar o mapa:
uf_centro <- read.csv2("data/UF.csv")
colnames(uf_centro) <- c("UF","latitude","longitude")
#escolas <- escolas %>% filter(UF=="PE") #-> Filtro para testar com dataset menor

#◘ Função que remove os outliers ◘
remove_outliers <- function(base,nota) {
  x<- tibble(base[1],
             base[5],
             base[8],
             base[which( colnames(base)=={{nota}})]
  )
  
  b<-as.vector(unlist(x[4]))
  outliers <- boxplot(b, plot=FALSE)$out
  x<- x[-which(b %in% outliers),]
}


#◘ Função que constroi o boxplot ◘
p_bloxplot <- function(){
  
  # Visualizando a distribuição das notas com o Boxplot
  y.title <- paste("Média escolar de ","notas"," - 5º EF 2019")
  my_title <- paste("Média das turmas do 5º EF do estado ", "uf", " no SAEB 2019")
  ggplot(x, aes_string(x = "aglomerado",  y = nota)) +geom_boxplot(fill="slateblue", alpha=0.2) + ggtitle(my_title) + xlab("Aglomerado") + ylab(y.title)  + theme_minimal() + theme(
    plot.title=element_text( hjust=0.5, vjust=0.5, face='bold')
  )
}


#◘ Função que retorna o teste t formatado ◘
t_test <- function(x) {
  i <-as.vector(unlist(x[which( colnames(x)==nota) ]))
  test <- t.test(i ~ aglomerado , data = x)
  #  use sprintf() to format t.test() results compactly
  sprintf(
    "p value: %0.3f/n[%0.2f, %0.2f]",
    test$p.value, test$conf.int[1], test$conf.int[2]
  )
}


#Nota selecionada
nota = "ME5F_LP"

# ◘ Função para gerar a linha da tabela agrupada por UF ◘
tabela <- function(uf) {
  x <- escolas %>% filter(UF == {{uf}})
  x <- remove_outliers(x,nota)
  teste <- t_test(x)
  linha <- data.frame({{uf}},
                      mean(as.vector(unlist(x[which( colnames(x)==nota) ]))),
                      teste)
  colnames(linha) <- NULL#"UF","Média","t-test")
  return(linha)
}

# ◘ Função para construir a tabela ◘

tabela_for <- function(){
  uf <- str_sort(unique(escolas$UF))
  y <- vector()
  df <- data.frame(y)
  for(i in uf) {
    tryCatch(y<-
               data.frame( tabela({{i}})),
             warning = function(w) {return(paste(i,"Erro no estado "))},# era pra incluir os estados sem aglomerados urbanos, mas não está funcionando
             error = function(e) {return(paste(i," não apresenta n° de escolas suficiente"));# era pra incluir os estados sem aglomerados urbanos, mas não está funcionando
               NaN}
    )
    df <- rbind(df,y)
    
  }
  colnames(df) <- c("UF",nota,paste("t-teste: ",nota))
  return(unique(df))
}



nota = "ME5F_LP"
tabela_pt <- tabela_for()

nota = "ME5F_MT"
tabela_mt <- tabela_for()




tabela_notas <- left_join(tabela_pt,tabela_mt)
tabela_notas$`t-teste:  ME5F_MT` <- str_replace_na(tabela_notas$`t-teste:  ME5F_MT`,'Não disponível')


tabela_resultado <- tabela_notas %>%
  transmute(
    UF = UF,
    Resultado_PT = ifelse(as.numeric(substr(`t-teste:  ME5F_LP`,10,14)) <=  0.05,
                          "O estudo indica que as médias das escolas localizadas em aglomerados subnormais é significativamente diferente da média do restante das escolas",
                          "O estudo indica que as médias das escolas localizadas em aglomerados subnormais NÃO é significativamente diferente da média do restante das escolas"),
    Resultado_MT = ifelse(as.numeric(substr(`t-teste:  ME5F_MT`,10,14)) <=  0.05,
                          "O estudo indica que as médias das escolas localizadas em aglomerados subnormais é significativamente diferente da média do restante das escolas",
                          "O estudo indica que as médias das escolas localizadas em aglomerados subnormais NÃO é significativamente diferente da média do restante das escolas")
  )

# tabela_geral <- left_join(tabela_notas,tabela_resultado,by = 'UF')
# tabela_geral <- transmute(tabela_geral,
#   UF = UF,
#   Portugues = paste('Média: ',ceiling(as.numeric(Portugues)),' - ',Resultado_PT),
#   Matematica   = paste('Média: ',ceiling( as.numeric(Matematica))  ,' - ',Resultado_MT)
# 
# )


# Mapa --------------------------------------------------------------------

# Shape do aglomerado 
shp_agl0 <- read_rds("data/shape.rds")
coln<-colnames(shp_agl0)
shp_agl <- shp_agl0 %>% 
  transmute(
    UF_COD = as.numeric(uf),
    aglomerado = NM_AGSN ,
    geometry = geometry
  ) %>% unique()

# Unindo os campos de UF para poder filtrar no mapa
shp_agl$UF <- ufs$UF[match(shp_agl$UF_COD,ufs$UF_COD)]







### SHINY UI ##############################################################################################################################################



ui <- bootstrapPage(
  
  navbarPage(theme = shinytheme("yeti"), collapsible = TRUE,
             tags$img(src='logo_saeb1.png'),#height='30',width='30') ,
             id="nav",
             #"SAEB 2019"
             tabPanel("SAEB 2019",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          mainPanel(id = "controls", class = "panel panel-default",
                                    top = 1, left = 1, width = "100%", fixed=FALSE,
                                    draggable = FALSE, height = "100%",
                                    span(h3("Médias SAEB 2019 :"), style="color:#cc4c02"),
                                    tableOutput("table_uf"),
                                    
                                    # absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 50, fixed=TRUE, draggable = TRUE, height = "auto",
                                    #               
                                    #              tags$img(src='logo.png',height='60',width='100'),
                                    #              tags$img(src='ngr.png',height='60',width='100'))#absolutePanel
                          )#mainPanel
                      )#div
             ),#tabPanel
             tabPanel("Análise UF",
                      fluidRow(
                        span(h3("Análise do SAEB 2019 :"), style="color:#cc4c02"),
                        selectInput("in_uf", label = h3("UF"), choices = sort(unique(escolas$UF)), selected = "PE")
                      ),
                      fluidRow(          
                        column(12,tableOutput("resultado")),
                        column(12,plotOutput("boxplot_pt")),
                        column(12,plotOutput("boxplot_mt")),
                      )
             ),
             tabPanel("Mapa",
                      leafletOutput("lf_map")
             )#tabPanel
  )#navbarPage
)#bootstrapPage


### SHINY SERVER ###

server = function(input, output, session) {
  
  # Dataset filtrado uf
  #escolas <- reactive()
  selected <- reactive({
    uf = as.character(input$in_uf)
    escolas%>%filter(UF == !!uf)
  })
  shp_agl_rc <- reactive({
    uf = as.character(input$in_uf)
    shp_agl%>%filter(UF == !!uf)
  })
  
  center_map <- reactive({
    uf = as.character(input$in_uf)
    uf_centro %>% filter(UF==!!uf)
  })
  
  # Dataset do teste
  rm_outliers <- reactive({
    
    if(input$in_disciplina =='Português') {
      
      remove_outliers(selected(),'ME5F_LP')%>%mutate(nota = ME5F_LP)
    }
    else {
      
      remove_outliers(selected(),'ME5F_MT')%>%mutate(nota = ME5F_MT)
    }})
  
  rm_outliers_pt <- reactive({
    
    remove_outliers(selected(),'ME5F_LP')%>%mutate(nota = ME5F_LP)
    
  })
  rm_outliers_mt <- reactive({
    
    remove_outliers(selected(),'ME5F_MT')%>%mutate(nota = ME5F_MT)
    
  })
  
  
  
  
  output$table_uf <- renderTable({
    tabela_notas})
  
  output$boxplot_pt <- renderPlot({
    rm_outliers_pt() %>% ggplot(aes_string(x = "aglomerado",  y = "nota")) +geom_boxplot(fill="slateblue", alpha=0.2) + ggtitle("Portugês") + xlab("Aglomerado") + ylab("y.title")  + theme_minimal() + theme(plot.title=element_text( hjust=0.5, vjust=0.5, face='bold'))
    
  })
  
  
  output$boxplot_mt <- renderPlot({
    rm_outliers_mt() %>% ggplot(aes_string(x = "aglomerado",  y = "nota")) +geom_boxplot(fill="slateblue", alpha=0.2) + ggtitle("Matemática") + xlab("Aglomerado") + ylab("y.title")  + theme_minimal() + theme(plot.title=element_text( hjust=0.5, vjust=0.5, face='bold'))
    
  })
  
  
  
  # #Renderizar o resultado
  output$resultado <- renderTable({
    filter(tabela_resultado,UF==input$in_uf) 
  })
  
  # Renderizar o mapa   
  output$lf_map <- renderLeaflet({
    uf_centro=center_map()
    lat = as.numeric(uf_centro$latitude)
    long <- as.numeric(uf_centro$longitude)
    
    leaflet() %>%
      setView(lng = long, lat = lat, zoom = 8) %>%
      addTiles() %>%
      # addLegend("bottomright", pal = NULL, values = NULL,
      #             title = "<small>Escolas</small>") %>%
      clearMarkers() %>%
      clearShapes() %>%
      addPolygons(data = shp_agl_rc(), weight = 1, smoothFactor = 0.5, ) %>%
      addCircleMarkers(data = selected(), lng = ~longitude, lat = ~latitude, radius = 2, color = "red", label = ~escola)
  }) #, popup = cvli_plot$popup, group = "Status"
  
  
}

#runApp(shinyApp(ui, server), launch.browser = TRUE)
shinyApp(ui, server)
#library(rsconnect)
#deployApp(account="vac-lshtm")
















