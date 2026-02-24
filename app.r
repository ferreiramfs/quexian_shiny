library(shiny); library(bslib); library(thematic); library(leaflet); library(sf)
library(fst); library(dplyr); library(ggplot2); library(scales)

#Shapes mapa
shape_municipal    <- readRDS("data/municipal.rds")
shape_regional     <- readRDS("data/regional.rds")
shape_mesoregional <- readRDS("data/meso.rds")
shape_macroregional <- readRDS("data/macro.rds")

#Dados pré-processados
univariado_num      <- readRDS("data/univariado_num.rds")
univariado_cat      <- readRDS("data/univariado_cat.rds")
bivariado_num_stats <- readRDS("data/bivariado_num_stats.rds")
bivariado_cat       <- readRDS("data/bivariado_cat.rds")

#Prevalências por agrupamento regional
prevalencia_municipal    <- read_fst("data/municipal.fst")
prevalencia_regional     <- read_fst("data/regional.fst")
prevalencia_mesoregional <- read_fst("data/mesoregional.fst")
prevalencia_macroregional <- read_fst("data/macroregional.fst")

#Variáveis
ac_agrupadas <- c(
  "Defeito do Tubo Neural", "Microcefalia", "Cardiopatias Congênitas",
  "Fendas Orais", "Órgãos Genitais", "Defeitos de Membros",
  "Defeitos de Parede Abdominal", "Síndrome de Down", "Todas"
)

var_num <- c(
  'idhm', 'idhm_educacao', 'idhm_longevidade', 'idhm_renda',
  'porcentagem_da_populacao_baixa_renda', 'renda_domiciliar_per_capita',
  'mortalidade', 'taxa_de_analfabetismo', 'cobertura_bcg',
  'IDADEMAE', 'PESO', 'IDADEPAI'
)

var_cat <- c(
  'ESCMAE', 'SEXO', 'RACACORMAE', 'ESTCIVMAE', 'CONSULTAS',
  'APGAR1', 'APGAR5', 'LOCNASC', 'CODOCUPMAE', 'QTDGESTANT',
  'GRAVIDEZ', 'PARTO', 'SEMAGESTAC'
)

ggplot2::theme_set(ggplot2::theme_minimal())
thematic_shiny()
tam_fonte_eixos <- 10

#UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  title = "Análise ACs",
  h2("Análise de Anomalias Congênitas", style = "text-align: center;"),
  
  fluidRow(
    column(4, selectInput("tipo_analise", "Tipo de análise:",
                          choices = c("Univariado", "Bivariado", "Temporal"))),
    column(4, conditionalPanel(
      condition = "input.tipo_analise != 'Univariado'",
      selectInput("anomalia", "Selecione a AC:",
                  choices = ac_agrupadas, selected = ac_agrupadas[1])
    )),
    column(4, conditionalPanel(
      condition = "input.tipo_analise != 'Temporal'",
      selectInput("variavel", "Selecione variável explicativa:",
                  choices = c(var_num, var_cat), selected = var_num[1])
    ))
  ),
  
  conditionalPanel(
    condition = "input.tipo_analise == 'Temporal'",
    fluidRow(
      column(
        width = 12,
        uiOutput("titulo_mapa"),
        fluidRow(
          column(4, selectInput("nivel_geo", "Nível geográfico:",
                                choices = c("Municipal", "Regional", "Mesoregional", "Macroregional"),
                                selected = "Municipal")),
          column(8, sliderInput("ano_mapa", "Selecione o ano:",
                                min = 2013, max = 2022, value = 2013,
                                step = 1, sep = "", animate = TRUE))
        ),
        leafletOutput("mapa_anomalia", height = "600px")
      )
    )
  ),
  
  hr(),
  uiOutput("painel_graficos")
)

#Servidor
server <- function(input, output, session) {
  
  output$painel_graficos <- renderUI({
    if (input$tipo_analise == "Bivariado") {
      fluidRow(plotOutput("grafico_bi", height = "500px"))
    } else if (input$tipo_analise == "Univariado") {
      fluidRow(plotOutput("grafico_uni", height = "500px"))
    }
  })
  
  mod_univariate_server(input, output, session,
                        univariado_num = univariado_num,
                        univariado_cat = univariado_cat,
                        var_num = var_num,
                        var_cat = var_cat)
  
  mod_bivariate_server(input, output, session,
                       bivariado_cat       = bivariado_cat,
                       bivariado_num_stats = bivariado_num_stats,
                       var_num = var_num,
                       var_cat = var_cat)
  
  mod_temporal_server(input, output, session,
                      prevalencia_municipal     = prevalencia_municipal,
                      prevalencia_regional      = prevalencia_regional,
                      prevalencia_mesoregional  = prevalencia_mesoregional,
                      prevalencia_macroregional = prevalencia_macroregional)
}

#Análise Univariada
mod_univariate_server <- function(input, output, session,
                                  univariado_num, univariado_cat,
                                  var_num, var_cat) {
  output$grafico_uni <- renderPlot({
    req(input$variavel)
    var <- input$variavel
    validate(need(var %in% c(var_num, var_cat), "Variável inválida"))
    
    if (var %in% var_num) {
      dados <- univariado_num %>% filter(variavel == var)
      req(nrow(dados) > 0)
      
      ggplot(dados, aes(x = x, y = y, width = width)) +
        geom_col(fill = "#5bc0de", color = "white") +
        labs(
          x     = var,
          y     = "Frequência",
          title = paste("Distribuição de", var)
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title   = element_text(hjust = 0.5, face = "bold"),
          axis.text.x  = element_text(size = tam_fonte_eixos),
          axis.text.y  = element_text(size = tam_fonte_eixos)
        )
      
    } else {
      dados <- univariado_cat %>% filter(variavel == var)
      req(nrow(dados) > 0)
      
      ggplot(dados, aes(x = categoria, y = n)) +
        geom_col(fill = "#5bc0de") +
        geom_text(
          aes(label = format(n, big.mark = ",", decimal.mark = ".")),
          vjust = -0.4, size = 3.5, color = "gray20"
        ) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
        labs(
          x     = var,
          y     = "Frequência",
          title = paste("Distribuição de", var)
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title  = element_text(hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = tam_fonte_eixos, angle = 45, hjust = 1),
          axis.text.y = element_text(size = tam_fonte_eixos)
        )
    }
  })
}

#Análise Bivariada
mod_bivariate_server <- function(input, output, session,
                                 bivariado_num_stats,
                                 bivariado_cat,
                                 var_num, var_cat) {
  output$grafico_bi <- renderPlot({
    var  <- input$variavel
    anom <- input$anomalia
    
    if (var %in% var_num) {
      stats <- bivariado_num_stats %>% filter(variavel == var, anomalia == anom)
      req(nrow(stats) > 0)
      
      stats$anomalia_valor <- factor(stats$anomalia_valor, levels = c(FALSE, TRUE))
      cores <- c("FALSE" = "#5bc0de", "TRUE" = "#d9534f")
      
      ggplot(stats, aes(x = anomalia_valor, fill = anomalia_valor)) +

        geom_errorbar(
          aes(ymin = bigode_inf, ymax = bigode_sup),
          width = 0.2, color = "gray40", linewidth = 0.4
        ) +

        geom_crossbar(
          aes(y = mediana, ymin = q1, ymax = q3),
          width = 0.5, alpha = 0.8, color = "gray30", linewidth = 0.4
        ) +

        geom_point(
          aes(y = mediana),
          color = "white", size = 3
        ) +

        geom_text(
          aes(y = bigode_inf, label = paste0("n=", format(n, big.mark = ",", decimal.mark = "."))),
          vjust = 1.8, size = 3.5, color = "gray40"
        ) +
        scale_fill_manual(values = cores, guide = "none") +
        labs(
          x       = anom,
          y       = var,
          title   = paste("Distribuição de", var, "por", anom),
          caption = "Ponto branco = mediana  |  Caixa = IQR (Q1–Q3)  |  Barras = 1,5×IQR (Tukey)"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title   = element_text(hjust = 0.5, face = "bold"),
          plot.caption = element_text(color = "gray50", size = 9),
          axis.text.x  = element_text(size = tam_fonte_eixos, face = "bold"),
          axis.text.y  = element_text(size = tam_fonte_eixos)
        )
      
    } else if (var %in% var_cat) {
      dados <- bivariado_cat %>%
        filter(variavel == var, anomalia == anom, anomalia_valor == TRUE)
      
      req(nrow(dados) > 0)
      
      ggplot(dados, aes(x = categoria, y = prev_10k)) +
        geom_col(fill = "#5bc0de") +
        geom_text(
          aes(label = format(round(prev_10k, 1), nsmall = 1, decimal.mark = ",")),
          vjust = -1.8, size = 3.5, color = "gray20"
        ) +
        geom_text(
          aes(label = paste0("n=", format(total, big.mark = ",", decimal.mark = "."))),
          vjust = -0.4, size = 3, color = "gray50"
        ) +
        scale_y_continuous(
          expand = expansion(mult = c(0, 0.18)),
          labels = function(x) paste0(x, " /10k")
        ) +
        labs(
          x       = var,
          y       = paste("Prevalência de", anom, "(por 10.000 NV)"),
          title   = paste("Prevalência de", anom, "por", var),
          caption = paste("Total de casos com anomalia:", format(sum(dados$n), big.mark = ",", decimal.mark = "."))
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title  = element_text(hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = tam_fonte_eixos, face = "bold",
                                     angle = 45, hjust = 1),
          axis.text.y = element_text(size = tam_fonte_eixos, face = "bold"),
          plot.caption = element_text(hjust = 0)
        )
    }
  })
}

#Análise Temporal (mapa)
mod_temporal_server <- function(input, output, session,
                                prevalencia_municipal,
                                prevalencia_regional,
                                prevalencia_mesoregional,
                                prevalencia_macroregional) {

  prevalencias <- list(
    "Municipal"     = prevalencia_municipal,
    "Regional"      = prevalencia_regional,
    "Mesoregional"  = prevalencia_mesoregional,
    "Macroregional" = prevalencia_macroregional
  )
  
  output$titulo_mapa <- renderUI({
    req(input$anomalia, input$ano_mapa, input$nivel_geo)
    
    nivel_texto <- switch(
      input$nivel_geo,
      "Municipal"    = "Município",
      "Regional"     = "Regional de Saúde",
      "Mesoregional" = "Mesoregião",
      "Macroregional" = "Macroregional de Saúde"
    )
    
    h3(
      paste0("Prevalência de ", input$anomalia, " por ", nivel_texto, " em ", input$ano_mapa),
      style = "text-align:center; margin-top:10px; font-weight:600;"
    )
  })
  
  output$mapa_anomalia <- renderLeaflet({
    req(input$anomalia, input$ano_mapa, input$nivel_geo)
    
    niveis <- list(
      "Municipal" = list(
        df         = prevalencia_municipal,
        shape      = shape_municipal,
        join_key_x = "CC_2",
        join_key_y = "cod",
        col_prev   = "prevalencia",
        label_col  = "name_muni"
      ),
      "Regional" = list(
        df         = prevalencia_regional,
        shape      = shape_regional,
        join_key_x = "REGIONA",
        join_key_y = "regional",
        col_prev   = "prevalencia",
        label_col  = "REGIONA"
      ),
      "Mesoregional" = list(
        df         = prevalencia_mesoregional,
        shape      = shape_mesoregional,
        join_key_x = "meso",
        join_key_y = "meso",
        col_prev   = "prevalencia",
        label_col  = "meso"
      ),
      "Macroregional" = list(
        df         = prevalencia_macroregional,
        shape      = shape_macroregional,
        join_key_x = "MACRO",
        join_key_y = "macro",
        col_prev   = "prevalencia",
        label_col  = "MACRO"
      )
    )
    
    nivel_info <- niveis[[input$nivel_geo]]
    
    dados <- nivel_info$df %>%
      filter(anomalia == input$anomalia, ANO_NASC == input$ano_mapa)
    
    dados_mapa <- nivel_info$shape %>%
      mutate(across(nivel_info$join_key_x, as.character)) %>%
      left_join(
        dados %>% mutate(across(nivel_info$join_key_y, as.character)),
        by = setNames(nivel_info$join_key_y, nivel_info$join_key_x)
      )
    
    var_prev   <- dados_mapa[[nivel_info$col_prev]]
    label_nome <- dados_mapa[[nivel_info$label_col]]
    
    base_dados <- prevalencias[[input$nivel_geo]]
    dom_fixo   <- range(
      base_dados$prevalencia[base_dados$anomalia == input$anomalia],
      na.rm = TRUE
    )
    
    pal <- colorNumeric(palette = "Greens", domain = dom_fixo, na.color = "gray90")
    
    leaflet(dados_mapa,
            options = leafletOptions(
              dragging         = FALSE,
              zoomControl      = FALSE,
              scrollWheelZoom  = FALSE,
              doubleClickZoom  = FALSE,
              boxZoom          = FALSE,
              touchZoom        = FALSE,
              keyboard         = FALSE,
              minZoom          = 7,
              maxZoom          = 7,
              zoom             = 7,
              preferCanvas     = TRUE
            )
    ) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor       = ~pal(var_prev),
        color           = "#BBB",
        weight          = 1,
        opacity         = 1,
        fillOpacity     = 0.8,
        smoothFactor    = 0.2,
        highlightOptions = highlightOptions(
          weight = 2, color = "#666", fillOpacity = 0.9, bringToFront = TRUE
        ),
        label = ~paste0(label_nome, ": ", round(var_prev, 1), " /10k")
      ) %>%
      addLegend(
        position  = "bottomright",
        pal       = pal,
        values    = dom_fixo,
        title     = paste("Prevalência de", input$anomalia, "-", input$ano_mapa),
        labFormat = labelFormat(suffix = " /10k")
      ) %>%
      setView(lng = -51.5, lat = -24.5, zoom = 7) %>%
      setMaxBounds(lng1 = -54.0, lat1 = -26.7, lng2 = -48.0, lat2 = -22.5)
  })
}

#Shiny APP
shinyApp(ui = ui, server = server)