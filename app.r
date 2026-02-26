library(shiny)
library(bslib)
library(thematic)
library(leaflet)
library(sf)
library(fst)
library(dplyr)
library(ggplot2)
library(scales)
library(gstat)

# ─────────────────────────────────────────────────────────────────────────────
# Shapes
# ─────────────────────────────────────────────────────────────────────────────
shape_municipal    <- readRDS("data/municipal.rds")
shape_regional     <- readRDS("data/regional.rds")
shape_mesoregional <- readRDS("data/meso.rds")
shape_macroregional <- readRDS("data/macro.rds")

# ─────────────────────────────────────────────────────────────────────────────
# Dados pré-processados (deploy leve — sem a base completa)
# ─────────────────────────────────────────────────────────────────────────────
univariado_num      <- readRDS("data/univariado_num.rds")
univariado_cat      <- readRDS("data/univariado_cat.rds")
bivariado_num_stats <- readRDS("data/bivariado_num_stats.rds")
bivariado_cat       <- readRDS("data/bivariado_cat.rds")

# ─────────────────────────────────────────────────────────────────────────────
# Prevalências por agrupamento regional
# ─────────────────────────────────────────────────────────────────────────────
prevalencia_municipal    <- read_fst("data/municipal.fst")
prevalencia_regional     <- read_fst("data/regional.fst")
prevalencia_mesoregional <- read_fst("data/mesoregional.fst")
prevalencia_macroregional <- read_fst("data/macroregional.fst")

# ─────────────────────────────────────────────────────────────────────────────
# Constantes
# ─────────────────────────────────────────────────────────────────────────────
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

# ─────────────────────────────────────────────────────────────────────────────
# UI
# ─────────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  title = 'Análise ACs',
  h2("Análise de Anomalias Congênitas", align = "center"),
  
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
          column(4, radioButtons("modo_periodo", "Período:",
                                 choices = c("Anual", "Completo (2013–2022)"),
                                 selected = "Anual", inline = TRUE)),
          column(4, conditionalPanel(
            condition = "input.modo_periodo == 'Anual'",
            sliderInput("ano_mapa", "Selecione o ano:",
                        min = 2013, max = 2022, value = 2013,
                        step = 1, sep = "", animate = TRUE)
          ))
        ),
        fluidRow(
          column(7, leafletOutput("mapa_anomalia",    height = "500px")),
          column(5, plotOutput("variograma_anomalia", height = "500px"))
        )
      )
    )
  ),
  
  hr(),
  uiOutput("painel_graficos")
)

# ─────────────────────────────────────────────────────────────────────────────
# SERVER
# ─────────────────────────────────────────────────────────────────────────────
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

# ─────────────────────────────────────────────────────────────────────────────
# MÓDULO: Análise Univariada
# ─────────────────────────────────────────────────────────────────────────────
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
      
      # ← usa a largura real de cada bin para reproduzir o histograma fielmente
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

# ─────────────────────────────────────────────────────────────────────────────
# MÓDULO: Análise Bivariada
# ─────────────────────────────────────────────────────────────────────────────
mod_bivariate_server <- function(input, output, session,
                                 bivariado_num_stats,
                                 bivariado_cat,
                                 var_num, var_cat) {
  output$grafico_bi <- renderPlot({
    var  <- input$variavel
    anom <- input$anomalia
    
    if (var %in% var_num) {
      # ── Boxplot reconstruído a partir de estatísticas pré-calculadas ───────
      stats <- bivariado_num_stats %>% filter(variavel == var, anomalia == anom)
      req(nrow(stats) > 0)
      
      # FALSE (sem anomalia) à esquerda, TRUE (com anomalia) à direita
      stats$anomalia_valor <- factor(stats$anomalia_valor, levels = c(FALSE, TRUE))
      cores <- c("FALSE" = "#5bc0de", "TRUE" = "#d9534f")
      
      ggplot(stats, aes(x = anomalia_valor, fill = anomalia_valor)) +
        # Bigodes Tukey (1.5×IQR)
        geom_errorbar(
          aes(ymin = bigode_inf, ymax = bigode_sup),
          width = 0.2, color = "gray40", linewidth = 0.4
        ) +
        # Caixa Q1–Q3
        geom_crossbar(
          aes(y = mediana, ymin = q1, ymax = q3),
          width = 0.5, alpha = 0.8, color = "gray30", linewidth = 0.4
        ) +
        # Ponto da mediana
        geom_point(
          aes(y = mediana),
          color = "white", size = 3
        ) +
        # Rótulo n= abaixo de cada caixa
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
      # ── Prevalência por 10.000 NV — todas as categorias, incluindo 0 casos ─
      # Busca todas as categorias conhecidas para esta variável (do univariado)
      # Todas as categorias já estão presentes no pré-processamento,
      # incluindo as com n = 0 e prev_10k = 0
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
          plot.caption  = element_text(hjust = 0),
          axis.text.x = element_text(size = tam_fonte_eixos, face = "bold",
                                     angle = 45, hjust = 1),
          axis.text.y = element_text(size = tam_fonte_eixos, face = "bold")
        )
    }
  })
}

# ─────────────────────────────────────────────────────────────────────────────
# MÓDULO: Análise Temporal (mapa + variograma)
# ─────────────────────────────────────────────────────────────────────────────
mod_temporal_server <- function(input, output, session,
                                prevalencia_municipal,
                                prevalencia_regional,
                                prevalencia_mesoregional,
                                prevalencia_macroregional) {
  
  # Named list para lookup direto — evita get() com strings dinâmicas
  prevalencias <- list(
    "Municipal"     = prevalencia_municipal,
    "Regional"      = prevalencia_regional,
    "Mesoregional"  = prevalencia_mesoregional,
    "Macroregional" = prevalencia_macroregional
  )
  
  niveis <- list(
    "Municipal" = list(
      df         = prevalencia_municipal,
      shape      = shape_municipal,
      join_key_x = "CC_2",
      join_key_y = "cod",
      col_prev   = "prevalencia",
      label_col  = "NAME_2"
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
  
  # ── Reactive compartilhado — evita duplicar o join no mapa e no variograma ──
  dados_mapa_r <- reactive({
    req(input$anomalia, input$nivel_geo, input$modo_periodo)
    
    nivel_info <- niveis[[input$nivel_geo]]
    chave_geo  <- nivel_info$join_key_y   # coluna de ID geográfico nos dados
    
    if (input$modo_periodo == "Anual") {
      req(input$ano_mapa)
      dados <- nivel_info$df %>%
        filter(anomalia == input$anomalia, ANO_NASC == input$ano_mapa)
    } else {
      # Período completo — agrega somando casos e nascidos, recalcula prevalência
      dados <- nivel_info$df %>%
        filter(anomalia == input$anomalia) %>%
        group_by(across(all_of(chave_geo)), anomalia) %>%
        summarise(
          casos    = sum(casos,    na.rm = TRUE),
          nascidos = sum(nascidos, na.rm = TRUE),
          .groups  = "drop"
        ) %>%
        mutate(prevalencia = ifelse(nascidos > 0, (casos / nascidos) * 10000, NA_real_))
    }
    
    nivel_info$shape %>%
      mutate(across(nivel_info$join_key_x, as.character)) %>%
      left_join(
        dados %>% mutate(across(all_of(chave_geo), as.character)),
        by = setNames(chave_geo, nivel_info$join_key_x)
      )
  })
  
  output$titulo_mapa <- renderUI({
    req(input$anomalia, input$nivel_geo, input$modo_periodo)
    
    nivel_texto <- switch(
      input$nivel_geo,
      "Municipal"     = "Município",
      "Regional"      = "Regional de Saúde",
      "Mesoregional"  = "Mesoregião",
      "Macroregional" = "Macroregional de Saúde"
    )
    
    periodo_texto <- if (input$modo_periodo == "Anual") {
      req(input$ano_mapa)
      input$ano_mapa
    } else {
      "2013–2022"
    }
    
    h3(
      paste0("Prevalência de ", input$anomalia, " por ", nivel_texto, " — ", periodo_texto),
      style = "text-align:center; margin-top:10px; font-weight:600;"
    )
  })
  
  output$mapa_anomalia <- renderLeaflet({
    dados_mapa  <- dados_mapa_r()
    nivel_info  <- niveis[[input$nivel_geo]]
    var_prev    <- dados_mapa[[nivel_info$col_prev]]
    label_nome  <- dados_mapa[[nivel_info$label_col]]
    
    base_dados <- prevalencias[[input$nivel_geo]]
    dom_fixo <- if (input$modo_periodo == "Anual") {
      # Escala fixa considerando todos os anos — comparação temporal consistente
      range(
        base_dados$prevalencia[base_dados$anomalia == input$anomalia],
        na.rm = TRUE
      )
    } else {
      # Escala do período completo — usa os valores já calculados no reactive
      range(var_prev, na.rm = TRUE)
    }
    
    pal <- colorNumeric(palette = "Greens", domain = dom_fixo, na.color = "gray90")
    
    leaflet(dados_mapa,
            options = leafletOptions(
              dragging        = FALSE, zoomControl     = FALSE,
              scrollWheelZoom = FALSE, doubleClickZoom = FALSE,
              boxZoom         = FALSE, touchZoom       = FALSE,
              keyboard        = FALSE, minZoom         = 7,
              maxZoom         = 7,    zoom             = 7,
              preferCanvas    = TRUE
            )
    ) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor        = ~pal(var_prev),
        color            = "#BBB", weight = 1, opacity = 1,
        fillOpacity      = 0.8,    smoothFactor = 0.2,
        highlightOptions = highlightOptions(
          weight = 2, color = "#666", fillOpacity = 0.9, bringToFront = TRUE
        ),
        label = ~paste0(label_nome, ": ", round(var_prev, 1), " /10k")
      ) %>%
      addLegend(
        position  = "bottomright", pal = pal, values = dom_fixo,
        title     = paste("Prevalência de", input$anomalia, "-", input$ano_mapa),
        labFormat = labelFormat(suffix = " /10k")
      ) %>%
      setView(lng = -51.5, lat = -24.5, zoom = 7) %>%
      setMaxBounds(lng1 = -54.0, lat1 = -26.7, lng2 = -48.0, lat2 = -22.5)
  })
  
  output$variograma_anomalia <- renderPlot({
    dados_mapa <- dados_mapa_r()
    nivel_info <- niveis[[input$nivel_geo]]
    
    # Padroniza o nome da coluna de prevalência
    if (nivel_info$col_prev != "prevalencia") {
      dados_mapa <- dados_mapa %>%
        rename(prevalencia = all_of(nivel_info$col_prev))
    }
    
    # Centroides projetados em metros (SIRGAS 2000 UTM zona 22S)
    sf_pts <- dados_mapa %>%
      select(prevalencia, geometry) %>%
      filter(!is.na(prevalencia)) %>%
      st_set_agr("constant") %>%
      st_centroid() %>%
      st_transform(31982)
    
    validate(
      need(nrow(sf_pts) >= 5,
           "Poucos dados para calcular o variograma neste filtro.")
    )
    
    # Breaks logarítmicos — distribui melhor os pares no nível municipal
    coords   <- st_coordinates(sf_pts)
    dist_max <- max(dist(coords))
    dist_min <- min(dist(coords)[dist(coords) > 0])
    breaks   <- exp(seq(log(dist_min), log(dist_max * 0.6), length.out = 16))
    
    vgm_emp <- variogram(
      prevalencia ~ 1,
      data       = sf_pts,
      boundaries = breaks
    )
    
    vgm_emp$dist_km <- vgm_emp$dist / 1000
    sill_ref        <- var(sf_pts$prevalencia)
    
    ggplot(vgm_emp, aes(x = dist_km, y = gamma)) +
      geom_hline(yintercept = sill_ref, linetype = "dashed",
                 color = "gray60", linewidth = 0.5) +
      annotate("text", x = max(vgm_emp$dist_km) * 0.98, y = sill_ref,
               label = "variância total", hjust = 1, vjust = -0.5,
               size = 3, color = "gray50") +
      geom_line(color = "#2c7bb6", alpha = 0.4, linewidth = 0.5) +
      geom_point(aes(size = np), color = "#2c7bb6", alpha = 0.85) +
      scale_x_log10(labels = comma) +
      scale_size_continuous(
        name   = "Nº de pares",
        range  = c(2, 8),
        labels = comma
      ) +
      labs(
        x        = "Distância (km, escala log)",
        y        = "Semivariância",
        title    = "Variograma Empírico",
        subtitle = paste0(
          input$anomalia, " · ", input$nivel_geo, " · ",
          if (input$modo_periodo == "Anual") input$ano_mapa else "2013-2022"
        ),
        caption  = paste0(
          "Abaixo da linha tracejada -> autocorrelação espacial positiva\n",
          "Acima -> variabilidade maior entre vizinhos do que no conjunto"
        )
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title    = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 10),
        plot.caption  = element_text(color = "gray50", size = 8, lineheight = 1.3, hjust = 0),
        legend.position = "bottom"
      )
  })
}

# ─────────────────────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
