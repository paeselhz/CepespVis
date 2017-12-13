  ##### Treemap PREFEITO

  output$tree_prefeito <- renderD3plus({

    est <- input$escolhe_est_prefeito

    year <- input$ano_mun_prefeito
    
    city <- input$escolhe_mun_prefeito

    sel_est <- tse_capitais %>%
                 filter(estados == est)

    nome_est <- as.character(sel_est$siglas)
    
    if(input$pref_turno == 'turno_1'){
      turno <- 1
    }
    else {
      turno <- 2
    }

  tree_aux_1 <- tse_data %>%
    filter(NOME_MUNICIPIO == city,
           NOME_UF == est,
           NUM_TURNO == turno,
           DESCRICAO_CARGO == "PREFEITO",
           ANO_ELEICAO == year) %>%
    rename('Composição da Legenda' = COMPOSICAO_LEGENDA,
           'Nome do Candidato' = NOME_CANDIDATO,
           'Situação no Turno' = DESC_SIT_TOT_TURNO,
           'Quantidade de Votos' = QTDE_VOTOS)

  d3plus(data = tree_aux_1,
         type = "tree_map",
         id = c("SIGLA_PARTIDO"),
         width = "100%",
         locale = 'pt_BR',
         height = 300,
         clean_previous = TRUE) %>%
    d3plusSize(value = "Quantidade de Votos") %>%
    d3plusLegend(value = TRUE, order = list(sort = "desc", value = "size")) %>%
    d3plusDepth(0) %>%
    d3plusLabels(valign = "top") %>%
    d3plusFont(family = "Roboto") %>%
    d3plusTitle(paste0("Eleições para Prefeito em ", city,"/",nome_est, " no ano de ", year)) %>%
    d3plusTooltip(value = c("Composição da Legenda",
                            "Nome do Candidato",
                            "Situação no Turno"))
    
    
  })

  output$tree_prefeito_final <- renderUI({
    d3plusOutput("tree_prefeito")
  })


##### Treemap VEREADOR

   output$tree_vereador <- renderD3plus({

    est <- input$escolhe_est_vereador

    year <- input$ano_mun_vereador
    
    city <- input$escolhe_mun_vereador

    sel_est <- tse_capitais %>%
                 filter(estados == est)

    nome_est <- as.character(sel_est$siglas)

    tree_aux_2 <- tse_data %>%
      filter(ANO_ELEICAO == year,
             NOME_UF == est,
             NOME_MUNICIPIO == city,
             DESCRICAO_CARGO == "VEREADOR") %>%
      mutate(CANDI_DISPLAY = paste0(NOME_CANDIDATO, " - ", SIGLA_PARTIDO, " - ", DESC_SIT_TOT_TURNO)) %>%
      rename('Quantidade de Votos' = QTDE_VOTOS)
    
    d3plus(data = tree_aux_2,
           type = "tree_map",
           id = c("COMPOSICAO_LEGENDA", "CANDI_DISPLAY"),
           width = "100%",
           locale = 'pt_BR',
           height = 300,
           clean_previous = TRUE) %>%
      d3plusSize(value = "Quantidade de Votos") %>%
      d3plusLegend(value = TRUE, order = list(sort = "desc", value = "size")) %>%
      d3plusDepth(0) %>%
      d3plusLabels(valign = "top") %>%
      d3plusFont(family = "Roboto") %>%
      d3plusTitle(paste0("Eleições para Vereador em ", city,"/",nome_est, " no ano de ", year))
    
  })
  
  output$tree_vereador_final <- renderUI({
    d3plusOutput("tree_vereador")
  })

  ##### TREEMAP Deputado Estadual
  ### Treemap Deputado Estadual para o Estado

  output$tree_dep_est_est <- renderD3plus({
    
    year <- input$ano_est_dep_est
    
    est <- input$escolhe_est_dep_est_est

    tree_aux_2 <- tse_data %>%
      filter(ANO_ELEICAO == year,
             NOME_UF == est,
             DESCRICAO_CARGO == "DEPUTADO ESTADUAL") %>%
      group_by(NOME_CANDIDATO, NOME_UF) %>%
      mutate(COD_MUN_IBGE = NULL, NOME_MUNICIPIO = NULL, NOME_MESO = NULL, NOME_MICRO = NULL,
             TOTAL_VOTOS = sum(QTDE_VOTOS),
             QTDE_VOTOS = NULL,
             CANDI_DISPLAY = paste0(NOME_CANDIDATO, " - ", SIGLA_PARTIDO, " - ", DESC_SIT_TOT_TURNO)) %>%
      rename('Quantidade de Votos' = TOTAL_VOTOS) %>%
      unique() %>%
      ungroup()
    
    d3plus(data = tree_aux_2,
           type = "tree_map",
           id = c("COMPOSICAO_LEGENDA", "CANDI_DISPLAY"),
           width = "100%",
           locale = 'pt_BR',
           height = 300,
           clean_previous = TRUE) %>%
      d3plusSize(value = "Quantidade de Votos") %>%
      d3plusLegend(value = TRUE, order = list(sort = "desc", value = "size")) %>%
      d3plusDepth(0) %>%
      d3plusLabels(valign = "top") %>%
      d3plusTitle(paste0("Eleições para Deputado Estadual em ", est, " no ano de ", year)) %>%
      d3plusFont(family = "Roboto")
  })
  
  output$tree_dep_est_final_est <- renderUI({
    d3plusOutput("tree_dep_est_est")
  })

  ##### TREEMAP SENADORES
  ### TREEMAP Senadores para o ESTADO

  output$tree_senador_est <- renderD3plus({
    
    year <- input$ano_senador_est
    
    est <- input$escolhe_est_senador_est

    tree_aux_2 <- tse_data %>%
      filter(ANO_ELEICAO == year,
             NOME_UF == est,
             DESCRICAO_CARGO == "SENADOR") %>%
      group_by(NOME_CANDIDATO, NOME_UF) %>%
      mutate(COD_MUN_IBGE = NULL, NOME_MUNICIPIO = NULL, NOME_MESO = NULL, NOME_MICRO = NULL,
             TOTAL_VOTOS = sum(QTDE_VOTOS),
             QTDE_VOTOS = NULL) %>%
      rename('Quantidade de Votos' = TOTAL_VOTOS,
             'Nome do Candidato' = NOME_CANDIDATO,
             'Situação na Eleição' = DESC_SIT_TOT_TURNO,
             'Composição da Legenda' = COMPOSICAO_LEGENDA) %>%
      unique() %>%
      ungroup()
    
    d3plus(data = tree_aux_2,
           type = "tree_map",
           id = c("SIGLA_PARTIDO"),
           width = "100%",
           locale = 'pt_BR',
           height = 300,
           clean_previous = TRUE) %>%
      d3plusSize(value = "Quantidade de Votos") %>%
      d3plusLegend(value = TRUE, order = list(sort = "desc", value = "size")) %>%
      d3plusDepth(0) %>%
      d3plusLabels(valign = "top") %>%
      d3plusFont(family = "Roboto") %>%
      d3plusTitle(paste0("Eleições para Senador em ", est, " no ano de ", year)) %>%
      d3plusTooltip(value = c("Composição da Legenda",
                              "Nome do Candidato",
                              "Situação na Eleição"))
  })

  output$tree_senador_final_est <- renderUI({
      d3plusOutput("tree_senador_est")
  })

  ##### TREEMAP GOVERNADORES

  ### TREEMAP GOVERNADORES PARA O ESTADO

  output$tree_gov_est <- renderD3plus({
    
    year <- input$ano_gov_est
    
    est <- input$escolhe_est_gov_est

    if(input$gov_turno_est == 'turno_1'){
      turno <- 1
    }
    else {
      turno <- 2
    }

    tree_aux_2 <- tse_data %>%
      filter(ANO_ELEICAO == year,
             NOME_UF == est,
             NUM_TURNO == turno,
             DESCRICAO_CARGO == "GOVERNADOR") %>%
      group_by(NOME_CANDIDATO, NOME_UF) %>%
      mutate(COD_MUN_IBGE = NULL, NOME_MUNICIPIO = NULL, NOME_MESO = NULL, NOME_MICRO = NULL,
             TOTAL_VOTOS = sum(QTDE_VOTOS),
             QTDE_VOTOS = NULL) %>%
      rename('Composição da Legenda' = COMPOSICAO_LEGENDA,
             'Nome do Candidato' = NOME_CANDIDATO,
             'Situação no Turno' = DESC_SIT_TOT_TURNO,
             'Quantidade de Votos' = TOTAL_VOTOS) %>%
      unique() %>%
      ungroup()
    
    d3plus(data = tree_aux_2,
           type = "tree_map",
           id = c("SIGLA_PARTIDO"),
           width = "100%",
           height = 300,
           locale = 'pt_BR',
           clean_previous = TRUE) %>%
      d3plusSize(value = "Quantidade de Votos") %>%
      d3plusLegend(value = TRUE, order = list(sort = "desc", value = "size")) %>%
      d3plusDepth(0) %>%
      d3plusLabels(valign = "top") %>%
      d3plusFont(family = "Roboto") %>%
      d3plusTitle(paste0("Eleições para Governador em ", est, " no ano de ", year)) %>%
      d3plusTooltip(value = c("Composição da Legenda",
                              "Nome do Candidato",
                              "Situação no Turno"))
  })

  output$tree_gov_final_est <- renderUI({
      d3plusOutput("tree_gov_est")
  })

  ##### TREEMAP Deputado Federal
  ### Treemap Deputado Federal para o Estado

  output$tree_dep_fed_est <- renderD3plus({
    
    year <- input$ano_dep_fed_est
    
    est <- input$escolhe_est_dep_fed_est

    tree_aux_2 <- tse_data %>%
      filter(ANO_ELEICAO == year,
             NOME_UF == est,
             DESCRICAO_CARGO == "DEPUTADO FEDERAL") %>%
      group_by(NOME_CANDIDATO, NOME_UF) %>%
      mutate(COD_MUN_IBGE = NULL, NOME_MUNICIPIO = NULL, NOME_MESO = NULL, NOME_MICRO = NULL,
             TOTAL_VOTOS = sum(QTDE_VOTOS),
             QTDE_VOTOS = NULL,
             CANDI_DISPLAY = paste0(NOME_CANDIDATO, " - ", SIGLA_PARTIDO, " - ", DESC_SIT_TOT_TURNO)) %>%
      rename('Quantidade de Votos' = TOTAL_VOTOS) %>%
      unique() %>%
      ungroup()
    
    d3plus(data = tree_aux_2,
           type = "tree_map",
           id = c("COMPOSICAO_LEGENDA", "CANDI_DISPLAY"),
           width = "100%",
           height = 300,
           locale = 'pt_BR',
           clean_previous = TRUE) %>%
      d3plusSize(value = "Quantidade de Votos") %>%
      d3plusLegend(value = TRUE, order = list(sort = "desc", value = "size")) %>%
      d3plusDepth(0) %>%
      d3plusLabels(valign = "top") %>%
      d3plusFont(family = "Roboto") %>%
      d3plusTitle(paste0("Eleições para Deputado Federal em ", est, " no ano de ", year))
  })
  
  output$tree_dep_fed_final_est <- renderUI({
    d3plusOutput("tree_dep_fed_est")
  })


  ##### TREEMAP PRESIDENCIA

output$tree_pres_pais <- renderD3plus({
    
    year <- input$ano_pres_pais

    if(input$pres_turno_pais == 'turno_1'){
      turno <- 1
    }
    else {
      turno <- 2
    }

    tree_aux_2 <- tse_data %>%
      filter(ANO_ELEICAO == year,
             NUM_TURNO == turno,
             DESCRICAO_CARGO == "PRESIDENTE") %>%
      group_by(NOME_CANDIDATO) %>%
      mutate(COD_MUN_IBGE = NULL, NOME_MUNICIPIO = NULL, NOME_MESO = NULL, NOME_MICRO = NULL, NOME_UF = NULL,
             TOTAL_VOTOS = sum(QTDE_VOTOS),
             QTDE_VOTOS = NULL) %>%
      rename('Quantidade de Votos' = TOTAL_VOTOS,
             'Nome do Candidato' = NOME_CANDIDATO,
             'Composição da Legenda' = COMPOSICAO_LEGENDA,
             'Situação no Turno' = DESC_SIT_TOT_TURNO) %>%
      unique() %>%
      ungroup()
    
    d3plus(data = tree_aux_2,
           type = "tree_map",
           id = c("SIGLA_PARTIDO"),
           width = "100%",
           height = 300,
           locale = 'pt_BR',
           clean_previous = TRUE) %>%
      d3plusSize(value = "Quantidade de Votos") %>%
      d3plusLegend(value = TRUE, order = list(sort = "desc", value = "size")) %>%
      d3plusDepth(0) %>%
      d3plusLabels(valign = "top") %>%
      d3plusFont(family = "Roboto") %>%
      d3plusTitle(paste0("Eleições para Presidente no ano de ", year)) %>%
      d3plusTooltip(value = c("Composição da Legenda",
                              "Nome do Candidato",
                              "Situação no Turno"))
  })

  output$tree_pres_final_pais <- renderUI({
      d3plusOutput("tree_pres_pais")
  })
