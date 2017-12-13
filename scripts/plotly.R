  ##### Gráfico PREFEITOS CANDIDATOS

  output$grafico_prefeitos_candidatos <- renderPlotly({

    city <- input$escolhe_mun_prefeito
    
    year <- input$ano_mun_prefeito

    est <- input$escolhe_est_prefeito

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
                         ANO_ELEICAO == year,
                         NUM_TURNO == turno,
                         NOME_UF == est,
                         DESCRICAO_CARGO == "PREFEITO") %>%
                  arrange(desc(QTDE_VOTOS)) %>%
                  mutate(PERC_VOTOS = (QTDE_VOTOS/sum(QTDE_VOTOS))*100)
    
    tree_aux_1$SIGLA_PARTIDO <- factor(tree_aux_1$SIGLA_PARTIDO, levels = unique(tree_aux_1$SIGLA_PARTIDO)[order(tree_aux_1$QTDE_VOTOS, decreasing = TRUE)])
    
    plot_ly(tree_aux_1,x = ~SIGLA_PARTIDO, 
            y = ~QTDE_VOTOS, 
            #sort = FALSE,
            type = 'bar', 
            color = ~SIGLA_PARTIDO,
            hoverinfo="text", ##Tirar tool tips pq formatacao era feia
            text = ~paste0(paste0(NOME_CANDIDATO, " - ", SIGLA_PARTIDO), "<br>",
                           "Total de Votos: ", QTDE_VOTOS, "<br>",
                           "Percentual de Votos: ", round(PERC_VOTOS, 3), "% <br>",
                           DESC_SIT_TOT_TURNO)) %>%
      layout(title = paste0("Votação dos Candidatos para Prefeito em ", city,"/",nome_est, " no ano de ", year),
             xaxis = list(title = ""),
             yaxis = list(title = ""),
             showlegend = FALSE,
             font = list(family = "Roboto"))
  })

  ##### Gráfico VEREADORES ELEITOS

  output$grafico_vereadores_eleitos <- renderPlotly({

    city <- input$escolhe_mun_vereador
    
    year <- input$ano_mun_vereador

    est <- input$escolhe_est_vereador
    
    sel_est <- tse_capitais %>%
                 filter(estados == est)

    nome_est <- as.character(sel_est$siglas)

    tree_aux_1 <- tse_data %>%
                    filter(NOME_MUNICIPIO == city,
                           ANO_ELEICAO == year,
                           DESCRICAO_CARGO == "VEREADOR",
                           NOME_UF == est) %>%
                    mutate(PERC_VOTOS = (QTDE_VOTOS/sum(QTDE_VOTOS))*100) %>% 
                    filter(DESC_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA", "ELEITO", "MÉDIA")) %>%
                    arrange(desc(QTDE_VOTOS))
                    
    
    tree_aux_1$NOME_CANDIDATO <- factor(tree_aux_1$NOME_CANDIDATO, levels = unique(tree_aux_1$NOME_CANDIDATO)[order(tree_aux_1$QTDE_VOTOS, decreasing = TRUE)])
    
    plot_ly(tree_aux_1,x = ~NOME_CANDIDATO, 
            y = ~QTDE_VOTOS, 
            #sort = FALSE,
            type = 'bar', 
            color = ~SIGLA_PARTIDO,
            hoverinfo="text", ##Tirar tool tips pq formatacao era feia
            text = ~paste0(paste0(NOME_CANDIDATO, " - ", SIGLA_PARTIDO), "<br>",
                           "Total de Votos: ", QTDE_VOTOS, "<br>",
                           "Percentual de Votos: ", round(PERC_VOTOS,3), "% <br>",
                           DESC_SIT_TOT_TURNO)) %>%
      layout(title = paste0("Votação dos Eleitos para Vereador em ", city,"/",nome_est, " no ano de ", year),
             xaxis = list(title = "",
                          showticklabels = FALSE),
             yaxis = list(title = ""),
             legend = list(orientation = 'h'),
             font = list(family = "Roboto"))
  })

  ### GRAFICO QUOCIENTES ELEITORAIS

    output$qe_vereador <- renderText({
      city <- input$escolhe_mun_vereador
    
      year <- input$ano_mun_vereador

      est <- input$escolhe_est_vereador
      
      sel_est <- tse_capitais %>%
                   filter(estados == est)

      nome_est <- as.character(sel_est$siglas)

      tree_aux_1 <- tse_data %>%
        filter(NOME_MUNICIPIO == city,
               ANO_ELEICAO == year,
               DESCRICAO_CARGO == "VEREADOR",
               NOME_UF == est) %>%
        filter(DESC_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA", "ELEITO", "MÉDIA")) %>%
        mutate(N_ELEITOS = n()) %>%
        select(N_ELEITOS) %>%
        unique()

      tot <- unique(tree_aux_1$N_ELEITOS)

      rm(tree_aux_1)

      data_aux <- tse_qp %>%
        filter(NOME_MUNICIPIO == city,
               SIGLA_UF == nome_est,
               ANO_ELEICAO == year) %>%
        select(-SIGLA_PARTIDO) %>%
        unique() %>%
        mutate(N_VOTOS = QTDE_VOTOS_NOMINAIS + QTDE_VOTOS_LEGENDA,
               N_VOTOS_TOT = sum(N_VOTOS),
               N_VAGAS = tot,
               QE = round(N_VOTOS_TOT/N_VAGAS)) %>%
        select(QE) %>% unique()

        qe_final <- unique(data_aux$QE)

      paste0("Quociente Eleitoral em ", city, " no ano de ", year, ": ",qe_final)
      })

    output$grafico_quociente_partidos <- renderPlotly({

    city <- input$escolhe_mun_vereador
    
    year <- input$ano_mun_vereador

    est <- input$escolhe_est_vereador
    
    sel_est <- tse_capitais %>%
                 filter(estados == est)

    nome_est <- as.character(sel_est$siglas)

    tree_aux_1 <- tse_data %>%
      filter(NOME_MUNICIPIO == city,
             ANO_ELEICAO == year,
             DESCRICAO_CARGO == "VEREADOR",
             NOME_UF == est) %>%
      filter(DESC_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA", "ELEITO", "MÉDIA")) %>%
      mutate(N_ELEITOS = n()) %>%
      select(N_ELEITOS) %>%
      unique()

    tot <- unique(tree_aux_1$N_ELEITOS)

    rm(tree_aux_1)

    data_aux <- tse_qp %>%
      filter(NOME_MUNICIPIO == city,
             SIGLA_UF == nome_est,
             ANO_ELEICAO == year) %>%
      select(-SIGLA_PARTIDO) %>%
      unique() %>%
      mutate(N_VOTOS = QTDE_VOTOS_NOMINAIS + QTDE_VOTOS_LEGENDA,
             N_VOTOS_TOT = sum(N_VOTOS),
             N_VAGAS = tot,
             QE = round(N_VOTOS_TOT/N_VAGAS),
             QP = floor(N_VOTOS/QE)) %>%
      select(COMPOSICAO_LEGENDA, N_VOTOS, 
             N_VOTOS_TOT, N_VAGAS, QE, QP) %>% unique() %>%
      filter(QP != 0)


    data_aux$COMPOSICAO_LEGENDA <- factor(data_aux$COMPOSICAO_LEGENDA, 
                                          levels = unique(data_aux$COMPOSICAO_LEGENDA)[order(data_aux$QP, decreasing = FALSE)])

    plot_ly(data_aux, x = ~QP, 
            y = ~COMPOSICAO_LEGENDA, 
            type = 'bar', 
            orientation = 'h',
            color = ~COMPOSICAO_LEGENDA,
            hoverinfo="text", ##Tirar tool tips pq formatacao era feia
            text = ~paste0(COMPOSICAO_LEGENDA, "<br>",
                           "Número de Vagas por Quociente Partidário: ", QP, "<br>")) %>%
              layout(title = paste0("Distribuição do Quociente Partidário em ", city,"/",nome_est, " no ano de ", year),
                     xaxis = list(title = ""),
                     yaxis = list(title = "",
                                  showticklabels = FALSE),
            legend = list(orientation = 'h'),
            font = list(family = "Roboto"))
  })


  ##### Gráficos DEPUTADOS ESTADUAIS
  ### Gráfico Tab Estados

  output$grafico_dep_est_eleitos_est <- renderPlotly({
   year <- input$ano_est_dep_est
    
    est <- input$escolhe_est_dep_est_est

    tree_aux_1 <- tse_data %>%
                    filter(NOME_UF == est,
                           ANO_ELEICAO == year,
                           DESCRICAO_CARGO == "DEPUTADO ESTADUAL", 
                           DESC_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA", "ELEITO", "MÉDIA")) %>%
                    group_by(NOME_CANDIDATO, NOME_UF) %>%
          mutate(COD_MUN_IBGE = NULL, NOME_MUNICIPIO = NULL, NOME_MESO = NULL, NOME_MICRO = NULL,
                   TOTAL_VOTOS = sum(QTDE_VOTOS),
                   QTDE_VOTOS = NULL,
                   CANDI_DISPLAY = paste0(NOME_CANDIDATO, " - ", SIGLA_PARTIDO, " - ", DESC_SIT_TOT_TURNO),
                   COLIGA_DISPLAY = paste0(COMPOSICAO_LEGENDA)) %>%
          unique() %>%
          ungroup() %>%
                    arrange(desc(TOTAL_VOTOS))
    
    tree_aux_1$NOME_CANDIDATO <- factor(tree_aux_1$NOME_CANDIDATO, levels = unique(tree_aux_1$NOME_CANDIDATO)[order(tree_aux_1$TOTAL_VOTOS, decreasing = TRUE)])
    
    plot_ly(tree_aux_1,x = ~NOME_CANDIDATO, 
            y = ~TOTAL_VOTOS, 
            #sort = FALSE,
            type = 'bar', 
            color = ~SIGLA_PARTIDO,
            hoverinfo="text", ##Tirar tool tips pq formatacao era feia
            text = ~paste0(paste0(NOME_CANDIDATO, " - ", SIGLA_PARTIDO), "<br>",
                           "Total de Votos: ", TOTAL_VOTOS, "<br>",
                           DESC_SIT_TOT_TURNO)) %>%
      layout(xaxis = list(title = "",
                          showticklabels = FALSE),
             yaxis = list(title = ""),
             legend = list(orientation = 'h'),
             font = list(family = "Faustina"))
  })

  ##### Gráfico Senadores
  ### Gráfico Senadores para ESTADO

    output$grafico_senadores_cand_est <- renderPlotly({

    est <- input$escolhe_est_senador_est
    
    year <- input$ano_senador_est
    
    tree_aux_1 <- tse_data %>%
      filter(NOME_UF == est,
             ANO_ELEICAO == year,
             DESCRICAO_CARGO == "SENADOR") %>%
      select(SIGLA_PARTIDO, QTDE_VOTOS, NOME_CANDIDATO, DESC_SIT_TOT_TURNO) %>%
      group_by(NOME_CANDIDATO) %>%
      mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
      unique() %>%
      arrange(desc(QTDE_VOTOS)) 

    tree_aux_1$NOME_CANDIDATO <- factor(tree_aux_1$NOME_CANDIDATO, levels = unique(tree_aux_1$NOME_CANDIDATO)[order(tree_aux_1$QTDE_VOTOS, decreasing = TRUE)])

    plot_ly(tree_aux_1,x = ~NOME_CANDIDATO, 
            y = ~QTDE_VOTOS, 
            #sort = FALSE,
            type = 'bar', 
            color = ~SIGLA_PARTIDO,
            hoverinfo="text", ##Tirar tool tips pq formatacao era feia
            text = ~paste0(paste0(NOME_CANDIDATO, " - ", SIGLA_PARTIDO), "<br>",
                           "Total de Votos: ", QTDE_VOTOS, "<br>",
                           DESC_SIT_TOT_TURNO)) %>%
      layout(xaxis = list(title = "", showticklabels = FALSE),
             yaxis = list(title = ""),
             showlegend = FALSE,
             font = list(family = "Faustina"))
  })

    ##### GRÁFICO PARA GOVERNADORES

    ### Gráfico Governadores por Estado

    output$grafico_gov_est <- renderPlotly({

    est <- input$escolhe_est_gov_est
    
    year <- input$ano_gov_est

    if(input$gov_turno_est == 'turno_1'){
      turno <- 1
    }
    else {
      turno <- 2
    }
    
    tree_aux_1 <- tse_data %>%
      filter(NOME_UF == est,
             ANO_ELEICAO == year,
             NUM_TURNO == turno,
             DESCRICAO_CARGO == "GOVERNADOR") %>%
      select(SIGLA_PARTIDO, QTDE_VOTOS, NOME_CANDIDATO, DESC_SIT_TOT_TURNO) %>%
      group_by(NOME_CANDIDATO) %>%
      mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
      unique() %>%
      arrange(desc(QTDE_VOTOS)) 
    
    tree_aux_1$SIGLA_PARTIDO <- factor(tree_aux_1$SIGLA_PARTIDO, levels = unique(tree_aux_1$SIGLA_PARTIDO)[order(tree_aux_1$QTDE_VOTOS, decreasing = TRUE)])

    plot_ly(tree_aux_1,x = ~SIGLA_PARTIDO, 
            y = ~QTDE_VOTOS, 
            #sort = FALSE,
            type = 'bar', 
            color = ~SIGLA_PARTIDO,
            hoverinfo="text", ##Tirar tool tips pq formatacao era feia
            text = ~paste0(paste0(NOME_CANDIDATO, " - ", SIGLA_PARTIDO), "<br>",
                           "Total de Votos: ", QTDE_VOTOS, "<br>",
                           DESC_SIT_TOT_TURNO)) %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = ""),
             showlegend = FALSE,
             font = list(family = "Faustina"))
  })

  ##### Gráficos DEPUTADOS FEDERAIS
  ### Gráfico Tab Estados

  output$grafico_dep_fed_est <- renderPlotly({

   year <- input$ano_dep_fed_est
    
    est <- input$escolhe_est_dep_fed_est

    sel_est <- tse_capitais %>%
                filter(estados == est)

    tree_aux_1 <- tse_data %>%
                    filter(NOME_UF == est,
                           ANO_ELEICAO == year,
                           DESCRICAO_CARGO == "DEPUTADO FEDERAL", 
                           DESC_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA", "ELEITO", "MÉDIA")) %>%
                    group_by(NOME_CANDIDATO, NOME_UF) %>%
          mutate(COD_MUN_IBGE = NULL, NOME_MUNICIPIO = NULL, NOME_MESO = NULL, NOME_MICRO = NULL,
                   TOTAL_VOTOS = sum(QTDE_VOTOS),
                   QTDE_VOTOS = NULL,
                   CANDI_DISPLAY = paste0(NOME_CANDIDATO, " - ", SIGLA_PARTIDO, " - ", DESC_SIT_TOT_TURNO),
                   COLIGA_DISPLAY = paste0(COMPOSICAO_LEGENDA)) %>%
          unique() %>%
          ungroup() %>%
                    arrange(desc(TOTAL_VOTOS))
    
    tree_aux_1$NOME_CANDIDATO <- factor(tree_aux_1$NOME_CANDIDATO, levels = unique(tree_aux_1$NOME_CANDIDATO)[order(tree_aux_1$TOTAL_VOTOS, decreasing = TRUE)])
    
    plot_ly(tree_aux_1,x = ~NOME_CANDIDATO, 
            y = ~TOTAL_VOTOS, 
            #sort = FALSE,
            type = 'bar', 
            color = ~SIGLA_PARTIDO,
            hoverinfo="text", ##Tirar tool tips pq formatacao era feia
            text = ~paste0(paste0(NOME_CANDIDATO, " - ", SIGLA_PARTIDO), "<br>",
                           "Total de Votos: ", TOTAL_VOTOS, "<br>",
                           DESC_SIT_TOT_TURNO)) %>%
      layout(xaxis = list(title = "",
                          showticklabels = FALSE),
             yaxis = list(title = ""),
             legend = list(orientation = 'h'),
             font = list(family = "Faustina"))
  })


    ##### GRÁFICO PARA PRESIDENTES

    ### Gráfico Presidentes por Estado

    output$grafico_pres_pais <- renderPlotly({
    
    year <- input$ano_pres_pais

    if(input$pres_turno_pais == 'turno_1'){
      turno <- 1
    }
    else {
      turno <- 2
    }
    
    tree_aux_1 <- tse_data %>%
      filter(ANO_ELEICAO == year,
             NUM_TURNO == turno,
             DESCRICAO_CARGO == "PRESIDENTE") %>%
      select(SIGLA_PARTIDO, QTDE_VOTOS, NOME_CANDIDATO, DESC_SIT_TOT_TURNO) %>%
      group_by(NOME_CANDIDATO) %>%
      mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
      unique() %>%
      arrange(desc(QTDE_VOTOS)) 
    
    tree_aux_1$SIGLA_PARTIDO <- factor(tree_aux_1$SIGLA_PARTIDO, levels = unique(tree_aux_1$SIGLA_PARTIDO)[order(tree_aux_1$QTDE_VOTOS, decreasing = TRUE)])

    plot_ly(tree_aux_1,x = ~SIGLA_PARTIDO, 
            y = ~QTDE_VOTOS, 
            #sort = FALSE,
            type = 'bar', 
            color = ~SIGLA_PARTIDO,
            hoverinfo="text", ##Tirar tool tips pq formatacao era feia
            text = ~paste0(paste0(NOME_CANDIDATO, " - ", SIGLA_PARTIDO), "<br>",
                           "Total de Votos: ", QTDE_VOTOS, "<br>",
                           DESC_SIT_TOT_TURNO)) %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = ""),
             showlegend = FALSE,
             font = list(family = "Faustina"))
  })