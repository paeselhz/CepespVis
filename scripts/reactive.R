observe({
        est <- input$escolhe_est_prefeito

        mun_aux <- tse_data_ui %>%
                      select(NOME_UF, NOME_MUNICIPIO) %>%
                      filter(NOME_UF == est)

        sel_mun <- tse_capitais %>%
                      filter(estados == est)

        updateSelectInput(session, 
                          'escolhe_mun_prefeito', 
                          choices = unique(mun_aux$NOME_MUNICIPIO),
                          selected = as.character(sel_mun[,2]))
    })

   observe({
        est <- input$escolhe_est_vereador

        mun_aux <- tse_data_ui %>%
                      select(NOME_UF, NOME_MUNICIPIO) %>%
                      filter(NOME_UF == est)

        sel_mun <- tse_capitais %>%
                      filter(estados == est)

        updateSelectInput(session, 
                          'escolhe_mun_vereador', 
                          choices = unique(mun_aux$NOME_MUNICIPIO),
                          selected = as.character(sel_mun[,2]))
    })

   observe({
        est <- input$escolhe_est_dep_est

        year <- input$ano_mun_dep_est

        cand_aux <- tse_cand %>%
                          filter(NOME_UF == est,
                                 ANO_ELEICAO == year,
                                 DESCRICAO_CARGO == "DEPUTADO ESTADUAL",
                                 DESC_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA", "ELEITO", "MÉDIA")) %>%
                          arrange(NOME_CANDIDATO)

        updateSelectInput(session,
                          "escolhe_cand_dep_est",
                          choices = unique(cand_aux$NOME_CANDIDATO))
    })

    observe({
        est <- input$escolhe_est_dep_est_micro

        year <- input$ano_micro_dep_est

        cand_aux <- tse_cand %>%
                          filter(NOME_UF == est,
                                 ANO_ELEICAO == year,
                                 DESCRICAO_CARGO == "DEPUTADO ESTADUAL",
                                 DESC_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA", "ELEITO", "MÉDIA")) %>%
                          arrange(NOME_CANDIDATO)

        updateSelectInput(session,
                          "escolhe_cand_dep_est_micro",
                          choices = unique(cand_aux$NOME_CANDIDATO))
    })

    observe({
        est <- input$escolhe_est_dep_est_meso

        year <- input$ano_meso_dep_est

        cand_aux <- tse_cand %>%
                          filter(NOME_UF == est,
                                 ANO_ELEICAO == year,
                                 DESCRICAO_CARGO == "DEPUTADO ESTADUAL",
                                 DESC_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA", "ELEITO", "MÉDIA")) %>%
                          arrange(NOME_CANDIDATO)

        updateSelectInput(session, 
                          'escolhe_cand_dep_est_meso', 
                          choices = unique(cand_aux$NOME_CANDIDATO))
    })

    observe({
        est <- input$escolhe_est_senador_mun

        year <- input$ano_mun_senador

        cand_aux <- tse_cand %>%
                          filter(NOME_UF == est,
                                 ANO_ELEICAO == year,
                                 DESCRICAO_CARGO == "SENADOR") %>%
                          arrange(NOME_CANDIDATO)

        updateSelectInput(session, 
                          'escolhe_senador_mun',
                          choices = unique(cand_aux$NOME_CANDIDATO))
    })

    observe({
        est <- input$escolhe_est_senador_micro

        year <- input$ano_micro_senador

        cand_aux <- tse_cand %>%
                          filter(NOME_UF == est,
                                 ANO_ELEICAO == year,
                                 DESCRICAO_CARGO == "SENADOR") %>%
                          arrange(NOME_CANDIDATO)

        updateSelectInput(session, 
                          'escolhe_senador_micro',
                          choices = unique(cand_aux$NOME_CANDIDATO))
    })

    observe({
        est <- input$escolhe_est_senador_meso

        year <- input$ano_meso_senador

        cand_aux <- tse_cand %>%
                          filter(NOME_UF == est,
                                 ANO_ELEICAO == year,
                                 DESCRICAO_CARGO == "SENADOR") %>%
                          arrange(NOME_CANDIDATO)

        updateSelectInput(session, 
                          'escolhe_senador_meso',
                          choices = unique(cand_aux$NOME_CANDIDATO))
    })

    observe({
          est <- input$escolhe_est_gov_mun

          year <- input$ano_mun_gov

          if(input$gov_turno_mun == 'turno_1'){
              turno <- 1
            }
            else {
              turno <- 2
            }

        cand_aux <- tse_cand %>%
                          filter(NOME_UF == est,
                                 ANO_ELEICAO == year,
                                 NUM_TURNO == turno,
                                 DESCRICAO_CARGO == "GOVERNADOR") %>%
                          arrange(NOME_CANDIDATO)

        updateSelectInput(session, 
                          'escolhe_gov_mun',
                          choices = unique(cand_aux$NOME_CANDIDATO))
    })

    observe({
          est <- input$escolhe_est_gov_micro

          year <- input$ano_micro_gov

          if(input$gov_turno_micro == 'turno_1'){
              turno <- 1
            }
            else {
              turno <- 2
            }

        cand_aux <- tse_cand %>%
                          filter(NOME_UF == est,
                                 ANO_ELEICAO == year,
                                 NUM_TURNO == turno,
                                 DESCRICAO_CARGO == "GOVERNADOR") %>%
                          arrange(NOME_CANDIDATO)

        updateSelectInput(session, 
                          'escolhe_gov_micro',
                          choices = unique(cand_aux$NOME_CANDIDATO))
    })

    observe({
          est <- input$escolhe_est_gov_meso

          year <- input$ano_meso_gov

          if(input$gov_turno_meso == 'turno_1'){
              turno <- 1
            }
            else {
              turno <- 2
            }

        cand_aux <- tse_cand %>%
                          filter(NOME_UF == est,
                                 ANO_ELEICAO == year,
                                 NUM_TURNO == turno,
                                 DESCRICAO_CARGO == "GOVERNADOR") %>%
                          arrange(NOME_CANDIDATO)

        updateSelectInput(session, 
                          'escolhe_gov_meso',
                          choices = unique(cand_aux$NOME_CANDIDATO))
    })

   observe({
        est <- input$escolhe_est_dep_fed

        year <- input$ano_mun_dep_fed

        cand_aux <- tse_cand %>%
                          filter(NOME_UF == est,
                                 ANO_ELEICAO == year,
                                 DESCRICAO_CARGO == "DEPUTADO FEDERAL",
                                 DESC_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA", "ELEITO", "MÉDIA")) %>%
                          arrange(NOME_CANDIDATO)

        updateSelectInput(session,
                          "escolhe_cand_dep_fed",
                          choices = unique(cand_aux$NOME_CANDIDATO))
    })

    observe({
        est <- input$escolhe_est_dep_fed_micro

        year <- input$ano_micro_dep_fed

        cand_aux <- tse_cand %>%
                          filter(NOME_UF == est,
                                 ANO_ELEICAO == year,
                                 DESCRICAO_CARGO == "DEPUTADO FEDERAL",
                                 DESC_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA", "ELEITO", "MÉDIA")) %>%
                          arrange(NOME_CANDIDATO)

        updateSelectInput(session,
                          "escolhe_cand_dep_fed_micro",
                          choices = unique(cand_aux$NOME_CANDIDATO))
    })

    observe({
        est <- input$escolhe_est_dep_fed_meso

        year <- input$ano_meso_dep_fed

        cand_aux <- tse_cand %>%
                          filter(NOME_UF == est,
                                 ANO_ELEICAO == year,
                                 DESCRICAO_CARGO == "DEPUTADO FEDERAL",
                                 DESC_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA", "ELEITO", "MÉDIA")) %>%
                          arrange(NOME_CANDIDATO)

        updateSelectInput(session, 
                          'escolhe_cand_dep_fed_meso', 
                          choices = unique(cand_aux$NOME_CANDIDATO))
    })

    observe({
          year <- input$ano_pres_est

          if(input$pres_turno_est == 'turno_1'){
              turno <- 1
            }
            else {
              turno <- 2
            }

        cand_aux <- tse_cand %>%
                          filter(ANO_ELEICAO == year,
                                 NUM_TURNO == turno,
                                 DESCRICAO_CARGO == "PRESIDENTE") %>%
                          arrange(NOME_CANDIDATO)

        updateSelectInput(session, 
                          'escolhe_pres_est',
                          choices = unique(cand_aux$NOME_CANDIDATO))
    })

    observe({
          est <- input$escolhe_est_pres_mun

          year <- input$ano_mun_pres

          if(input$pres_turno_mun == 'turno_1'){
              turno <- 1
            }
            else {
              turno <- 2
            }

        cand_aux <- tse_cand %>%
                          filter(NOME_UF == est,
                                 ANO_ELEICAO == year,
                                 NUM_TURNO == turno,
                                 DESCRICAO_CARGO == "PRESIDENTE") %>%
                          arrange(NOME_CANDIDATO)

        updateSelectInput(session, 
                          'escolhe_pres_mun',
                          choices = unique(cand_aux$NOME_CANDIDATO))
    })

    observe({
          est <- input$escolhe_est_pres_micro

          year <- input$ano_micro_pres

          if(input$pres_turno_micro == 'turno_1'){
              turno <- 1
            }
            else {
              turno <- 2
            }

        cand_aux <- tse_cand %>%
                          filter(NOME_UF == est,
                                 ANO_ELEICAO == year,
                                 NUM_TURNO == turno,
                                 DESCRICAO_CARGO == "PRESIDENTE") %>%
                          arrange(NOME_CANDIDATO)

        updateSelectInput(session, 
                          'escolhe_pres_micro',
                          choices = unique(cand_aux$NOME_CANDIDATO))
    })

    observe({
          est <- input$escolhe_est_pres_meso

          year <- input$ano_meso_pres

          if(input$pres_turno_meso == 'turno_1'){
              turno <- 1
            }
            else {
              turno <- 2
            }

        cand_aux <- tse_cand %>%
                          filter(NOME_UF == est,
                                 ANO_ELEICAO == year,
                                 NUM_TURNO == turno,
                                 DESCRICAO_CARGO == "PRESIDENTE") %>%
                          arrange(NOME_CANDIDATO)

        updateSelectInput(session, 
                          'escolhe_pres_meso',
                          choices = unique(cand_aux$NOME_CANDIDATO))
    })

    observe({
        year <- input$ano_markov_pais

        cand_aux <- tse_cand %>%
                          filter(ANO_ELEICAO == year,
                                 DESCRICAO_CARGO == "PRESIDENTE",
                                 DESC_SIT_TOT_TURNO == "2º TURNO") %>%
                          arrange(NOME_CANDIDATO)

        updateSelectInput(session, 
                          'markov_escolhe_presidente',
                          choices = unique(cand_aux$NOME_CANDIDATO))
    })