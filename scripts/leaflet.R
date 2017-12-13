output$titulo_mapa_dep_est_mun <- renderText({
  
  est <- input$escolhe_est_dep_est

    year <- input$ano_mun_dep_est

    cand <- input$escolhe_cand_dep_est

    data_aux_titulo <- tse_data %>%
              filter(NOME_UF == est,
                     ANO_ELEICAO == year,
                     DESCRICAO_CARGO == "DEPUTADO ESTADUAL",
                     NOME_CANDIDATO == cand) %>%
              mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
              select(ANO_ELEICAO, NOME_CANDIDATO, SIGLA_PARTIDO, QTDE_VOTOS, DESC_SIT_TOT_TURNO) %>%
              unique()                
  
  paste0(data_aux_titulo$NOME_CANDIDATO, " - ", data_aux_titulo$SIGLA_PARTIDO, " - ", data_aux_titulo$DESC_SIT_TOT_TURNO ,": ",
       data_aux_titulo$QTDE_VOTOS, " votos em ", data_aux_titulo$ANO_ELEICAO)
  
  })

output$mapa_dep_est_mun <- renderLeaflet({
    
    est <- input$escolhe_est_dep_est

    year <- input$ano_mun_dep_est

    cand <- input$escolhe_cand_dep_est

    aux_cap <- tse_capitais %>%
                filter(estados == est)

    aux_map <- mapa_BR %>% subset(ESTADO == aux_cap$id_uf)

    if(input$dep_est_map_choice_mun == 'orig_vot'){
      data_aux <- tse_data %>%
        filter(NOME_UF == est,
               ANO_ELEICAO == year,
               DESCRICAO_CARGO == "DEPUTADO ESTADUAL") %>%
        group_by(NOME_MUNICIPIO) %>%
        mutate(QTDE_EST_TOT = sum(QTDE_VOTOS)) %>%
        arrange(QTDE_EST_TOT) %>%
        ungroup() %>%
        filter(NOME_CANDIDATO == cand) %>%
        unique() %>%
        mutate(QTDE_VOT_CAND = sum(QTDE_VOTOS),
               PERC_VOT = (QTDE_VOTOS/QTDE_VOT_CAND)*100)
    } else {
      data_aux <- tse_data %>%
        filter(NOME_UF == est,
               ANO_ELEICAO == year,
               DESCRICAO_CARGO == "DEPUTADO ESTADUAL") %>%
        group_by(NOME_MUNICIPIO) %>%
        mutate(QTDE_EST_TOT = sum(QTDE_VOTOS)) %>%
        arrange(QTDE_EST_TOT) %>%
        ungroup() %>%
        filter(NOME_CANDIDATO == cand) %>%
        unique() %>%
        mutate(PERC_VOT = (QTDE_VOTOS/QTDE_EST_TOT)*100,
               QTDE_VOT_CAND = sum(QTDE_VOTOS))
    }

    aux_map_2 <- sp::merge(x = aux_map, y = data_aux, by.x = "GEOCODIGO", by.y = "COD_MUN_IBGE")

    aux_map_2$QTDE_VOTOS[is.na(aux_map_2$QTDE_VOTOS)] <- 0
    aux_map_2$QTDE_EST_TOT[is.na(aux_map_2$QTDE_EST_TOT)] <- max(aux_map_2$QTDE_EST_TOT)
    aux_map_2$PERC_VOT[is.na(aux_map_2$PERC_VOT)] <- 0

    classes <- c(0, 0.1, 0.25, 0.5, 1, 2, 5, ceiling(max(aux_map_2$PERC_VOT)))
    pal_cor <- colorBin(c("#6A1103", "#BA0004", "#E7400B", "#FEA527", "#FDFE65", "#9CE400", "#359800"), domain = aux_map_2$PERC_VOT, bins = unique(classes))


    leaflet(data = aux_map_2) %>% 
      addTiles('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
      addPolygons(weight = 0.5, fillColor = ~pal_cor(PERC_VOT), # Weight e a grossura das bordas
                  color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                  smoothFactor = 0.25,
                  popup = paste0(aux_map_2$NOME_MUNICIPIO.x, "<br>",
                                 "Percentual de Votos: ", round(aux_map_2$PERC_VOT, 3), "%", "<br>",
                                 "Total de Votos para o candidato em ", aux_map_2$NOME_MUNICIPIO.x, ": ", aux_map_2$QTDE_VOTOS, "<br>",
                                 "Total de Votos em ", aux_map_2$NOME_MUNICIPIO.x, ": ", aux_map_2$QTDE_EST_TOT, "<br>",
                                 "Total de Votos para o candidato no Estado: ", aux_map_2$QTDE_VOT_CAND),
                  label = ~NOME_MUNICIPIO.x) %>% 
      addLegend(position = "bottomright", pal = pal_cor, values = ~PERC_VOT,
                labFormat = labelFormat(suffix = "%"))

  })

output$titulo_mapa_dep_est_micro <- renderText({
  
    est <- input$escolhe_est_dep_est_micro

    year <- input$ano_micro_dep_est

    cand <- input$escolhe_cand_dep_est_micro

    data_aux_titulo <- tse_data %>%
              filter(NOME_UF == est,
                     ANO_ELEICAO == year,
                     DESCRICAO_CARGO == "DEPUTADO ESTADUAL",
                     NOME_CANDIDATO == cand) %>%
              mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
              select(ANO_ELEICAO, NOME_CANDIDATO, SIGLA_PARTIDO, QTDE_VOTOS, DESC_SIT_TOT_TURNO) %>%
              unique()                
  
  paste0(data_aux_titulo$NOME_CANDIDATO, " - ", data_aux_titulo$SIGLA_PARTIDO, " - ", data_aux_titulo$DESC_SIT_TOT_TURNO ,": ",
       data_aux_titulo$QTDE_VOTOS, " votos em ", data_aux_titulo$ANO_ELEICAO)
  
  })

output$mapa_dep_est_micro <- renderLeaflet({
    
    est <- input$escolhe_est_dep_est_micro

    year <- input$ano_micro_dep_est

    cand <- input$escolhe_cand_dep_est_micro

    aux_cap <- tse_capitais %>%
                filter(estados == est)

    aux_map <- mapa_micro %>% subset(ESTADO == aux_cap$id_uf)

    if(input$dep_est_map_choice_micro == 'orig_vot'){
      data_aux <- tse_data %>%
        filter(NOME_UF == est,
               ANO_ELEICAO == year,
               DESCRICAO_CARGO == "DEPUTADO ESTADUAL") %>%
        group_by(NOME_MICRO) %>%
        mutate(QTDE_EST_TOT = sum(QTDE_VOTOS),
               NOME_MUNICIPIO = NULL,
               COD_MUN_IBGE = NULL) %>%
        arrange(QTDE_EST_TOT) %>%
        ungroup() %>%
        filter(NOME_CANDIDATO == cand) %>%
        group_by(NOME_MICRO) %>%
        mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
        unique() %>% ungroup() %>%
        mutate(QTDE_VOT_CAND = sum(QTDE_VOTOS),
               PERC_VOT = (QTDE_VOTOS/QTDE_VOT_CAND)*100)
    } else {
      data_aux <- tse_data %>%
        filter(NOME_UF == est,
               ANO_ELEICAO == year,
               DESCRICAO_CARGO == "DEPUTADO ESTADUAL") %>%
        group_by(NOME_MICRO) %>%
        mutate(QTDE_EST_TOT = sum(QTDE_VOTOS),
               NOME_MUNICIPIO = NULL,
               COD_MUN_IBGE = NULL) %>%
        arrange(QTDE_EST_TOT) %>%
        ungroup() %>%
        filter(NOME_CANDIDATO == cand) %>%
        group_by(NOME_MICRO) %>%
        mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
        unique() %>% ungroup() %>%
        mutate(PERC_VOT = (QTDE_VOTOS/QTDE_EST_TOT)*100,
               QTDE_VOT_CAND = sum(QTDE_VOTOS))
               
    }


    #aux_map_2 <- merge(x = aux_map, y = data_aux, by.x = "NOME_MESO", by.y = "NOME_MESO")

    aux_map_2 <- sp::merge(aux_map, data_aux, by = "NOME_MICRO", duplicateGeoms = T)

    aux_map_2$QTDE_VOTOS[is.na(aux_map_2$QTDE_VOTOS)] <- 0
    aux_map_2$QTDE_EST_TOT[is.na(aux_map_2$QTDE_EST_TOT)] <- max(aux_map_2$QTDE_EST_TOT)
    aux_map_2$PERC_VOT[is.na(aux_map_2$PERC_VOT)] <- 0

    classes <- c(0, 0.1, 0.25, 0.5, 1, 2, 5, ceiling(max(aux_map_2$PERC_VOT)))
    pal_cor <- colorBin(c("#6A1103", "#BA0004", "#E7400B", "#FEA527", "#FDFE65", "#9CE400", "#359800"), domain = aux_map_2$PERC_VOT, bins = unique(classes))


    leaflet(data = aux_map_2) %>% 
      addTiles('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
      addPolygons(weight = 0.5, fillColor = ~pal_cor(PERC_VOT), # Weight e a grossura das bordas
                  color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                  smoothFactor = 0.25,
                  popup = paste0(aux_map_2$NOME_MICRO, "<br>",
                                 "Percentual de Votos: ", round(aux_map_2$PERC_VOT, 3), "%", "<br>",
                                 "Total de Votos para o candidato em ", aux_map_2$NOME_MICRO, ": ", aux_map_2$QTDE_VOTOS, "<br>",
                                 "Total de Votos em ", aux_map_2$NOME_MICRO, ": ", aux_map_2$QTDE_EST_TOT, "<br>",
                                 "Total de Votos para o candidato no Estado: ", aux_map_2$QTDE_VOT_CAND),
                  label = ~NOME_MICRO) %>% 
      addLegend(position = "bottomright", pal = pal_cor, values = ~PERC_VOT,
                labFormat = labelFormat(suffix = "%"))

  })

output$titulo_mapa_dep_est_meso <- renderText({
  
    est <- input$escolhe_est_dep_est_meso

    year <- input$ano_meso_dep_est

    cand <- input$escolhe_cand_dep_est_meso

    data_aux_titulo <- tse_data %>%
              filter(NOME_UF == est,
                     ANO_ELEICAO == year,
                     DESCRICAO_CARGO == "DEPUTADO ESTADUAL",
                     NOME_CANDIDATO == cand) %>%
              mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
              select(ANO_ELEICAO, NOME_CANDIDATO, SIGLA_PARTIDO, QTDE_VOTOS, DESC_SIT_TOT_TURNO) %>%
              unique()                
  
  paste0(data_aux_titulo$NOME_CANDIDATO, " - ", data_aux_titulo$SIGLA_PARTIDO, " - ", data_aux_titulo$DESC_SIT_TOT_TURNO ,": ",
       data_aux_titulo$QTDE_VOTOS, " votos em ", data_aux_titulo$ANO_ELEICAO)
  
  })

output$mapa_dep_est_meso <- renderLeaflet({
    
    est <- input$escolhe_est_dep_est_meso

    year <- input$ano_meso_dep_est

    cand <- input$escolhe_cand_dep_est_meso

    aux_cap <- tse_capitais %>%
                filter(estados == est)

    aux_map <- mapa_meso %>% subset(ESTADO == aux_cap$id_uf)

    if(input$dep_est_map_choice_meso == 'orig_vot'){
      data_aux <- tse_data %>%
        filter(NOME_UF == est,
               ANO_ELEICAO == year,
               DESCRICAO_CARGO == "DEPUTADO ESTADUAL") %>%
        group_by(NOME_MESO) %>%
        mutate(QTDE_EST_TOT = sum(QTDE_VOTOS),
               NOME_MUNICIPIO = NULL,
               COD_MUN_IBGE = NULL,
               NOME_MICRO = NULL) %>%
        arrange(QTDE_EST_TOT) %>%
        ungroup() %>%
        filter(NOME_CANDIDATO == cand) %>%
        group_by(NOME_MESO) %>%
        mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
        unique() %>% ungroup() %>%
        mutate(QTDE_VOT_CAND = sum(QTDE_VOTOS),
               PERC_VOT = (QTDE_VOTOS/QTDE_VOT_CAND)*100)
    } else {
      data_aux <- tse_data %>%
        filter(NOME_UF == est,
               ANO_ELEICAO == year,
               DESCRICAO_CARGO == "DEPUTADO ESTADUAL") %>%
        group_by(NOME_MESO) %>%
        mutate(QTDE_EST_TOT = sum(QTDE_VOTOS),
               NOME_MUNICIPIO = NULL,
               COD_MUN_IBGE = NULL,
               NOME_MICRO = NULL) %>%
        arrange(QTDE_EST_TOT) %>%
        ungroup() %>%
        filter(NOME_CANDIDATO == cand) %>%
        group_by(NOME_MESO) %>%
        mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
        unique() %>% ungroup() %>%
        mutate(PERC_VOT = (QTDE_VOTOS/QTDE_EST_TOT)*100,
               QTDE_VOT_CAND = sum(QTDE_VOTOS))
               
    }


    #aux_map_2 <- merge(x = aux_map, y = data_aux, by.x = "NOME_MESO", by.y = "NOME_MESO")

    aux_map_2 <- sp::merge(aux_map, data_aux, by = "NOME_MESO", duplicateGeoms = T)

    aux_map_2$QTDE_VOTOS[is.na(aux_map_2$QTDE_VOTOS)] <- 0
    aux_map_2$QTDE_EST_TOT[is.na(aux_map_2$QTDE_EST_TOT)] <- max(aux_map_2$QTDE_EST_TOT)
    aux_map_2$PERC_VOT[is.na(aux_map_2$PERC_VOT)] <- 0

    classes <- c(0, 0.1, 0.25, 0.5, 1, 2, 5, ceiling(max(aux_map_2$PERC_VOT)))
    pal_cor <- colorBin(c("#6A1103", "#BA0004", "#E7400B", "#FEA527", "#FDFE65", "#9CE400", "#359800"), domain = aux_map_2$PERC_VOT, bins = unique(classes))


    leaflet(data = aux_map_2) %>% 
      addTiles('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
      addPolygons(weight = 0.5, fillColor = ~pal_cor(PERC_VOT), # Weight e a grossura das bordas
                  color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                  smoothFactor = 0.25,
                  popup = paste0(aux_map_2$NOME_MESO, "<br>",
                                 "Percentual de Votos: ", round(aux_map_2$PERC_VOT, 3), "%", "<br>",
                                 "Total de Votos para o candidato em ", aux_map_2$NOME_MESO, ": ", aux_map_2$QTDE_VOTOS, "<br>",
                                 "Total de Votos em ", aux_map_2$NOME_MESO, ": ", aux_map_2$QTDE_EST_TOT, "<br>",
                                 "Total de Votos para o candidato no Estado: ", aux_map_2$QTDE_VOT_CAND),
                  label = ~NOME_MESO) %>% 
      addLegend(position = "bottomright", pal = pal_cor, values = ~PERC_VOT,
                labFormat = labelFormat(suffix = "%"))

  })


output$titulo_mapa_senadores_mun <- renderText({
  
    est <- input$escolhe_est_senador_mun

    year <- input$ano_mun_senador

    cand <- input$escolhe_senador_mun

    data_aux_titulo <- tse_data %>%
              filter(NOME_UF == est,
                     ANO_ELEICAO == year,
                     DESCRICAO_CARGO == "SENADOR",
                     NOME_CANDIDATO == cand) %>%
              mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
              select(ANO_ELEICAO, NOME_CANDIDATO, SIGLA_PARTIDO, QTDE_VOTOS, DESC_SIT_TOT_TURNO) %>%
              unique()                
  
  paste0(data_aux_titulo$NOME_CANDIDATO, " - ", data_aux_titulo$SIGLA_PARTIDO, " - ", data_aux_titulo$DESC_SIT_TOT_TURNO ,": ",
       data_aux_titulo$QTDE_VOTOS, " votos em ", data_aux_titulo$ANO_ELEICAO)
  
  })

output$mapa_senadores_mun <- renderLeaflet({
    
    est <- input$escolhe_est_senador_mun

    year <- input$ano_mun_senador

    cand <- input$escolhe_senador_mun

    aux_cap <- tse_capitais %>%
                filter(estados == est)

    aux_map <- mapa_BR %>% subset(ESTADO == aux_cap$id_uf)

    if(input$senador_choice_mun == 'orig_vot'){
      data_aux <- tse_data %>%
        filter(NOME_UF == est,
               ANO_ELEICAO == year,
               DESCRICAO_CARGO == "SENADOR") %>%
        group_by(NOME_MUNICIPIO) %>%
        mutate(QTDE_EST_TOT = sum(QTDE_VOTOS)) %>%
        arrange(QTDE_EST_TOT) %>%
        ungroup() %>%
        filter(NOME_CANDIDATO == cand) %>%
        unique() %>%
        mutate(QTDE_VOT_CAND = sum(QTDE_VOTOS),
               PERC_VOT = (QTDE_VOTOS/QTDE_VOT_CAND)*100)

        classes <- c(0, 0.1, 0.25, 0.5, 1, 2, 5, ceiling(max(data_aux$PERC_VOT)))
    } else {
      data_aux <- tse_data %>%
        filter(NOME_UF == est,
               ANO_ELEICAO == year,
               DESCRICAO_CARGO == "SENADOR") %>%
        group_by(NOME_MUNICIPIO) %>%
        mutate(QTDE_EST_TOT = sum(QTDE_VOTOS)) %>%
        arrange(QTDE_EST_TOT) %>%
        ungroup() %>%
        filter(NOME_CANDIDATO == cand) %>%
        unique() %>%
        mutate(PERC_VOT = (QTDE_VOTOS/QTDE_EST_TOT)*100,
               QTDE_VOT_CAND = sum(QTDE_VOTOS))

      classes <- c(0, 1, 2.5, 5, 10, 25, 50, ceiling(max(data_aux$PERC_VOT)))
    }


    aux_map_2 <- sp::merge(x = aux_map, y = data_aux, by.x = "GEOCODIGO", by.y = "COD_MUN_IBGE")

    aux_map_2$QTDE_VOTOS[is.na(aux_map_2$QTDE_VOTOS)] <- 0
    aux_map_2$QTDE_EST_TOT[is.na(aux_map_2$QTDE_EST_TOT)] <- max(aux_map_2$QTDE_EST_TOT)
    aux_map_2$PERC_VOT[is.na(aux_map_2$PERC_VOT)] <- 0

    #classes <- c(0, 0.1, 0.25, 0.5, 1, 2, 5, ceiling(max(aux_map_2$PERC_VOT)))
    pal_cor <- colorBin(c("#6A1103", "#BA0004", "#E7400B", "#FEA527", "#FDFE65", "#9CE400", "#359800"), domain = aux_map_2$PERC_VOT, bins = unique(classes))

    leaflet(data = aux_map_2) %>% 
      addTiles('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
      addPolygons(weight = 0.5, fillColor = ~pal_cor(PERC_VOT), # Weight e a grossura das bordas
                  color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                  smoothFactor = 0.25,
                  popup = paste0(aux_map_2$NOME_MUNICIPIO.x, "<br>",
                                 "Percentual de Votos: ", round(aux_map_2$PERC_VOT, 3), "%", "<br>",
                                 "Total de Votos para o candidato em ", aux_map_2$NOME_MUNICIPIO.x, ": ", aux_map_2$QTDE_VOTOS, "<br>",
                                 "Total de Votos em ", aux_map_2$NOME_MUNICIPIO.x, ": ", aux_map_2$QTDE_EST_TOT, "<br>",
                                 "Total de Votos para o candidato no Estado: ", aux_map_2$QTDE_VOT_CAND),
                  label = ~NOME_MUNICIPIO.x) %>% 
      addLegend(position = "bottomright", pal = pal_cor, values = ~PERC_VOT,
                labFormat = labelFormat(suffix = "%"))

  })

output$titulo_mapa_senadores_micro <- renderText({
  
    est <- input$escolhe_est_senador_micro

    year <- input$ano_micro_senador

    cand <- input$escolhe_senador_micro

    data_aux_titulo <- tse_data %>%
              filter(NOME_UF == est,
                     ANO_ELEICAO == year,
                     DESCRICAO_CARGO == "SENADOR",
                     NOME_CANDIDATO == cand) %>%
              mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
              select(ANO_ELEICAO, NOME_CANDIDATO, SIGLA_PARTIDO, QTDE_VOTOS, DESC_SIT_TOT_TURNO) %>%
              unique()                
  
  paste0(data_aux_titulo$NOME_CANDIDATO, " - ", data_aux_titulo$SIGLA_PARTIDO, " - ", data_aux_titulo$DESC_SIT_TOT_TURNO ,": ",
       data_aux_titulo$QTDE_VOTOS, " votos em ", data_aux_titulo$ANO_ELEICAO)
  
  })

output$mapa_senadores_micro <- renderLeaflet({
    
    est <- input$escolhe_est_senador_micro

    year <- input$ano_micro_senador

    cand <- input$escolhe_senador_micro

    aux_cap <- tse_capitais %>%
                filter(estados == est)

    aux_map <- mapa_micro %>% subset(ESTADO == aux_cap$id_uf)

    if(input$senador_choice_micro == 'orig_vot'){
      data_aux <- tse_data %>%
        filter(NOME_UF == est,
               ANO_ELEICAO == year,
               DESCRICAO_CARGO == "SENADOR") %>%
        group_by(NOME_MICRO) %>%
        mutate(QTDE_EST_TOT = sum(QTDE_VOTOS),
               NOME_MUNICIPIO = NULL,
               COD_MUN_IBGE = NULL) %>%
        arrange(QTDE_EST_TOT) %>%
        ungroup() %>%
        filter(NOME_CANDIDATO == cand) %>%
        group_by(NOME_MICRO) %>%
        mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
        unique() %>% ungroup() %>%
        mutate(QTDE_VOT_CAND = sum(QTDE_VOTOS),
               PERC_VOT = (QTDE_VOTOS/QTDE_VOT_CAND)*100)

        classes <- c(0, 0.1, 0.25, 0.5, 1, 2, 5, ceiling(max(data_aux$PERC_VOT)))
    } else {
      data_aux <- tse_data %>%
        filter(NOME_UF == est,
               ANO_ELEICAO == year,
               DESCRICAO_CARGO == "SENADOR") %>%
        group_by(NOME_MICRO) %>%
        mutate(QTDE_EST_TOT = sum(QTDE_VOTOS),
               NOME_MUNICIPIO = NULL,
               COD_MUN_IBGE = NULL) %>%
        arrange(QTDE_EST_TOT) %>%
        ungroup() %>%
        filter(NOME_CANDIDATO == cand) %>%
        group_by(NOME_MICRO) %>%
        mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
        unique() %>% ungroup() %>%
        mutate(PERC_VOT = (QTDE_VOTOS/QTDE_EST_TOT)*100,
               QTDE_VOT_CAND = sum(QTDE_VOTOS))

        classes <- c(0, 5, 10, 20, 30, 35, 40, ceiling(max(data_aux$PERC_VOT)))
               
    }


    #aux_map_2 <- merge(x = aux_map, y = data_aux, by.x = "NOME_MESO", by.y = "NOME_MESO")

    aux_map_2 <- sp::merge(aux_map, data_aux, by = "NOME_MICRO", duplicateGeoms = T)

    aux_map_2$QTDE_VOTOS[is.na(aux_map_2$QTDE_VOTOS)] <- 0
    aux_map_2$QTDE_EST_TOT[is.na(aux_map_2$QTDE_EST_TOT)] <- max(aux_map_2$QTDE_EST_TOT)
    aux_map_2$PERC_VOT[is.na(aux_map_2$PERC_VOT)] <- 0

    #classes <- c(0, 0.1, 0.25, 0.5, 1, 2, 5, ceiling(max(aux_map_2$PERC_VOT)))
    pal_cor <- colorBin(c("#6A1103", "#BA0004", "#E7400B", "#FEA527", "#FDFE65", "#9CE400", "#359800"), domain = aux_map_2$PERC_VOT, bins = unique(classes))


    leaflet(data = aux_map_2) %>% 
      addTiles('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
      addPolygons(weight = 0.5, fillColor = ~pal_cor(PERC_VOT), # Weight e a grossura das bordas
                  color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                  smoothFactor = 0.25,
                  popup = paste0(aux_map_2$NOME_MICRO, "<br>",
                                 "Percentual de Votos: ", round(aux_map_2$PERC_VOT, 3), "%", "<br>",
                                 "Total de Votos para o candidato em ", aux_map_2$NOME_MICRO, ": ", aux_map_2$QTDE_VOTOS, "<br>",
                                 "Total de Votos em ", aux_map_2$NOME_MICRO, ": ", aux_map_2$QTDE_EST_TOT, "<br>",
                                 "Total de Votos para o candidato no Estado: ", aux_map_2$QTDE_VOT_CAND),
                  label = ~NOME_MICRO) %>% 
      addLegend(position = "bottomright", pal = pal_cor, values = ~PERC_VOT,
                labFormat = labelFormat(suffix = "%"))

  })

output$titulo_mapa_senadores_meso <- renderText({
  
    est <- input$escolhe_est_senador_meso

    year <- input$ano_meso_senador

    cand <- input$escolhe_senador_meso

    data_aux_titulo <- tse_data %>%
              filter(NOME_UF == est,
                     ANO_ELEICAO == year,
                     DESCRICAO_CARGO == "SENADOR",
                     NOME_CANDIDATO == cand) %>%
              mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
              select(ANO_ELEICAO, NOME_CANDIDATO, SIGLA_PARTIDO, QTDE_VOTOS, DESC_SIT_TOT_TURNO) %>%
              unique()                
  
  paste0(data_aux_titulo$NOME_CANDIDATO, " - ", data_aux_titulo$SIGLA_PARTIDO, " - ", data_aux_titulo$DESC_SIT_TOT_TURNO ,": ",
       data_aux_titulo$QTDE_VOTOS, " votos em ", data_aux_titulo$ANO_ELEICAO)
  
  })

output$mapa_senadores_meso <- renderLeaflet({
    
    est <- input$escolhe_est_senador_meso

    year <- input$ano_meso_senador

    cand <- input$escolhe_senador_meso

    aux_cap <- tse_capitais %>%
                filter(estados == est)

    aux_map <- mapa_meso %>% subset(ESTADO == aux_cap$id_uf)


    if(input$senador_choice_meso == 'orig_vot'){
      data_aux <- tse_data %>%
        filter(NOME_UF == est,
               ANO_ELEICAO == year,
               DESCRICAO_CARGO == "SENADOR") %>%
        group_by(NOME_MESO) %>%
        mutate(QTDE_EST_TOT = sum(QTDE_VOTOS),
               NOME_MUNICIPIO = NULL,
               COD_MUN_IBGE = NULL,
               NOME_MICRO = NULL) %>%
        arrange(QTDE_EST_TOT) %>%
        ungroup() %>%
        filter(NOME_CANDIDATO == cand) %>%
        group_by(NOME_MESO) %>%
        mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
        unique() %>% ungroup() %>%
        mutate(QTDE_VOT_CAND = sum(QTDE_VOTOS),
               PERC_VOT = (QTDE_VOTOS/QTDE_VOT_CAND)*100)

        classes <- c(0, 0.1, 0.25, 0.5, 1, 2, 5, ceiling(max(data_aux$PERC_VOT)))
    } else {
      data_aux <- tse_data %>%
        filter(NOME_UF == est,
               ANO_ELEICAO == year,
               DESCRICAO_CARGO == "SENADOR") %>%
        group_by(NOME_MESO) %>%
        mutate(QTDE_EST_TOT = sum(QTDE_VOTOS),
               NOME_MUNICIPIO = NULL,
               COD_MUN_IBGE = NULL,
               NOME_MICRO = NULL) %>%
        arrange(QTDE_EST_TOT) %>%
        ungroup() %>%
        filter(NOME_CANDIDATO == cand) %>%
        group_by(NOME_MESO) %>%
        mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
        unique() %>% ungroup() %>%
        mutate(PERC_VOT = (QTDE_VOTOS/QTDE_EST_TOT)*100,
               QTDE_VOT_CAND = sum(QTDE_VOTOS))

        classes <- c(0, 5, 10, 20, 30, 35, 40, ceiling(max(data_aux$PERC_VOT)))
               
    }


    #aux_map_2 <- merge(x = aux_map, y = data_aux, by.x = "NOME_MESO", by.y = "NOME_MESO")

    aux_map_2 <- sp::merge(aux_map, data_aux, by = "NOME_MESO", duplicateGeoms = T)

    aux_map_2$QTDE_VOTOS[is.na(aux_map_2$QTDE_VOTOS)] <- 0
    aux_map_2$QTDE_EST_TOT[is.na(aux_map_2$QTDE_EST_TOT)] <- max(aux_map_2$QTDE_EST_TOT)
    aux_map_2$PERC_VOT[is.na(aux_map_2$PERC_VOT)] <- 0

    #classes <- c(0, 0.1, 0.25, 0.5, 1, 2, 5, ceiling(max(aux_map_2$PERC_VOT)))
    pal_cor <- colorBin(c("#6A1103", "#BA0004", "#E7400B", "#FEA527", "#FDFE65", "#9CE400", "#359800"), domain = aux_map_2$PERC_VOT, bins = unique(classes))


    leaflet(data = aux_map_2) %>% 
      addTiles('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
      addPolygons(weight = 0.5, fillColor = ~pal_cor(PERC_VOT), # Weight e a grossura das bordas
                  color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                  smoothFactor = 0.25,
                  popup = paste0(aux_map_2$NOME_MESO, "<br>",
                                 "Percentual de Votos: ", round(aux_map_2$PERC_VOT, 3), "%", "<br>",
                                 "Total de Votos para o candidato em ", aux_map_2$NOME_MESO, ": ", aux_map_2$QTDE_VOTOS, "<br>",
                                 "Total de Votos em ", aux_map_2$NOME_MESO, ": ", aux_map_2$QTDE_EST_TOT, "<br>",
                                 "Total de Votos para o candidato no Estado: ", aux_map_2$QTDE_VOT_CAND),
                  label = ~NOME_MESO) %>% 
      addLegend(position = "bottomright", pal = pal_cor, values = ~PERC_VOT,
                labFormat = labelFormat(suffix = "%"))

  })

output$titulo_mapa_gov_mun <- renderText({
  
    est <- input$escolhe_est_gov_mun

    year <- input$ano_mun_gov

    cand <- input$escolhe_gov_mun

    if(input$gov_turno_mun == 'turno_1'){
        turno <- 1
      }
      else {
        turno <- 2
      }

    data_aux_titulo <- tse_data %>%
              filter(NOME_UF == est,
                     ANO_ELEICAO == year,
                     DESCRICAO_CARGO == "GOVERNADOR",
                     NOME_CANDIDATO == cand,
                     NUM_TURNO == turno) %>%
              mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
              select(ANO_ELEICAO, NOME_CANDIDATO, SIGLA_PARTIDO, QTDE_VOTOS, DESC_SIT_TOT_TURNO) %>%
              unique()                
  
  paste0(data_aux_titulo$NOME_CANDIDATO, " - ", data_aux_titulo$SIGLA_PARTIDO, " - ", data_aux_titulo$DESC_SIT_TOT_TURNO ,": ",
       data_aux_titulo$QTDE_VOTOS, " votos em ", data_aux_titulo$ANO_ELEICAO)
  
  })

output$mapa_gov_mun <- renderLeaflet({
    
    est <- input$escolhe_est_gov_mun

    year <- input$ano_mun_gov

    cand <- input$escolhe_gov_mun

    if(input$gov_turno_mun == 'turno_1'){
        turno <- 1
      }
      else {
        turno <- 2
      }

    aux_cap <- tse_capitais %>%
                filter(estados == est)

    aux_map <- mapa_BR %>% subset(ESTADO == aux_cap$id_uf)

    if(input$gov_choice_mun == 'orig_vot'){
      data_aux <- tse_data %>%
        filter(NOME_UF == est,
               ANO_ELEICAO == year,
               DESCRICAO_CARGO == "GOVERNADOR",
               NUM_TURNO == turno) %>%
        group_by(NOME_MUNICIPIO) %>%
        mutate(QTDE_EST_TOT = sum(QTDE_VOTOS)) %>%
        arrange(QTDE_EST_TOT) %>%
        ungroup() %>%
        filter(NOME_CANDIDATO == cand) %>%
        unique() %>%
        mutate(QTDE_VOT_CAND = sum(QTDE_VOTOS),
               PERC_VOT = (QTDE_VOTOS/QTDE_VOT_CAND)*100)

        classes <- c(0, 0.1, 0.25, 0.5, 1, 2, 5, ceiling(max(data_aux$PERC_VOT)))
    } else {
      data_aux <- tse_data %>%
        filter(NOME_UF == est,
               ANO_ELEICAO == year,
               DESCRICAO_CARGO == "GOVERNADOR",
               NUM_TURNO == turno) %>%
        group_by(NOME_MUNICIPIO) %>%
        mutate(QTDE_EST_TOT = sum(QTDE_VOTOS)) %>%
        arrange(QTDE_EST_TOT) %>%
        ungroup() %>%
        filter(NOME_CANDIDATO == cand) %>%
        unique() %>%
        mutate(PERC_VOT = (QTDE_VOTOS/QTDE_EST_TOT)*100,
               QTDE_VOT_CAND = sum(QTDE_VOTOS))

        classes <- c(0, 5, 10, 20, 30, 35, 40, ceiling(max(data_aux$PERC_VOT)))
    }


    aux_map_2 <- sp::merge(x = aux_map, y = data_aux, by.x = "GEOCODIGO", by.y = "COD_MUN_IBGE")

    aux_map_2$QTDE_VOTOS[is.na(aux_map_2$QTDE_VOTOS)] <- 0
    aux_map_2$QTDE_EST_TOT[is.na(aux_map_2$QTDE_EST_TOT)] <- max(aux_map_2$QTDE_EST_TOT)
    aux_map_2$PERC_VOT[is.na(aux_map_2$PERC_VOT)] <- 0

    #classes <- c(0, 0.1, 0.25, 0.5, 1, 2, 5, ceiling(max(aux_map_2$PERC_VOT)))
    pal_cor <- colorBin(c("#6A1103", "#BA0004", "#E7400B", "#FEA527", "#FDFE65", "#9CE400", "#359800"), domain = aux_map_2$PERC_VOT, bins = unique(classes))


    leaflet(data = aux_map_2) %>% 
      addTiles('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
      addPolygons(weight = 0.5, fillColor = ~pal_cor(PERC_VOT), # Weight e a grossura das bordas
                  color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                  smoothFactor = 0.25,
                  popup = paste0(aux_map_2$NOME_MUNICIPIO.x, "<br>",
                                 "Percentual de Votos: ", round(aux_map_2$PERC_VOT, 3), "%", "<br>",
                                 "Total de Votos para o candidato em ", aux_map_2$NOME_MUNICIPIO.x, ": ", aux_map_2$QTDE_VOTOS, "<br>",
                                 "Total de Votos em ", aux_map_2$NOME_MUNICIPIO.x, ": ", aux_map_2$QTDE_EST_TOT, "<br>",
                                 "Total de Votos para o candidato no Estado: ", aux_map_2$QTDE_VOT_CAND),
                  label = ~NOME_MUNICIPIO.x) %>% 
      addLegend(position = "bottomright", pal = pal_cor, values = ~PERC_VOT,
                labFormat = labelFormat(suffix = "%"))

  })

output$titulo_mapa_gov_micro <- renderText({
  
    est <- input$escolhe_est_gov_micro

    year <- input$ano_micro_gov

    cand <- input$escolhe_gov_micro

    if(input$gov_turno_micro == 'turno_1'){
        turno <- 1
      }
      else {
        turno <- 2
      }

    data_aux_titulo <- tse_data %>%
              filter(NOME_UF == est,
                     ANO_ELEICAO == year,
                     DESCRICAO_CARGO == "GOVERNADOR",
                     NOME_CANDIDATO == cand,
                     NUM_TURNO == turno) %>%
              mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
              select(ANO_ELEICAO, NOME_CANDIDATO, SIGLA_PARTIDO, QTDE_VOTOS, DESC_SIT_TOT_TURNO) %>%
              unique()                
  
  paste0(data_aux_titulo$NOME_CANDIDATO, " - ", data_aux_titulo$SIGLA_PARTIDO, " - ", data_aux_titulo$DESC_SIT_TOT_TURNO ,": ",
       data_aux_titulo$QTDE_VOTOS, " votos em ", data_aux_titulo$ANO_ELEICAO)
  
  })

output$mapa_gov_micro <- renderLeaflet({
    
    est <- input$escolhe_est_gov_micro

    year <- input$ano_micro_gov

    cand <- input$escolhe_gov_micro

    if(input$gov_turno_micro == 'turno_1'){
        turno <- 1
      }
      else {
        turno <- 2
      }

    aux_cap <- tse_capitais %>%
                filter(estados == est)

    aux_map <- mapa_micro %>% subset(ESTADO == aux_cap$id_uf)

    if(input$gov_choice_micro == 'orig_vot'){
      data_aux <- tse_data %>%
        filter(NOME_UF == est,
               ANO_ELEICAO == year,
               DESCRICAO_CARGO == "GOVERNADOR",
               NUM_TURNO == turno) %>%
        group_by(NOME_MICRO) %>%
        mutate(QTDE_EST_TOT = sum(QTDE_VOTOS),
               NOME_MUNICIPIO = NULL,
               COD_MUN_IBGE = NULL) %>%
        arrange(QTDE_EST_TOT) %>%
        ungroup() %>%
        filter(NOME_CANDIDATO == cand) %>%
        group_by(NOME_MICRO) %>%
        mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
        unique() %>% ungroup() %>%
        mutate(QTDE_VOT_CAND = sum(QTDE_VOTOS),
               PERC_VOT = (QTDE_VOTOS/QTDE_VOT_CAND)*100)

        classes <- c(0, 0.1, 0.25, 0.5, 1, 2, 5, ceiling(max(data_aux$PERC_VOT)))
    } else {
      data_aux <- tse_data %>%
        filter(NOME_UF == est,
               ANO_ELEICAO == year,
               DESCRICAO_CARGO == "GOVERNADOR",
               NUM_TURNO == turno) %>%
        group_by(NOME_MICRO) %>%
        mutate(QTDE_EST_TOT = sum(QTDE_VOTOS),
               NOME_MUNICIPIO = NULL,
               COD_MUN_IBGE = NULL) %>%
        arrange(QTDE_EST_TOT) %>%
        ungroup() %>%
        filter(NOME_CANDIDATO == cand) %>%
        group_by(NOME_MICRO) %>%
        mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
        unique() %>% ungroup() %>%
        mutate(PERC_VOT = (QTDE_VOTOS/QTDE_EST_TOT)*100,
               QTDE_VOT_CAND = sum(QTDE_VOTOS))

        classes <- c(0, 5, 10, 20, 30, 35, 40, ceiling(max(data_aux$PERC_VOT)))
               
    }


    #aux_map_2 <- merge(x = aux_map, y = data_aux, by.x = "NOME_MESO", by.y = "NOME_MESO")

    aux_map_2 <- sp::merge(aux_map, data_aux, by = "NOME_MICRO", duplicateGeoms = T)

    aux_map_2$QTDE_VOTOS[is.na(aux_map_2$QTDE_VOTOS)] <- 0
    aux_map_2$QTDE_EST_TOT[is.na(aux_map_2$QTDE_EST_TOT)] <- max(aux_map_2$QTDE_EST_TOT)
    aux_map_2$PERC_VOT[is.na(aux_map_2$PERC_VOT)] <- 0

    #classes <- c(0, 0.1, 0.25, 0.5, 1, 2, 5, ceiling(max(aux_map_2$PERC_VOT)))
    pal_cor <- colorBin(c("#6A1103", "#BA0004", "#E7400B", "#FEA527", "#FDFE65", "#9CE400", "#359800"), domain = aux_map_2$PERC_VOT, bins = unique(classes))


    leaflet(data = aux_map_2) %>% 
      addTiles('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
      addPolygons(weight = 0.5, fillColor = ~pal_cor(PERC_VOT), # Weight e a grossura das bordas
                  color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                  smoothFactor = 0.25,
                  popup = paste0(aux_map_2$NOME_MICRO, "<br>",
                                 "Percentual de Votos: ", round(aux_map_2$PERC_VOT, 3), "%", "<br>",
                                 "Total de Votos para o candidato em ", aux_map_2$NOME_MICRO, ": ", aux_map_2$QTDE_VOTOS, "<br>",
                                 "Total de Votos em ", aux_map_2$NOME_MICRO, ": ", aux_map_2$QTDE_EST_TOT, "<br>",
                                 "Total de Votos para o candidato no Estado: ", aux_map_2$QTDE_VOT_CAND),
                  label = ~NOME_MICRO) %>% 
      addLegend(position = "bottomright", pal = pal_cor, values = ~PERC_VOT,
                labFormat = labelFormat(suffix = "%"))

  })

output$titulo_mapa_gov_meso <- renderText({
  
    est <- input$escolhe_est_gov_meso

    year <- input$ano_meso_gov

    cand <- input$escolhe_gov_meso

    if(input$gov_turno_meso == 'turno_1'){
        turno <- 1
      }
      else {
        turno <- 2
      }

    data_aux_titulo <- tse_data %>%
              filter(NOME_UF == est,
                     ANO_ELEICAO == year,
                     DESCRICAO_CARGO == "GOVERNADOR",
                     NOME_CANDIDATO == cand,
                     NUM_TURNO == turno) %>%
              mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
              select(ANO_ELEICAO, NOME_CANDIDATO, SIGLA_PARTIDO, QTDE_VOTOS, DESC_SIT_TOT_TURNO) %>%
              unique()                
  
  paste0(data_aux_titulo$NOME_CANDIDATO, " - ", data_aux_titulo$SIGLA_PARTIDO, " - ", data_aux_titulo$DESC_SIT_TOT_TURNO ,": ",
       data_aux_titulo$QTDE_VOTOS, " votos em ", data_aux_titulo$ANO_ELEICAO)
  
  })

output$mapa_gov_meso <- renderLeaflet({
    
    est <- input$escolhe_est_gov_meso

    year <- input$ano_meso_gov

    cand <- input$escolhe_gov_meso

    if(input$gov_turno_meso == 'turno_1'){
        turno <- 1
      }
      else {
        turno <- 2
      }


    aux_cap <- tse_capitais %>%
                filter(estados == est)

    aux_map <- mapa_meso %>% subset(ESTADO == aux_cap$id_uf)

    if(input$gov_choice_meso == 'orig_vot'){
      data_aux <- tse_data %>%
        filter(NOME_UF == est,
               ANO_ELEICAO == year,
               DESCRICAO_CARGO == "GOVERNADOR",
               NUM_TURNO == turno) %>%
        group_by(NOME_MESO) %>%
        mutate(QTDE_EST_TOT = sum(QTDE_VOTOS),
               NOME_MUNICIPIO = NULL,
               COD_MUN_IBGE = NULL,
               NOME_MICRO = NULL) %>%
        arrange(QTDE_EST_TOT) %>%
        ungroup() %>%
        filter(NOME_CANDIDATO == cand) %>%
        group_by(NOME_MESO) %>%
        mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
        unique() %>% ungroup() %>%
        mutate(QTDE_VOT_CAND = sum(QTDE_VOTOS),
               PERC_VOT = (QTDE_VOTOS/QTDE_VOT_CAND)*100)

        classes <- c(0, 0.1, 0.25, 0.5, 1, 2, 5, ceiling(max(data_aux$PERC_VOT)))
    } else {
      data_aux <- tse_data %>%
        filter(NOME_UF == est,
               ANO_ELEICAO == year,
               DESCRICAO_CARGO == "GOVERNADOR",
               NUM_TURNO == turno) %>%
        group_by(NOME_MESO) %>%
        mutate(QTDE_EST_TOT = sum(QTDE_VOTOS),
               NOME_MUNICIPIO = NULL,
               COD_MUN_IBGE = NULL,
               NOME_MICRO = NULL) %>%
        arrange(QTDE_EST_TOT) %>%
        ungroup() %>%
        filter(NOME_CANDIDATO == cand) %>%
        group_by(NOME_MESO) %>%
        mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
        unique() %>% ungroup() %>%
        mutate(PERC_VOT = (QTDE_VOTOS/QTDE_EST_TOT)*100,
               QTDE_VOT_CAND = sum(QTDE_VOTOS))

        classes <- c(0, 5, 10, 20, 30, 35, 40, ceiling(max(data_aux$PERC_VOT)))
               
    }


    #aux_map_2 <- merge(x = aux_map, y = data_aux, by.x = "NOME_MESO", by.y = "NOME_MESO")

    aux_map_2 <- sp::merge(aux_map, data_aux, by = "NOME_MESO", duplicateGeoms = T)

    aux_map_2$QTDE_VOTOS[is.na(aux_map_2$QTDE_VOTOS)] <- 0
    aux_map_2$QTDE_EST_TOT[is.na(aux_map_2$QTDE_EST_TOT)] <- max(aux_map_2$QTDE_EST_TOT)
    aux_map_2$PERC_VOT[is.na(aux_map_2$PERC_VOT)] <- 0

    #classes <- c(0, 0.1, 0.25, 0.5, 1, 2, 5, ceiling(max(aux_map_2$PERC_VOT)))
    pal_cor <- colorBin(c("#6A1103", "#BA0004", "#E7400B", "#FEA527", "#FDFE65", "#9CE400", "#359800"), domain = aux_map_2$PERC_VOT, bins = unique(classes))


    leaflet(data = aux_map_2) %>% 
      addTiles('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
      addPolygons(weight = 0.5, fillColor = ~pal_cor(PERC_VOT), # Weight e a grossura das bordas
                  color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                  smoothFactor = 0.25,
                  popup = paste0(aux_map_2$NOME_MESO, "<br>",
                                 "Percentual de Votos: ", round(aux_map_2$PERC_VOT, 3), "%", "<br>",
                                 "Total de Votos para o candidato em ", aux_map_2$NOME_MESO, ": ", aux_map_2$QTDE_VOTOS, "<br>",
                                 "Total de Votos em ", aux_map_2$NOME_MESO, ": ", aux_map_2$QTDE_EST_TOT, "<br>",
                                 "Total de Votos para o candidato no Estado: ", aux_map_2$QTDE_VOT_CAND),
                  label = ~NOME_MESO) %>% 
      addLegend(position = "bottomright", pal = pal_cor, values = ~PERC_VOT,,
                labFormat = labelFormat(suffix = "%"))

  })

output$titulo_mapa_dep_fed_mun <- renderText({
  
    est <- input$escolhe_est_dep_fed

    year <- input$ano_mun_dep_fed

    cand <- input$escolhe_cand_dep_fed

    data_aux_titulo <- tse_data %>%
              filter(NOME_UF == est,
                     ANO_ELEICAO == year,
                     DESCRICAO_CARGO == "DEPUTADO FEDERAL",
                     NOME_CANDIDATO == cand) %>%
              mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
              select(ANO_ELEICAO, NOME_CANDIDATO, SIGLA_PARTIDO, QTDE_VOTOS, DESC_SIT_TOT_TURNO) %>%
              unique()                
  
  paste0(data_aux_titulo$NOME_CANDIDATO, " - ", data_aux_titulo$SIGLA_PARTIDO, " - ", data_aux_titulo$DESC_SIT_TOT_TURNO ,": ",
       data_aux_titulo$QTDE_VOTOS, " votos em ", data_aux_titulo$ANO_ELEICAO)

  })
  

output$mapa_dep_fed_mun <- renderLeaflet({
    
    est <- input$escolhe_est_dep_fed

    year <- input$ano_mun_dep_fed

    cand <- input$escolhe_cand_dep_fed

    aux_cap <- tse_capitais %>%
                filter(estados == est)

    aux_map <- mapa_BR %>% subset(ESTADO == aux_cap$id_uf)

    if(input$dep_fed_map_choice_mun == 'orig_vot'){
      data_aux <- tse_data %>%
        filter(NOME_UF == est,
               ANO_ELEICAO == year,
               DESCRICAO_CARGO == "DEPUTADO FEDERAL") %>%
        group_by(NOME_MUNICIPIO) %>%
        mutate(QTDE_EST_TOT = sum(QTDE_VOTOS)) %>%
        arrange(QTDE_EST_TOT) %>%
        ungroup() %>%
        filter(NOME_CANDIDATO == cand) %>%
        unique() %>%
        mutate(QTDE_VOT_CAND = sum(QTDE_VOTOS),
               PERC_VOT = (QTDE_VOTOS/QTDE_VOT_CAND)*100)
    } else {
      data_aux <- tse_data %>%
        filter(NOME_UF == est,
               ANO_ELEICAO == year,
               DESCRICAO_CARGO == "DEPUTADO FEDERAL") %>%
        group_by(NOME_MUNICIPIO) %>%
        mutate(QTDE_EST_TOT = sum(QTDE_VOTOS)) %>%
        arrange(QTDE_EST_TOT) %>%
        ungroup() %>%
        filter(NOME_CANDIDATO == cand) %>%
        unique() %>%
        mutate(PERC_VOT = (QTDE_VOTOS/QTDE_EST_TOT)*100,
               QTDE_VOT_CAND = sum(QTDE_VOTOS))
    }

    aux_map_2 <- sp::merge(x = aux_map, y = data_aux, by.x = "GEOCODIGO", by.y = "COD_MUN_IBGE")

    aux_map_2$QTDE_VOTOS[is.na(aux_map_2$QTDE_VOTOS)] <- 0
    aux_map_2$QTDE_EST_TOT[is.na(aux_map_2$QTDE_EST_TOT)] <- max(aux_map_2$QTDE_EST_TOT)
    aux_map_2$PERC_VOT[is.na(aux_map_2$PERC_VOT)] <- 0

    classes <- c(0, 0.1, 0.25, 0.5, 1, 2, 5, ceiling(max(aux_map_2$PERC_VOT)))
    pal_cor <- colorBin(c("#6A1103", "#BA0004", "#E7400B", "#FEA527", "#FDFE65", "#9CE400", "#359800"), domain = aux_map_2$PERC_VOT, bins = unique(classes))


    leaflet(data = aux_map_2) %>% 
      addTiles('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
      addPolygons(weight = 0.5, fillColor = ~pal_cor(PERC_VOT), # Weight e a grossura das bordas
                  color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                  smoothFactor = 0.25,
                  popup = paste0(aux_map_2$NOME_MUNICIPIO.x, "<br>",
                                 "Percentual de Votos: ", round(aux_map_2$PERC_VOT, 3), "%", "<br>",
                                 "Total de Votos para o candidato em ", aux_map_2$NOME_MUNICIPIO.x, ": ", aux_map_2$QTDE_VOTOS, "<br>",
                                 "Total de Votos em ", aux_map_2$NOME_MUNICIPIO.x, ": ", aux_map_2$QTDE_EST_TOT, "<br>",
                                 "Total de Votos para o candidato no Estado: ", aux_map_2$QTDE_VOT_CAND),
                  label = ~NOME_MUNICIPIO.x) %>% 
      addLegend(position = "bottomright", pal = pal_cor, values = ~PERC_VOT,
                labFormat = labelFormat(suffix = "%"))

  })

output$titulo_mapa_dep_fed_micro <- renderText({
  
    est <- input$escolhe_est_dep_fed_micro

    year <- input$ano_micro_dep_fed

    cand <- input$escolhe_cand_dep_fed_micro

    data_aux_titulo <- tse_data %>%
              filter(NOME_UF == est,
                     ANO_ELEICAO == year,
                     DESCRICAO_CARGO == "DEPUTADO FEDERAL",
                     NOME_CANDIDATO == cand) %>%
              mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
              select(ANO_ELEICAO, NOME_CANDIDATO, SIGLA_PARTIDO, QTDE_VOTOS, DESC_SIT_TOT_TURNO) %>%
              unique()                
  
  paste0(data_aux_titulo$NOME_CANDIDATO, " - ", data_aux_titulo$SIGLA_PARTIDO, " - ", data_aux_titulo$DESC_SIT_TOT_TURNO ,": ",
       data_aux_titulo$QTDE_VOTOS, " votos em ", data_aux_titulo$ANO_ELEICAO)
  
  })

output$mapa_dep_fed_micro <- renderLeaflet({
    
    est <- input$escolhe_est_dep_fed_micro

    year <- input$ano_micro_dep_fed

    cand <- input$escolhe_cand_dep_fed_micro

    aux_cap <- tse_capitais %>%
                filter(estados == est)

    aux_map <- mapa_micro %>% subset(ESTADO == aux_cap$id_uf)

    if(input$dep_fed_map_choice_micro == 'orig_vot'){
      data_aux <- tse_data %>%
        filter(NOME_UF == est,
               ANO_ELEICAO == year,
               DESCRICAO_CARGO == "DEPUTADO FEDERAL") %>%
        group_by(NOME_MICRO) %>%
        mutate(QTDE_EST_TOT = sum(QTDE_VOTOS),
               NOME_MUNICIPIO = NULL,
               COD_MUN_IBGE = NULL) %>%
        arrange(QTDE_EST_TOT) %>%
        ungroup() %>%
        filter(NOME_CANDIDATO == cand) %>%
        group_by(NOME_MICRO) %>%
        mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
        unique() %>% ungroup() %>%
        mutate(QTDE_VOT_CAND = sum(QTDE_VOTOS),
               PERC_VOT = (QTDE_VOTOS/QTDE_VOT_CAND)*100)
    } else {
      data_aux <- tse_data %>%
        filter(NOME_UF == est,
               ANO_ELEICAO == year,
               DESCRICAO_CARGO == "DEPUTADO FEDERAL") %>%
        group_by(NOME_MICRO) %>%
        mutate(QTDE_EST_TOT = sum(QTDE_VOTOS),
               NOME_MUNICIPIO = NULL,
               COD_MUN_IBGE = NULL) %>%
        arrange(QTDE_EST_TOT) %>%
        ungroup() %>%
        filter(NOME_CANDIDATO == cand) %>%
        group_by(NOME_MICRO) %>%
        mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
        unique() %>% ungroup() %>%
        mutate(PERC_VOT = (QTDE_VOTOS/QTDE_EST_TOT)*100,
               QTDE_VOT_CAND = sum(QTDE_VOTOS))
               
    }


    #aux_map_2 <- merge(x = aux_map, y = data_aux, by.x = "NOME_MESO", by.y = "NOME_MESO")

    aux_map_2 <- sp::merge(aux_map, data_aux, by = "NOME_MICRO", duplicateGeoms = T)

    aux_map_2$QTDE_VOTOS[is.na(aux_map_2$QTDE_VOTOS)] <- 0
    aux_map_2$QTDE_EST_TOT[is.na(aux_map_2$QTDE_EST_TOT)] <- max(aux_map_2$QTDE_EST_TOT)
    aux_map_2$PERC_VOT[is.na(aux_map_2$PERC_VOT)] <- 0

    classes <- c(0, 0.1, 0.25, 0.5, 1, 2, 5, ceiling(max(aux_map_2$PERC_VOT)))
    pal_cor <- colorBin(c("#6A1103", "#BA0004", "#E7400B", "#FEA527", "#FDFE65", "#9CE400", "#359800"), domain = aux_map_2$PERC_VOT, bins = unique(classes))


    leaflet(data = aux_map_2) %>% 
      addTiles('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
      addPolygons(weight = 0.5, fillColor = ~pal_cor(PERC_VOT), # Weight e a grossura das bordas
                  color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                  smoothFactor = 0.25,
                  popup = paste0(aux_map_2$NOME_MICRO, "<br>",
                                 "Percentual de Votos: ", round(aux_map_2$PERC_VOT, 3), "%", "<br>",
                                 "Total de Votos para o candidato em ", aux_map_2$NOME_MICRO, ": ", aux_map_2$QTDE_VOTOS, "<br>",
                                 "Total de Votos em ", aux_map_2$NOME_MICRO, ": ", aux_map_2$QTDE_EST_TOT, "<br>",
                                 "Total de Votos para o candidato no Estado: ", aux_map_2$QTDE_VOT_CAND),
                  label = ~NOME_MICRO) %>% 
      addLegend(position = "bottomright", pal = pal_cor, values = ~PERC_VOT,
                labFormat = labelFormat(suffix = "%"))

  })

output$titulo_mapa_dep_fed_meso <- renderText({
  
    est <- input$escolhe_est_dep_fed_meso

    year <- input$ano_meso_dep_fed

    cand <- input$escolhe_cand_dep_fed_meso

    data_aux_titulo <- tse_data %>%
              filter(NOME_UF == est,
                     ANO_ELEICAO == year,
                     DESCRICAO_CARGO == "DEPUTADO FEDERAL",
                     NOME_CANDIDATO == cand) %>%
              mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
              select(ANO_ELEICAO, NOME_CANDIDATO, SIGLA_PARTIDO, QTDE_VOTOS, DESC_SIT_TOT_TURNO) %>%
              unique()                
  
  paste0(data_aux_titulo$NOME_CANDIDATO, " - ", data_aux_titulo$SIGLA_PARTIDO, " - ", data_aux_titulo$DESC_SIT_TOT_TURNO ,": ",
       data_aux_titulo$QTDE_VOTOS, " votos em ", data_aux_titulo$ANO_ELEICAO)
  
  })

output$mapa_dep_fed_meso <- renderLeaflet({
    
    est <- input$escolhe_est_dep_fed_meso

    year <- input$ano_meso_dep_fed

    cand <- input$escolhe_cand_dep_fed_meso

    aux_cap <- tse_capitais %>%
                filter(estados == est)

    aux_map <- mapa_meso %>% subset(ESTADO == aux_cap$id_uf)

    if(input$dep_fed_map_choice_meso == 'orig_vot'){
      data_aux <- tse_data %>%
        filter(NOME_UF == est,
               ANO_ELEICAO == year,
               DESCRICAO_CARGO == "DEPUTADO FEDERAL") %>%
        group_by(NOME_MESO) %>%
        mutate(QTDE_EST_TOT = sum(QTDE_VOTOS),
               NOME_MUNICIPIO = NULL,
               COD_MUN_IBGE = NULL,
               NOME_MICRO = NULL) %>%
        arrange(QTDE_EST_TOT) %>%
        ungroup() %>%
        filter(NOME_CANDIDATO == cand) %>%
        group_by(NOME_MESO) %>%
        mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
        unique() %>% ungroup() %>%
        mutate(QTDE_VOT_CAND = sum(QTDE_VOTOS),
               PERC_VOT = (QTDE_VOTOS/QTDE_VOT_CAND)*100)
    } else {
      data_aux <- tse_data %>%
        filter(NOME_UF == est,
               ANO_ELEICAO == year,
               DESCRICAO_CARGO == "DEPUTADO FEDERAL") %>%
        group_by(NOME_MESO) %>%
        mutate(QTDE_EST_TOT = sum(QTDE_VOTOS),
               NOME_MUNICIPIO = NULL,
               COD_MUN_IBGE = NULL,
               NOME_MICRO = NULL) %>%
        arrange(QTDE_EST_TOT) %>%
        ungroup() %>%
        filter(NOME_CANDIDATO == cand) %>%
        group_by(NOME_MESO) %>%
        mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
        unique() %>% ungroup() %>%
        mutate(PERC_VOT = (QTDE_VOTOS/QTDE_EST_TOT)*100,
               QTDE_VOT_CAND = sum(QTDE_VOTOS))
               
    }


    #aux_map_2 <- merge(x = aux_map, y = data_aux, by.x = "NOME_MESO", by.y = "NOME_MESO")

    aux_map_2 <- sp::merge(aux_map, data_aux, by = "NOME_MESO", duplicateGeoms = T)

    aux_map_2$QTDE_VOTOS[is.na(aux_map_2$QTDE_VOTOS)] <- 0
    aux_map_2$QTDE_EST_TOT[is.na(aux_map_2$QTDE_EST_TOT)] <- max(aux_map_2$QTDE_EST_TOT)
    aux_map_2$PERC_VOT[is.na(aux_map_2$PERC_VOT)] <- 0

    classes <- c(0, 0.1, 0.25, 0.5, 1, 2, 5, ceiling(max(aux_map_2$PERC_VOT)))
    pal_cor <- colorBin(c("#6A1103", "#BA0004", "#E7400B", "#FEA527", "#FDFE65", "#9CE400", "#359800"), domain = aux_map_2$PERC_VOT, bins = unique(classes))


    leaflet(data = aux_map_2) %>% 
      addTiles('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
      addPolygons(weight = 0.5, fillColor = ~pal_cor(PERC_VOT), # Weight e a grossura das bordas
                  color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                  smoothFactor = 0.25,
                  popup = paste0(aux_map_2$NOME_MESO, "<br>",
                                 "Percentual de Votos: ", round(aux_map_2$PERC_VOT, 3), "%", "<br>",
                                 "Total de Votos para o candidato em ", aux_map_2$NOME_MESO, ": ", aux_map_2$QTDE_VOTOS, "<br>",
                                 "Total de Votos em ", aux_map_2$NOME_MESO, ": ", aux_map_2$QTDE_EST_TOT, "<br>",
                                 "Total de Votos para o candidato no Estado: ", aux_map_2$QTDE_VOT_CAND),
                  label = ~NOME_MESO) %>% 
      addLegend(position = "bottomright", pal = pal_cor, values = ~PERC_VOT,
                labFormat = labelFormat(suffix = "%"))

  })

output$titulo_mapa_pres_est <- renderText({
  
    year <- input$ano_pres_est

    cand <- input$escolhe_pres_est

    if(input$pres_turno_est == 'turno_1'){
        turno <- 1
      }
      else {
        turno <- 2
      }

    data_aux_titulo <- tse_data %>%
              filter(ANO_ELEICAO == year,
                     DESCRICAO_CARGO == "PRESIDENTE",
                     NOME_CANDIDATO == cand,
                     NUM_TURNO == turno) %>%
              mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
              select(ANO_ELEICAO, NOME_CANDIDATO, SIGLA_PARTIDO, QTDE_VOTOS, DESC_SIT_TOT_TURNO) %>%
              unique()                
  
  paste0(data_aux_titulo$NOME_CANDIDATO, " - ", data_aux_titulo$SIGLA_PARTIDO, " - ", data_aux_titulo$DESC_SIT_TOT_TURNO ,": ",
       data_aux_titulo$QTDE_VOTOS, " votos em ", data_aux_titulo$ANO_ELEICAO)
  
  })

output$mapa_pres_est <- renderLeaflet({
    
    year <- input$ano_pres_est

    cand <- input$escolhe_pres_est

    if(input$pres_turno_est == 'turno_1'){
        turno <- 1
      }
      else {
        turno <- 2
      }

    aux_map <- mapa_estados

    if(input$pres_choice_est == 'orig_vot'){
        data_aux <- tse_data %>%
          filter(ANO_ELEICAO == year,
                 DESCRICAO_CARGO == "PRESIDENTE",
                 NUM_TURNO == turno) %>%
          group_by(NOME_UF) %>%
          mutate(QTDE_EST_TOT = sum(QTDE_VOTOS),
                 NOME_MUNICIPIO = NULL,
                 COD_MUN_IBGE = NULL,
                 NOME_MICRO = NULL,
                 NOME_MESO = NULL) %>%
          arrange(QTDE_EST_TOT) %>%
          ungroup() %>%
          filter(NOME_CANDIDATO == cand) %>%
          group_by(NOME_UF) %>%
          mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
          unique() %>% ungroup() %>%
          mutate(QTDE_VOT_CAND = sum(QTDE_VOTOS),
                 PERC_VOT = (QTDE_VOTOS/QTDE_VOT_CAND)*100)

        classes <- c(0, 0.1, 0.25, 0.5, 1, 2, 5, ceiling(max(data_aux$PERC_VOT)))
    } else {
        data_aux <- tse_data %>%
            filter(ANO_ELEICAO == year,
                   DESCRICAO_CARGO == "PRESIDENTE",
                   NUM_TURNO == turno) %>%
            group_by(NOME_UF) %>%
            mutate(QTDE_EST_TOT = sum(QTDE_VOTOS),
                   NOME_MUNICIPIO = NULL,
                   COD_MUN_IBGE = NULL,
                   NOME_MICRO = NULL,
                   NOME_MESO = NULL) %>%
            arrange(QTDE_EST_TOT) %>%
            ungroup() %>%
            filter(NOME_CANDIDATO == cand) %>%
            group_by(NOME_UF) %>%
            mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
            unique() %>% ungroup() %>%
            mutate(PERC_VOT = (QTDE_VOTOS/QTDE_EST_TOT)*100,
                   QTDE_VOT_CAND = sum(QTDE_VOTOS))

            classes <- c(0, 5, 10, 20, 30, 35, 40, ceiling(max(data_aux$PERC_VOT)))
    }


aux_map_2 <- sp::merge(x = aux_map, y = data_aux, by.x = "estados", by.y = "NOME_UF")

aux_map_2$QTDE_VOTOS[is.na(aux_map_2$QTDE_VOTOS)] <- 0
aux_map_2$QTDE_EST_TOT[is.na(aux_map_2$QTDE_EST_TOT)] <- max(aux_map_2$QTDE_EST_TOT)
aux_map_2$PERC_VOT[is.na(aux_map_2$PERC_VOT)] <- 0

#classes <- c(0, 0.1, 0.25, 0.5, 1, 2, 5, ceiling(max(aux_map_2$PERC_VOT)))
pal_cor <- colorBin(c("#6A1103", "#BA0004", "#E7400B", "#FEA527", "#FDFE65", "#9CE400", "#359800"), domain = aux_map_2$PERC_VOT, bins = unique(classes))


leaflet(data = aux_map_2) %>% 
  addTiles('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
  addPolygons(weight = 0.5, fillColor = ~pal_cor(PERC_VOT), # Weight e a grossura das bordas
              color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
              smoothFactor = 0.25,
              popup = paste0(aux_map_2$estados, "<br>",
                             "Percentual de Votos: ", round(aux_map_2$PERC_VOT, 3), "%", "<br>",
                             "Total de Votos para o candidato em ", aux_map_2$estados, ": ", aux_map_2$QTDE_VOTOS, "<br>",
                             "Total de Votos em ", aux_map_2$estados, ": ", aux_map_2$QTDE_EST_TOT, "<br>",
                             "Total de Votos para o candidato no Estado: ", aux_map_2$QTDE_VOT_CAND),
              label = ~estados) %>% 
  addLegend(position = "bottomright", pal = pal_cor, values = ~PERC_VOT,
            labFormat = labelFormat(suffix = "%"))

  })

output$titulo_mapa_pres_mun <- renderText({
  
    est <- input$escolhe_est_pres_mun

    year <- input$ano_mun_pres

    cand <- input$escolhe_pres_mun

    if(input$pres_turno_mun == 'turno_1'){
        turno <- 1
      }
      else {
        turno <- 2
      }

    data_aux_titulo <- tse_data %>%
              filter(NOME_UF == est,
                     ANO_ELEICAO == year,
                     DESCRICAO_CARGO == "PRESIDENTE",
                     NOME_CANDIDATO == cand,
                     NUM_TURNO == turno) %>%
              mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
              select(ANO_ELEICAO, NOME_CANDIDATO, SIGLA_PARTIDO, QTDE_VOTOS, DESC_SIT_TOT_TURNO) %>%
              unique()                
  
  paste0(data_aux_titulo$NOME_CANDIDATO, " - ", data_aux_titulo$SIGLA_PARTIDO, " - ", data_aux_titulo$DESC_SIT_TOT_TURNO ,": ",
       data_aux_titulo$QTDE_VOTOS, " votos em ", data_aux_titulo$ANO_ELEICAO)
  
  })

output$mapa_pres_mun <- renderLeaflet({
    
    est <- input$escolhe_est_pres_mun

    year <- input$ano_mun_pres

    cand <- input$escolhe_pres_mun

    if(input$pres_turno_mun == 'turno_1'){
        turno <- 1
      }
      else {
        turno <- 2
      }

    aux_cap <- tse_capitais %>%
                filter(estados == est)

    aux_map <- mapa_BR %>% subset(ESTADO == aux_cap$id_uf)

    if(input$pres_choice_mun == 'orig_vot'){
      data_aux <- tse_data %>%
        filter(NOME_UF == est,
               ANO_ELEICAO == year,
               DESCRICAO_CARGO == "PRESIDENTE",
               NUM_TURNO == turno) %>%
        group_by(NOME_MUNICIPIO) %>%
        mutate(QTDE_EST_TOT = sum(QTDE_VOTOS)) %>%
        arrange(QTDE_EST_TOT) %>%
        ungroup() %>%
        filter(NOME_CANDIDATO == cand) %>%
        unique() %>%
        mutate(QTDE_VOT_CAND = sum(QTDE_VOTOS),
               PERC_VOT = (QTDE_VOTOS/QTDE_VOT_CAND)*100)

        classes <- c(0, 0.1, 0.25, 0.5, 1, 2, 5, ceiling(max(data_aux$PERC_VOT)))
    } else {
      data_aux <- tse_data %>%
        filter(NOME_UF == est,
               ANO_ELEICAO == year,
               DESCRICAO_CARGO == "PRESIDENTE",
               NUM_TURNO == turno) %>%
        group_by(NOME_MUNICIPIO) %>%
        mutate(QTDE_EST_TOT = sum(QTDE_VOTOS)) %>%
        arrange(QTDE_EST_TOT) %>%
        ungroup() %>%
        filter(NOME_CANDIDATO == cand) %>%
        unique() %>%
        mutate(PERC_VOT = (QTDE_VOTOS/QTDE_EST_TOT)*100,
               QTDE_VOT_CAND = sum(QTDE_VOTOS))

        classes <- c(0, 5, 10, 20, 30, 35, 40, ceiling(max(data_aux$PERC_VOT)))
    }


    aux_map_2 <- sp::merge(x = aux_map, y = data_aux, by.x = "GEOCODIGO", by.y = "COD_MUN_IBGE")

    aux_map_2$QTDE_VOTOS[is.na(aux_map_2$QTDE_VOTOS)] <- 0
    aux_map_2$QTDE_EST_TOT[is.na(aux_map_2$QTDE_EST_TOT)] <- max(aux_map_2$QTDE_EST_TOT)
    aux_map_2$PERC_VOT[is.na(aux_map_2$PERC_VOT)] <- 0

    #classes <- c(0, 0.1, 0.25, 0.5, 1, 2, 5, ceiling(max(aux_map_2$PERC_VOT)))
    pal_cor <- colorBin(c("#6A1103", "#BA0004", "#E7400B", "#FEA527", "#FDFE65", "#9CE400", "#359800"), domain = aux_map_2$PERC_VOT, bins = unique(classes))


    leaflet(data = aux_map_2) %>% 
      addTiles('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
      addPolygons(weight = 0.5, fillColor = ~pal_cor(PERC_VOT), # Weight e a grossura das bordas
                  color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                  smoothFactor = 0.25,
                  popup = paste0(aux_map_2$NOME_MUNICIPIO.x, "<br>",
                                 "Percentual de Votos: ", round(aux_map_2$PERC_VOT, 3), "%", "<br>",
                                 "Total de Votos para o candidato em ", aux_map_2$NOME_MUNICIPIO.x, ": ", aux_map_2$QTDE_VOTOS, "<br>",
                                 "Total de Votos em ", aux_map_2$NOME_MUNICIPIO.x, ": ", aux_map_2$QTDE_EST_TOT, "<br>",
                                 "Total de Votos para o candidato no Estado: ", aux_map_2$QTDE_VOT_CAND),
                  label = ~NOME_MUNICIPIO.x) %>% 
      addLegend(position = "bottomright", pal = pal_cor, values = ~PERC_VOT,
                labFormat = labelFormat(suffix = "%"))

  })

output$titulo_mapa_pres_micro <- renderText({
  
    est <- input$escolhe_est_pres_micro

    year <- input$ano_micro_pres

    cand <- input$escolhe_pres_micro

    if(input$pres_turno_micro == 'turno_1'){
        turno <- 1
      }
      else {
        turno <- 2
      }

    data_aux_titulo <- tse_data %>%
              filter(NOME_UF == est,
                     ANO_ELEICAO == year,
                     DESCRICAO_CARGO == "PRESIDENTE",
                     NOME_CANDIDATO == cand,
                     NUM_TURNO == turno) %>%
              mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
              select(ANO_ELEICAO, NOME_CANDIDATO, SIGLA_PARTIDO, QTDE_VOTOS, DESC_SIT_TOT_TURNO) %>%
              unique()                
  
  paste0(data_aux_titulo$NOME_CANDIDATO, " - ", data_aux_titulo$SIGLA_PARTIDO, " - ", data_aux_titulo$DESC_SIT_TOT_TURNO ,": ",
       data_aux_titulo$QTDE_VOTOS, " votos em ", data_aux_titulo$ANO_ELEICAO)
  
  })


output$mapa_pres_micro <- renderLeaflet({
    
    est <- input$escolhe_est_pres_micro

    year <- input$ano_micro_pres

    cand <- input$escolhe_pres_micro

    if(input$pres_turno_micro == 'turno_1'){
        turno <- 1
      }
      else {
        turno <- 2
      }

    aux_cap <- tse_capitais %>%
                filter(estados == est)

    aux_map <- mapa_micro %>% subset(ESTADO == aux_cap$id_uf)

    if(input$pres_choice_micro == 'orig_vot'){
      data_aux <- tse_data %>%
        filter(NOME_UF == est,
               ANO_ELEICAO == year,
               DESCRICAO_CARGO == "PRESIDENTE",
               NUM_TURNO == turno) %>%
        group_by(NOME_MICRO) %>%
        mutate(QTDE_EST_TOT = sum(QTDE_VOTOS),
               NOME_MUNICIPIO = NULL,
               COD_MUN_IBGE = NULL) %>%
        arrange(QTDE_EST_TOT) %>%
        ungroup() %>%
        filter(NOME_CANDIDATO == cand) %>%
        group_by(NOME_MICRO) %>%
        mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
        unique() %>% ungroup() %>%
        mutate(QTDE_VOT_CAND = sum(QTDE_VOTOS),
               PERC_VOT = (QTDE_VOTOS/QTDE_VOT_CAND)*100)

        classes <- c(0, 0.1, 0.25, 0.5, 1, 2, 5, ceiling(max(data_aux$PERC_VOT)))
    } else {
      data_aux <- tse_data %>%
        filter(NOME_UF == est,
               ANO_ELEICAO == year,
               DESCRICAO_CARGO == "PRESIDENTE",
               NUM_TURNO == turno) %>%
        group_by(NOME_MICRO) %>%
        mutate(QTDE_EST_TOT = sum(QTDE_VOTOS),
               NOME_MUNICIPIO = NULL,
               COD_MUN_IBGE = NULL) %>%
        arrange(QTDE_EST_TOT) %>%
        ungroup() %>%
        filter(NOME_CANDIDATO == cand) %>%
        group_by(NOME_MICRO) %>%
        mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
        unique() %>% ungroup() %>%
        mutate(PERC_VOT = (QTDE_VOTOS/QTDE_EST_TOT)*100,
               QTDE_VOT_CAND = sum(QTDE_VOTOS))

        classes <- c(0, 5, 10, 20, 30, 35, 40, ceiling(max(data_aux$PERC_VOT)))
               
    }


    #aux_map_2 <- merge(x = aux_map, y = data_aux, by.x = "NOME_MESO", by.y = "NOME_MESO")

    aux_map_2 <- sp::merge(aux_map, data_aux, by = "NOME_MICRO", duplicateGeoms = T)

    aux_map_2$QTDE_VOTOS[is.na(aux_map_2$QTDE_VOTOS)] <- 0
    aux_map_2$QTDE_EST_TOT[is.na(aux_map_2$QTDE_EST_TOT)] <- max(aux_map_2$QTDE_EST_TOT)
    aux_map_2$PERC_VOT[is.na(aux_map_2$PERC_VOT)] <- 0

    #classes <- c(0, 0.1, 0.25, 0.5, 1, 2, 5, ceiling(max(aux_map_2$PERC_VOT)))
    pal_cor <- colorBin(c("#6A1103", "#BA0004", "#E7400B", "#FEA527", "#FDFE65", "#9CE400", "#359800"), domain = aux_map_2$PERC_VOT, bins = unique(classes))


    leaflet(data = aux_map_2) %>% 
      addTiles('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
      addPolygons(weight = 0.5, fillColor = ~pal_cor(PERC_VOT), # Weight e a grossura das bordas
                  color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                  smoothFactor = 0.25,
                  popup = paste0(aux_map_2$NOME_MICRO, "<br>",
                                 "Percentual de Votos: ", round(aux_map_2$PERC_VOT, 3), "%", "<br>",
                                 "Total de Votos para o candidato em ", aux_map_2$NOME_MICRO, ": ", aux_map_2$QTDE_VOTOS, "<br>",
                                 "Total de Votos em ", aux_map_2$NOME_MICRO, ": ", aux_map_2$QTDE_EST_TOT, "<br>",
                                 "Total de Votos para o candidato no Estado: ", aux_map_2$QTDE_VOT_CAND),
                  label = ~NOME_MICRO) %>% 
      addLegend(position = "bottomright", pal = pal_cor, values = ~PERC_VOT,
                labFormat = labelFormat(suffix = "%"))

  })

output$titulo_mapa_pres_meso <- renderText({
  
    est <- input$escolhe_est_pres_meso

    year <- input$ano_meso_pres

    cand <- input$escolhe_pres_meso

    if(input$pres_turno_meso == 'turno_1'){
        turno <- 1
      }
      else {
        turno <- 2
      }

    data_aux_titulo <- tse_data %>%
              filter(NOME_UF == est,
                     ANO_ELEICAO == year,
                     DESCRICAO_CARGO == "PRESIDENTE",
                     NOME_CANDIDATO == cand,
                     NUM_TURNO == turno) %>%
              mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
              select(ANO_ELEICAO, NOME_CANDIDATO, SIGLA_PARTIDO, QTDE_VOTOS, DESC_SIT_TOT_TURNO) %>%
              unique()                
  
  paste0(data_aux_titulo$NOME_CANDIDATO, " - ", data_aux_titulo$SIGLA_PARTIDO, " - ", data_aux_titulo$DESC_SIT_TOT_TURNO ,": ",
       data_aux_titulo$QTDE_VOTOS, " votos em ", data_aux_titulo$ANO_ELEICAO)
  
  })

output$mapa_pres_meso <- renderLeaflet({
    
    est <- input$escolhe_est_pres_meso

    year <- input$ano_meso_pres

    cand <- input$escolhe_pres_meso

    if(input$pres_turno_meso == 'turno_1'){
        turno <- 1
      }
      else {
        turno <- 2
      }


    aux_cap <- tse_capitais %>%
                filter(estados == est)

    aux_map <- mapa_meso %>% subset(ESTADO == aux_cap$id_uf)

    if(input$pres_choice_meso == 'orig_vot'){
      data_aux <- tse_data %>%
        filter(NOME_UF == est,
               ANO_ELEICAO == year,
               DESCRICAO_CARGO == "PRESIDENTE",
               NUM_TURNO == turno) %>%
        group_by(NOME_MESO) %>%
        mutate(QTDE_EST_TOT = sum(QTDE_VOTOS),
               NOME_MUNICIPIO = NULL,
               COD_MUN_IBGE = NULL,
               NOME_MICRO = NULL) %>%
        arrange(QTDE_EST_TOT) %>%
        ungroup() %>%
        filter(NOME_CANDIDATO == cand) %>%
        group_by(NOME_MESO) %>%
        mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
        unique() %>% ungroup() %>%
        mutate(QTDE_VOT_CAND = sum(QTDE_VOTOS),
               PERC_VOT = (QTDE_VOTOS/QTDE_VOT_CAND)*100)

        classes <- c(0, 0.1, 0.25, 0.5, 1, 2, 5, ceiling(max(data_aux$PERC_VOT)))
    } else {
      data_aux <- tse_data %>%
        filter(NOME_UF == est,
               ANO_ELEICAO == year,
               DESCRICAO_CARGO == "PRESIDENTE",
               NUM_TURNO == turno) %>%
        group_by(NOME_MESO) %>%
        mutate(QTDE_EST_TOT = sum(QTDE_VOTOS),
               NOME_MUNICIPIO = NULL,
               COD_MUN_IBGE = NULL,
               NOME_MICRO = NULL) %>%
        arrange(QTDE_EST_TOT) %>%
        ungroup() %>%
        filter(NOME_CANDIDATO == cand) %>%
        group_by(NOME_MESO) %>%
        mutate(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
        unique() %>% ungroup() %>%
        mutate(PERC_VOT = (QTDE_VOTOS/QTDE_EST_TOT)*100,
               QTDE_VOT_CAND = sum(QTDE_VOTOS))

        classes <- c(0, 5, 10, 20, 30, 35, 40, ceiling(max(data_aux$PERC_VOT)))
               
    }


    #aux_map_2 <- merge(x = aux_map, y = data_aux, by.x = "NOME_MESO", by.y = "NOME_MESO")

    aux_map_2 <- sp::merge(aux_map, data_aux, by = "NOME_MESO", duplicateGeoms = T)

    aux_map_2$QTDE_VOTOS[is.na(aux_map_2$QTDE_VOTOS)] <- 0
    aux_map_2$QTDE_EST_TOT[is.na(aux_map_2$QTDE_EST_TOT)] <- max(aux_map_2$QTDE_EST_TOT)
    aux_map_2$PERC_VOT[is.na(aux_map_2$PERC_VOT)] <- 0

    #classes <- c(0, 0.1, 0.25, 0.5, 1, 2, 5, ceiling(max(aux_map_2$PERC_VOT)))
    pal_cor <- colorBin(c("#6A1103", "#BA0004", "#E7400B", "#FEA527", "#FDFE65", "#9CE400", "#359800"), domain = aux_map_2$PERC_VOT, bins = unique(classes))


    leaflet(data = aux_map_2) %>% 
      addTiles('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
      addPolygons(weight = 0.5, fillColor = ~pal_cor(PERC_VOT), # Weight e a grossura das bordas
                  color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                  smoothFactor = 0.25,
                  popup = paste0(aux_map_2$NOME_MESO, "<br>",
                                 "Percentual de Votos: ", round(aux_map_2$PERC_VOT, 3), "%", "<br>",
                                 "Total de Votos para o candidato em ", aux_map_2$NOME_MESO, ": ", aux_map_2$QTDE_VOTOS, "<br>",
                                 "Total de Votos em ", aux_map_2$NOME_MESO, ": ", aux_map_2$QTDE_EST_TOT, "<br>",
                                 "Total de Votos para o candidato no Estado: ", aux_map_2$QTDE_VOT_CAND),
                  label = ~NOME_MESO) %>% 
      addLegend(position = "bottomright", pal = pal_cor, values = ~PERC_VOT,,
                labFormat = labelFormat(suffix = "%"))

  })
