output$titulo_tab_dep_est_mun <- renderText({
	
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

output$tab_dep_est_mun <- renderFormattable({

	est <- input$escolhe_est_dep_est

    year <- input$ano_mun_dep_est

    cand <- input$escolhe_cand_dep_est

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
	           PERC_VOT_CAND = round((QTDE_VOTOS/QTDE_VOT_CAND)*100, 2),
	           PERC_VOT_REG = round((QTDE_VOTOS/QTDE_EST_TOT)*100, 2)) %>%
	    select(NOME_MUNICIPIO, QTDE_VOTOS, QTDE_EST_TOT, PERC_VOT_CAND, PERC_VOT_REG) %>%
	    arrange(desc(PERC_VOT_CAND)) %>%
	    unique()
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
	    mutate(PERC_VOT_REG = round((QTDE_VOTOS/QTDE_EST_TOT)*100, 2),
	           QTDE_VOT_CAND = sum(QTDE_VOTOS),
	           PERC_VOT_CAND = round((QTDE_VOTOS/QTDE_VOT_CAND)*100, 2)) %>%
	    select(NOME_MUNICIPIO, QTDE_VOTOS, QTDE_EST_TOT, PERC_VOT_CAND, PERC_VOT_REG) %>%
	    arrange(desc(PERC_VOT_REG)) %>%
	    unique()
	}

	colnames(data_aux) <- c("Nome do Município", "Votação do Candidato na Região", "Votação Total da Região", 
		                    "Percentual em Relação aos Votos do Candidato", "Percentual em Relação aos Votos da Região")

	data_aux_2 <- data_aux %>%
					mutate("Percentual em Relação aos Votos do Candidato" = paste0(`Percentual em Relação aos Votos do Candidato`, "%"),
						   "Percentual em Relação aos Votos da Região" = paste0(`Percentual em Relação aos Votos da Região`, "%")) 

	fixedWidth = 290

	formattable(data_aux_2,
		        align = c("l", "c", "c", "c", "c"))

	})

output$titulo_tab_dep_est_micro <- renderText({
	
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


output$tab_dep_est_micro <- renderFormattable({

    est <- input$escolhe_est_dep_est_micro

    year <- input$ano_micro_dep_est

    cand <- input$escolhe_cand_dep_est_micro

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
	           PERC_VOT_CAND = round((QTDE_VOTOS/QTDE_VOT_CAND)*100, 2),
	           PERC_VOT_REG = round((QTDE_VOTOS/QTDE_EST_TOT)*100, 2)) %>%
	    select(NOME_MICRO, QTDE_VOTOS, QTDE_EST_TOT, PERC_VOT_CAND, PERC_VOT_REG) %>%
	    arrange(desc(PERC_VOT_CAND)) %>%
	    unique()
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
	    mutate(QTDE_VOT_CAND = sum(QTDE_VOTOS),
	           PERC_VOT_CAND = round((QTDE_VOTOS/QTDE_VOT_CAND)*100, 2),
	           PERC_VOT_REG = round((QTDE_VOTOS/QTDE_EST_TOT)*100, 2)) %>%
	    select(NOME_MICRO, QTDE_VOTOS, QTDE_EST_TOT, PERC_VOT_CAND, PERC_VOT_REG) %>%
	    arrange(desc(PERC_VOT_REG)) %>%
	    unique()
	}

	colnames(data_aux) <- c("Nome da Microrregião", "Votação do Candidato na Região", "Votação Total da Região", 
		                    "Percentual em Relação aos Votos do Candidato", "Percentual em Relação aos Votos da Região")

	data_aux_2 <- data_aux %>%
					mutate("Percentual em Relação aos Votos do Candidato" = paste0(`Percentual em Relação aos Votos do Candidato`, "%"),
						   "Percentual em Relação aos Votos da Região" = paste0(`Percentual em Relação aos Votos da Região`, "%")) 	

	fixedWidth = 290

	formattable(data_aux_2,
		        align = c("l", "c", "c", "c", "c"))

	})

output$titulo_tab_dep_est_meso <- renderText({
	
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

output$tab_dep_est_meso <- renderFormattable({

    est <- input$escolhe_est_dep_est_meso

    year <- input$ano_meso_dep_est

    cand <- input$escolhe_cand_dep_est_meso

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
	           PERC_VOT_CAND = round((QTDE_VOTOS/QTDE_VOT_CAND)*100, 2),
	           PERC_VOT_REG = round((QTDE_VOTOS/QTDE_EST_TOT)*100, 2)) %>%
	    select(NOME_MESO, QTDE_VOTOS, QTDE_EST_TOT, PERC_VOT_CAND, PERC_VOT_REG) %>%
	    arrange(desc(PERC_VOT_CAND)) %>%
	    unique()
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
	    mutate(QTDE_VOT_CAND = sum(QTDE_VOTOS),
	           PERC_VOT_CAND = round((QTDE_VOTOS/QTDE_VOT_CAND)*100, 2),
	           PERC_VOT_REG = round((QTDE_VOTOS/QTDE_EST_TOT)*100, 2)) %>%
	    select(NOME_MESO, QTDE_VOTOS, QTDE_EST_TOT, PERC_VOT_CAND, PERC_VOT_REG) %>%
	    arrange(desc(PERC_VOT_REG)) %>%
	    unique()
	}

	colnames(data_aux) <- c("Nome da Mesorregião", "Votação do Candidato na Região", "Votação Total da Região", 
		                    "Percentual em Relação aos Votos do Candidato", "Percentual em Relação aos Votos da Região")

	data_aux_2 <- data_aux %>%
					mutate("Percentual em Relação aos Votos do Candidato" = paste0(`Percentual em Relação aos Votos do Candidato`, "%"),
						   "Percentual em Relação aos Votos da Região" = paste0(`Percentual em Relação aos Votos da Região`, "%")) 

	fixedWidth = 290

	formattable(data_aux_2,
		        align = c("l", "c", "c", "c", "c"))

	})

output$titulo_tab_senadores_mun <- renderText({
	
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

output$tab_senadores_mun <- renderFormattable({

    est <- input$escolhe_est_senador_mun

    year <- input$ano_mun_senador

    cand <- input$escolhe_senador_mun

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
	           PERC_VOT_CAND = round((QTDE_VOTOS/QTDE_VOT_CAND)*100, 2),
	           PERC_VOT_REG = round((QTDE_VOTOS/QTDE_EST_TOT)*100, 2)) %>%
	    select(NOME_MUNICIPIO, QTDE_VOTOS, QTDE_EST_TOT, PERC_VOT_CAND, PERC_VOT_REG) %>%
	    arrange(desc(PERC_VOT_CAND)) %>%
	    unique()
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
	    mutate(PERC_VOT_REG = round((QTDE_VOTOS/QTDE_EST_TOT)*100, 2),
	           QTDE_VOT_CAND = sum(QTDE_VOTOS),
	           PERC_VOT_CAND = round((QTDE_VOTOS/QTDE_VOT_CAND)*100, 2)) %>%
	    select(NOME_MUNICIPIO, QTDE_VOTOS, QTDE_EST_TOT, PERC_VOT_CAND, PERC_VOT_REG) %>%
	    arrange(desc(PERC_VOT_REG)) %>%
	    unique()
	}

	colnames(data_aux) <- c("Nome do Município", "Votação do Candidato na Região", "Votação Total da Região", 
		                    "Percentual em Relação aos Votos do Candidato", "Percentual em Relação aos Votos da Região")

	data_aux_2 <- data_aux %>%
					mutate("Percentual em Relação aos Votos do Candidato" = paste0(`Percentual em Relação aos Votos do Candidato`, "%"),
						   "Percentual em Relação aos Votos da Região" = paste0(`Percentual em Relação aos Votos da Região`, "%")) 

	fixedWidth = 290

	formattable(data_aux_2,
		        align = c("l", "c", "c", "c", "c"))

	})

output$titulo_tab_senadores_micro <- renderText({
	
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


output$tab_senadores_micro <- renderFormattable({

    est <- input$escolhe_est_senador_micro

    year <- input$ano_micro_senador

    cand <- input$escolhe_senador_micro

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
	           PERC_VOT_CAND = round((QTDE_VOTOS/QTDE_VOT_CAND)*100, 2),
	           PERC_VOT_REG = round((QTDE_VOTOS/QTDE_EST_TOT)*100, 2)) %>%
	    select(NOME_MICRO, QTDE_VOTOS, QTDE_EST_TOT, PERC_VOT_CAND, PERC_VOT_REG) %>%
	    arrange(desc(PERC_VOT_CAND)) %>%
	    unique()
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
	    mutate(QTDE_VOT_CAND = sum(QTDE_VOTOS),
	           PERC_VOT_CAND = round((QTDE_VOTOS/QTDE_VOT_CAND)*100, 2),
	           PERC_VOT_REG = round((QTDE_VOTOS/QTDE_EST_TOT)*100, 2)) %>%
	    select(NOME_MICRO, QTDE_VOTOS, QTDE_EST_TOT, PERC_VOT_CAND, PERC_VOT_REG) %>%
	    arrange(desc(PERC_VOT_REG)) %>%
	    unique()
	}

	colnames(data_aux) <- c("Nome da Microrregião", "Votação do Candidato na Região", "Votação Total da Região", 
		                    "Percentual em Relação aos Votos do Candidato", "Percentual em Relação aos Votos da Região")

	data_aux_2 <- data_aux %>%
					mutate("Percentual em Relação aos Votos do Candidato" = paste0(`Percentual em Relação aos Votos do Candidato`, "%"),
						   "Percentual em Relação aos Votos da Região" = paste0(`Percentual em Relação aos Votos da Região`, "%")) 	

	fixedWidth = 290

	formattable(data_aux_2,
		        align = c("l", "c", "c", "c", "c"))

	})

output$titulo_tab_senadores_meso <- renderText({
	
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

output$tab_senadores_meso <- renderFormattable({

    est <- input$escolhe_est_senador_meso

    year <- input$ano_meso_senador

    cand <- input$escolhe_senador_meso

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
	           PERC_VOT_CAND = round((QTDE_VOTOS/QTDE_VOT_CAND)*100, 2),
	           PERC_VOT_REG = round((QTDE_VOTOS/QTDE_EST_TOT)*100, 2)) %>%
	    select(NOME_MESO, QTDE_VOTOS, QTDE_EST_TOT, PERC_VOT_CAND, PERC_VOT_REG) %>%
	    arrange(desc(PERC_VOT_CAND)) %>%
	    unique()
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
	    mutate(QTDE_VOT_CAND = sum(QTDE_VOTOS),
	           PERC_VOT_CAND = round((QTDE_VOTOS/QTDE_VOT_CAND)*100, 2),
	           PERC_VOT_REG = round((QTDE_VOTOS/QTDE_EST_TOT)*100, 2)) %>%
	    select(NOME_MESO, QTDE_VOTOS, QTDE_EST_TOT, PERC_VOT_CAND, PERC_VOT_REG) %>%
	    arrange(desc(PERC_VOT_REG)) %>%
	    unique()
	}

	colnames(data_aux) <- c("Nome da Mesorregião", "Votação do Candidato na Região", "Votação Total da Região", 
		                    "Percentual em Relação aos Votos do Candidato", "Percentual em Relação aos Votos da Região")

	data_aux_2 <- data_aux %>%
					mutate("Percentual em Relação aos Votos do Candidato" = paste0(`Percentual em Relação aos Votos do Candidato`, "%"),
						   "Percentual em Relação aos Votos da Região" = paste0(`Percentual em Relação aos Votos da Região`, "%")) 

	fixedWidth = 290

	formattable(data_aux_2,
		        align = c("l", "c", "c", "c", "c"))

	})

output$titulo_tab_gov_mun <- renderText({
	
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

output$tab_gov_mun <- renderFormattable({

    est <- input$escolhe_est_gov_mun

    year <- input$ano_mun_gov

    cand <- input$escolhe_gov_mun

    if(input$gov_turno_mun == 'turno_1'){
        turno <- 1
      }
      else {
        turno <- 2
      }

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
	           PERC_VOT_CAND = round((QTDE_VOTOS/QTDE_VOT_CAND)*100, 2),
	           PERC_VOT_REG = round((QTDE_VOTOS/QTDE_EST_TOT)*100, 2)) %>%
	    select(NOME_MUNICIPIO, QTDE_VOTOS, QTDE_EST_TOT, PERC_VOT_CAND, PERC_VOT_REG) %>%
	    arrange(desc(PERC_VOT_CAND)) %>%
	    unique()
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
	    mutate(PERC_VOT_REG = round((QTDE_VOTOS/QTDE_EST_TOT)*100, 2),
	           QTDE_VOT_CAND = sum(QTDE_VOTOS),
	           PERC_VOT_CAND = round((QTDE_VOTOS/QTDE_VOT_CAND)*100, 2)) %>%
	    select(NOME_MUNICIPIO, QTDE_VOTOS, QTDE_EST_TOT, PERC_VOT_CAND, PERC_VOT_REG) %>%
	    arrange(desc(PERC_VOT_REG)) %>%
	    unique()
	}

	colnames(data_aux) <- c("Nome do Município", "Votação do Candidato na Região", "Votação Total da Região", 
		                    "Percentual em Relação aos Votos do Candidato", "Percentual em Relação aos Votos da Região")

	data_aux_2 <- data_aux %>%
					mutate("Percentual em Relação aos Votos do Candidato" = paste0(`Percentual em Relação aos Votos do Candidato`, "%"),
						   "Percentual em Relação aos Votos da Região" = paste0(`Percentual em Relação aos Votos da Região`, "%")) 

	fixedWidth = 290

	formattable(data_aux_2,
		        align = c("l", "c", "c", "c", "c"))

	})

output$titulo_tab_gov_micro <- renderText({
	
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


output$tab_gov_micro <- renderFormattable({

    est <- input$escolhe_est_gov_micro

    year <- input$ano_micro_gov

    cand <- input$escolhe_gov_micro

    if(input$gov_turno_micro == 'turno_1'){
        turno <- 1
      }
      else {
        turno <- 2
      }

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
	           PERC_VOT_CAND = round((QTDE_VOTOS/QTDE_VOT_CAND)*100, 2),
	           PERC_VOT_REG = round((QTDE_VOTOS/QTDE_EST_TOT)*100, 2)) %>%
	    select(NOME_MICRO, QTDE_VOTOS, QTDE_EST_TOT, PERC_VOT_CAND, PERC_VOT_REG) %>%
	    arrange(desc(PERC_VOT_CAND)) %>%
	    unique()
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
	    mutate(QTDE_VOT_CAND = sum(QTDE_VOTOS),
	           PERC_VOT_CAND = round((QTDE_VOTOS/QTDE_VOT_CAND)*100, 2),
	           PERC_VOT_REG = round((QTDE_VOTOS/QTDE_EST_TOT)*100, 2)) %>%
	    select(NOME_MICRO, QTDE_VOTOS, QTDE_EST_TOT, PERC_VOT_CAND, PERC_VOT_REG) %>%
	    arrange(desc(PERC_VOT_REG)) %>%
	    unique()
	}

	colnames(data_aux) <- c("Nome da Microrregião", "Votação do Candidato na Região", "Votação Total da Região", 
		                    "Percentual em Relação aos Votos do Candidato", "Percentual em Relação aos Votos da Região")

	data_aux_2 <- data_aux %>%
					mutate("Percentual em Relação aos Votos do Candidato" = paste0(`Percentual em Relação aos Votos do Candidato`, "%"),
						   "Percentual em Relação aos Votos da Região" = paste0(`Percentual em Relação aos Votos da Região`, "%")) 	

	fixedWidth = 290

	formattable(data_aux_2,
		        align = c("l", "c", "c", "c", "c"))

	})

output$titulo_tab_gov_meso <- renderText({
	
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

output$tab_gov_meso <- renderFormattable({

    est <- input$escolhe_est_gov_meso

    year <- input$ano_meso_gov

    cand <- input$escolhe_gov_meso

    if(input$gov_turno_meso == 'turno_1'){
        turno <- 1
      }
      else {
        turno <- 2
      }

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
	           PERC_VOT_CAND = round((QTDE_VOTOS/QTDE_VOT_CAND)*100, 2),
	           PERC_VOT_REG = round((QTDE_VOTOS/QTDE_EST_TOT)*100, 2)) %>%
	    select(NOME_MESO, QTDE_VOTOS, QTDE_EST_TOT, PERC_VOT_CAND, PERC_VOT_REG) %>%
	    arrange(desc(PERC_VOT_CAND)) %>%
	    unique()
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
	    mutate(QTDE_VOT_CAND = sum(QTDE_VOTOS),
	           PERC_VOT_CAND = round((QTDE_VOTOS/QTDE_VOT_CAND)*100, 2),
	           PERC_VOT_REG = round((QTDE_VOTOS/QTDE_EST_TOT)*100, 2)) %>%
	    select(NOME_MESO, QTDE_VOTOS, QTDE_EST_TOT, PERC_VOT_CAND, PERC_VOT_REG) %>%
	    arrange(desc(PERC_VOT_REG)) %>%
	    unique()
	}

	colnames(data_aux) <- c("Nome da Mesorregião", "Votação do Candidato na Região", "Votação Total da Região", 
		                    "Percentual em Relação aos Votos do Candidato", "Percentual em Relação aos Votos da Região")

	data_aux_2 <- data_aux %>%
					mutate("Percentual em Relação aos Votos do Candidato" = paste0(`Percentual em Relação aos Votos do Candidato`, "%"),
						   "Percentual em Relação aos Votos da Região" = paste0(`Percentual em Relação aos Votos da Região`, "%")) 

	fixedWidth = 290

	formattable(data_aux_2,
		        align = c("l", "c", "c", "c", "c"))

	})

output$titulo_tab_dep_fed_mun <- renderText({
	
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

output$tab_dep_fed_mun <- renderFormattable({

    est <- input$escolhe_est_dep_fed

    year <- input$ano_mun_dep_fed

    cand <- input$escolhe_cand_dep_fed

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
	           PERC_VOT_CAND = round((QTDE_VOTOS/QTDE_VOT_CAND)*100, 2),
	           PERC_VOT_REG = round((QTDE_VOTOS/QTDE_EST_TOT)*100, 2)) %>%
	    select(NOME_MUNICIPIO, QTDE_VOTOS, QTDE_EST_TOT, PERC_VOT_CAND, PERC_VOT_REG) %>%
	    arrange(desc(PERC_VOT_CAND)) %>%
	    unique()
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
	    mutate(PERC_VOT_REG = round((QTDE_VOTOS/QTDE_EST_TOT)*100, 2),
	           QTDE_VOT_CAND = sum(QTDE_VOTOS),
	           PERC_VOT_CAND = round((QTDE_VOTOS/QTDE_VOT_CAND)*100, 2)) %>%
	    select(NOME_MUNICIPIO, QTDE_VOTOS, QTDE_EST_TOT, PERC_VOT_CAND, PERC_VOT_REG) %>%
	    arrange(desc(PERC_VOT_REG)) %>%
	    unique()
	}

	colnames(data_aux) <- c("Nome do Município", "Votação do Candidato na Região", "Votação Total da Região", 
		                    "Percentual em Relação aos Votos do Candidato", "Percentual em Relação aos Votos da Região")

	data_aux_2 <- data_aux %>%
					mutate("Percentual em Relação aos Votos do Candidato" = paste0(`Percentual em Relação aos Votos do Candidato`, "%"),
						   "Percentual em Relação aos Votos da Região" = paste0(`Percentual em Relação aos Votos da Região`, "%")) 

	fixedWidth = 290

	formattable(data_aux_2,
		        align = c("l", "c", "c", "c", "c"))

	})

output$titulo_tab_dep_fed_micro <- renderText({
	
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


output$tab_dep_fed_micro <- renderFormattable({

    est <- input$escolhe_est_dep_fed_micro

    year <- input$ano_micro_dep_fed

    cand <- input$escolhe_cand_dep_fed_micro

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
	           PERC_VOT_CAND = round((QTDE_VOTOS/QTDE_VOT_CAND)*100, 2),
	           PERC_VOT_REG = round((QTDE_VOTOS/QTDE_EST_TOT)*100, 2)) %>%
	    select(NOME_MICRO, QTDE_VOTOS, QTDE_EST_TOT, PERC_VOT_CAND, PERC_VOT_REG) %>%
	    arrange(desc(PERC_VOT_CAND)) %>%
	    unique()
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
	    mutate(QTDE_VOT_CAND = sum(QTDE_VOTOS),
	           PERC_VOT_CAND = round((QTDE_VOTOS/QTDE_VOT_CAND)*100, 2),
	           PERC_VOT_REG = round((QTDE_VOTOS/QTDE_EST_TOT)*100, 2)) %>%
	    select(NOME_MICRO, QTDE_VOTOS, QTDE_EST_TOT, PERC_VOT_CAND, PERC_VOT_REG) %>%
	    arrange(desc(PERC_VOT_REG)) %>%
	    unique()
	}

	colnames(data_aux) <- c("Nome da Microrregião", "Votação do Candidato na Região", "Votação Total da Região", 
		                    "Percentual em Relação aos Votos do Candidato", "Percentual em Relação aos Votos da Região")

	data_aux_2 <- data_aux %>%
					mutate("Percentual em Relação aos Votos do Candidato" = paste0(`Percentual em Relação aos Votos do Candidato`, "%"),
						   "Percentual em Relação aos Votos da Região" = paste0(`Percentual em Relação aos Votos da Região`, "%")) 	

	fixedWidth = 290

	formattable(data_aux_2,
		        align = c("l", "c", "c", "c", "c"))

	})

output$titulo_tab_dep_fed_meso <- renderText({
	
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

output$tab_dep_fed_meso <- renderFormattable({

    est <- input$escolhe_est_dep_fed_meso

    year <- input$ano_meso_dep_fed

    cand <- input$escolhe_cand_dep_fed_meso

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
	           PERC_VOT_CAND = round((QTDE_VOTOS/QTDE_VOT_CAND)*100, 2),
	           PERC_VOT_REG = round((QTDE_VOTOS/QTDE_EST_TOT)*100, 2)) %>%
	    select(NOME_MESO, QTDE_VOTOS, QTDE_EST_TOT, PERC_VOT_CAND, PERC_VOT_REG) %>%
	    arrange(desc(PERC_VOT_CAND)) %>%
	    unique()
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
	    mutate(QTDE_VOT_CAND = sum(QTDE_VOTOS),
	           PERC_VOT_CAND = round((QTDE_VOTOS/QTDE_VOT_CAND)*100, 2),
	           PERC_VOT_REG = round((QTDE_VOTOS/QTDE_EST_TOT)*100, 2)) %>%
	    select(NOME_MESO, QTDE_VOTOS, QTDE_EST_TOT, PERC_VOT_CAND, PERC_VOT_REG) %>%
	    arrange(desc(PERC_VOT_REG)) %>%
	    unique()
	}

	colnames(data_aux) <- c("Nome da Mesorregião", "Votação do Candidato na Região", "Votação Total da Região", 
		                    "Percentual em Relação aos Votos do Candidato", "Percentual em Relação aos Votos da Região")

	data_aux_2 <- data_aux %>%
					mutate("Percentual em Relação aos Votos do Candidato" = paste0(`Percentual em Relação aos Votos do Candidato`, "%"),
						   "Percentual em Relação aos Votos da Região" = paste0(`Percentual em Relação aos Votos da Região`, "%")) 

	fixedWidth = 290

	formattable(data_aux_2,
		        align = c("l", "c", "c", "c", "c"))

	})

output$titulo_tab_pres_est <- renderText({
	
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

output$tab_pres_est <- renderFormattable({

    year <- input$ano_pres_est

    cand <- input$escolhe_pres_est

    if(input$pres_turno_est == 'turno_1'){
        turno <- 1
      }
      else {
        turno <- 2
      }

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
	           PERC_VOT_CAND = round((QTDE_VOTOS/QTDE_VOT_CAND)*100, 2),
	           PERC_VOT_REG = round((QTDE_VOTOS/QTDE_EST_TOT)*100, 2)) %>%
	    select(NOME_UF, QTDE_VOTOS, QTDE_EST_TOT, PERC_VOT_CAND, PERC_VOT_REG) %>%
	    arrange(desc(PERC_VOT_CAND)) %>%
	    unique()
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
	    mutate(QTDE_VOT_CAND = sum(QTDE_VOTOS),
	           PERC_VOT_CAND = round((QTDE_VOTOS/QTDE_VOT_CAND)*100, 2),
	           PERC_VOT_REG = round((QTDE_VOTOS/QTDE_EST_TOT)*100, 2)) %>%
	    select(NOME_UF, QTDE_VOTOS, QTDE_EST_TOT, PERC_VOT_CAND, PERC_VOT_REG) %>%
	    arrange(desc(PERC_VOT_REG)) %>%
	    unique()
	}

	colnames(data_aux) <- c("Nome do Estado", "Votação do Candidato no Estado", "Votação Total no Estado", 
		                    "Percentual em Relação aos Votos do Candidato", "Percentual em Relação aos Votos do Estado")

	data_aux_2 <- data_aux %>%
					mutate("Percentual em Relação aos Votos do Candidato" = paste0(`Percentual em Relação aos Votos do Candidato`, "%"),
						   "Percentual em Relação aos Votos do Estado" = paste0(`Percentual em Relação aos Votos do Estado`, "%")) 

	fixedWidth = 290

	formattable(data_aux_2,
		        align = c("l", "c", "c", "c", "c"))

	})

output$titulo_tab_pres_mun <- renderText({
	
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

output$tab_pres_mun <- renderFormattable({

    est <- input$escolhe_est_pres_mun

    year <- input$ano_mun_pres

    cand <- input$escolhe_pres_mun

    if(input$pres_turno_mun == 'turno_1'){
        turno <- 1
      }
      else {
        turno <- 2
      }

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
	           PERC_VOT_CAND = round((QTDE_VOTOS/QTDE_VOT_CAND)*100, 2),
	           PERC_VOT_REG = round((QTDE_VOTOS/QTDE_EST_TOT)*100, 2)) %>%
	    select(NOME_MUNICIPIO, QTDE_VOTOS, QTDE_EST_TOT, PERC_VOT_CAND, PERC_VOT_REG) %>%
	    arrange(desc(PERC_VOT_CAND)) %>%
	    unique()
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
	    mutate(PERC_VOT_REG = round((QTDE_VOTOS/QTDE_EST_TOT)*100, 2),
	           QTDE_VOT_CAND = sum(QTDE_VOTOS),
	           PERC_VOT_CAND = round((QTDE_VOTOS/QTDE_VOT_CAND)*100, 2)) %>%
	    select(NOME_MUNICIPIO, QTDE_VOTOS, QTDE_EST_TOT, PERC_VOT_CAND, PERC_VOT_REG) %>%
	    arrange(desc(PERC_VOT_REG)) %>%
	    unique()
	}

	colnames(data_aux) <- c("Nome do Município", "Votação do Candidato na Região", "Votação Total da Região", 
		                    "Percentual em Relação aos Votos do Candidato", "Percentual em Relação aos Votos da Região")

	data_aux_2 <- data_aux %>%
					mutate("Percentual em Relação aos Votos do Candidato" = paste0(`Percentual em Relação aos Votos do Candidato`, "%"),
						   "Percentual em Relação aos Votos da Região" = paste0(`Percentual em Relação aos Votos da Região`, "%")) 

	fixedWidth = 290

	formattable(data_aux_2,
		        align = c("l", "c", "c", "c", "c"))

	})

output$titulo_tab_pres_micro <- renderText({
	
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


output$tab_pres_micro <- renderFormattable({

    est <- input$escolhe_est_pres_micro

    year <- input$ano_micro_pres

    cand <- input$escolhe_pres_micro

    if(input$pres_turno_micro == 'turno_1'){
        turno <- 1
      }
      else {
        turno <- 2
      }

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
	           PERC_VOT_CAND = round((QTDE_VOTOS/QTDE_VOT_CAND)*100, 2),
	           PERC_VOT_REG = round((QTDE_VOTOS/QTDE_EST_TOT)*100, 2)) %>%
	    select(NOME_MICRO, QTDE_VOTOS, QTDE_EST_TOT, PERC_VOT_CAND, PERC_VOT_REG) %>%
	    arrange(desc(PERC_VOT_CAND)) %>%
	    unique()
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
	    mutate(QTDE_VOT_CAND = sum(QTDE_VOTOS),
	           PERC_VOT_CAND = round((QTDE_VOTOS/QTDE_VOT_CAND)*100, 2),
	           PERC_VOT_REG = round((QTDE_VOTOS/QTDE_EST_TOT)*100, 2)) %>%
	    select(NOME_MICRO, QTDE_VOTOS, QTDE_EST_TOT, PERC_VOT_CAND, PERC_VOT_REG) %>%
	    arrange(desc(PERC_VOT_REG)) %>%
	    unique()
	}

	colnames(data_aux) <- c("Nome da Microrregião", "Votação do Candidato na Região", "Votação Total da Região", 
		                    "Percentual em Relação aos Votos do Candidato", "Percentual em Relação aos Votos da Região")

	data_aux_2 <- data_aux %>%
					mutate("Percentual em Relação aos Votos do Candidato" = paste0(`Percentual em Relação aos Votos do Candidato`, "%"),
						   "Percentual em Relação aos Votos da Região" = paste0(`Percentual em Relação aos Votos da Região`, "%")) 	

	fixedWidth = 290

	formattable(data_aux_2,
		        align = c("l", "c", "c", "c", "c"))

	})

output$titulo_tab_pres_meso <- renderText({
	
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

output$tab_pres_meso <- renderFormattable({

    est <- input$escolhe_est_pres_meso

    year <- input$ano_meso_pres

    cand <- input$escolhe_pres_meso

    if(input$pres_turno_meso == 'turno_1'){
        turno <- 1
      }
      else {
        turno <- 2
      }

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
	           PERC_VOT_CAND = round((QTDE_VOTOS/QTDE_VOT_CAND)*100, 2),
	           PERC_VOT_REG = round((QTDE_VOTOS/QTDE_EST_TOT)*100, 2)) %>%
	    select(NOME_MESO, QTDE_VOTOS, QTDE_EST_TOT, PERC_VOT_CAND, PERC_VOT_REG) %>%
	    arrange(desc(PERC_VOT_CAND)) %>%
	    unique()
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
	    mutate(QTDE_VOT_CAND = sum(QTDE_VOTOS),
	           PERC_VOT_CAND = round((QTDE_VOTOS/QTDE_VOT_CAND)*100, 2),
	           PERC_VOT_REG = round((QTDE_VOTOS/QTDE_EST_TOT)*100, 2)) %>%
	    select(NOME_MESO, QTDE_VOTOS, QTDE_EST_TOT, PERC_VOT_CAND, PERC_VOT_REG) %>%
	    arrange(desc(PERC_VOT_REG)) %>%
	    unique()
	}

	colnames(data_aux) <- c("Nome da Mesorregião", "Votação do Candidato na Região", "Votação Total da Região", 
		                    "Percentual em Relação aos Votos do Candidato", "Percentual em Relação aos Votos da Região")

	data_aux_2 <- data_aux %>%
					mutate("Percentual em Relação aos Votos do Candidato" = paste0(`Percentual em Relação aos Votos do Candidato`, "%"),
						   "Percentual em Relação aos Votos da Região" = paste0(`Percentual em Relação aos Votos da Região`, "%")) 

	fixedWidth = 290

	formattable(data_aux_2,
		        align = c("l", "c", "c", "c", "c"))

	})