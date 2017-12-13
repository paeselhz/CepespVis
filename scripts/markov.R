output$mapa_pres_markov <- renderLeaflet({
    
    year <- input$ano_markov_pais

    cand_eleito <- input$markov_escolhe_presidente

    est <- input$markov_state_choice

    if(input$checkbox_markov_pres == 'markov_mun_choice'){

    	aux_cap <- tse_capitais %>%
                filter(estados == est)

    	aux_map <- mapa_BR %>% subset(ESTADO == aux_cap$id_uf)
	   	
	   	data_aux_1 <- tse_data %>%
		  filter(ANO_ELEICAO == year,
		         DESCRICAO_CARGO == "PRESIDENTE",
		         NUM_TURNO == 1,
		         NOME_UF == est) %>%
		  group_by(COD_MUN_IBGE) %>%
		  mutate(RANK = row_number(-QTDE_VOTOS)) %>%
		  arrange(NOME_MUNICIPIO, desc(RANK)) %>%
		  filter(RANK == 1) %>%
		  mutate(ANTES = ifelse(NOME_CANDIDATO == cand_eleito, 1, 0)) %>%
		  select(COD_MUN_IBGE, ANTES)

		data_aux_2 <- tse_data %>%
		  filter(ANO_ELEICAO == year,
		         DESCRICAO_CARGO == "PRESIDENTE",
		         NUM_TURNO == 2,
		         NOME_UF == est) %>%
		  group_by(COD_MUN_IBGE) %>%
		  mutate(RANK2 = row_number(-QTDE_VOTOS)) %>%
		  arrange(COD_MUN_IBGE, desc(RANK2)) %>%
		  filter(RANK2 == 1) %>%
		  mutate(DEPOIS = ifelse(NOME_CANDIDATO == cand_eleito, 1, 0)) %>%
		  select(COD_MUN_IBGE, DEPOIS)

		data_aux_3 <- merge(x = data_aux_1, y = data_aux_2, 
		                    by = "COD_MUN_IBGE")

		cores_T <- c("#d61111", "#b0db23", "#068722", "#fffa00")

		tooltip_T <- c(paste0("Continuou sem votar em ", cand_eleito, "<br>"),
		               paste0("Passou a votar em ", cand_eleito, "<br>"),
		               paste0("Continuou a votar em ", cand_eleito, "<br>"),
		               paste0("Deixou de votar em ", cand_eleito, "<br>"))

		df_pre <- data_aux_3 %>%
		  mutate(COR = ifelse(ANTES == 0 & DEPOIS == 0, 
		                      cores_T[1], 
		                      ifelse(ANTES == 0 & DEPOIS > 0, 
		                             cores_T[2],
		                             ifelse(ANTES > 0 & DEPOIS > 0, 
		                                    cores_T[3], cores_T[4]))),
		         HOVER = ifelse(ANTES == 0 & DEPOIS == 0, 
		                        tooltip_T[1], 
		                        ifelse(ANTES == 0 & DEPOIS > 0, 
		                               tooltip_T[2],
		                               ifelse(ANTES > 0 & DEPOIS > 0, 
		                                      tooltip_T[3], tooltip_T[4]))))

		aux_map_2 <- sp::merge(x = aux_map, y = df_pre, by.x = "GEOCODIGO", by.y = "COD_MUN_IBGE")

		leaflet(data = aux_map_2) %>% 
		  addTiles('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
		  addPolygons(weight = 0.5, fillColor = ~COR, # Weight e a grossura das bordas
		              color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
		              smoothFactor = 0.25,
		              popup = paste0(aux_map_2$NOME_MUNICIPIO, "<br>",
		                             aux_map_2$HOVER),
		              label = ~NOME_MUNICIPIO) %>% 
		  addLegend(position = "bottomright", colors = ~unique(COR), 
		            labels = ~unique(HOVER))
	   
    } else if(input$checkbox_markov_pres == 'markov_meso_choice') {
		    data_aux_1 <- tse_data %>%
			  filter(ANO_ELEICAO == year,
			         DESCRICAO_CARGO == "PRESIDENTE",
			         NUM_TURNO == 1) %>%
			  group_by(NOME_MESO) %>%
			  mutate(RANK = row_number(-QTDE_VOTOS)) %>%
			  arrange(NOME_MESO, desc(RANK)) %>%
			  filter(RANK == 1) %>%
			  mutate(ANTES = ifelse(NOME_CANDIDATO == cand_eleito, 1, 0)) %>%
			  select(NOME_MESO, ANTES)

			data_aux_2 <- tse_data %>%
			  filter(ANO_ELEICAO == year,
			         DESCRICAO_CARGO == "PRESIDENTE",
			         NUM_TURNO == 2) %>%
			  group_by(NOME_MESO) %>%
			  mutate(RANK2 = row_number(-QTDE_VOTOS)) %>%
			  arrange(NOME_MESO, desc(RANK2)) %>%
			  filter(RANK2 == 1) %>%
			  mutate(DEPOIS = ifelse(NOME_CANDIDATO == cand_eleito, 1, 0)) %>%
			  select(NOME_MESO, DEPOIS)

			data_aux_3 <- merge(x = data_aux_1, y = data_aux_2, 
			                    by = "NOME_MESO")

			cores_T <- c("#d61111", "#b0db23", "#068722", "#fffa00")

			tooltip_T <- c(paste0("Continuou sem votar em ", cand_eleito, "<br>"),
			               paste0("Passou a votar em ", cand_eleito, "<br>"),
			               paste0("Continuou a votar em ", cand_eleito, "<br>"),
			               paste0("Deixou de votar em ", cand_eleito, "<br>"))

			df_pre <- data_aux_3 %>%
			  mutate(COR = ifelse(ANTES == 0 & DEPOIS == 0, 
			                      cores_T[1], 
			                      ifelse(ANTES == 0 & DEPOIS > 0, 
			                             cores_T[2],
			                             ifelse(ANTES > 0 & DEPOIS > 0, 
			                                    cores_T[3], cores_T[4]))),
			         HOVER = ifelse(ANTES == 0 & DEPOIS == 0, 
			                        tooltip_T[1], 
			                        ifelse(ANTES == 0 & DEPOIS > 0, 
			                               tooltip_T[2],
			                               ifelse(ANTES > 0 & DEPOIS > 0, 
			                                      tooltip_T[3], tooltip_T[4]))))

			aux_map_2 <- sp::merge(x = mapa_meso, y = df_pre, by = "NOME_MESO")

			leaflet(data = aux_map_2) %>% 
			  addTiles('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
			  addPolygons(weight = 0.5, fillColor = ~COR, # Weight e a grossura das bordas
			              color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
			              smoothFactor = 0.25,
			              popup = paste0(aux_map_2$NOME_MESO, "<br>",
			                             aux_map_2$HOVER),
			              label = ~NOME_MESO) %>% 
			  addLegend(position = "bottomright", colors = ~unique(COR), 
			            labels = ~unique(HOVER))
    } else {

	   	data_aux_1 <- tse_data %>%
		  filter(ANO_ELEICAO == year,
		         DESCRICAO_CARGO == "PRESIDENTE",
		         NUM_TURNO == 1) %>%
		  mutate(NOME_MICRO_2 = paste0(NOME_UF, " - ", NOME_MICRO)) %>%
		  group_by(NOME_MICRO_2) %>%
		  mutate(RANK = row_number(-QTDE_VOTOS)) %>%
		  arrange(NOME_MICRO_2, desc(RANK)) %>%
		  filter(RANK == 1) %>%
		  mutate(ANTES = ifelse(NOME_CANDIDATO == cand_eleito, 1, 0)) %>%
		  select(NOME_MICRO_2, ANTES)

		data_aux_2 <- tse_data %>%
		  filter(ANO_ELEICAO == year,
		         DESCRICAO_CARGO == "PRESIDENTE",
		         NUM_TURNO == 2) %>%
		  mutate(NOME_MICRO_2 = paste0(NOME_UF, " - ", NOME_MICRO)) %>%
		  group_by(NOME_MICRO_2) %>%
		  mutate(RANK2 = row_number(-QTDE_VOTOS)) %>%
		  arrange(NOME_MICRO_2, desc(RANK2)) %>%
		  filter(RANK2 == 1) %>%
		  mutate(DEPOIS = ifelse(NOME_CANDIDATO == cand_eleito, 1, 0)) %>%
		  select(NOME_MICRO_2, DEPOIS)

		data_aux_3 <- merge(x = data_aux_1, y = data_aux_2, 
		                    by = "NOME_MICRO_2")

		cores_T <- c("#d61111", "#b0db23", "#068722", "#fffa00")

		tooltip_T <- c(paste0("Continuou sem votar em ", cand_eleito, "<br>"),
		               paste0("Passou a votar em ", cand_eleito, "<br>"),
		               paste0("Continuou a votar em ", cand_eleito, "<br>"),
		               paste0("Deixou de votar em ", cand_eleito, "<br>"))

		df_pre <- data_aux_3 %>%
		  mutate(COR = ifelse(ANTES == 0 & DEPOIS == 0, 
		                      cores_T[1], 
		                      ifelse(ANTES == 0 & DEPOIS > 0, 
		                             cores_T[2],
		                             ifelse(ANTES > 0 & DEPOIS > 0, 
		                                    cores_T[3], cores_T[4]))),
		         HOVER = ifelse(ANTES == 0 & DEPOIS == 0, 
		                        tooltip_T[1], 
		                        ifelse(ANTES == 0 & DEPOIS > 0, 
		                               tooltip_T[2],
		                               ifelse(ANTES > 0 & DEPOIS > 0, 
		                                      tooltip_T[3], tooltip_T[4]))))

		aux_map_2 <- sp::merge(x = mapa_micro, y = df_pre, by = "NOME_MICRO_2")

		leaflet(data = aux_map_2) %>% 
		  addTiles('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
		  addPolygons(weight = 0.5, fillColor = ~COR, # Weight e a grossura das bordas
		              color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
		              smoothFactor = 0.25,
		              popup = paste0(aux_map_2$NOME_MICRO_2, "<br>",
		                             aux_map_2$HOVER),
		              label = ~NOME_MICRO_2) %>% 
		  addLegend(position = "bottomright", colors = ~unique(COR), 
		            labels = ~unique(HOVER))

    }

    })
