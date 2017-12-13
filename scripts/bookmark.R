   setBookmarkExclude(c("bookmark1", "bookmark2", "bookmark3", "bookmark4", "bookmark5", "bookmark6",
   						"bookmark7", "bookmark8", "bookmark9", "bookmark10", "bookmark11", "bookmark12",
   						"bookmark13", "bookmark14", "bookmark15", "bookmark16", "bookmark17", "bookmark18",
   						"bookmark19", "bookmark20", "bookmark21", "bookmark22", "bookmark23", "bookmark24"))

   observeEvent(input$bookmark1, {
        session$doBookmark()
   })

   observeEvent(input$bookmark2, {
        session$doBookmark()
   })

   observeEvent(input$bookmark3, {
        session$doBookmark()
   })

   observeEvent(input$bookmark4, {
        session$doBookmark()
   })

	observeEvent(input$bookmark5, {
	        session$doBookmark()
	   })

	observeEvent(input$bookmark6, {
	        session$doBookmark()
	   })

	observeEvent(input$bookmark7, {
	        session$doBookmark()
	   })

	observeEvent(input$bookmark8, {
	        session$doBookmark()
	   })

	observeEvent(input$bookmark9, {
	        session$doBookmark()
	   })

	observeEvent(input$bookmark10, {
	        session$doBookmark()
	   })

	observeEvent(input$bookmark11, {
        session$doBookmark()
   })

	observeEvent(input$bookmark12, {
        session$doBookmark()
   })

	observeEvent(input$bookmark13, {
        session$doBookmark()
   })

	observeEvent(input$bookmark14, {
        session$doBookmark()
   })

	observeEvent(input$bookmark15, {
        session$doBookmark()
   })

	observeEvent(input$bookmark16, {
        session$doBookmark()
   })

	observeEvent(input$bookmark17, {
        session$doBookmark()
   })

	observeEvent(input$bookmark18, {
        session$doBookmark()
   })

	observeEvent(input$bookmark19, {
        session$doBookmark()
   })

	observeEvent(input$bookmark20, {
        session$doBookmark()
   })

	observeEvent(input$bookmark21, {
        session$doBookmark()
   })

	observeEvent(input$bookmark22, {
        session$doBookmark()
   })

	observeEvent(input$bookmark23, {
        session$doBookmark()
   })

	observeEvent(input$bookmark24, {
        session$doBookmark()
   })

	onBookmark(function(state){
		#PREFEITO
		state$values$estado_prefeito <- input$escolhe_est_prefeito
		state$values$municipio_prefeito <- input$escolhe_mun_prefeito
		#VEREADOR
		state$values$estado_vereador <-input$escolhe_est_vereador
		state$values$municipio_vereador <- input$escolhe_mun_vereador
		#DEPUTADO ESTADUAL ESTADO
		state$values$estado_deputado_estadual_estado <- input$escolhe_est_dep_est_est
		#DEPUTADO ESTADUAL MUNICIPIO
		state$values$estado_deputado_estadual_municipio <- input$escolhe_est_dep_est
		state$values$candidato_deputado_estadual_municipio <- input$escolhe_cand_dep_est
		#DEPUTADO ESTADUAL MICRORREGIAO
		state$values$estado_deputado_estadual_microrregiao <- input$escolhe_est_dep_est_micro
		state$values$candidato_deputado_estadual_microrregiao <- input$escolhe_cand_dep_est_micro
		#DEPUTADO ESTADUAL MESORREGIAO
		state$values$estado_deputado_estadual_mesorregiao <- input$escolhe_est_dep_est_meso
		state$values$candidato_deputado_estadual_mesorregiao <- input$escolhe_cand_dep_est_meso
		#SENADOR ESTADO
		state$values$estado_senador_estado <- input$escolhe_est_senador_est
		#SENADOR MUNICIPIO
		state$values$estado_senador_municipio <- input$escolhe_est_senador_mun
		state$values$candidato_senador_municipio <- input$escolhe_senador_mun
		#SENADOR MICRORREGIAO
		state$values$estado_senador_microrregiao <- input$escolhe_est_senador_micro
		state$values$candidato_senador_microrregiao <- input$escolhe_senador_micro
		#SENADOR MESORREGIAO
		state$values$estado_senador_mesorregiao <- input$escolhe_est_senador_meso
		state$values$candidato_senador_mesorregiao <- input$escolhe_senador_meso
		#GOVERNADOR ESTADO
		state$values$estado_governador_estado <- input$escolhe_est_gov_est
		#GOVERNADOR MUNICIPIO
		state$values$estado_governador_municipio <- input$escolhe_est_gov_mun
		state$values$candidato_governador_municipio <- input$escolhe_gov_mun
		#GOVERNADOR MICRORREGIAO
		state$values$estado_governador_microrregiao <- input$escolhe_est_gov_micro
		state$values$candidato_governador_microrregiao <- input$escolhe_gov_micro
		#GOVERNADOR MESORREGIAO
		state$values$estado_governador_mesorregiao <- input$escolhe_est_gov_meso
		state$values$candidato_governador_mesorregiao <- input$escolhe_gov_meso
		#DEPUTADO FEDERAL ESTADO
		state$values$estado_deputado_federal_estado <- input$escolhe_est_dep_fed_est
		#DEPUTADO FEDERAL MUNICIPIO
		state$values$estado_deputado_federal_municipio <- input$escolhe_est_dep_fed
		state$values$candidato_deputado_federal_municipio <- input$escolhe_cand_dep_fed
		#DEPUTADO FEDERAL MICRORREGIAO
		state$values$estado_deputado_federal_microrregiao <- input$escolhe_est_dep_fed_micro
		state$values$candidato_deputado_federal_microrregiao <- input$escolhe_cand_dep_fed_micro
		#DEPUTADO FEDERAL MESORREGIAO
		state$values$estado_deputado_federal_mesorregiao <- input$escolhe_est_dep_fed_meso
		state$values$candidato_deputado_federal_mesorregiao <- input$escolhe_cand_dep_fed_meso
		#PRESIDENTE ESTADO
		state$values$candidato_presidente_estado <- input$escolhe_pres_est
		#PRESIDENTE MUNICIPIO
		state$values$estado_presidente_municipio <- input$escolhe_est_pres_mun
		state$values$candidato_presidente_municipio <- input$escolhe_pres_mun
		#PRESIDENTE MICRORREGIAO
		state$values$estado_presidente_microrregiao <- input$escolhe_est_pres_micro
		state$values$candidato_presidente_microrregiao <- input$escolhe_pres_micro
		#PRESIDENTE MESORREGIAO
		state$values$estado_presidente_mesorregiao <- input$escolhe_est_pres_meso
		state$values$candidato_presidente_mesorregiao <- input$escolhe_pres_meso
		})

	onRestored(function(state){
		#PREFEITO
		updateSelectInput(session, 'escolhe_est_prefeito', selected = state$values$estado_prefeito)
		updateSelectInput(session, 'escolhe_mun_prefeito', selected = state$values$municipio_prefeito)
		#VEREADOR
		updateSelectInput(session, 'escolhe_est_vereador', selected = state$values$estado_vereador)
		updateSelectInput(session, 'escolhe_mun_vereador', selected = state$values$municipio_vereador)
		#DEPUTADO ESTADUAL ESTADO
		updateSelectInput(session, 'escolhe_est_dep_est_est', selected = state$values$estado_deputado_estadual_estado)
		#DEPUTADO ESTADUAL MUNICIPIO
		updateSelectInput(session, 'escolhe_est_dep_est', selected = state$values$estado_deputado_estadual_municipio)
		updateSelectInput(session, 'escolhe_cand_dep_est', selected = state$values$candidato_deputado_estadual_municipio)
		#DEPUTADO ESTADUAL MICRORREGIAO
		updateSelectInput(session, 'escolhe_est_dep_est_micro', selected = state$values$estado_deputado_estadual_microrregiao)
		updateSelectInput(session, 'escolhe_cand_dep_est_micro', selected = state$values$candidato_deputado_estadual_microrregiao)
		#DEPUTADO ESTADUAL MESORREGIAO
		updateSelectInput(session, 'escolhe_est_dep_est_meso', selected = state$values$estado_deputado_estadual_mesorregiao)
		updateSelectInput(session, 'escolhe_cand_dep_est_meso', selected = state$values$candidato_deputado_estadual_mesorregiao)
		#SENADOR ESTADO
		updateSelectInput(session, 'escolhe_est_senador_est', selected = state$values$estado_senador_estado)
		#SENADOR MUNICIPIO
		updateSelectInput(session, 'escolhe_est_senador_mun', selected = state$values$estado_senador_municipio)
		updateSelectInput(session, 'escolhe_senador_mun', selected = state$values$candidato_senador_municipio)
		#SENADOR MICRORREGIAO
		updateSelectInput(session, 'escolhe_est_senador_micro', selected = state$values$estado_senador_microrregiao)
		updateSelectInput(session, 'escolhe_senador_micro', selected = state$values$candidato_senador_microrregiao)
		#SENADOR MESORREGIAO
		updateSelectInput(session, 'escolhe_est_senador_meso', selected = state$values$estado_senador_mesorregiao)
		updateSelectInput(session, 'escolhe_senador_meso', selected = state$values$candidato_senador_mesorregiao)
		#GOVERNADOR ESTADO
		updateSelectInput(session, 'escolhe_est_gov_est', selected = state$values$estado_governador_estado)
		#GOVERNADOR MUNICIPIO
		updateSelectInput(session, 'escolhe_est_gov_mun', selected = state$values$estado_governador_municipio)
		updateSelectInput(session, 'escolhe_gov_mun', selected = state$values$candidato_governador_municipio)
		#GOVERNADOR MICRORREGIAO
		updateSelectInput(session, 'escolhe_est_gov_micro', selected = state$values$estado_governador_microrregiao)
		updateSelectInput(session, 'escolhe_gov_micro', selected = state$values$candidato_governador_microrregiao)
		#GOVERNADOR MESORREGIAO
		updateSelectInput(session, 'escolhe_est_gov_meso', selected = state$values$estado_governador_mesorregiao)
		updateSelectInput(session, 'escolhe_gov_meso', selected = state$values$candidato_governador_mesorregiao)
		#DEPUTADO FEDERAL ESTADO
		updateSelectInput(session, 'escolhe_est_dep_fed_est', selected = state$values$estado_deputado_federal_estado)
		#DEPUTADO FEDERAL MUNICIPIO
		updateSelectInput(session, 'escolhe_est_dep_fed', selected = state$values$estado_deputado_federal_municipio)
		updateSelectInput(session, 'escolhe_cand_dep_fed', selected = state$values$candidato_deputado_federal_municipio)
		#DEPUTADO FEDERAL MICRORREGIAO
		updateSelectInput(session, 'escolhe_est_dep_fed_micro', selected = state$values$estado_deputado_federal_microrregiao)
		updateSelectInput(session, 'escolhe_cand_dep_fed_micro', selected = state$values$candidato_deputado_federal_microrregiao)
		#DEPUTADO FEDERAL MESORREGIAO
		updateSelectInput(session, 'escolhe_est_dep_fed_meso', selected = state$values$estado_deputado_federal_mesorregiao)
		updateSelectInput(session, 'escolhe_cand_dep_fed_meso', selected = state$values$candidato_deputado_federal_mesorregiao)
		#PRESIDENTE ESTADO
		updateSelectInput(session, 'escolhe_pres_est', selected = state$values$candidato_presidente_estado)
		#PRESIDENTE MUNICIPIO
		updateSelectInput(session, 'escolhe_est_pres_mun', selected = state$values$estado_presidente_municipio)
		updateSelectInput(session, 'escolhe_pres_mun', selected = state$values$candidato_presidente_municipio)
		#PRESIDENTE MICRORREGIAO
		updateSelectInput(session, 'escolhe_est_pres_micro', selected = state$values$estado_presidente_microrregiao)
		updateSelectInput(session, 'escolhe_pres_micro', selected = state$values$candidato_presidente_microrregiao)
		#PRESIDENTE MESORREGIAO
		updateSelectInput(session, 'escolhe_est_pres_meso', selected = state$values$estado_presidente_mesorregiao)
		updateSelectInput(session, 'escolhe_pres_meso', selected = state$values$candidato_presidente_mesorregiao)
		})


