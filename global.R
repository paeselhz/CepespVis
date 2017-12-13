library(shiny)
library(cepespR) #devtools::install_github("Cepesp-Fgv/cepesp-r")
library(dplyr)
library(feather)
library(D3plusR) #devtools::install_github('paulofelipe/D3plusR')
library(plotly)
library(shinythemes)
library(shinycssloaders) #devtools::install_github('andrewsali/shinycssloaders')
library(leaflet)
library(RColorBrewer)
library(sp)
library(stringi)
library(formattable)
library(shinyWidgets)

enableBookmarking(store = c("server"))

tse_data <- read_feather("cepesp_base_join_feather_v2.feather") %>%
				mutate(NOME_CANDIDATO = NOME_URNA_CANDIDATO,
					   NOME_URNA_CANDIDATO = NULL)

tse_cand <- tse_data %>%
                select(NOME_UF, NOME_CANDIDATO, ANO_ELEICAO, DESC_SIT_TOT_TURNO, DESCRICAO_CARGO, NUM_TURNO) %>%
                unique()


capitais <- c("SÃO PAULO", "RIO DE JANEIRO", "SALVADOR", "FORTALEZA", 
			  "BELO HORIZONTE", "MANAUS", "CURITIBA", "RECIFE", "PORTO ALEGRE",
			  "GOIÂNIA", "BELÉM", "SÃO LUÍS", "MACEIÓ", "NATAL", "CAMPO GRANDE",
			  "TERESINA", "JOÃO PESSOA", "ARACAJU", "CUIABÁ", "PORTO VELHO",
			  "FLORIANÓPOLIS", "MACAPÁ", "RIO BRANCO", "VITÓRIA", "BOA VISTA", "PALMAS", "BRASILIA")

estados <- c("São Paulo", "Rio de Janeiro", "Bahia", "Ceará", "Minas Gerais",
	         "Amazonas", "Paraná", "Pernambuco", "Rio Grande do Sul", "Goiás",
	         "Pará", "Maranhão", "Alagoas", "Rio Grande do Norte", "Mato Grosso do Sul",
	         "Piauí", "Paraíba", "Sergipe", "Mato Grosso", "Rondônia", "Santa Catarina",
	         "Amapá", "Acre", "Espírito Santo", "Roraima", "Tocantins", "Distrito Federal")

siglas <- c("SP", "RJ", "BA", "CE", "MG", "AM", "PR", "PE", "RS", "GO",
	        "PA", "MA", "AL", "RN", "MS", "PI", "PB", "SE", "MT", "RO",
	        "SC", "AP", "AC", "ES", "RR", "TO", "DF")

id_uf <- c(35, 33, 29, 23, 31, 13, 41, 26, 43, 52, 
	       15, 21, 27, 24, 50, 22, 25, 28, 51, 11,
	       42, 16, 12, 32, 14, 17, 53)

tse_capitais <- as_data_frame(estados)
tse_capitais[,2] <- as_data_frame(capitais)
tse_capitais[,3] <- as_data_frame(siglas)
tse_capitais[,4] <- as_data_frame(id_uf)
colnames(tse_capitais) <- c("estados", "capitais", "siglas", "id_uf")

rm(capitais, id_uf, estados, siglas)

tse_data_ui <- cepespdata(year=2014, 
                       position="President", 
                       regional_aggregation="Municipality", 
                       political_aggregation="Candidate", cache = TRUE) %>%
					  select(NOME_MUNICIPIO, NOME_UF, NOME_MICRO, NOME_MESO, COD_MUN_IBGE) %>%
					  mutate(ESTADO = as.integer(COD_MUN_IBGE/100000)) %>%
					  filter(ESTADO != 0) %>%
					  mutate(ESTADO = NULL) %>%
					  unique()

tse_data_ui_mun <- cepespdata(year=2016, 
                       position="Mayor", 
                       regional_aggregation="Municipality", 
                       political_aggregation="Candidate", cache = TRUE) %>%
					  select(NOME_MUNICIPIO, NOME_UF, NOME_MICRO, NOME_MESO, COD_MUN_IBGE) %>%
					  unique()

tse_qp <- readRDS("ELEC_QP.rds")

mapa_BR <- readRDS("MapaBrasil_v2.RDS")
mapa_BR$NOME_MUNICIPIO <- stri_conv(mapa_BR$NOME_MUNICIPIO, "latin1", "utf-8")

mapa_micro <- readRDS("MAPABRMICRO_v2.RDS")
mapa_micro$NOME_MICRO <- stri_conv(mapa_micro$NOME_MICRO, "latin1", "utf-8")
mapa_micro$NOME_MICRO_2 <- stri_conv(mapa_micro$NOME_MICRO_2, "latin1", "utf-8")

mapa_meso <- readRDS("MAPABRMESO_v2.RDS")
mapa_meso$NOME_MESO <- stri_conv(mapa_meso$NOME_MESO, "latin1", "utf-8")

mapa_estados <- readRDS("MAPABRESTADO_v2.RDS")

rm(filter_index)

