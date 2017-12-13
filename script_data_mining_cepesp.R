library(readr)
library(tibble)
library(dplyr)
library(feather)

# setwd("/home/paeselab-rstudio")
setwd("C:/Users/Luis Paese/Desktop/cepespvis")
dir.create("cepesp_data")

years <- c(1998,2000,2002,2004,2006,2008,2010,2012,2014,2016)

for(ano in 1:length(years)){
  year_use <- years[ano]
  
  if(year_use%%4 != 0){
    position <- c(1,3,5,6,7)
  }
  else {
    position <- c(11,13)
  }
  for(cargo in 1:length(position)){
    cargo_use <- position[cargo]
    nome_url <- paste0("http://cepesp.io/api/consulta/tse?ano=", year_use,
                       "&cargo=", cargo_use,"&agregacao_regional=6&agregacao_politica=2",
                       "&selected_columns[]=NOME_MUNICIPIO",
                       "&selected_columns[]=QTDE_VOTOS",
                       "&selected_columns[]=DESC_SIT_TOT_TURNO",
                       "&selected_columns[]=NOME_UF",
                       "&selected_columns[]=SIGLA_PARTIDO",
                       "&selected_columns[]=NOME_MICRO",
                       "&selected_columns[]=NUM_TURNO",
                       "&selected_columns[]=COD_MUN_IBGE",
                       "&selected_columns[]=ANO_ELEICAO",
                       "&selected_columns[]=NOME_CANDIDATO",
                       "&selected_columns[]=DESCRICAO_CARGO",
                       "&selected_columns[]=NOME_MESO",
                       "&selected_columns[]=COMPOSICAO_LEGENDA",
                       "&selected_columns[]=NOME_URNA_CANDIDATO")
    print(nome_url)
    
    nome_save <- paste0("cepesp_data/dados_", year_use, "_", cargo_use,".csv")
    
    download.file(url = nome_url,
                  destfile = nome_save)
    
    nome_url <- NULL
  }
}

join_base <- as_data_frame(NULL)

for(ano in 1:length(years)){
  year_use <- years[ano]
  
  if(year_use%%4 != 0){
    position <- c(1,3,5,6,7)
  }
  else {
    position <- c(11,13)
  }
  for(cargo in 1:length(position)){
    cargo_use <- position[cargo]
    
    nome_read <- paste0("cepesp_data/dados_", year_use, "_", cargo_use, ".csv")
    
    temp <- read_csv(nome_read)
    
    join_base <- bind_rows(join_base, temp)
    
    temp <- NULL
    nome_read <- NULL
  }
}

saveRDS(join_base, "cepesp_base_join_v2.rds")
write_feather(join_base, "cepesp_base_join_feather_v2.feather")


