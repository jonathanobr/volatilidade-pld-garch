# src/01_ingestao_db.R
source("src/00_config.R")
library(readr)

# 1. Estabelecer conexão com o banco
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("DB_NAME"), host = Sys.getenv("DB_HOST"),
  port = Sys.getenv("DB_PORT"), user = Sys.getenv("DB_USER"), password = Sys.getenv("DB_PASSWORD")
)

# Criar schema se não existir
dbExecute(con, "CREATE SCHEMA IF NOT EXISTS volatilidade;")

# Força o PostgreSQL a ler as strings como Dia/Mês/Ano
dbExecute(con, "SET datestyle = 'ISO, DMY';")

# 2. Definição Centralizada dos Caminhos
dir_brutos <- "dados brutos/"
arquivos <- list(
  pld      = paste0(dir_brutos, "historico_pld_semanal.csv"),
  gas      = paste0(dir_brutos, "gas futuros semanal.csv"),
  petroleo = paste0(dir_brutos, "dados_petroleo.csv"),
  ptax     = paste0(dir_brutos, "dados_ptax.csv"),
  ena = paste0(dir_brutos, "dados_ena.csv")
)

# 3. Leitura e Upload das Tabelas Temporárias
# Lemos tudo como texto (character) para não perder formatação, deixando o SQL tratar

cat("Subindo PLD...\n")
df_pld <- read_delim(arquivos$pld, delim = ";", locale = locale(encoding = "UTF-8"), col_types = cols(.default = "c"))
# Remove fantasmas BOM
colnames(df_pld) <- c("ano", "mes", "semana", "data_inicio", "data_fim", "sudeste", "sul", "nordeste", "norte")
dbWriteTable(con, DBI::Id(schema = "volatilidade", table = "temp_historico_pld"), df_pld, overwrite = TRUE)

cat("Subindo Gás...\n")
df_gas <- read_csv(arquivos$gas, locale = locale(encoding = "UTF-8"), col_types = cols(.default = "c"))
dbWriteTable(con, DBI::Id(schema = "volatilidade", table = "temp_precos_gas"), df_gas, overwrite = TRUE)

cat("Subindo Petróleo...\n")
df_petroleo <- read_delim(arquivos$petroleo, delim = ";", locale = locale(encoding = "UTF-8"), col_types = cols(.default = "c"))
# Sincroniza os nomes para bater com o que o SQL espera
colnames(df_petroleo) <- c("data", "codigo_serie", "data_completa", "dia", "mes", "ano", "valor_usd_texto")
dbWriteTable(con, DBI::Id(schema = "volatilidade", table = "temp_precos_petroleo"), df_petroleo, overwrite = TRUE)

cat("Subindo PTAX...\n")
df_ptax <- read_delim(arquivos$ptax, delim = ";", locale = locale(encoding = "UTF-8"), col_types = cols(.default = "c"))
# Sincroniza os nomes para bater com o que o SQL espera
colnames(df_ptax) <- c("data", "codigo_serie", "data_completa", "dia", "mes", "ano", "valor_usd_texto")
dbWriteTable(con, DBI::Id(schema = "volatilidade", table = "temp_ptax"), df_ptax, overwrite = TRUE)

cat("ENA — detectando modo de ingestão...\n")

caminho_ena_semanal <- arquivos$ena  # "dados brutos/dados_ena.csv"

if (file.exists(caminho_ena_semanal)) {
  # ── MODO RÁPIDO: CSV semanal pré-processado já disponível ──────────────────
  # Usa o arquivo dados_ena.csv gerado previamente (distribuído com o projeto
  # ou criado pelo MODO COMPLETO abaixo numa execução anterior).
  cat("  [SKIP] dados_ena.csv encontrado — subindo direto para o banco.\n")

  df_ena_semanal <- read_delim(
    caminho_ena_semanal, delim = ";",
    locale = locale(encoding = "UTF-8"),
    col_types = cols(.default = "c")
  )

  # Sobe como tabela_ena_semanal (referenciada pelo 06_tabela_mestra.sql)
  dbExecute(con, "DROP TABLE IF EXISTS volatilidade.tabela_ena_semanal CASCADE;")
  dbWriteTable(
    con, DBI::Id(schema = "volatilidade", table = "tabela_ena_semanal"),
    df_ena_semanal, overwrite = TRUE
  )
  cat("  [OK] tabela_ena_semanal criada no banco com", nrow(df_ena_semanal), "semanas.\n")

} else {
  # ── MODO COMPLETO: processa a partir dos CSVs diários do ONS ───────────────
  # Requer que 00_coleta_dados.py tenha baixado os arquivos em 'dados brutos/ena/'.
  # Ao final, re-exporta o resultado semanal como dados_ena.csv para uso futuro.
  cat("  [FULL] dados_ena.csv não encontrado — processando a partir dos CSVs diários.\n")

  # a. Colunas originais do ONS
  colunas_ena <- c(
    "id_subsistema", "nom_subsistema", "ena_data",
    "ena_bruta_regiao_mwmed", "ena_bruta_regiao_percentualmlt",
    "ena_armazenavel_regiao_mwmed", "ena_armazenavel_regiao_percentualmlt"
  )

  # b. Ler e empilhar todos os CSVs anuais
  anos_ena     <- 2000:2025
  arquivos_ena <- paste0(dir_brutos, "ena/ENA_DIARIO_SUBSISTEMA_", anos_ena, ".csv")

  lista_df_ena <- lapply(arquivos_ena, function(caminho) {
    if (file.exists(caminho)) {
      read_delim(caminho, delim = ";",
                 locale = locale(encoding = "LATIN1"),
                 col_names = colunas_ena,
                 col_types = cols(.default = "c"))
    } else {
      warning(paste("Arquivo diário não encontrado:", caminho))
      NULL
    }
  })

  df_ena_diario <- bind_rows(lista_df_ena)
  cat("  Arquivos diários lidos:", nrow(df_ena_diario), "registros.\n")

  # c. Sobe os dados diários como tabela temporária
  dbWriteTable(
    con, DBI::Id(schema = "volatilidade", table = "temp_ena"),
    df_ena_diario, overwrite = TRUE
  )

  # d. Executa os passos 1–3 do 05_trata_ena.sql (diário → transposta → semanal)
  #    A exportação do CSV é feita a seguir no R (evita COPY TO com caminho absoluto).
  cat("  Executando ETL: diário → transposta → semanal...\n")
  sql_ena <- readLines("queries/etl/05_trata_ena.sql", warn = FALSE) |>
    paste(collapse = "\n")

  for (cmd in Filter(nchar, trimws(strsplit(sql_ena, ";")[[1]]))) {
    dbExecute(con, cmd)
  }

  # e. Re-exporta a tabela semanal para CSV (para uso futuro sem reprocessamento)
  cat("  Exportando ENA semanal para", caminho_ena_semanal, "...\n")
  df_ena_semanal <- dbGetQuery(
    con,
    "SELECT * FROM volatilidade.tabela_ena_semanal ORDER BY data_inicio ASC;"
  )
  write_delim(df_ena_semanal, caminho_ena_semanal, delim = ";", na = "")
  cat("  [OK] dados_ena.csv salvo com", nrow(df_ena_semanal), "semanas.\n")
  # tabela_ena_semanal já está no banco (criada pelo 05_trata_ena.sql acima).
  # O 06_tabela_mestra.sql a referencia diretamente — nenhuma ação adicional necessária.
}

# 4. Executar as Queries de Tratamento e Agregação
# Obs: 05_trata_ena.sql é executado dentro do bloco ENA acima (modo completo)
# ou ignorado (modo rápido, pois a tabela semanal já está pronta no banco).
queries_etl <- c(
  "queries/etl/01_trata_pld.sql",
  "queries/etl/02_trata_gas.sql",
  "queries/etl/03_trata_petroleo.sql",
  "queries/etl/04_trata_ptax.sql",
  "queries/etl/06_tabela_mestra.sql"
)

# Lê a lista de arquivos SQL
for (q in queries_etl) {
  cat("Executando:", q, "...\n")
  
  # Lê o arquivo e consolida em uma única string de texto
  sql_str <- readLines(q, warn = FALSE) |> paste(collapse = "\n")
  
  # Fatiar a string toda vez que encontrar um ponto e vírgula (;)
  comandos <- unlist(strsplit(sql_str, ";"))
  
  # Executa cada pedaço de código separadamente no banco de dados
  for (cmd in comandos) {
    cmd_trim <- trimws(cmd) # Remove espaços e quebras de linha soltas
    
    # Se o pedaço não for apenas um espaço em branco, ele executa
    if (nchar(cmd_trim) > 0) {
      dbExecute(con, cmd_trim)
    }
  }
}

# EXPORTAÇÃO DO DATASET FINAL (Desacoplando a modelagem do Banco de Dados)
cat("Exportando Tabela Mestra para arquivos locais...\n")

query_extracao <- "SELECT * FROM volatilidade.tabela_final_consolidada ORDER BY data_inicio ASC;"
dataset_final <- dbGetQuery(con, query_extracao)

# Salva em CSV para enviar ao orientador/banca
write_csv(dataset_final, "data/dataset_final_tcc.csv")

# Salva em RDS (formato nativo do R, muito mais rápido de ler)
saveRDS(dataset_final, "data/dataset_final_tcc.rds")

dbDisconnect(con)
cat("ETL Concluído com sucesso! Dataset final salvo na pasta 'data/'.\n")