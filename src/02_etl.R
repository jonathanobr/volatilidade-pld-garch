source("src/00_config.R")

# 1. Leitura do arquivo consolidado (gerado pelo pipeline de ingestão)
# Assim a banca consegue rodar seu código R a partir deste ponto sem precisar do PostgreSQL
df_raw <- readRDS("data/dataset_final_tcc.rds")

# 2. Transformação Econométrica (Normalização)
df_model <- df_raw %>%
  # Filtro de data que você utilizava no seu código inicial
  filter(data_inicio >= as.Date('2001-08-01') & data_inicio <= as.Date('2025-07-31')) %>%
  arrange(data_inicio) %>%
  mutate(
    # Variáveis Dependentes
    ret_pld_se = c(NA, diff(log(pld_medio_se))),
    ret_pld_s  = c(NA, diff(log(pld_medio_s))),
    ret_pld_ne  = c(NA, diff(log(pld_medio_ne))),
    ret_pld_n  = c(NA, diff(log(pld_medio_n))),
    
    # Condicionantes (Variáveis Explicativas)
    ret_petroleo_lag1 = c(NA, diff(log(preco_petroleo_brl_lag1))),
    ret_gas_lag1      = c(NA, diff(log(preco_gas_brl_lag1))),
    ena_se_scaled = ena_arm_mlt_se_lag1 / 100,
    ena_s_scaled  = ena_arm_mlt_s_lag1  / 100,
    ena_ne_scaled  = ena_arm_mlt_ne_lag1  / 100,
    ena_n_scaled  = ena_arm_mlt_n_lag1  / 100
  ) %>%
  na.omit()

# 3. Teste de "censura" regulatória - freios de piso e teto do PLD no universo amostral
diagnosticar_censura <- function(serie_retornos, nome_sub) {
  serie_retornos <- na.omit(serie_retornos)
  total_obs <- length(serie_retornos)
  
  # Quantos dados estão no intervalo "morto" (entre -0.1% e 0.1%)
  zeros_artificiais <- sum(abs(serie_retornos) <= 0.001)
  pct_censurado <- (zeros_artificiais / total_obs) * 100
  
  # Variância com e sem os zeros
  var_bruta <- var(serie_retornos)
  var_filtrada <- var(serie_retornos[abs(serie_retornos) > 0.001])
  distorcao <- ((var_filtrada - var_bruta) / var_filtrada) * 100
  
  data.frame(
    `Subsistema` = nome_sub,
    `Obs. Totais` = total_obs,
    `Semanas Travadas` = zeros_artificiais,
    `% Censurado` = pct_censurado,
    `Variância Bruta` = var_bruta,
    `Variância Filtrada` = var_filtrada,
    `Distorção (%)` = distorcao,
    check.names = FALSE
  )
}

# Gerando o dataframe consolidado para os 4 subsistemas
df_censura <- bind_rows(
  diagnosticar_censura(df_model$ret_pld_se, "Sudeste"),
  diagnosticar_censura(df_model$ret_pld_s, "Sul"),
  diagnosticar_censura(df_model$ret_pld_ne, "Nordeste"),
  diagnosticar_censura(df_model$ret_pld_n, "Norte")
)

# sem uso: Filtragem e Z-Score (Remoção da censura regulatória)
#df_se_fil_z <- df_model %>% 
#  filter(abs(ret_pld_se) > 0.001) %>%
#  mutate(
#    ena_z       = as.numeric(scale(ena_se_scaled)),
#    petroleo_z  = as.numeric(scale(ret_petroleo_lag1)),
#    gas_z       = as.numeric(scale(ret_gas_lag1))
#  )

# Salvar dados processados para as próximas etapas
saveRDS(df_model, "data/df_model.rds")
saveRDS(df_censura, "data/tabela_censura.rds")
#saveRDS(df_se_fil_z, "data/df_se_fil_z.rds")

cat("Transformações realizadas com sucesso. Dados exportados em arquivo.\n")