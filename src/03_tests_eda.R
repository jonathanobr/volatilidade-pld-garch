# src/03_tests_eda.R
source("src/00_config.R")

# 1. Carregar o dataframe base
df_model <- readRDS("data/df_model.rds")

# 2. Função para Filtragem e Padronização (Z-Score) por Subsistema
preparar_subsistema <- function(df, var_retorno, var_ena_scaled) {
  df %>%
    arrange(data_inicio) %>%
    mutate(
      dummy_institucional = ifelse(abs(.data[[var_retorno]]) <= 0.001, 1, 0),
      ena_z      = as.numeric(scale(.data[[var_ena_scaled]])),
      petroleo_z = as.numeric(scale(ret_petroleo_lag1)),
      gas_z      = as.numeric(scale(ret_gas_lag1))
    )
}

df_se_full_z <- preparar_subsistema(df_model, "ret_pld_se", "ena_se_scaled")
df_s_full_z  <- preparar_subsistema(df_model, "ret_pld_s", "ena_s_scaled")
df_ne_full_z <- preparar_subsistema(df_model, "ret_pld_ne", "ena_ne_scaled")
df_n_full_z  <- preparar_subsistema(df_model, "ret_pld_n", "ena_n_scaled")

# Salvar as versões filtradas (Sudeste e Sul serão usadas no GARCH)
saveRDS(df_se_full_z, "data/df_se_full_z.rds")
saveRDS(df_s_full_z, "data/df_s_full_z.rds")
saveRDS(df_ne_full_z, "data/df_ne_full_z.rds")
saveRDS(df_n_full_z, "data/df_n_full_z.rds")

# 3. Testes de Raiz Unitária (Estacionariedade)
rodar_testes_raiz_unitaria <- function(serie_temporal, nome_variavel) {
  serie_limpa <- na.omit(as.numeric(serie_temporal))
  serie_limpa <- serie_limpa[is.finite(serie_limpa)]
  
  test_drift <- ur.df(serie_limpa, type = "drift", lags = 1)
  test_none  <- ur.df(serie_limpa, type = "none", lags = 1)
  test_pp    <- ur.pp(serie_limpa, type = "Z-tau", model = "constant", lags = "short")
  test_kpss  <- ur.kpss(serie_limpa, type = "mu") 
  
  data.frame(
    Variavel = nome_variavel,
    ADF_Drift_Stat = round(test_drift@teststat[1], 3),
    ADF_None_Stat  = round(test_none@teststat[1], 3),
    PP_Stat        = round(test_pp@teststat, 3),
    KPSS_Stat      = round(test_kpss@teststat, 3)
  )
}

cat("\n--- Testes de Estacionariedade (Séries Filtradas) ---\n")
lista_testes <- bind_rows(
  rodar_testes_raiz_unitaria(df_se_full_z$ret_pld_se, "Retorno SE"),
  rodar_testes_raiz_unitaria(df_s_full_z$ret_pld_s, "Retorno S"),
  rodar_testes_raiz_unitaria(df_ne_full_z$ret_pld_ne, "Retorno NE"),
  rodar_testes_raiz_unitaria(df_n_full_z$ret_pld_n, "Retorno N")
)
print(lista_testes)
# Salvar a tabela para ser consumida pelo módulo de relatórios
saveRDS(lista_testes, "data/tabela_estacionariedade.rds")

# 4. Correlogramas (FAC e FACP) - Nível e Quadrado
# Exportar esses gráficos via pdf() ou png() para relatórios
par(mfrow=c(2,2)) 
acf(df_se_full_z$ret_pld_se, main="FAC: Nível (Sudeste filtrado)")
pacf(df_se_full_z$ret_pld_se, main="FACP: Nível (Sudeste filtrado)")
par(mfrow=c(1,1))

par(mfrow=c(2,2)) 
acf(df_s_full_z$ret_pld_s, main="FAC: Nível (Sul filtrado)")
pacf(df_s_full_z$ret_pld_s, main="FACP: Nível (Sul filtrado)")
par(mfrow=c(1,1))

par(mfrow=c(2,2)) 
acf(df_ne_full_z$ret_pld_ne, main="FAC: Nível (Nordeste filtrado)")
pacf(df_ne_full_z$ret_pld_ne, main="FACP: Nível (Nordeste filtrado)")
par(mfrow=c(1,1))

par(mfrow=c(2,2)) 
acf(df_n_full_z$ret_pld_n, main="FAC: Nível (Norte filtrado)")
pacf(df_n_full_z$ret_pld_n, main="FACP: Nível (Norte filtrado)")
par(mfrow=c(1,1))

# 5. Seleção da Equação da Média (ARMA) e Teste ARCH-LM nos resíduos
testar_arma_arch <- function(df, variavel_retorno, nome_sub) {
  cat(sprintf("\n--- Seleção ARMA e ARCH-LM: %s ---\n", nome_sub))
  
  # Inclui a dummy na equação da média para o auto.arima isolar o platô regulatório
  matriz_xreg <- as.matrix(df$dummy_institucional)
  
  # Estima o ARMA com o critério AIC
  modelo_arma <- auto.arima(df[[variavel_retorno]], xreg = matriz_xreg, max.p = 8, max.q = 8, seasonal = FALSE, stationary = TRUE)
  print(modelo_arma)
  
  # Extrai os resíduos e aplica o ARCH-LM
  residuos_arma <- residuals(modelo_arma)
  teste_arch <- ArchTest(residuos_arma, lags = 12)
  
  cat(sprintf("ARCH-LM (Lag 12): Chi-Quadrado = %.4f, p-valor = %.4f\n", 
              teste_arch$statistic, teste_arch$p.value))
  cat(ifelse(teste_arch$p.value < 0.05, "-> PRESENÇA de efeitos ARCH.\n", "-> AUSÊNCIA de efeitos ARCH.\n"))
  
  return(modelo_arma)
}

arma_se <- testar_arma_arch(df_se_full_z, "ret_pld_se", "Sudeste")
arma_s  <- testar_arma_arch(df_s_full_z, "ret_pld_s", "Sul")
arma_ne <- testar_arma_arch(df_ne_full_z, "ret_pld_ne", "Nordeste")
arma_n  <- testar_arma_arch(df_n_full_z, "ret_pld_n", "Norte")

# 6. Salvar objetos computados
# Crucial para que o script 05_reporting.R não precise recalcular nada, apenas formatar as saídas.
modelos_para_salvar <- list(
  "arma_se" = arma_se, "arma_s" = arma_s, "arma_ne" = arma_ne, "arma_n" = arma_n
)

for (nome in names(modelos_para_salvar)) {
  saveRDS(modelos_para_salvar[[nome]], file = paste0("models/", nome, ".rds"))
}
cat("Modelos estimados e salvos com sucesso no diretório 'models/'.\n")
