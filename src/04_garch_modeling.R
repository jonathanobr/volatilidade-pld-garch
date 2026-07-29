# src/04_garch_modeling.R
source("src/00_config.R")

# 1. Carregar bases padronizadas
df_se_full_z <- readRDS("data/df_se_full_z.rds")
df_s_full_z  <- readRDS("data/df_s_full_z.rds")
df_ne_full_z <- readRDS("data/df_ne_full_z.rds")
df_n_full_z <- readRDS("data/df_n_full_z.rds")

# 2. Construção das Matrizes Exógenas
# Sudeste
reg_se_m4 <- as.matrix(df_se_full_z %>% select(ena_z, petroleo_z, gas_z, dummy_institucional))
# Sul
reg_s_m4  <- as.matrix(df_s_full_z %>% select(ena_z, petroleo_z, gas_z)) # dummy_institucional
# Comentados pela ausência de efeitos ARCH
# Nordeste
#reg_ne_m4 <- as.matrix(df_ne_full_z %>% select(ena_z, petroleo_z, gas_z, dummy_institucional))
# Norte
#reg_n_m4  <- as.matrix(df_n_full_z %>% select(ena_z, petroleo_z, gas_z, dummy_institucional))

# Matriz exclusiva para a média (espelha o auto.arima)
mxreg_se <- as.matrix(df_se_full_z %>% select(dummy_institucional))
mxreg_s  <- as.matrix(df_s_full_z %>% select(dummy_institucional))
mxreg_ne <- as.matrix(df_ne_full_z %>% select(dummy_institucional))
mxreg_n  <- as.matrix(df_n_full_z %>% select(dummy_institucional))

# 3. Especificação dos Modelos

cat("\n--- Carregando especificações da média (ARMA) do Script 03 ---\n")

arma_se <- readRDS("models/arma_se.rds")
arma_s  <- readRDS("models/arma_s.rds")
arma_ne <- readRDS("models/arma_ne.rds")
arma_n  <- readRDS("models/arma_n.rds")

# Função auxiliar para extrair a ordem p e q do objeto auto.arima
get_arma_order <- function(modelo_arima) {
  # arimaorder() retorna um vetor c(p, d, q) ou c(p, d, q, P, D, Q, m)
  ordem <- arimaorder(modelo_arima)
  return(c(as.numeric(ordem["p"]), as.numeric(ordem["q"])))
}

order_se <- get_arma_order(arma_se)
order_s  <- get_arma_order(arma_s)
order_ne <- get_arma_order(arma_ne)
order_n  <- get_arma_order(arma_n)

# --- Especificações SE ---
spec_se_puro <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model     = list(armaOrder = order_se, include.mean = TRUE, external.regressors = mxreg_se),
  distribution.model = "sstd"
)
spec_se_m4 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1), external.regressors = reg_se_m4),
  mean.model     = list(armaOrder = order_se, include.mean = TRUE, external.regressors = mxreg_se),
  distribution.model = "sstd"
)

# --- Especificações S ---
spec_s_puro <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model     = list(armaOrder = c(1, 0), include.mean = TRUE, external.regressors = mxreg_s),
  #mean.model     = list(armaOrder = order_s, include.mean = TRUE, external.regressors = mxreg_s),
  distribution.model = "std"
)
spec_s_m4 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1), external.regressors = reg_s_m4),
#  mean.model     = list(armaOrder = order_s, include.mean = TRUE, external.regressors = mxreg_s),
  mean.model     = list(armaOrder = c(1, 0), include.mean = TRUE, external.regressors = mxreg_s),
  distribution.model = "std"
)

# --- Especificações NE ---
#spec_ne_puro <- ugarchspec(
#  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
#  mean.model     = list(armaOrder = order_ne, include.mean = TRUE, external.regressors = mxreg_ne),
#  distribution.model = "sstd"
#)
#spec_ne_m4 <- ugarchspec(
#  variance.model = list(model = "sGARCH", garchOrder = c(1, 1), external.regressors = reg_ne_m4),
#  mean.model     = list(armaOrder = order_ne, include.mean = TRUE, external.regressors = mxreg_ne),
#  distribution.model = "sstd"
#)

# --- Especificações N ---
#spec_n_puro <- ugarchspec(
#  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
#  mean.model     = list(armaOrder = order_n, include.mean = TRUE, external.regressors = mxreg_ne),
#  distribution.model = "sstd"
#)
#spec_n_m4 <- ugarchspec(
#  variance.model = list(model = "sGARCH", garchOrder = c(1, 1), external.regressors = reg_n_m4),
#  mean.model     = list(armaOrder = order_n, include.mean = TRUE, external.regressors = mxreg_n),
#  distribution.model = "sstd"
#)

# 4. Seleção de Seed via módulo externo
specs_list <- list(
  se_puro = spec_se_puro,
  se_m4   = spec_se_m4,
  s_puro  = spec_s_puro,
  s_m4    = spec_s_m4
)

dados_list <- list(
  se_puro = df_se_full_z$ret_pld_se,
  se_m4   = df_se_full_z$ret_pld_se,
  s_puro  = df_s_full_z$ret_pld_s,
  s_m4    = df_s_full_z$ret_pld_s
)

# ctrl_fixo: parâmetros do solver independentes da seed.
# Espelhado no 04a para garantir que a varredura valide seeds
# sob as MESMAS condições usadas na estimação final.
ctrl_fixo <- list(n.restarts = 15, tol = 1e-4)

nseed <- source("src/04a_seed_selection.R", local = TRUE)$value

# 5. Estimação e Otimização

# hybrid é o solver mais robusto e comum para distribuições std
# nlminb solver mais adequado para distribuições std com platôs, gosolnp é uma alternativa
#solver.control = list(rseed = nseed, tol = 1e-4))
#, fit.control = list(scale = 1)) 
# solver = "hybrid", solver.control = list(tol = 1e-4)) 

ctrl_otimizacao <- c(ctrl_fixo, list(rseed = nseed))

cat("\n--- Treinando Modelos: Sudeste ---\n")
fit_se_puro <- ugarchfit(spec = spec_se_puro, data = df_se_full_z$ret_pld_se, 
                         solver = "gosolnp", solver.control = ctrl_otimizacao)
fit_se_m4   <- ugarchfit(spec = spec_se_m4,   data = df_se_full_z$ret_pld_se, 
                         solver = "gosolnp", solver.control = ctrl_otimizacao)

cat("\n--- Treinando Modelos: Sul ---\n")
fit_s_puro <- ugarchfit(spec = spec_s_puro, data = df_s_full_z$ret_pld_s, 
                        solver = "gosolnp", solver.control = ctrl_otimizacao)
fit_s_m4   <- ugarchfit(spec = spec_s_m4,   data = df_s_full_z$ret_pld_s, 
                        solver = "gosolnp", solver.control = ctrl_otimizacao)

cat("\n--- Treinando Modelos ---\n")


cat("\n--- Diagnóstico da Matriz Hessiana (Autovalores) ---\n")

# Função auxiliar para verificar a positividade-definida da matriz de covariância
check_eigenvalues <- function(fit, nome_modelo = "Modelo não especificado") {
  cat(sprintf("\n[%s]\n", nome_modelo))
  
  # Extrai a matriz de variância-covariância de forma segura
  vcov_mat <- tryCatch(vcov(fit), error = function(e) NULL)
  
  if(is.null(vcov_mat) || any(is.na(vcov_mat))) {
    cat("Erro: Matriz de covariância indisponível ou corrompida.\n")
    return(invisible(NULL))
  }
  
  # Calcula os autovalores
  ev <- tryCatch(eigen(vcov_mat, only.values = TRUE)$values, error = function(e) NULL)
  
  if(is.null(ev)) {
    cat("Erro: Falha ao calcular os autovalores.\n")
    return(invisible(NULL))
  }
  
  min_ev <- min(ev)
  max_ev <- max(ev)
  cond_num <- max_ev / min_ev
  
  cat("Menor autovalor :", sprintf("%e", min_ev), "\n")
  cat("Maior autovalor :", sprintf("%e", max_ev), "\n")
  cat("Condição        :", sprintf("%e", cond_num), "\n")
  
  # Aplica o critério de validação com a tolerância regulatória (1e-4) do seeder
  tol_rel <- 1e-4
  if (all(ev > -tol_rel)) {
    cat("Status          : OK (Positiva-Definida sob a tolerância EPI de 1e-4)\n")
  } else {
    cat("Status          : FALHA (Matriz Não Positiva-Definida - Cuidado com Inferência!)\n")
  }
}

# Executa o diagnóstico para os 4 modelos finais
check_eigenvalues(fit_se_puro, "Sudeste/Centro-Oeste (GARCH Puro)")
check_eigenvalues(fit_se_m4,   "Sudeste/Centro-Oeste (GARCH-X)")
check_eigenvalues(fit_s_puro,  "Sul (GARCH Puro)")
check_eigenvalues(fit_s_m4,    "Sul (GARCH-X)")
cat(strrep("-", 60), "\n")

# 6. Diagnóstico de Resíduos
cat("\n--- Diagnóstico de Resíduos: Teste de Ljung-Box nos Resíduos Quadrados (Lag 12) ---\n")
res_std_se_puro <- residuals(fit_se_puro, standardize = TRUE)
res_std_s_puro  <- residuals(fit_s_puro, standardize = TRUE)
res_std_se_m4 <- residuals(fit_se_m4, standardize = TRUE)
res_std_s_m4  <- residuals(fit_s_m4, standardize = TRUE)

# Função auxiliar para converter os objetos xts em vetores numéricos limpos
safe_ljung_box <- function(residuos, nome_modelo = "Modelo não especificado") {
  # Remove NAs e converte para vetor numérico puro
  res_limpos <- na.omit(as.numeric(residuos))
  
  if(length(res_limpos) == 0) {
    return(paste0("Erro: Resíduos vazios (O modelo GARCH '", nome_modelo, "' não convergiu)"))
  }
  
  return(Box.test(res_limpos^2, lag = 12, type = "Ljung-Box", fitdf = 2))
}

cat("\nSudeste/Centro-Oeste (Puro):\n")
print(safe_ljung_box(res_std_se_puro, nome_modelo = "SE/CO GARCH"))

cat("\nSudeste/Centro-Oeste (M4):\n")
print(safe_ljung_box(res_std_se_m4, nome_modelo = "SE/CO GARCH-X"))

cat("\nSul (Puro):\n")
print(safe_ljung_box(res_std_s_puro, nome_modelo = "Sul GARCH"))

cat("\nSul (M4):\n")
print(safe_ljung_box(res_std_s_m4, nome_modelo = "Sul GARCH-X"))

# 7. Salvar objetos computados
modelos_para_salvar <- list(
  "fit_se_puro" = fit_se_puro, "fit_se_m4" = fit_se_m4, "fit_s_puro"  = fit_s_puro,  "fit_s_m4"  = fit_s_m4
  #  "fit_ne_puro" = fit_ne_puro, "fit_ne_m4" = fit_ne_m4, "fit_n_puro" = fit_n_puro, "fit_n_m4" = fit_n_m4
)

for (nome in names(modelos_para_salvar)) {
  saveRDS(modelos_para_salvar[[nome]], file = paste0("models/", nome, ".rds"))
}
cat("Modelos estimados e salvos com sucesso no diretório 'models/'.\n")
cat(sprintf("Seed utilizada: %d (selecionada por 04a_seed_selection.R)\n", nseed))