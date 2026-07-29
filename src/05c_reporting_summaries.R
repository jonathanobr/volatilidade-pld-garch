# src/05c_reporting_summaries.R
source("src/00_config.R")

if(!dir.exists("relatorios/sumarios_modelos")) dir.create("relatorios/sumarios_modelos", recursive = TRUE)

# ==============================================================================
# 1. CARREGAMENTO DOS MODELOS
# ==============================================================================
cat("\n[1/2] Carregando modelos GARCH e GARCH-X para extração de logs...\n")

fit_se_puro <- readRDS("models/fit_se_puro.rds")
fit_s_puro  <- readRDS("models/fit_s_puro.rds")
#fit_ne_puro <- readRDS("models/fit_ne_puro.rds")
#fit_n_puro  <- readRDS("models/fit_n_puro.rds")

fit_se_m4   <- readRDS("models/fit_se_m4.rds")
fit_s_m4    <- readRDS("models/fit_s_m4.rds")
#fit_ne_m4   <- readRDS("models/fit_ne_m4.rds")
#fit_n_m4    <- readRDS("models/fit_n_m4.rds")

# ==============================================================================
# 2. CAPTURA DO OUTPUT DE CONSOLE (SHOW)
# ==============================================================================
cat("\n[2/2] Exportando resultados da função show() para .txt...\n")

capture.output(show(fit_se_puro), file = "relatorios/sumarios_modelos/summary_fit_se_puro.txt")
capture.output(show(fit_s_puro),  file = "relatorios/sumarios_modelos/summary_fit_s_puro.txt")
capture.output(show(fit_se_m4),   file = "relatorios/sumarios_modelos/summary_fit_se_m4.txt")
capture.output(show(fit_s_m4),    file = "relatorios/sumarios_modelos/summary_fit_s_m4.txt")

#capture.output(show(fit_ne_puro), file = "relatorios/sumarios_modelos/summary_fit_ne_puro.txt")
#capture.output(show(fit_n_puro),  file = "relatorios/sumarios_modelos/summary_fit_n_puro.txt")
#capture.output(show(fit_ne_m4),   file = "relatorios/sumarios_modelos/summary_fit_ne_m4.txt")
#capture.output(show(fit_n_m4),    file = "relatorios/sumarios_modelos/summary_fit_n_m4.txt")

cat("\nArquivos de texto exportados com sucesso em 'relatorios/sumarios_modelos/'!\n")