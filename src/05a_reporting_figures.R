# src/05a_reporting_figures.R
source("src/00_config.R")

# Criar diretório para as figuras se não existir
if(!dir.exists("relatorios/figuras")) dir.create("relatorios/figuras", recursive = TRUE)

# ==============================================================================
# 1. CARREGAMENTO DOS DADOS E MODELOS ESTIMADOS
# ==============================================================================
cat("\n[1/2] Carregando modelos estimados nas etapas anteriores...\n")

# Requerimento de dados das etapas 02 e 03 para plotagem (adicionado para funcionamento autônomo)
df_model     <- readRDS("data/df_model.rds")
df_se_full_z <- readRDS("data/df_se_full_z.rds")
df_s_full_z  <- readRDS("data/df_s_full_z.rds")
#df_ne_full_z <- readRDS("data/df_ne_full_z.rds")
#df_n_full_z  <- readRDS("data/df_n_full_z.rds")

fit_se_m4   <- readRDS("models/fit_se_m4.rds")
fit_s_m4    <- readRDS("models/fit_s_m4.rds")
#fit_ne_m4   <- readRDS("models/fit_ne_m4.rds")
#fit_n_m4    <- readRDS("models/fit_n_m4.rds")

# ==============================================================================
# 2. EXPORTAÇÃO DE FIGURAS (GRÁFICOS DE ALTA RESOLUÇÃO E STORYTELLING)
# ==============================================================================
cat("\n[2/2] Gerando e exportando gráficos de alta resolução...\n")

# A. Retornos: Nível e Quadrado combinados em painel (Figuras 4 a 7)
plot_retornos_combinados <- function(df, var_y, titulo_base, filename) {
  
  # Gráfico Superior: Retorno em Nível
  p_nivel <- ggplot(df, aes(x = data_inicio, y = .data[[var_y]])) +
    geom_line(color = "#34495e", linewidth = 0.4) + theme_minimal() +
    labs(title = paste(titulo_base, "- Nível"), y = "Retorno", x = "") +
    theme(axis.text.x = element_blank()) # Esconde o eixo X do gráfico de cima para colar no de baixo
  
  # Gráfico Inferior: Retorno ao Quadrado
  df$var_y_2 <- df[[var_y]]^2
  p_quadrado <- ggplot(df, aes(x = data_inicio, y = var_y_2)) +
    geom_line(color = "#c0392b", linewidth = 0.4) + theme_minimal() +
    labs(title = paste(titulo_base, "- Quadrado (Clusters de Volatilidade)"), y = expression("Retorno"^2), x = "Ano")
  
  # Empilha os gráficos usando o patchwork
  p_final <- p_nivel / p_quadrado
  
  ggsave(paste0("relatorios/figuras/", filename), plot = p_final, width = 10, height = 6, dpi = 300, bg = "white")
}

plot_retornos_combinados(df_model, "ret_pld_se", "Sudeste",  "Fig04_Retornos_Painel_SE.png")
plot_retornos_combinados(df_model, "ret_pld_s",  "Sul",      "Fig05_Retornos_Painel_S.png")
plot_retornos_combinados(df_model, "ret_pld_ne", "Nordeste", "Fig06_Retornos_Painel_NE.png")
plot_retornos_combinados(df_model, "ret_pld_n",  "Norte",    "Fig07_Retornos_Painel_N.png")

# B. Painel de Histogramas Unificado (Figura 8)
plot_hist_obj <- function(df, var_x, titulo) {
  media  <- mean(df[[var_x]], na.rm = TRUE)
  desvio <- sd(df[[var_x]], na.rm = TRUE)
  ggplot(df, aes(x = .data[[var_x]])) +
    geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "#3498db", alpha = 0.7, color = "white") +
    stat_function(fun = dnorm, args = list(mean = media, sd = desvio), color = "#e74c3c", linewidth = 1) +
    theme_minimal() + labs(title = titulo, x = "Retorno", y = "Densidade")
}

p_hist_agrupado <- (plot_hist_obj(df_model, "ret_pld_se", "Sudeste") | plot_hist_obj(df_model, "ret_pld_s", "Sul")) / 
  (plot_hist_obj(df_model, "ret_pld_ne", "Nordeste") | plot_hist_obj(df_model, "ret_pld_n", "Norte")) +
  plot_annotation(
    title = 'Distribuição dos Retornos do PLD vs. Curva Normal',
    #subtitle = 'Evidência visual de Leptocurtose nos quatro subsistemas',
    theme = theme(plot.title = element_text(size = 14, face = "bold"))
  )
ggsave("relatorios/figuras/Fig08_Histogramas_Agrupados.png", plot = p_hist_agrupado, width = 11, height = 7, dpi = 300, bg = "white")

# Coeficientes de curtose
PerformanceAnalytics::kurtosis(na.omit(df_model$ret_pld_se), method = "excess")
PerformanceAnalytics::kurtosis(na.omit(df_model$ret_pld_s), method = "excess")
PerformanceAnalytics::kurtosis(na.omit(df_model$ret_pld_ne), method = "excess")
PerformanceAnalytics::kurtosis(na.omit(df_model$ret_pld_n), method = "excess")
# Coeficientes de assimetria
PerformanceAnalytics::skewness(na.omit(df_model$ret_pld_se))
PerformanceAnalytics::skewness(na.omit(df_model$ret_pld_s))

# C. Correlogramas ACF e PACF apenas para Retornos Quadrados (Figuras 9 a 12)
exportar_acf_pacf_quadrado <- function(serie, titulo_base, filename) {
  png(paste0("relatorios/figuras/", filename), width = 3600, height = 1800, res = 300, bg = "white") 
  par(mfrow=c(1,2), mar=c(4,4,4,2), cex.main=1.3, cex.axis=1.1, cex.lab=1.1)
  
  acf(serie^2, main=paste("FAC (Retorno Quadrado):", titulo_base), ylab="ACF", lwd=2, lag.max = 52)
  pacf(serie^2, main=paste("FACP (Retorno Quadrado):", titulo_base), ylab="PACF", lwd=2, lag.max = 52)
  dev.off()
}

exportar_acf_pacf_quadrado(na.omit(df_model$ret_pld_se), "Sudeste",  "Fig09_ACF_Quad_SE.png")
exportar_acf_pacf_quadrado(na.omit(df_model$ret_pld_s),  "Sul",      "Fig10_ACF_Quad_S.png")
exportar_acf_pacf_quadrado(na.omit(df_model$ret_pld_ne), "Nordeste", "Fig11_ACF_Quad_NE.png")
exportar_acf_pacf_quadrado(na.omit(df_model$ret_pld_n),  "Norte",    "Fig12_ACF_Quad_N.png")

# D1. Diagnóstico de Resíduos GARCH-X SE (Figura 13 - Painel 2x2)
residuos_se <- as.numeric(residuals(fit_se_m4, standardize=TRUE))
residuos_quad_se <- residuos_se^2

png("relatorios/figuras/Fig13_Diagnostico_Residuos_GARCH-X_SE.png", width = 4800, height = 2400, res = 300, bg = "white")
par(mfrow=c(1,2), mar=c(4,4,4,2), cex.main=1.4, cex.axis=1.2, cex.lab=1.2) # seria mfrow=c(4,2) se tívessemos os 4 subsistemas

acf(residuos_quad_se, main="FAC Resíduos Quadrados (SE)", ylab="ACF", lwd=2, lag.max=52)
pacf(residuos_quad_se, main="FACP Resíduos Quadrados (SE)", ylab="PACF", lwd=2, lag.max=52)
dev.off()

# D2. Diagnóstico de Resíduos GARCH-X S (Figura 14 - Painel 2x2)
residuos_s <- as.numeric(residuals(fit_s_m4, standardize=TRUE))
residuos_quad_s <- residuos_s^2

png("relatorios/figuras/Fig14_Diagnostico_Residuos_GARCH-X_S.png", width = 4800, height = 2400, res = 300, bg = "white")
par(mfrow=c(1,2), mar=c(4,4,4,2), cex.main=1.4, cex.axis=1.2, cex.lab=1.2) # seria mfrow=c(4,2) se tívessemos os 4 subsistemas

acf(residuos_quad_s, main="FAC Resíduos Quadrados (S)", ylab="ACF", lwd=2, lag.max=52)
pacf(residuos_quad_s, main="FACP Resíduos Quadrados (S)", ylab="PACF", lwd=2, lag.max=52)
dev.off()

# acf(residuals(fit_ne_m4, standardize=TRUE)^2, main="FAC Resíduos Quadrados (NE)", ylab="ACF", lwd=2, lag.max=52)
# pacf(residuals(fit_ne_m4, standardize=TRUE)^2, main="FACP Resíduos Quadrados (NE)", ylab="PACF", lwd=2, lag.max=52)

# acf(residuals(fit_n_m4, standardize=TRUE)^2, main="FAC Resíduos Quadrados (N)", ylab="ACF", lwd=2, lag.max=52)
# pacf(residuals(fit_n_m4, standardize=TRUE)^2, main="FACP Resíduos Quadrados (N)", ylab="PACF", lwd=2, lag.max=52)
# dev.off()

# E. Volatilidade Condicional Estimada com Eventos Históricos (Figura 15)
vol_se <- data.frame(Data = df_se_full_z$data_inicio, Sigma = sigma(fit_se_m4), Subsistema = "Sudeste")
vol_s  <- data.frame(Data = df_s_full_z$data_inicio,  Sigma = sigma(fit_s_m4),  Subsistema = "Sul")
#vol_ne <- data.frame(Data = df_ne_full_z$data_inicio, Sigma = sigma(fit_ne_m4), Subsistema = "Nordeste")
#vol_n  <- data.frame(Data = df_n_full_z$data_inicio,  Sigma = sigma(fit_n_m4),  Subsistema = "Norte")
vol_consolidada <- bind_rows(vol_se, vol_s)

# Base de eventos críticos do setor
eventos <- data.frame(
  Data = as.Date(c("2001-06-01", "2014-01-01", "2020-03-01", "2021-06-01")),
  Label = c("Racionamento (2001)", "Crise Hídrica (2014-15)", "Pandemia COVID-19 (2020)", "Escassez Hídrica (2021)")
)

y_max_label <- max(vol_consolidada$Sigma) * 0.95

p_vol <- ggplot(vol_consolidada, aes(x = Data, y = Sigma, color = Subsistema)) + 
  geom_line(alpha = 0.8, linewidth = 0.6) + 
  scale_color_manual(values = c("Sudeste"="#2c3e50", "Sul"="#e74c3c", "Nordeste"="#27ae60", "Norte"="#8e44ad")) + 
  geom_vline(data = eventos, aes(xintercept = Data), linetype = "dashed", color = "gray40", linewidth = 0.7) +
  geom_text(data = eventos, aes(x = Data, y = y_max_label, label = Label), 
            inherit.aes = FALSE, angle = 90, vjust = -0.5, size = 3.5, color = "gray20", fontface = "italic") +
  labs(title = "Volatilidade Condicional Estimada (GARCH-X)", y = expression(sigma[t]), x = "Ano") + 
  theme_minimal() + theme(legend.position = "bottom")

ggsave("relatorios/figuras/Fig15_Volatilidade_Padronizada_Eventos.png", plot = p_vol, width = 15, height = 12, dpi = 300, bg = "white")

cat("\n[Figuras geradas e salvas com sucesso]\n")