# src/05b_reporting_tables.R
source("src/00_config.R")

# ==============================================================================
# FUNÇÕES AUXILIARES DE FORMATAÇÃO E TRANSPOSIÇÃO
# ==============================================================================

# 1. Transposição segura (Termos nas linhas, Subsistemas nas colunas)
transpor_tabela_modelos <- function(df) {
  col_sub <- which(colnames(df) == "Subsistema")
  df_t <- as.data.frame(t(df[, -col_sub, drop = FALSE]), stringsAsFactors = FALSE)
  colnames(df_t) <- df$Subsistema
  df_t <- cbind(Termo = rownames(df_t), df_t)
  rownames(df_t) <- NULL
  return(df_t)
}

# 2. Pipeline unificado de limpeza, renomeação e transposição
compilar_tabela_formatada <- function(df_lista, dict_nomes, ordem) {
  df <- bind_rows(df_lista)
  df <- as.data.frame(df)
  
  # Substitui os NAs (como o 'skew' ausente no modelo Sul) por traços
  df[is.na(df)] <- "-"
  
  # Renomeia as colunas de forma segura, independente da posição
  novos_nomes <- sapply(colnames(df), function(x) ifelse(x %in% names(dict_nomes), dict_nomes[x], x))
  colnames(df) <- unname(novos_nomes)
  
  # Ordena as colunas ancorando o Subsistema sempre na primeira posição
  cols_ordenadas <- c("Subsistema", intersect(ordem, colnames(df)))
  df <- df[, cols_ordenadas, drop = FALSE]
  
  # Transpõe e retorna
  return(transpor_tabela_modelos(df))
}

# ==============================================================================
# 1. CARREGAMENTO DOS DADOS E MODELOS ESTIMADOS
# ==============================================================================
cat("\n[1/2] Carregando modelos estimados nas etapas anteriores...\n")

df_estacionariedade <- readRDS("data/tabela_estacionariedade.rds")
df_censura <- readRDS("data/tabela_censura.rds")

modelos_arma <- list(
  SE = readRDS("models/arma_se.rds"),
  S  = readRDS("models/arma_s.rds"),
  NE = readRDS("models/arma_ne.rds"),
  N  = readRDS("models/arma_n.rds")
)

fit_se_puro <- readRDS("models/fit_se_puro.rds")
fit_s_puro  <- readRDS("models/fit_s_puro.rds")
#fit_ne_puro <- readRDS("models/fit_ne_puro.rds")
#fit_n_puro  <- readRDS("models/fit_n_puro.rds")

fit_se_m4   <- readRDS("models/fit_se_m4.rds")
fit_s_m4    <- readRDS("models/fit_s_m4.rds")
#fit_ne_m4   <- readRDS("models/fit_ne_m4.rds")
#fit_n_m4    <- readRDS("models/fit_n_m4.rds")

# ==============================================================================
# 2. DICIONÁRIO PARAMÉTRICO E EXPORTAÇÃO
# ==============================================================================
cat("\n[2/2] Criando e formatando tabelas consolidadas...\n")

# Dicionário de Parâmetros e Ordem Estrita de Exibição
nomes_de_para <- c(
  "ar1" = "AR(1)", "ar2" = "AR(2)", "ar3" = "AR(3)", "ar4" = "AR(4)", "ar5" = "AR(5)", 
  "ma1" = "MA(1)", "ma2" = "MA(2)", "ma3" = "MA(3)", "ma4" = "MA(4)", "ma5" = "MA(5)", 
  "intercept" = "Constante (Média)", "mu" = "Constante (Média)",
  "mxreg1" = "Dummy Institucional (Média)",
  "omega" = "Constante (Variância)", "alpha1" = "ARCH(1) (Choque)", "beta1" = "GARCH(1) (Persistência)", 
  "skew" = "Assimetria (Skew)", "shape" = "Forma (Caudas)",
  "vxreg1" = "ENA Armazenável", "vxreg2" = "Retorno Petróleo", "vxreg3" = "Retorno Gás Natural", "vxreg4" = "Dummy Institucional (Variância)" 
)

ordem_apresentacao <- c(
  #"Constante (Média)", "mxreg1" = "Dummy Institucional (Média)",
  "Constante (Média)", "Dummy Institucional (Média)",
  "AR(1)", "AR(2)", "AR(3)", "AR(4)", "AR(5)", 
  "MA(1)", "MA(2)", "MA(3)", "MA(4)", "MA(5)",
  "Constante (Variância)", "ARCH(1) (Choque)", "GARCH(1) (Persistência)",
  "ENA Armazenável", "Retorno Petróleo", "Retorno Gás Natural", "Dummy Institucional (Variância)",
  "Assimetria (Skew)", "Forma (Caudas)"
)

# --- TABELAS 3 E 4: CENSURA E ESTACIONARIEDADE ---
tab_censura <- flextable(df_censura) %>%
  set_caption("Tabela 3 - Diagnóstico de Censura Regulatória (Freios de Piso e Teto do PLD)") %>%
  colformat_double(j = 4:7, digits = 4) %>% align(align = "center", part = "all") %>%
  set_table_properties(width = 1, layout = "autofit") %>% theme_booktabs() %>%
  add_footer_lines("Fonte: elaboração própria (2026).") %>%
  add_footer_lines("Nota: A distorção indica o quanto os zeros artificiais reduzem a variância incondicional.")

tab_estacionariedade <- flextable(df_estacionariedade) %>%
  set_caption("Tabela 4 - Resultados dos Testes de Raiz Unitária (ADF/PP) e Estacionariedade (KPSS)") %>%
  set_header_labels(
    Variavel = "Subsistema", 
    ADF_Drift_Stat = "ADF (Drift)", 
    ADF_None_Stat = "ADF (Nenhum)", 
    PP_Stat = "PP (Z-tau)", 
    KPSS_Stat = "KPSS (mu)"
  ) %>%
  align(align = "center", part = "all") %>% 
  set_table_properties(width = 1, layout = "autofit") %>% theme_booktabs() %>%
  add_footer_lines("Fonte: elaboração própria (2026).") %>%
  add_footer_lines("Nota: Os testes ADF e PP têm como hipótese nula a presença de raiz unitária (não-estacionariedade). O teste KPSS tem como hipótese nula a estacionariedade da série.")

# --- TABELA 5: ARCH-LM TEST ---
arch_se <- FinTS::ArchTest(residuals(modelos_arma$SE), lags = 12)
arch_s  <- FinTS::ArchTest(residuals(modelos_arma$S),  lags = 12)
arch_ne <- FinTS::ArchTest(residuals(modelos_arma$NE), lags = 12)
arch_n  <- FinTS::ArchTest(residuals(modelos_arma$N),  lags = 12)

df_arch_lm <- data.frame(
  `Subsistema` = c("Sudeste/Centro-Oeste", "Sul", "Nordeste", "Norte"),
  `Estatística Chi-Quadrado` = c(arch_se$statistic, arch_s$statistic, arch_ne$statistic, arch_n$statistic),
  `p-valor` = c(arch_se$p.value, arch_s$p.value, arch_ne$p.value, arch_n$p.value), 
  check.names = FALSE
)

tab_arch <- flextable(df_arch_lm) %>%
  set_caption("Tabela 5 - Resultados do teste ARCH-LM aplicado sobre os resíduos da equação da média (ARMA) por subsistema") %>%
  colformat_double(j = 2:3, digits = 4) %>%
  bold(i = ~ `p-valor` < 0.05, j = "p-valor") %>%
  bg(i = ~ `p-valor` < 0.05, bg = "#F4F9F4") %>%
  align(j = 2:3, align = "center", part = "all") %>%
  set_table_properties(width = 1, layout = "autofit") %>% 
  theme_booktabs() %>% 
  add_footer_lines("Fonte: elaboração própria (2026).")

# --- TABELA 6: ARMA (MÉDIA CONDICIONAL) ---
cat("\nGerando Tabela de Modelos ARMA...\n")

extrair_arma <- function(modelo, nome_sub) {
  coefs <- modelo$coef
  se <- sqrt(diag(modelo$var.coef))
  
  if (length(coefs) == 0) return(data.frame(Subsistema = nome_sub, stringsAsFactors = FALSE))
  
  df_list <- list(Subsistema = nome_sub)
  
  nomes_originais <- names(coefs)
  nomes_originais[grepl("dummy|xreg|matriz", nomes_originais, ignore.case = TRUE)] <- "mxreg1"
  
  for(i in seq_along(coefs)) {
    z_stat <- coefs[i] / se[i]
    p_val <- 2 * (1 - pnorm(abs(z_stat)))
    asteriscos <- ifelse(p_val < 0.01, "***", ifelse(p_val < 0.05, "**", ifelse(p_val < 0.1, "*", "")))
    df_list[[ nomes_originais[i] ]] <- sprintf("%.4f%s\n(%.4f)", coefs[i], asteriscos, se[i])
  }
  
  return(as.data.frame(df_list, stringsAsFactors = FALSE))
}

arma_full <- list(
  extrair_arma(modelos_arma$SE, "Sudeste/Centro-Oeste"),
  extrair_arma(modelos_arma$S, "Sul"),
  extrair_arma(modelos_arma$NE, "Nordeste"),
  extrair_arma(modelos_arma$N, "Norte")
)

tab_arma <- flextable(compilar_tabela_formatada(arma_full, nomes_de_para, ordem_apresentacao)) %>%
  set_caption("Tabela 6 - Modelos de Média Condicional ARMA") %>%
  set_table_properties(width = 1, layout = "autofit") %>% 
  theme_booktabs() %>%
  align(align = "center", part = "all") %>%
  add_footer_lines("Fonte: elaboração própria (2026).") %>%
  add_footer_lines("Nota: Erro padrão entre parênteses. Significância: * p<0.1; ** p<0.05; *** p<0.01.")


# --- FUNÇÃO AUXILIAR PARA PLOTAR RESULTADOS GARCH ---
extrair_garch <- function(fit, nome_sub) {
  # Tenta extrair a matriz robusta primeiro
  coefs <- fit@fit$robust.matcoef
  # Se a matriz robusta falhar (NULL), usa a matriz de erros padrão
  if (is.null(coefs)) coefs <- fit@fit$matcoef
  # Se o modelo não tiver convergido de forma alguma
  if (is.null(coefs)) return(data.frame(Subsistema = nome_sub, stringsAsFactors = FALSE))
  
  # Ancorando a identidade da série no índice primário da lista
  df_list <- list(Subsistema = nome_sub)
  
  for(i in 1:nrow(coefs)) {
    est <- coefs[i, 1]
    se <- coefs[i, 2] 
    pval <- coefs[i, 4]
    
    asteriscos <- ifelse(is.na(pval), "", 
                         ifelse(pval < 0.01, "***", 
                                ifelse(pval < 0.05, "**", 
                                       ifelse(pval < 0.1, "*", ""))))
    se_formatado <- ifelse(is.na(se), "NA", sprintf("%.4f", se))
    df_list[[ rownames(coefs)[i] ]] <- sprintf("%.4f%s\n(%s)", est, asteriscos, se_formatado)
  }
  
  return(as.data.frame(df_list, stringsAsFactors = FALSE))
}


# --- TABELA 7: GARCH PURO (REFERÊNCIA) ---
cat("\nGerando Tabela de GARCH Puro...\n")

garch_puro_list <- list(
  extrair_garch(fit_se_puro, "Sudeste/Centro-Oeste"),
  extrair_garch(fit_s_puro, "Sul")
  # extrair_garch(fit_ne_puro, "Nordeste"),
  # extrair_garch(fit_n_puro, "Norte")
)

tab_garch_puro <- flextable(compilar_tabela_formatada(garch_puro_list, nomes_de_para, ordem_apresentacao)) %>%
  set_caption("Tabela 7 - Modelos de Volatilidade Condicional GARCH Puro") %>%
  align(align = "center", part = "all") %>%
  set_table_properties(width = 1, layout = "autofit") %>% theme_booktabs() %>%
  add_footer_lines("Fonte: elaboração própria (2026).") %>%
  add_footer_lines("Nota: Erros padrões robustos entre parênteses. Significância: *** 1%, ** 5%, * 10%.")

# --- TABELA 8: MODELOS GARCH-X ---
cat("\nGerando Tabela de GARCH-X\n")

garch_x_list <- list(
  extrair_garch(fit_se_m4, "Sudeste/Centro-Oeste"),
  extrair_garch(fit_s_m4, "Sul")
  # extrair_garch(fit_ne_m4, "Nordeste"),
  # extrair_garch(fit_n_m4, "Norte")
)

tab_garch_x <- flextable(compilar_tabela_formatada(garch_x_list, nomes_de_para, ordem_apresentacao)) %>%
  set_caption("Tabela 8 - Modelos de Volatilidade Condicional GARCH-X") %>%
  align(align = "center", part = "all") %>%
  set_table_properties(width = 1, layout = "autofit") %>% theme_booktabs() %>%
  add_footer_lines("Fonte: elaboração própria (2026).") %>%
  add_footer_lines("Nota: Erros padrões robustos entre parênteses. Significância: *** 1%, ** 5%, * 10%.")


# --- TABELA 9: DIAGNÓSTICO COMPARATIVO DE RESÍDUOS (LJUNG-BOX) ---
cat("\nCalculando testes de Ljung-Box para modelos Puros e GARCH-X...\n")

# Função para extrair Ljung-Box sem quebrar o data.frame enquanto extrai vetor numérico puro
safe_lb_table <- function(fit) {
  # Tenta extrair resíduos; se der erro crítico, devolve NULL
  res_brutos <- tryCatch(residuals(fit, standardize = TRUE), error = function(e) NULL)
  res_limpos <- as.numeric(na.omit(res_brutos))
  
  # Se o modelo não convergiu (resíduos vazios), devolve lista com NAs
  if(length(res_limpos) == 0) {
    return(list(statistic = NA, p.value = NA))
  }
  
  return(Box.test(res_limpos^2, lag = 12, type = "Ljung-Box", fitdf = 2))
}

lb_se_puro <- safe_lb_table(fit_se_puro)
lb_se_m4   <- safe_lb_table(fit_se_m4)
lb_s_puro  <- safe_lb_table(fit_s_puro)
lb_s_m4    <- safe_lb_table(fit_s_m4)

df_ljung_box <- data.frame(
  `Modelo Estudo` = c("Sudeste/Centro-Oeste - GARCH Puro", 
                      "Sudeste/Centro-Oeste - GARCH-X", 
                      "Sul - GARCH Puro", 
                      "Sul - GARCH-X"),
  `Estatística Chi-Quadrado` = c(lb_se_puro$statistic, lb_se_m4$statistic, lb_s_puro$statistic, lb_s_m4$statistic),
  `p-valor` = c(lb_se_puro$p.value, lb_se_m4$p.value, lb_s_puro$p.value, lb_s_m4$p.value),
  check.names = FALSE
)

print(df_ljung_box, digits = 12) # imprime no console as estatísticas com alta precisão

tab_ljung_box <- flextable(df_ljung_box) %>%
  set_caption("Tabela 9 - Resultados do teste de Ljung-Box (Lag 12) sobre os resíduos quadrados") %>%
  colformat_double(j = 2:3, digits = 4) %>%
  bold(i = ~ `p-valor` < 0.05, j = "p-valor") %>% 
  align(j = 2:3, align = "center", part = "all") %>%
  set_table_properties(width = 1, layout = "autofit") %>% theme_booktabs() %>% 
  add_footer_lines("Fonte: elaboração própria (2026).") %>%
  add_footer_lines("Nota: A hipótese nula indica ausência de autocorrelação serial nos resíduos quadrados.")

# --- TABELA 10: COMPARAÇÃO DE CRITÉRIOS DE INFORMAÇÃO (AIC/BIC) ---
extrair_ic <- function(mod_puro, mod_full, nome_sub) {
  # Tenta extrair a matriz de critérios; se o modelo não tiver convergido, retorna NAs
  ic_puro <- tryCatch({ as.numeric(rugarch::infocriteria(mod_puro)) }, error = function(e) c(NA, NA, NA, NA))
  ic_full <- tryCatch({ as.numeric(rugarch::infocriteria(mod_full)) }, error = function(e) c(NA, NA, NA, NA))
  
  df_temp <- data.frame(
    Subsistema = nome_sub, V1 = ic_puro[1], V2 = ic_full[1], V3 = ic_puro[2], V4 = ic_full[2], stringsAsFactors = FALSE
  )
  
  colnames(df_temp) <- c("Subsistema", "AIC (Puro)", "AIC (GARCH-X)", "BIC (Puro)", "BIC (GARCH-X)")
  return(df_temp)
}

df_ic <- as.data.frame(bind_rows(
  extrair_ic(fit_se_puro, fit_se_m4, "Sudeste/Centro-Oeste"),
  extrair_ic(fit_s_puro, fit_s_m4, "Sul")
  # extrair_ic(fit_ne_puro, fit_ne_m4, "Nordeste"),
  # extrair_ic(fit_n_puro, fit_n_m4, "Norte")
))

tab_ic <- flextable(df_ic) %>%
  set_caption("Tabela 10 - Critérios de Informação (AIC e BIC): GARCH Puro vs. GARCH-X") %>%
  colformat_double(j = 2:5, digits = 4) %>%
  align(j = 2:5, align = "center", part = "all") %>%
  set_table_properties(width = 1, layout = "autofit") %>% theme_booktabs() %>%
  add_footer_lines("Fonte: elaboração própria (2026).") %>%
  add_footer_lines("Nota: Valores menores indicam melhor ajuste penalizado à amostra.")

# --- COMPILAÇÃO DO ARQUIVO WORD FINAL ---
doc <- read_docx() %>%
  #body_add_par("ANEXO ECONOMÉTRICO: TABELAS CONSOLIDADAS", style = "heading 1") %>%
  body_add_flextable(tab_censura) %>% body_add_break() %>%
  body_add_flextable(tab_estacionariedade) %>% body_add_break() %>%
  body_add_flextable(tab_arch) %>% body_add_break() %>%
  body_add_flextable(tab_arma) %>% body_add_break() %>%
  body_add_flextable(tab_garch_puro) %>% body_add_break() %>%
  body_add_flextable(tab_garch_x) %>% body_add_break() %>%
  body_add_flextable(tab_ljung_box) %>% body_add_break() %>%
  body_add_flextable(tab_ic)

print(doc, target = "relatorios/Relatorio_Econometrico.docx")
cat("\nPipeline de Relatório Executado com sucesso! Arquivo consolidado salvo em 'relatorios/'.\n")