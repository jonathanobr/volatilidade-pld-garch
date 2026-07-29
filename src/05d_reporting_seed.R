# src/05d_reporting_seed.R
#
# MÓDULO DE RELATÓRIO DE SELEÇÃO DE SEED — Apêndice Metodológico
# Saída: relatorios/Apendice_Seed_Selection.docx
# ──────────────────────────────────────────────────────────────────────────────

source("src/00_config.R")

dir_seed <- file.path("relatorios", "seed")
dir_out <- "relatorios"

# ==============================================================================
# 0. FUNÇÕES AUXILIARES (Grid search de sementes)
# ==============================================================================

# Formata p-valor com asteriscos de significância
fmt_pval <- function(p, digits = 4) {
    if (is.na(p)) {
        return("—")
    }
    stars <- ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.1, "*", "")))
    sprintf(paste0("%.", digits, "f%s"), p, stars)
}

# Formata número com fallback para "—"
fmt_num <- function(x, digits = 2) {
    ifelse(is.na(x), "—", formatC(x, digits = digits, format = "f"))
}

# Aplica estilo acadêmico booktabs padronizado
estilo_academico <- function(ft, caption_text, notas = NULL) {
    ft <- ft |>
        set_caption(caption_text) |>
        theme_booktabs() |>
        align(align = "center", part = "all") |>
        align(j = 1, align = "left", part = "all") |>
        set_table_properties(width = 1, layout = "autofit")
    if (!is.null(notas)) {
        ft <- add_footer_lines(ft, values = notas)
    }
    
    ft <- add_footer_lines(ft, "Fonte: elaboração própria (2026).")
    
    ft
}

# ==============================================================================
# 1. CARREGAR ARTEFATOS GERADOS PELO 04a_seed_selection.R
# ==============================================================================

cat("\n[1/4] Carregando artefatos da varredura de sementes...\n")

path_cache <- file.path(dir_seed, "resultados_raw_varredura.rds")
path_resumo <- file.path(dir_seed, "02_varredura_completa.csv")
path_validas <- file.path(dir_seed, "03_seeds_validas.csv")
path_selecao <- file.path(dir_seed, "05_seed_selecionada.rds")
path_txt <- file.path(dir_seed, "01_resumo_varredura.txt")
path_estab <- file.path(dir_seed, "04_estabilidade_coeficientes.txt")

for (p in c(path_cache, path_resumo, path_validas, path_selecao)) {
    if (!file.exists(p)) stop(sprintf("[05d] Arquivo não encontrado: %s\n       Execute 04a_seed_selection.R primeiro.", p))
}

resumo_completo <- readr::read_csv(path_resumo, show_col_types = FALSE)
seeds_validas <- readr::read_csv(path_validas, show_col_types = FALSE)
nseed_sel <- readRDS(path_selecao)

# Recupera estatísticas do arquivo de texto (evita recalcular sem o RDS bruto completo)
txt_resumo <- readLines(path_txt)
n_testadas <- as.integer(sub(".*:\\s*(\\d+)", "\\1", grep("testadas", txt_resumo, value = TRUE)))
n_validas <- nrow(seeds_validas)
pct_validas <- 100 * n_validas / n_testadas
n_parciais <- as.integer(sub(".*:\\s*(\\d+)", "\\1", grep("n_ok = 3", txt_resumo, value = TRUE)))

# Taxas de falha por modelo (lidos do CSV completo)
taxa_falha <- function(col) round(100 * mean(!resumo_completo[[col]], na.rm = TRUE), 1)
falha_se_puro <- taxa_falha("se_puro_ok")
falha_se_m4 <- taxa_falha("se_m4_ok")
falha_s_puro <- taxa_falha("s_puro_ok")
falha_s_m4 <- taxa_falha("s_m4_ok")

cat(sprintf("  Seeds testadas : %d\n", n_testadas))
cat(sprintf("  Seeds válidas  : %d (%.1f%%)\n", n_validas, pct_validas))
cat(sprintf("  Seed selecionada: %d\n\n", nseed_sel))

# ==============================================================================
# 2. TABELA A1 — RESUMO DA VARREDURA (estatísticas + critérios de filtragem)
# ==============================================================================

cat("[2/4] Gerando Tabela A1 — Resumo da varredura...\n")

# Bloco 1: quantidades globais
df_a1_global <- data.frame(
    `Estatística` = c(
        "Sementes candidatas testadas",
        "Sementes válidas (todos os critérios satisfeitos)",
        "Sementes com convergência parcial (3 de 4 modelos)",
        "Sementes com 2 ou menos convergências"
    ),
    `Valor` = c(
        formatC(n_testadas, format = "d"),
        sprintf("%d (%.1f%%)", n_validas, pct_validas),
        formatC(n_parciais, format = "d"),
        formatC(n_testadas - n_validas - n_parciais, format = "d")
    ),
    check.names = FALSE
)

# Bloco 2: taxa de falha por modelo
df_a1_falha <- data.frame(
    `Estatística` = c(
        "Taxa de falha — Sudeste/CO GARCH Puro",
        "Taxa de falha — Sudeste/CO GARCH-X",
        "Taxa de falha — Sul GARCH Puro",
        "Taxa de falha — Sul GARCH-X"
    ),
    `Valor` = c(
        sprintf("%.1f%%", falha_se_puro),
        sprintf("%.1f%%", falha_se_m4),
        sprintf("%.1f%%", falha_s_puro),
        sprintf("%.1f%%", falha_s_m4)
    ),
    check.names = FALSE
)

# Bloco 3: critérios de seleção aplicados
df_a1_criterios <- data.frame(
    `Estatística` = c(
        "Critério 1 — Convergência do solver",
        "Critério 2 — Hessiana invertível (sem NAs)",
        "Critério 3 — Matriz de covariância positiva-semidefinida",
        "Critério 4 — Estacionariedade GARCH (α + β < 1,3)",
        "Desempate — Log-verossimilhança total máxima"
    ),
    `Valor` = c(
        "convergence == 0 (rugarch)",
        "!any(is.na(vcov)) & all(is.finite(vcov))",
        "min(autovalores) > −1×10⁻⁴",
        "α₁ + β₁ < 1,3 em todos os 4 modelos",
        "Σ LL(se_puro, se_m4, s_puro, s_m4) → máximo"
    ),
    check.names = FALSE
)

df_a1 <- bind_rows(df_a1_global, df_a1_falha, df_a1_criterios)

tab_a1 <- flextable(df_a1) |>
    estilo_academico(
        caption_text = "Tabela A1 — Resumo da Varredura Sistemática de Sementes Aleatórias",
        notas = c(
            "Nota: A varredura cobriu 500 sementes aleatórias (amostradas com set.seed(2024) de [1; 10.000.001]) acrescidas de 12 sementes de referência (conhecidas por falhar ou convergir), totalizando 512 candidatas. A tolerância do solver foi fixada em tol = 1×10⁻⁴ com n.restarts = 15 (gosolnp). O critério de estacionariedade foi relaxado para α + β < 1,3 em razão da série censurada."
        )
    ) |>
    bold(i = (nrow(df_a1_global) + nrow(df_a1_falha) + 1):nrow(df_a1), j = 1) |>
    bg(i = (nrow(df_a1_global) + 1):(nrow(df_a1_global) + nrow(df_a1_falha)), bg = "#F7F7F7") |>
    bg(i = (nrow(df_a1_global) + nrow(df_a1_falha) + 1):nrow(df_a1), bg = "#EEF4FB")

# ==============================================================================
# 3. TABELA A2 — CONJUNTO REDUZIDO DE SEMENTES VÁLIDAS
# ==============================================================================

cat("[3/4] Gerando Tabela A2 — Sementes válidas...\n")

# Garante que ll_total e ab_max existam (podem ter sido salvos pelo 04a ou não)
if (!"ll_total" %in% colnames(seeds_validas)) {
    seeds_validas <- seeds_validas |>
        dplyr::mutate(
            ll_total = ll_se_puro + ll_se_m4 + ll_s_puro + ll_s_m4,
            ab_max   = pmax(ab_se_puro, ab_se_m4, ab_s_puro, ab_s_m4, na.rm = TRUE)
        ) |>
        dplyr::arrange(dplyr::desc(ll_total))
}

df_a2 <- seeds_validas |>
    dplyr::select(
        seed, ll_total, ab_max,
        ll_se_puro, ll_se_m4, ll_s_puro, ll_s_m4
    ) |>
    dplyr::mutate(
        selecionada  = ifelse(seed == nseed_sel, "✓", ""),
        seed         = formatC(seed, format = "d", big.mark = ""),
        ll_total     = fmt_num(ll_total, 2),
        ab_max       = fmt_num(ab_max, 4),
        ll_se_puro   = fmt_num(ll_se_puro, 2),
        ll_se_m4     = fmt_num(ll_se_m4, 2),
        ll_s_puro    = fmt_num(ll_s_puro, 2),
        ll_s_m4      = fmt_num(ll_s_m4, 2)
    ) |>
    dplyr::relocate(selecionada, .after = seed) |>
    as.data.frame()

colnames(df_a2) <- c(
    "Semente", "Sel.", "LL Total",
    "α+β máx.", "LL SE Puro", "LL SE GARCH-X",
    "LL S Puro", "LL S GARCH-X"
)

tab_a2 <- flextable(df_a2) |>
    estilo_academico(
        caption_text = "Tabela A2 — Sementes Aleatórias Válidas (Conjunto Reduzido)",
        notas = c(
            paste0(
                "Nota: Apenas as sementes que satisfizeram simultaneamente os quatro critérios de seleção ",
                "em todos os modelos são listadas. A coluna 'Sel.' indica (✓) a semente efetivamente ",
                "utilizada na estimação final. LL = log-verossimilhança; α+β máx. = maior soma dos ",
                "parâmetros ARCH e GARCH entre os quatro modelos. Valores ordenados de forma decrescente por LL Total."
            )
        )
    ) |>
    bold(i = ~ Sel. == "✓") |>
    bg(i = ~ Sel. == "✓", bg = "#EEF4FB") |>
    color(i = ~ Sel. == "✓", j = "Sel.", color = "#1A5276") |>
    bold(i = ~ Sel. == "✓", j = "Sel.")

# ==============================================================================
# 4. TABELA A3 — SEMENTE SELECIONADA: DETALHE E JUSTIFICATIVA
# ==============================================================================

cat("[4/4] Gerando Tabela A3 — Detalhamento da semente selecionada...\n")

linha_sel <- seeds_validas |>
    dplyr::filter(seed == nseed_sel)

posicao_ranking <- which(seeds_validas$seed == nseed_sel)

# Verifica se temos as colunas de α+β individuais
tem_ab_ind <- all(c("ab_se_puro", "ab_se_m4", "ab_s_puro", "ab_s_m4") %in% colnames(linha_sel))

df_a3 <- data.frame(
    `Atributo` = c(
        "Semente (seed)",
        "Posição no ranking de LL Total",
        "Log-verossimilhança total (Σ LL)",
        "LL — Sudeste/CO GARCH Puro",
        "LL — Sudeste/CO GARCH-X",
        "LL — Sul GARCH Puro",
        "LL — Sul GARCH-X",
        if (tem_ab_ind) {
            c(
                "α + β — Sudeste/CO GARCH Puro",
                "α + β — Sudeste/CO GARCH-X",
                "α + β — Sul GARCH Puro",
                "α + β — Sul GARCH-X",
                "α + β máximo (entre os 4 modelos)"
            )
        } else {
            "α + β máximo (entre os 4 modelos)"
        },
        "Critério de desempate aplicado"
    ),
    `Valor` = c(
        formatC(nseed_sel, format = "d"),
        sprintf("%dº de %d sementes válidas", posicao_ranking, n_validas),
        fmt_num(linha_sel$ll_total, 4),
        fmt_num(linha_sel$ll_se_puro, 4),
        fmt_num(linha_sel$ll_se_m4, 4),
        fmt_num(linha_sel$ll_s_puro, 4),
        fmt_num(linha_sel$ll_s_m4, 4),
        if (tem_ab_ind) {
            c(
                fmt_num(linha_sel$ab_se_puro, 6),
                fmt_num(linha_sel$ab_se_m4, 6),
                fmt_num(linha_sel$ab_s_puro, 6),
                fmt_num(linha_sel$ab_s_m4, 6),
                fmt_num(linha_sel$ab_max, 6)
            )
        } else {
            fmt_num(linha_sel$ab_max, 6)
        },
        "Maior log-verossimilhança total (Σ LL)"
    ),
    check.names = FALSE
)

tab_a3 <- flextable(df_a3) |>
    estilo_academico(
        caption_text = sprintf(
            "Tabela A3 — Detalhamento da Semente Selecionada (seed = %d)", nseed_sel
        ),
        notas = c(
            "Nota 1: A semente foi escolhida automaticamente pelo script 04a_seed_selection.R como aquela com maior log-verossimilhança total entre todas as candidatas que satisfizeram simultaneamente os quatro critérios de seleção (convergência, invertibilidade da Hessiana, positividade da matriz de covariância e estacionariedade GARCH). O critério de seleção é independente do sinal ou significância dos coeficientes estimados.",
            "Nota 2: O limiar de α+β < 1,3 foi definido de forma a excluir soluções numericamente degeneradas (valores próximos a 2 ou superiores, indicativos de não-convergência mascarada), preservando ao mesmo tempo as soluções com persistência localmente explosiva mas numericamente estável (características documentadas por Engle e Bollerslev (1986) em séries com comportamento IGARCH e discutidas na seção 5.2.3)."
        )
    ) |>
    bold(i = 1, j = 2) |>
    bg(i = 1, bg = "#EEF4FB")

# ==============================================================================
# 5. COMPILAÇÃO DO DOCUMENTO WORD
# ==============================================================================

cat("\nCompilando documento Word...\n")

doc <- read_docx() |>
    body_add_par("Apêndice — Seleção Sistemática da Semente Aleatória", style = "heading 1") |>
    body_add_par(
        paste0(
            "Este apêndice documenta o procedimento de seleção da semente aleatória (seed) ",
            "utilizada na estimação dos modelos GARCH e GARCH-X. A escolha foi realizada por ",
            "varredura sistemática, avaliando ", formatC(n_testadas, format = "d"),
            " sementes candidatas segundo critérios objetivos e pré-definidos, garantindo ",
            "reprodutibilidade e independência em relação à inicialização do solver."
        ),
        style = "Normal"
    ) |>
    body_add_par("", style = "Normal") |>
    body_add_flextable(tab_a1) |>
    body_add_break() |>
    body_add_flextable(tab_a2) |>
    body_add_break() |>
    body_add_flextable(tab_a3)

path_out <- file.path(dir_out, "Apendice_Seed_Selection.docx")
print(doc, target = path_out)

cat(sprintf("\nApêndice gerado com sucesso: %s\n", path_out))
cat("  • Tabela A1 — Resumo da varredura (critérios + estatísticas agregadas)\n")
cat("  • Tabela A2 — Conjunto reduzido de sementes válidas\n")
cat("  • Tabela A3 — Detalhamento da semente selecionada\n\n")
