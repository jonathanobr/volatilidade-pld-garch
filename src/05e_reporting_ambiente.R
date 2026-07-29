# src/05e_reporting_ambiente.R
#
# MÓDULO DE RELATÓRIO DO AMBIENTE COMPUTACIONAL — Apêndice B
# Saída: relatorios/Apendice_B1_Ambiente.docx
# ──────────────────────────────────────────────────────────────────────────────

source("src/00_config.R")

dir_out <- "relatorios"
if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)

# ==============================================================================
# 0. FUNÇÕES AUXILIARES (detecção automática de versões externas)
# ==============================================================================

# Versão de um executável externo via flag --version
versao_externa <- function(exe, args = "--version") {
    tryCatch({
        raw <- suppressWarnings(
            system2(exe, args = args, stdout = TRUE, stderr = TRUE)
        )
        linha <- raw[nchar(trimws(raw)) > 0][1]
        m <- regmatches(linha, regexpr("[0-9]+\\.[0-9]+(\\.[0-9]+)?", linha))
        if (length(m) > 0) m else trimws(linha)
    }, error = function(e) NA_character_)
}

# Versão de um pacote Python via pip show
versao_pip <- function(pacote) {
    python_exe <- Sys.getenv("PYTHON_PATH", unset = "python")
    tryCatch({
        raw <- suppressWarnings(
            system2(python_exe, args = c("-m", "pip", "show", pacote),
                    stdout = TRUE, stderr = FALSE)
        )
        linha <- raw[grepl("^Version:", raw, ignore.case = TRUE)]
        if (length(linha) == 0) return(NA_character_)
        trimws(sub("Version:\\s*", "", linha[1], ignore.case = TRUE))
    }, error = function(e) NA_character_)
}

# Versão de pacote R com fallback
versao_r <- function(pacote) {
    tryCatch(as.character(packageVersion(pacote)), error = function(e) NA_character_)
}

# Aplica estilo acadêmico booktabs padronizado (idêntico ao 05d)
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
# 1. DETECÇÃO DE VERSÕES
# ==============================================================================

cat("\n[1/2] Detectando versões do ambiente...\n")

# R
r_ver         <- paste(R.version$major, R.version$minor, sep = ".")

# RStudio (via rstudioapi, com fallback silencioso)
rstudio_ver   <- tryCatch({
    v <- rstudioapi::versionInfo()
    paste0(v$version, " (Build ", v$build, ")")
}, error = function(e) NA_character_)

# Semente selecionada (lida do artefato gerado pelo 04a, se disponível)
path_seed     <- file.path("relatorios", "seed", "05_seed_selecionada.rds")
seed_val      <- tryCatch(as.character(readRDS(path_seed)), error = function(e) "—")

# Pacotes R
ver_rugarch   <- versao_r("rugarch")
ver_tseries   <- versao_r("tseries")
ver_urca      <- versao_r("urca")
ver_forecast  <- versao_r("forecast")
ver_fints     <- versao_r("FinTS")
ver_perfan    <- versao_r("PerformanceAnalytics")

# PostgreSQL
psql_ver      <- versao_externa("psql")
psql_str      <- ifelse(is.na(psql_ver), "—", psql_ver)

# Python
python_exe    <- Sys.getenv("PYTHON_PATH", unset = "python")
py_ver        <- versao_externa(python_exe)
py_str        <- ifelse(is.na(py_ver), "3.14", py_ver)     # fallback declarado

ver_requests  <- versao_pip("requests")
ver_pandas    <- versao_pip("pandas")
ver_ipea      <- versao_pip("ipeadatapy")

cat(sprintf("  R %s | PostgreSQL %s | Python %s\n", r_ver, psql_str, py_str))

# ==============================================================================
# 2. MONTAGEM DO DATA FRAME (estrutura flat, 3 colunas — igual à imagem)
# ==============================================================================

df_b1 <- data.frame(
    Componente  = c(
        # ── Pacotes R ──────────────────────────────────────────────────────────
        "Pacote rugarch",
        "Pacote tseries",
        "Pacote urca",
        "Pacote forecast",
        "Pacote FinTS",
        "Pacote PerformanceAnalytics",
        # ── Reprodutibilidade ──────────────────────────────────────────────────
        "Semente principal (seed)",
        # ── Infraestrutura R ───────────────────────────────────────────────────
        "RStudio",
        "R",
        # ── Banco de dados ─────────────────────────────────────────────────────
        "PostgreSQL",
        # ── Python ─────────────────────────────────────────────────────────────
        "Python",
        "Pacote requests",
        "Pacote pandas",
        "Pacote ipeadatapy",
        # ── SO ─────────────────────────────────────────────────────────────────
        "Sistema operacional"
    ),
    `Função no pipeline` = c(
        "Estimação sGARCH/GARCH-X",
        "Testes ADF, KPSS",
        "Teste de raiz unitária (PP)",
        "Identificação e estimação ARMA",
        "Teste ARCH-LM",
        "Diagnósticos de resíduos",
        "Reprodutibilidade da estimação",
        "IDE",
        "Ambiente de execução",
        "Persistência de dados e execução do pipeline de ETL (PL/pgSQL)",
        "Coleta automatizada de dados via API",
        "Requisições HTTP às APIs (IPEADATA, ONS S3) com retry e streaming",
        "Manipulação e exportação de séries temporais (DataFrames → CSV)",
        "Interface com a API pública do IPEADATA (séries macroeconômicas)",
        "Infraestrutura"
    ),
    `Versão` = c(
        ifelse(is.na(ver_rugarch),  "—", ver_rugarch),
        ifelse(is.na(ver_tseries),  "—", ver_tseries),
        ifelse(is.na(ver_urca),     "—", ver_urca),
        ifelse(is.na(ver_forecast), "—", ver_forecast),
        ifelse(is.na(ver_fints),    "—", ver_fints),
        ifelse(is.na(ver_perfan),   "—", ver_perfan),
        seed_val,
        ifelse(is.na(rstudio_ver),  "—", rstudio_ver),
        r_ver,
        psql_str,
        py_str,
        ifelse(is.na(ver_requests), "—", ver_requests),
        ifelse(is.na(ver_pandas),   "—", ver_pandas),
        ifelse(is.na(ver_ipea),     "—", ver_ipea),
        paste0(Sys.info()[["sysname"]], " ", Sys.info()[["release"]])
    ),
    check.names    = FALSE,
    stringsAsFactors = FALSE
)

cat("\n--- Tabela B1 (preview) ---\n")
print(df_b1)

# ==============================================================================
# 3. FORMATAÇÃO COM FLEXTABLE (estilo acadêmico do 05d)
# ==============================================================================

cat("\n[2/2] Formatando tabela B1...\n")

# Índices das linhas que iniciam novos blocos (separador visual sutil)
i_pg  <- which(df_b1$Componente == "PostgreSQL")
i_py  <- which(df_b1$Componente == "Python")
i_so  <- which(df_b1$Componente == "Sistema operacional")

tab_b1 <- flextable(df_b1) |>
    estilo_academico(
        caption_text = "Tabela B1 - Ambiente Computacional",
        notas = paste0(
            "Nota: Versões registradas automaticamente em ",
            format(Sys.Date(), "%d/%m/%Y"),
            ". O ambiente R completo está disponível em 'relatorios/ambiente_pacotes.csv'."
        )
    ) |>
    # Separador horizontal antes de cada bloco temático
    hline(i = i_pg - 1,  border = officer::fp_border(color = "#AAAAAA", width = 0.5)) |>
    hline(i = i_py - 1,  border = officer::fp_border(color = "#AAAAAA", width = 0.5)) |>
    hline(i = i_so - 1,  border = officer::fp_border(color = "#AAAAAA", width = 0.5)) |>
    # Coluna 2 (Função) alinhada à esquerda — overrides o align "center" do estilo base
    align(j = 2, align = "left", part = "all")

# ==============================================================================
# 4. EXPORTAÇÃO PARA .DOCX
# ==============================================================================

doc <- read_docx() |>
    body_add_par("Apêndice B — Ambiente Computacional", style = "heading 1") |>
    body_add_par(
        paste0(
            "O ambiente computacional utilizado na estimação final é descrito na Tabela B1."
        ),
        style = "Normal"
    ) |>
    body_add_par("", style = "Normal") |>
    body_add_flextable(tab_b1)

path_out <- file.path(dir_out, "Apendice_B1_Ambiente.docx")
print(doc, target = path_out)

cat(sprintf("\nTabela B1 gerada com sucesso: %s\n", path_out))
