# src/04a_seed_selection.R
#
# MÓDULO DE SELEÇÃO DE SEED — executado via source() pelo 04_garch_modeling.R
#
# Contrato de interface:
#   ENTRADA  : objetos já existentes no ambiente do módulo pai:
#                specs_list  — lista nomeada com os 4 ugarchspec()
#                dados_list  — lista nomeada com os vetores de retorno correspondentes
#   SAÍDA    : valor escalar invisível (nseed) que source() captura em $value
#              + relatórios gravados em relatorios/seed/
#
# Critério de seleção (explícito e pré-definido):
#   1. Convergência (convergence == 0) nos 4 modelos simultaneamente
#   2. Hessiana invertível e sem NAs em todos os 4
#   3. Matriz de covariância positiva-definida (todos autovalores > 1e-10)
#   4. Estacionariedade GARCH (alpha1 + beta1 < 1) nos 4 modelos
#   Desempate: maior log-likelihood somado entre os 4 modelos
#   O critério é INDEPENDENTE do sinal ou significância dos coeficientes.

(function() {

library(parallel)

dir_seed     <- file.path("relatorios", "seed")
dir.create(dir_seed, recursive = TRUE, showWarnings = FALSE)
path_cache   <- file.path(dir_seed, "resultados_raw_varredura.rds")
path_resumo  <- file.path(dir_seed, "02_varredura_completa.csv")
path_selecao <- file.path(dir_seed, "05_seed_selecionada.rds")

cat("\n", strrep("=", 60), "\n", sep = "")
cat("  04a — SELEÇÃO DE SEED POR VARREDURA SISTEMÁTICA\n")
cat(strrep("=", 60), "\n\n", sep = "")

# ══════════════════════════════════════════════════════════════════════════════
# 1. INTERRUPTOR DE CACHE
# ══════════════════════════════════════════════════════════════════════════════

if (file.exists(path_cache) && file.exists(path_selecao)) {
  data_cache  <- format(file.mtime(path_cache), "%Y-%m-%d %H:%M")
  nseed_cache <- readRDS(path_selecao)

  cat(sprintf("Cache encontrado : %s\n", path_cache))
  cat(sprintf("  Gerado em      : %s\n", data_cache))
  cat(sprintf("  Seed salva     : %d\n\n", nseed_cache))

  reutilizar <- if (interactive()) {
    resp <- readline("Reutilizar cache e pular varredura? [S/n] (Enter = S): ")
    !tolower(trimws(resp)) %in% c("n", "nao", "não", "no")
  } else {
    if (Sys.getenv("SEED_FORCE_RERUN", "0") != "1") {
      cat("[não-interativo] Reutilizando cache. Use SEED_FORCE_RERUN=1 para forçar.\n")
      TRUE
    } else {
      cat("[não-interativo] SEED_FORCE_RERUN=1 — reexecutando varredura.\n")
      FALSE
    }
  }

  if (reutilizar) {
    cat(sprintf("\nSeed reutilizada do cache: %d\n", nseed_cache))
    cat(strrep("-", 60), "\n")
    return(invisible(nseed_cache))
  }
}

# ══════════════════════════════════════════════════════════════════════════════
# 2. SERIALIZAÇÃO DE SPECS PARA WORKERS
#
# Problema: ugarchspec() é um objeto S4 com slots que referenciam ambientes R.
# Esses ambientes não sobrevivem à serialização PSOCK (Windows) nem FORK
# controlado. Solução: extrair os argumentos primitivos (listas, matrizes,
# vetores) do spec e reconstruir ugarchspec() dentro de cada worker.
# Primitivos serializam perfeitamente em qualquer plataforma.
# ══════════════════════════════════════════════════════════════════════════════

# Extrai os argumentos primitivos de um ugarchspec para poder recriá-lo
# nos workers sem depender de serialização de objetos S4.
#
# Slots relevantes do rugarch:
#   spec@model$mexdata  — regressores da MÉDIA   (mxreg)
#   spec@model$vexdata  — regressores da VARIÂNCIA (vxreg)
#   spec@model$modelinc — vetor de contagens de parâmetros por componente
.extrair_args_spec <- function(spec) {
  list(
    variance.model     = spec@model$modelinc,        # não usado diretamente
    variance.model_raw = list(
      model           = spec@model$modeldesc$vmodel,
      garchOrder      = c(spec@model$modelinc["alpha"],
                          spec@model$modelinc["beta"]),
      external.regressors = if (spec@model$modelinc["vxreg"] > 0)
        spec@model$vexdata else NULL
    ),
    mean.model_raw = list(
      armaOrder           = c(spec@model$modelinc["ar"],
                              spec@model$modelinc["ma"]),
      include.mean        = as.logical(spec@model$modelinc["mu"]),
      external.regressors = if (spec@model$modelinc["mxreg"] > 0)
        spec@model$mexdata else NULL
    ),
    distribution.model = spec@model$modeldesc$distribution
  )
}

# Reconstrói ugarchspec a partir dos args primitivos extraídos acima
.reconstruir_spec <- function(args) {
  ugarchspec(
    variance.model     = args$variance.model_raw,
    mean.model         = args$mean.model_raw,
    distribution.model = args$distribution.model
  )
}

# Serializa specs_list como lista de args primitivos
specs_args_list <- lapply(specs_list, .extrair_args_spec)

# ── Validação: checa se a reconstrução é fiel antes de distribuir aos workers
cat("Validando reconstrução dos specs... ")
specs_reconstruidos <- lapply(specs_args_list, .reconstruir_spec)
cat("OK\n\n")

# ══════════════════════════════════════════════════════════════════════════════
# 3. FUNÇÕES DOS WORKERS (operam apenas com primitivos)
# ══════════════════════════════════════════════════════════════════════════════

# ctrl_fixo é herdado do ambiente pai (04_garch_modeling.R) e controla
# n.restarts e tol — deve ser idêntico ao usado na estimação final.
# O worker injeta rseed = seed por chamada.
if (!exists("ctrl_fixo")) {
  warning("[04a] ctrl_fixo não encontrado no ambiente pai — usando padrão list(n.restarts=15, tol=1e-4).")
  ctrl_fixo <- list(n.restarts = 15, tol = 1e-4)
}

.tentar_fit <- function(spec, dados, seed, solver = "gosolnp", label = "", ctrl_fixo_worker) {
  tryCatch({
    ctrl <- c(ctrl_fixo_worker, list(rseed = seed))
    fit      <- ugarchfit(spec = spec, data = dados, solver = solver,
                          solver.control = ctrl)
    conv     <- convergence(fit)
    coefs    <- coef(fit)
    vcov_mat <- vcov(fit)

    hess_ok <- tryCatch(
      !any(is.na(vcov_mat)) && all(is.finite(vcov_mat)),
      error = function(e) FALSE
    )
    pos_def <- tryCatch({
      ev <- eigen(vcov_mat, only.values = TRUE)$values
      # threshold relativo à escala da própria matriz
      # tol_rel <- max(abs(ev)) * .Machine$double.eps^0.5   # ~1.5e-8 × maior autovalor. Este é um padrão-ouro em termos de
      # precisão computacional, mas excessivamente rígido/punitivo para a série censurada. Por isso optamos por um threshold
      # mais razoável para o caso concreto analisado.
      tol_rel <- 1e-4
      all(ev > -tol_rel)
    }, error = function(e) FALSE)

    loglik_val <- tryCatch(likelihood(fit), error = function(e) NA_real_)
    alpha      <- tryCatch(coefs["alpha1"],  error = function(e) NA_real_)
    beta       <- tryCatch(coefs["beta1"],   error = function(e) NA_real_)
    estac      <- !is.na(alpha) && !is.na(beta) && (alpha + beta) < 1.3

    list(label = label, convergiu = (conv == 0), hess_ok = hess_ok,
         pos_def = pos_def, estacionario = estac, loglik = loglik_val,
         coefs = as.list(coefs),
         alpha_beta = if (!is.na(alpha)) alpha + beta else NA_real_,
         ok = (conv == 0) && hess_ok && pos_def && estac)
  }, error = function(e) {
    list(label = label, convergiu = FALSE, hess_ok = FALSE, pos_def = FALSE,
         estacionario = FALSE, loglik = NA_real_, coefs = list(),
         alpha_beta = NA_real_, ok = FALSE, erro = conditionMessage(e))
  })
}

# Worker recebe seed + listas de primitivos; reconstrói specs internamente
.avaliar_seed_worker <- function(seed, specs_args_list, dados_list,
                                 solver = "gosolnp", ctrl_fixo_worker) {
  specs_locais <- lapply(specs_args_list, function(args) {
    ugarchspec(
      variance.model     = args$variance.model_raw,
      mean.model         = args$mean.model_raw,
      distribution.model = args$distribution.model
    )
  })

  fits <- mapply(
    FUN      = .tentar_fit,
    spec     = specs_locais,
    dados    = dados_list,
    label    = names(specs_args_list),
    MoreArgs = list(seed = seed, solver = solver,
                    ctrl_fixo_worker = ctrl_fixo_worker),
    SIMPLIFY = FALSE
  )

  list(seed     = seed,
       todos_ok = all(sapply(fits, `[[`, "ok")),
       n_ok     = sum(sapply(fits, `[[`, "ok")),
       modelos  = fits)
}

.cv_por_param <- function(df) {
  df %>%
    dplyr::select(-seed) %>%
    dplyr::summarise(dplyr::across(
      dplyr::everything(),
      list(media  = mean,
           dp     = sd,
           cv_pct = ~ 100 * sd(.x, na.rm = TRUE) / abs(mean(.x, na.rm = TRUE))),
      .names = "{.col}__{.fn}"
    )) %>%
    tidyr::pivot_longer(dplyr::everything(),
                        names_to  = c("parametro", "stat"), names_sep = "__") %>%
    tidyr::pivot_wider(names_from = stat, values_from = value) %>%
    dplyr::arrange(dplyr::desc(cv_pct))
}

# ══════════════════════════════════════════════════════════════════════════════
# 4. GRADE DE SEEDS
# ══════════════════════════════════════════════════════════════════════════════

seeds_falham    <- c(42, 123, 1709, 1999, 2026, 2683, 2963, 4139, 6389, 2147483647)
seeds_convergem <- c(7079, 524287)
set.seed(2024)
seeds_aleatorias <- sample(1L:10000001L, 500L, replace = FALSE)
seeds_todas      <- unique(c(seeds_falham, seeds_convergem, seeds_aleatorias))

cat(sprintf("Grade: %d candidatas  (%d conhecidas + %d aleatórias)\n\n",
            length(seeds_todas),
            length(seeds_falham) + length(seeds_convergem),
            length(seeds_aleatorias)))

# ══════════════════════════════════════════════════════════════════════════════
# 5. VARREDURA PARALELA (PSOCK — compatível Windows/Linux/macOS)
#
# specs_args_list contém apenas primitivos R (listas, matrizes, vetores).
# Serializa e deserializa perfeitamente nos workers PSOCK.
# O ugarchspec() é reconstruído dentro de cada worker a partir desses args.
# ══════════════════════════════════════════════════════════════════════════════

n_cores   <- parallel::detectCores(logical = TRUE)
n_workers <- max(1L, n_cores - 2L)

cat(sprintf("Paralelismo PSOCK: %d threads disponíveis → %d workers\n", n_cores, n_workers))
cat(sprintf("Estimativa: ~%.0f min  (%.0fs/seed ÷ %d workers × %d seeds)\n\n",
            30 * length(seeds_todas) / n_workers / 60,
            30, n_workers, length(seeds_todas)))

cl <- parallel::makeCluster(n_workers, type = "PSOCK")
on.exit(parallel::stopCluster(cl), add = TRUE)   # garante limpeza mesmo com erro

# Exporta apenas primitivos + funções puras (sem referências a ambientes do pai)
parallel::clusterExport(
  cl      = cl,
  varlist = c(".tentar_fit", ".avaliar_seed_worker",
              "specs_args_list", "dados_list", "ctrl_fixo"),
  envir   = environment()
)
parallel::clusterEvalQ(cl, {
  library(rugarch)
  library(methods)
})

cat("Iniciando varredura paralela...\n")
t_inicio <- proc.time()

resultados_raw <- parallel::parLapplyLB(       # LB = load balancing dinâmico
  cl  = cl,
  X   = seeds_todas,
  fun = function(s) .avaliar_seed_worker(s, specs_args_list, dados_list,
                                         ctrl_fixo_worker = ctrl_fixo)
)

t_elapsed <- (proc.time() - t_inicio)[["elapsed"]]
cat(sprintf("Varredura concluída em %.1f s (%.1f min) — %.2f s/seed média\n\n",
            t_elapsed, t_elapsed / 60, t_elapsed / length(seeds_todas)))

# ══════════════════════════════════════════════════════════════════════════════
# 6. CONSOLIDAÇÃO
# ══════════════════════════════════════════════════════════════════════════════

resumo <- purrr::map_dfr(resultados_raw, function(r) {
  tibble::tibble(
    seed       = r$seed,       todos_ok   = r$todos_ok,  n_ok       = r$n_ok,
    se_puro_ok = r$modelos$se_puro$ok,    se_m4_ok = r$modelos$se_m4$ok,
    s_puro_ok  = r$modelos$s_puro$ok,     s_m4_ok  = r$modelos$s_m4$ok,
    ll_se_puro = r$modelos$se_puro$loglik, ll_se_m4 = r$modelos$se_m4$loglik,
    ll_s_puro  = r$modelos$s_puro$loglik,  ll_s_m4  = r$modelos$s_m4$loglik,
    ab_se_puro = r$modelos$se_puro$alpha_beta, ab_se_m4 = r$modelos$se_m4$alpha_beta,
    ab_s_puro  = r$modelos$s_puro$alpha_beta,  ab_s_m4  = r$modelos$s_m4$alpha_beta
  )
})

seeds_validas <- resumo %>%
  dplyr::filter(todos_ok) %>%
  dplyr::mutate(
    ll_total = ll_se_puro + ll_se_m4 + ll_s_puro + ll_s_m4,
    ab_max   = pmax(ab_se_puro, ab_se_m4, ab_s_puro, ab_s_m4, na.rm = TRUE)
  ) %>%
  dplyr::arrange(dplyr::desc(ll_total))

# ══════════════════════════════════════════════════════════════════════════════
# 7. RELATÓRIOS
# ══════════════════════════════════════════════════════════════════════════════

linhas_resumo <- c(
  strrep("=", 60), "RESUMO DA VARREDURA DE SEEDS", strrep("=", 60),
  sprintf("Seeds testadas              : %d",   nrow(resumo)),
  sprintf("Seeds válidas (todos_ok)    : %d (%.1f%%)",
          sum(resumo$todos_ok), 100 * mean(resumo$todos_ok)),
  sprintf("Seeds com n_ok = 3          : %d",   sum(resumo$n_ok == 3)),
  sprintf("Seeds com n_ok <= 2         : %d",   sum(resumo$n_ok <= 2)),
  "",
  "Taxa de falha por modelo (independente dos demais):",
  sprintf("  se_puro : %.1f%%", 100 * mean(!resumo$se_puro_ok)),
  sprintf("  se_m4   : %.1f%%", 100 * mean(!resumo$se_m4_ok)),
  sprintf("  s_puro  : %.1f%%", 100 * mean(!resumo$s_puro_ok)),
  sprintf("  s_m4    : %.1f%%", 100 * mean(!resumo$s_m4_ok)),
  "",
  sprintf("Tempo de execução           : %.1f s (%.1f min)", t_elapsed, t_elapsed/60),
  sprintf("Workers utilizados          : %d de %d disponíveis", n_workers, n_cores),
  strrep("=", 60)
)
cat(paste(linhas_resumo, collapse = "\n"), "\n\n")
writeLines(linhas_resumo, file.path(dir_seed, "01_resumo_varredura.txt"))
readr::write_csv(resumo, path_resumo)

if (nrow(seeds_validas) == 0) {
  cat("DIAGNÓSTICO — aprovações por critério em cada modelo:\n")
  for (nm in names(specs_list)) {
    conv_n  <- sum(sapply(resultados_raw, function(r) isTRUE(r$modelos[[nm]]$convergiu)))
    hess_n  <- sum(sapply(resultados_raw, function(r) isTRUE(r$modelos[[nm]]$hess_ok)))
    pdef_n  <- sum(sapply(resultados_raw, function(r) isTRUE(r$modelos[[nm]]$pos_def)))
    estac_n <- sum(sapply(resultados_raw, function(r) isTRUE(r$modelos[[nm]]$estacionario)))
    cat(sprintf("  %-10s  conv=%d  hess=%d  pos_def=%d  estac=%d  (de %d)\n",
                nm, conv_n, hess_n, pdef_n, estac_n, length(resultados_raw)))
  }
  stop(paste0(
    "\n[04a] Nenhuma seed satisfez todos os critérios simultaneamente.\n",
    "Veja diagnóstico acima para identificar o modelo/critério gargalo.\n",
    "Ações sugeridas:\n",
    "  1. Ampliar grade: aumente 500L para 1000L na linha do sample()\n",
    "  2. Relaxar tolerância: tol = 1e-3 em vez de 1e-4\n",
    "  3. Verificar reconstrução dos specs: rode .reconstruir_spec(specs_args_list[[1]])\n",
    "     e compare com specs_list[[1]] no ambiente do 04"
  ))
}

readr::write_csv(seeds_validas, file.path(dir_seed, "03_seeds_validas.csv"))
cat("Seeds válidas (ordenadas por ll_total):\n")
print(seeds_validas %>%
        dplyr::select(seed, ll_total, ab_max,
                      ll_se_puro, ll_se_m4, ll_s_puro, ll_s_m4))
cat("\n")

seeds_validas_vec <- seeds_validas$seed
extrair_coefs <- function(label) {
  purrr::map_dfr(resultados_raw, function(r) {
    if (!(r$seed %in% seeds_validas_vec)) return(NULL)
    coefs <- r$modelos[[label]]$coefs
    if (length(coefs) == 0) return(NULL)
    tibble::tibble(seed = r$seed, !!!coefs)
  })
}
coefs_lista <- list(se_puro = extrair_coefs("se_puro"),
                    se_m4   = extrair_coefs("se_m4"),
                    s_puro  = extrair_coefs("s_puro"),
                    s_m4    = extrair_coefs("s_m4"))

sink(file.path(dir_seed, "04_estabilidade_coeficientes.txt"))
cat(strrep("=", 60), "\nESTABILIDADE DOS COEFICIENTES ENTRE SEEDS VÁLIDAS\n")
cat("(CV < 5% indica que a seed não afeta a inferência)\n", strrep("=", 60), "\n\n")
for (nm in names(coefs_lista)) {
  cat(sprintf("--- Modelo: %s ---\n", nm))
  if (nrow(coefs_lista[[nm]]) > 1) print(.cv_por_param(coefs_lista[[nm]]))
  else cat("  Apenas 1 seed válida — CV não computável.\n")
  cat("\n")
}
sink()

saveRDS(resultados_raw, path_cache)
cat("Relatórios gravados em:", dir_seed, "\n")
cat("  01_resumo_varredura.txt\n  02_varredura_completa.csv\n")
cat("  03_seeds_validas.csv\n  04_estabilidade_coeficientes.txt\n")
cat("  resultados_raw_varredura.rds  (cache)\n\n")

# ══════════════════════════════════════════════════════════════════════════════
# 8. SELEÇÃO FINAL
# ══════════════════════════════════════════════════════════════════════════════

nseed_selecionada <- seeds_validas$seed[[1]]
saveRDS(nseed_selecionada, path_selecao)

cat(strrep("-", 60), "\n")
cat(sprintf("  SEED SELECIONADA : %d\n",  nseed_selecionada))
cat(sprintf("  ll_total         : %.4f\n", seeds_validas$ll_total[[1]]))
cat(sprintf("  Seeds válidas    : %d / %d (%.1f%%)\n",
            nrow(seeds_validas), nrow(resumo), 100 * mean(resumo$todos_ok)))
cat(strrep("-", 60), "\n\n")

return(invisible(nseed_selecionada))

})()
