# run_pipeline.R

cat("Iniciando a execução do Pipeline Completo...\n")

# 0. Coleta de Dados via API (Python)
# Baixa automaticamente: Petróleo Brent, PTAX e Gás (IPEADATA) + ENA diária (ONS).
# Os arquivos são salvos em 'dados brutos/' antes da ingestão no banco.
#
# PYTHON_PATH pode ser definido no .env para apontar o executável correto
# (ex: caminho para o python de um venv ou conda). Padrão: "python".
#
# Comente o bloco abaixo se os dados em 'dados brutos/' já estiverem atualizados
# e você quiser pular esta etapa para economizar tempo.
python_exe <- Sys.getenv("PYTHON_PATH", unset = "python")
cat("[Etapa 0] Executando coleta Python com:", python_exe, "\n")
resultado_coleta <- system2(
  python_exe,
  args   = "src/00_coleta_dados.py",
  stdout = TRUE,
  stderr = TRUE
)
cat(resultado_coleta, sep = "\n")
if (!is.null(attr(resultado_coleta, "status")) && attr(resultado_coleta, "status") != 0) {
  stop("[Etapa 0] Coleta Python falhou. Verifique as mensagens acima antes de prosseguir.")
}
cat("[Etapa 0] Coleta concluída.\n\n")

# 1. Ingestão de Dados (CSVs/TXTs -> PostgreSQL -> Tabela Mestra Local)
# Obs: Se os dados brutos na pasta não mudaram,
# ou se optar por usar os dados locais armazenados em data\*.rds,
# comente a linha abaixo com '#' para rodar apenas
# a modelagem econométrica e relatórios associados.
source("src/01_ingestao_db.R")

# 2. Transformação e Filtragem (Cálculo de Lags, Retornos e Z-Score)
source("src/02_etl.R")

# 3. Rodar testes econométricos preliminares (Raiz Unitária, ARMA, ARCH-LM)
source("src/03_tests_eda.R")

# 4. Estimar todos os modelos GARCH e GARCH-X (Atenção: etapa intensiva em processamento)
source("src/04_garch_modeling.R")

# 5. Gerar relatórios finais (Gráficos .png e Tabelas .docx)
source("src/05a_reporting_figures.R")
source("src/05b_reporting_tables.R")
source("src/05c_reporting_summaries.R")
source("src/05d_reporting_seed.R")
source("src/05e_reporting_ambiente.R")

cat("\n======================================================\n")
message("PIPELINE EXECUTADO COM SUCESSO!")
message("- Dados brutos coletados em:      dados brutos/")
message("- Dados processados salvos em:    data/")
message("- Modelos econométricos salvos em: models/")
message("- Gráficos e Tabelas salvos em:   relatorios/")
cat("======================================================\n")