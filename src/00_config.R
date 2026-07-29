# Carregamento de Pacotes
#install.packages(c("DBI", "RPostgres", "dotenv", "rstudioapi", "tidyverse", "PerformanceAnalytics",
#"FinTS", "tseries", "urca", "forecast", "rugarch", "flextable", "officer", "gt"))
#install.packages("patchwork")
pacotes <- c(
    "DBI", "RPostgres", "dotenv", "tidyverse", "PerformanceAnalytics",
    "FinTS", "tseries", "urca", "forecast",
    "rugarch", "flextable", "officer", "patchwork", "gt"
)

# library em bloco para a lista de pacotes
lapply(pacotes, require, character.only = TRUE)

# Carregar variáveis de ambiente
dotenv::load_dot_env()

pacotes_versoes <- data.frame(
  Pacote  = pacotes,
  Versao  = sapply(pacotes, function(p) as.character(packageVersion(p))),
  row.names = NULL
)

# Visualizar no console
print(pacotes_versoes)

# Exportar para CSV (para o repositório Zenodo)
write.csv(pacotes_versoes, "relatorios/ambiente_pacotes.csv", row.names = FALSE)

# Versão do R e do RStudio (para o Apêndice B)
R.version.string
rstudioapi::versionInfo()$version