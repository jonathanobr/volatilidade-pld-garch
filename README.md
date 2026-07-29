# Vulnerabilidade Externa e Preços de Eletricidade no Brasil: Uma Análise Econométrica (2001–2025)

[![R](https://img.shields.io/badge/R-4.6.0-blue.svg)](https://www.r-project.org/)
[![Python](https://img.shields.io/badge/Python-3.14-green.svg)](https://www.python.org/)
[![PostgreSQL](https://img.shields.io/badge/PostgreSQL-11%2B-blue.svg)](https://www.postgresql.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

Este repositório contém o código-fonte integral, rotinas de ETL, scripts de modelagem econométrica (ARMA / sGARCH / GARCH-X) e geração automatizada de relatórios e figuras referentes ao Trabalho de Conclusão de Curso (TCC) em **Relações Internacionais e Integração** da **Universidade Federal da Integração Latino-Americana (UNILA)**.

---

## 📌 Informações Acadêmicas

- **Título:** Vulnerabilidade Externa e Preços de Eletricidade no Brasil: Uma Análise Econométrica (2001–2025)
- **Autor:** Jonathan Ramos Oliveira
- **Orientador:** Prof. Dr. Rodrigo da Silva Souza (UNILA)
- **Coorientadora:** Profª. Drª. Nayara Fatima Macedo de Medeiros Albrecht (Universiteit van Amsterdam — UvA)
- **Instituição:** Instituto Latino-Americano de Economia, Sociedade e Política (ILAESP / UNILA)
- **Ano:** 2026

---

## 💻 Arquitetura do Projeto e Estrutura de Pastas

```text
volatilidade-pld-garch/
├── run_pipeline.R                  # Script mestre que executa o pipeline sequencial (Etapas 0 a 5)
├── .env.sample                     # Template de variáveis de ambiente e conexões
├── .gitignore                      # Regras de versionamento
├── volatilidade.Rproj              # Projeto RStudio
│
├── src/                            # Scripts modulares de execução (R e Python)
│   ├── 00_coleta_dados.py          # Etapa 0: Coleta via API (IPEADATA e ONS S3)
│   ├── 00_config.R                 # Configurações globais, pacotes R e funções utilitárias
│   ├── 01_ingestao_db.R            # Etapa 1: Ingestão de dados brutos no PostgreSQL (Modo Rápido / Completo)
│   ├── 02_etl.R                    # Etapa 2: Carga direta pré-processada (fallback para execução desacoplada do DB)
│   ├── 03_tests_eda.R              # Etapa 3: Testes estatísticos (ADF, PP, KPSS, ARCH-LM) e análise exploratória
│   ├── 04_garch_modeling.R         # Etapa 4: Estimação dos modelos ARMA, sGARCH e GARCH-X
│   ├── 04a_seed_selection.R        # Varredura estocástica (grid search de 512 sementes)
│   ├── 05a_reporting_figures.R     # Etapa 5a: Geração automatizada das figuras (.png)
│   ├── 05b_reporting_tables.R      # Etapa 5b: Compilação das tabelas econométricas (.docx)
│   ├── 05c_reporting_summaries.R   # Etapa 5c: Exportação dos sumários dos modelos (.txt)
│   ├── 05d_reporting_seed.R        # Etapa 5d: Relatório do apêndice de seleção de seed (.docx)
│   └── 05e_reporting_ambiente.R    # Etapa 5e: Relatório do apêndice de ambiente computacional (.docx)
│
├── queries/etl/                    # Consultas SQL para transformação e agregação (PL/pgSQL)
│   ├── 01_trata_pld.sql
│   ├── 02_trata_gas.sql
│   ├── 03_trata_petroleo.sql
│   ├── 04_trata_ptax.sql
│   ├── 05_trata_ena.sql            # ETL hidrológico (diário ONS -> semanal por subsistema)
│   └── 06_tabela_mestra.sql        # Consolidação da tabela final com retornos e variáveis defasadas
│
├── dados brutos/                   # Arquivos CSV originais / coletados (PLD, Gás, Petróleo, PTAX, ENA)
├── data/                           # Datasets consolidados em formato CSV e RDS (`dataset_final_tcc.*`)
├── models/                         # Objetos dos modelos estimados salvos em `.rds`
└── relatorios/                     # Saídas finais: documentos Word (`.docx`), imagens (`.png`) e sumários (`.txt`)
    └── seed/                       # Artefatos do grid search de sementes aleatórias
```

---

## 🛠️ Requisitos de Ambiente

| Recurso | Versão Recomendada | Finalidade no Pipeline |
|---|---|---|
| **R** | 4.6.0+ | Ambiente de execução econométrica |
| **RStudio** | 2026.05.0 (Build 218) | IDE para desenvolvimento |
| **PostgreSQL** | 11+ | Banco de dados relacional e pipeline ETL (PL/pgSQL) |
| **Python** | 3.14+ | Coleta automatizada de dados via API |

### Pacotes R Principais
- **Modelagem & Séries Temporais:** `rugarch`, `forecast`, `tseries`, `urca`, `FinTS`, `PerformanceAnalytics`
- **Manipulação & Visualização:** `tidyverse` (`dplyr`, `readr`, `ggplot2`), `patchwork`
- **Banco de Dados & Infraestrutura:** `DBI`, `RPostgres`, `dotenv`
- **Geração de Relatórios:** `flextable`, `officer`, `gt`

### Pacotes Python Principais
- `requests` (Requisições HTTP com suporte a retry e streaming)
- `pandas` (Manipulação e exportação de DataFrames para CSV)
- `ipeadatapy` (Interface com a API pública do IPEADATA)

---

## 🚀 Como Executar o Pipeline

### 1. Clonar o Repositório
```bash
git clone https://github.com/jonathanobr/volatilidade-pld-garch.git
cd volatilidade-pld-garch
```

### 2. Configurar Variáveis de Ambiente
Copie o arquivo `.env.sample` criando o arquivo `.env` na raiz do projeto e preencha as credenciais do seu banco PostgreSQL:

```ini
DB_HOST=localhost
DB_PORT=5432
DB_NAME=seu_banco
DB_USER=seu_usuario
DB_PASSWORD=sua_senha
PYTHON_PATH=C:\caminho\para\seu\python.exe  # Opcional (se não estiver no PATH)
```

### 3. Execução Automatizada

Abra o projeto no RStudio (`volatilidade.Rproj`) e execute o script mestre `run_pipeline.R`:

```r
source("run_pipeline.R")
```

O script executará sequencialmente:
1. **Etapa 0 (`00_coleta_dados.py`):** Baixa/atualiza séries do IPEADATA e diários de ENA do ONS.
2. **Etapa 1 (`01_ingestao_db.R`):** Detecta se `dados_ena.csv` existe (**Modo Rápido**) ou empilha diários do ONS e roda ETL SQL (**Modo Completo**).
3. **Etapa 3 (`03_tests_eda.R`):** Realiza testes estatísticos de raiz unitária (ADF, PP, KPSS) e efeito ARCH-LM.
4. **Etapa 4 (`04_garch_modeling.R`):** Ajusta os modelos ARMA, sGARCH e GARCH-X fixando a semente selecionada (`seed = 9235111`).
5. **Etapa 5 (`05a` a `05e`):** Exporta todas as figuras (.png) e compila os relatórios em formato Word (.docx).

---

## ⚡ Reprodutibilidade e Seleção de Seed

A estimação dos modelos utiliza a semente **`seed = 9235111`**, definida por varredura sistemática (grid search em 512 sementes candidatas) documentada no **Apêndice A** do trabalho. A escolha atende simultaneamente aos critérios de:
- Convergência do solver numérico (`gosolnp`);
- Invertibilidade da matriz de variância-covariância (Hessiana sem NAs);
- Matriz de covariância positiva-semidefinida;
- Estacionariedade dos parâmetros do modelo ($\alpha + \beta < 1{,}3$);
- Maximização da log-verossimilhança total.

---

## 📜 Citação

Se utilizar este código ou os datasets gerados em sua pesquisa, por favor cite:

```bibtex
@mastersthesis{oliveira2026vulnerabilidade,
  author       = {Oliveira, Jonathan Ramos},
  title        = {Vulnerabilidade Externa e Preços de Eletricidade no Brasil: Uma Análise Econométrica (2001--2025)},
  school       = {Universidade Federal da Integração Latino-Americana (UNILA)},
  year         = {2026},
  type         = {Trabalho de Conclusão de Curso (Bacharelado em Relações Internacionais e Integração)},
  address      = {Foz do Iguaçu, PR}
}
```

---

## 📄 Licença

Este projeto está licenciado sob a Licença **MIT** — veja o arquivo [LICENSE](LICENSE) para mais detalhes.
