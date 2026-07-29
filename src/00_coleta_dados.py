# src/00_coleta_dados.py
#
# Coleta automática de dados via API para o projeto código_banca.
# Inspirado na estrutura de intermitencia/1.0_get_data.py.
#
# Séries coletadas:
#   1. IPEADATA — Petróleo Brent (EIA366_PBRENT366)
#   2. IPEADATA — PTAX / Câmbio EUR (GM366_ERC366)
#   3. IPEADATA — Importação de gás (FUNCEX12_MPEPET2N12)
#   4. ONS S3   — ENA Diária por Subsistema (CSVs anuais, 2000–ano corrente)
#
# Pré-requisitos:
#   pip install ipeadatapy pandas requests
#
# Uso direto:
#   python src/00_coleta_dados.py           # incremental (pula o que já existe)
#   python src/00_coleta_dados.py --force   # re-baixa tudo mesmo que já exista
#
# Chamada a partir do R (run_pipeline.R):
#   python_exe <- Sys.getenv("PYTHON_PATH", unset = "python")
#   system2(python_exe, args = "src/00_coleta_dados.py")

import argparse
import os
import sys
import time

import pandas as pd
import requests

# ==============================================================================
# CONFIGURAÇÃO DE CAMINHOS
# O script é chamado a partir da raiz do projeto (onde fica o run_pipeline.R),
# portanto os caminhos são relativos à raiz.
# ==============================================================================
DIR_DADOS_BRUTOS = "dados brutos"
DIR_ENA = os.path.join(DIR_DADOS_BRUTOS, "ena")

os.makedirs(DIR_DADOS_BRUTOS, exist_ok=True)
os.makedirs(DIR_ENA, exist_ok=True)

# ==============================================================================
# UTILITÁRIOS
# ==============================================================================

def _log(nivel: str, msg: str) -> None:
    """Imprime mensagem formatada no console."""
    prefixos = {"ok": "[OK]   ", "erro": "[ERRO] ", "skip": "[SKIP] ", "info": "[INFO] "}
    print(f"  {prefixos.get(nivel, '       ')}{msg}", flush=True)


def _download_streaming(url: str, caminho_saida: str, max_tentativas: int = 3) -> bool:
    """
    Baixa um arquivo via streaming com retry automático.
    Inspirado em intermitencia/1.0_get_data.py.
    Retorna True em sucesso, False em falha definitiva.
    """
    for tentativa in range(1, max_tentativas + 1):
        try:
            with requests.get(url, stream=True, timeout=60) as r:
                r.raise_for_status()
                with open(caminho_saida, "wb") as f:
                    for chunk in r.iter_content(chunk_size=16384):
                        f.write(chunk)
            return True

        except requests.exceptions.HTTPError as e:
            # 404 significa que o arquivo não existe no servidor — não tenta de novo
            if e.response is not None and e.response.status_code == 404:
                _log("erro", f"Não encontrado no servidor (404): {os.path.basename(caminho_saida)}")
                return False
            _log("erro", f"HTTP {e.response.status_code} — Tentativa {tentativa}/{max_tentativas}")

        except requests.exceptions.Timeout:
            _log("erro", f"Timeout — Tentativa {tentativa}/{max_tentativas}. Aguardando 10s...")
            time.sleep(10)

        except Exception as e:
            _log("erro", f"{e} — Tentativa {tentativa}/{max_tentativas}")
            time.sleep(5)

        # Remove arquivo parcial antes de tentar novamente
        if os.path.exists(caminho_saida):
            os.remove(caminho_saida)

    _log("erro", f"Falha definitiva após {max_tentativas} tentativas: {os.path.basename(caminho_saida)}")
    return False


# ==============================================================================
# 1. IPEADATA — COLETA DE SÉRIES TEMPORAIS
# ==============================================================================

def _baixar_serie_ipea(codigo_serie: str, caminho_saida: str, force: bool) -> bool:
    """
    Baixa uma série temporal do IPEADATA via ipeadatapy e salva como CSV.

    Formato de saída: separador ';', decimal ',', encoding UTF-8.
    Compatível com read_delim() do 01_ingestao_db.R.

    Retorna True em sucesso, False em falha.
    """
    if os.path.exists(caminho_saida) and not force:
        _log("skip", f"{os.path.basename(caminho_saida)} já existe. Use --force para re-baixar.")
        return True

    try:
        import ipeadatapy as ipea  # import tardio para não exigir o pacote se só usar ONS

        _log("info", f"Buscando série {codigo_serie} no IPEADATA...")
        df = ipea.timeseries(codigo_serie)

        if df is None or df.empty:
            _log("erro", f"A API retornou dados vazios para a série {codigo_serie}.")
            return False

        df.to_csv(caminho_saida, index=True, sep=";", decimal=",", encoding="utf-8")
        _log("ok", f"{os.path.basename(caminho_saida)} salvo ({len(df)} registros).")
        return True

    except ImportError:
        _log("erro", "Pacote 'ipeadatapy' não encontrado. Execute: pip install ipeadatapy")
        return False
    except Exception as e:
        _log("erro", f"Falha ao coletar série {codigo_serie}: {e}")
        return False


def coletar_ipea(force: bool = False) -> dict:
    """
    Coleta as três séries do IPEADATA necessárias para o pipeline.
    Retorna dict com resultado booleano por série.
    """
    print("\n" + "=" * 60)
    print("IPEADATA — Petróleo, PTAX e Gás")
    print("=" * 60)

    series = {
        "petroleo": {
            "codigo": "EIA366_PBRENT366",
            "arquivo": os.path.join(DIR_DADOS_BRUTOS, "dados_petroleo.csv"),
            "descricao": "Petróleo Brent (USD)",
        },
        "ptax": {
            "codigo": "GM366_ERC366",
            "arquivo": os.path.join(DIR_DADOS_BRUTOS, "dados_ptax.csv"),
            "descricao": "Câmbio BRL/EUR (PTAX)",
        },
        "gas": {
            "codigo": "FUNCEX12_MPEPET2N12",
            "arquivo": os.path.join(DIR_DADOS_BRUTOS, "dados_gas_imp.csv"),
            "descricao": "Importação de gás natural",
        },
    }

    resultados = {}
    for nome, cfg in series.items():
        print(f"\n  Série: {cfg['descricao']} ({cfg['codigo']})")
        resultados[nome] = _baixar_serie_ipea(cfg["codigo"], cfg["arquivo"], force)
        time.sleep(1.0)  # throttling básico entre requisições

    return resultados


# ==============================================================================
# 2. ONS S3 — ENA DIÁRIA POR SUBSISTEMA (CSVs anuais)
# ==============================================================================

def coletar_ena(force: bool = False) -> dict:
    """
    Baixa os arquivos CSV anuais de ENA Diária por Subsistema da ONS (S3 público).

    Cobertura: 2000 até o ano corrente.
    Os arquivos existentes NÃO são re-baixados (modo incremental), a menos
    que --force seja passado.

    URL base (padrão ONS):
      https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/
        ena_subsistema_di/ENA_DIARIO_SUBSISTEMA_{ano}.csv

    Retorna dict {ano: True/False}.
    """
    print("\n" + "=" * 60)
    print("ONS — ENA Diária por Subsistema (2000–hoje)")
    print("=" * 60)

    import datetime
    ano_atual = datetime.date.today().year

    url_base = (
        "https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/"
        "ena_subsistema_di/ENA_DIARIO_SUBSISTEMA_{ano}.csv"
    )

    resultados = {}
    for ano in range(2000, ano_atual + 1):
        nome_arq = f"ENA_DIARIO_SUBSISTEMA_{ano}.csv"
        caminho = os.path.join(DIR_ENA, nome_arq)
        url = url_base.format(ano=ano)

        print(f"\n  Ano {ano}:")

        if os.path.exists(caminho) and not force:
            _log("skip", f"{nome_arq} já existe.")
            resultados[ano] = True
            continue

        _log("info", f"Baixando {nome_arq}...")
        ok = _download_streaming(url, caminho)
        resultados[ano] = ok

        if ok:
            _log("ok", f"{nome_arq} baixado com sucesso.")
        # Pequena pausa para não sobrecarregar o S3
        time.sleep(0.5)

    return resultados


# ==============================================================================
# PONTO DE ENTRADA
# ==============================================================================

def main() -> int:
    """
    Executa a coleta completa.
    Retorna 0 em sucesso total, 1 se qualquer série falhou.
    """
    parser = argparse.ArgumentParser(
        description="Coleta automática de dados para o projeto código_banca."
    )
    parser.add_argument(
        "--force",
        action="store_true",
        help="Re-baixa todos os arquivos, mesmo que já existam localmente.",
    )
    parser.add_argument(
        "--apenas-ipea",
        dest="apenas_ipea",
        action="store_true",
        help="Coleta apenas as séries do IPEADATA (ignora a ENA do ONS).",
    )
    parser.add_argument(
        "--apenas-ena",
        dest="apenas_ena",
        action="store_true",
        help="Coleta apenas os arquivos de ENA do ONS (ignora o IPEADATA).",
    )
    args = parser.parse_args()

    print("\n" + "=" * 60)
    print("COLETA DE DADOS — código_banca")
    print("=" * 60)
    if args.force:
        print("  Modo: RE-DOWNLOAD FORÇADO (--force ativo)")
    else:
        print("  Modo: INCREMENTAL (pula arquivos já existentes)")

    falhas = []

    # --- IPEADATA ---
    if not args.apenas_ena:
        res_ipea = coletar_ipea(force=args.force)
        falhas += [k for k, v in res_ipea.items() if not v]

    # --- ENA / ONS ---
    if not args.apenas_ipea:
        res_ena = coletar_ena(force=args.force)
        falhas += [str(k) for k, v in res_ena.items() if not v]

    # --- Sumário final ---
    print("\n" + "=" * 60)
    if falhas:
        print(f"COLETA CONCLUÍDA COM {len(falhas)} FALHA(S): {', '.join(falhas)}")
        print("=" * 60)
        return 1
    else:
        print("COLETA CONCLUÍDA COM SUCESSO!")
        print("=" * 60)
        return 0


if __name__ == "__main__":
    sys.exit(main())
