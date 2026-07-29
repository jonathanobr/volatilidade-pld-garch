-- PASSO 1: TABELA DIÁRIA BASE 
-- Limpa os tipos de dados e padroniza as casas decimais
DROP TABLE IF EXISTS volatilidade.ena_diario_subsistema;
CREATE TABLE volatilidade.ena_diario_subsistema (
    id_registro_ena SERIAL PRIMARY KEY,
    id_subsistema varchar(2),
    nom_subsistema TEXT,
    ena_data DATE,
    ena_bruta_regiao_mwmed FLOAT,
    ena_bruta_regiao_percentualmlt FLOAT,
    ena_armazenavel_regiao_mwmed FLOAT,
    ena_armazenavel_regiao_percentualmlt FLOAT
);

INSERT INTO volatilidade.ena_diario_subsistema (
    id_subsistema, nom_subsistema, ena_data, 
    ena_bruta_regiao_mwmed, ena_bruta_regiao_percentualmlt, 
    ena_armazenavel_regiao_mwmed, ena_armazenavel_regiao_percentualmlt
)
SELECT 
    UPPER(id_subsistema),
    nom_subsistema,
    ena_data::DATE,
    REPLACE(ena_bruta_regiao_mwmed, ',', '.')::FLOAT,
    REPLACE(ena_bruta_regiao_percentualmlt, ',', '.')::FLOAT,
    REPLACE(ena_armazenavel_regiao_mwmed, ',', '.')::FLOAT,
    REPLACE(ena_armazenavel_regiao_percentualmlt, ',', '.')::FLOAT
FROM volatilidade.temp_ena
WHERE ena_data IS NOT NULL AND ena_data != 'ena_data'; -- Evita sujeira de cabeçalho


-- PASSO 2: TABELA INTERMEDIÁRIA TRANSPOSTA (PIVOTAGEM)
-- Coloca os subsistemas em colunas
DROP TABLE IF EXISTS volatilidade.ena_diario_transposta CASCADE;
CREATE TABLE volatilidade.ena_diario_transposta (
    data DATE PRIMARY KEY,
    ena_bruta_mw_se DOUBLE PRECISION, ena_bruta_mw_s DOUBLE PRECISION, ena_bruta_mw_ne DOUBLE PRECISION, ena_bruta_mw_n DOUBLE PRECISION, 
    ena_bruta_mlt_se DOUBLE PRECISION, ena_bruta_mlt_s DOUBLE PRECISION, ena_bruta_mlt_ne DOUBLE PRECISION, ena_bruta_mlt_n DOUBLE PRECISION,
    ena_arm_mw_se DOUBLE PRECISION, ena_arm_mw_s DOUBLE PRECISION, ena_arm_mw_ne DOUBLE PRECISION, ena_arm_mw_n DOUBLE PRECISION, 
    ena_arm_mlt_se DOUBLE PRECISION, ena_arm_mlt_s DOUBLE PRECISION, ena_arm_mlt_ne DOUBLE PRECISION, ena_arm_mlt_n DOUBLE PRECISION
);

INSERT INTO volatilidade.ena_diario_transposta
SELECT
    ena_data,
    MAX(CASE WHEN id_subsistema = 'SE' THEN ena_bruta_regiao_mwmed END),
    MAX(CASE WHEN id_subsistema = 'S'  THEN ena_bruta_regiao_mwmed END),
    MAX(CASE WHEN id_subsistema = 'NE' THEN ena_bruta_regiao_mwmed END),
    MAX(CASE WHEN id_subsistema = 'N'  THEN ena_bruta_regiao_mwmed END),
    
    MAX(CASE WHEN id_subsistema = 'SE' THEN ena_bruta_regiao_percentualmlt END),
    MAX(CASE WHEN id_subsistema = 'S'  THEN ena_bruta_regiao_percentualmlt END),
    MAX(CASE WHEN id_subsistema = 'NE' THEN ena_bruta_regiao_percentualmlt END),
    MAX(CASE WHEN id_subsistema = 'N'  THEN ena_bruta_regiao_percentualmlt END),

    MAX(CASE WHEN id_subsistema = 'SE' THEN ena_armazenavel_regiao_mwmed END),
    MAX(CASE WHEN id_subsistema = 'S'  THEN ena_armazenavel_regiao_mwmed END),
    MAX(CASE WHEN id_subsistema = 'NE' THEN ena_armazenavel_regiao_mwmed END),
    MAX(CASE WHEN id_subsistema = 'N'  THEN ena_armazenavel_regiao_mwmed END),

    MAX(CASE WHEN id_subsistema = 'SE' THEN ena_armazenavel_regiao_percentualmlt END),
    MAX(CASE WHEN id_subsistema = 'S'  THEN ena_armazenavel_regiao_percentualmlt END),
    MAX(CASE WHEN id_subsistema = 'NE' THEN ena_armazenavel_regiao_percentualmlt END),
    MAX(CASE WHEN id_subsistema = 'N'  THEN ena_armazenavel_regiao_percentualmlt END)
FROM volatilidade.ena_diario_subsistema
GROUP BY ena_data;


-- PASSO 3: TABELA FINAL SEMANAL TRATADA
-- Cruza a ENA transposta com o calendário do PLD e extrai as métricas
DROP TABLE IF EXISTS volatilidade.tabela_ena_semanal CASCADE;
CREATE TABLE volatilidade.tabela_ena_semanal (
    id SERIAL PRIMARY KEY,
    ano INTEGER, mes INTEGER, semana_operativa INTEGER,
    data_inicio DATE, data_fim DATE,
    ena_bruta_mw_se DOUBLE PRECISION, ena_bruta_mw_s DOUBLE PRECISION, ena_bruta_mw_ne DOUBLE PRECISION, ena_bruta_mw_n DOUBLE PRECISION,
    ena_bruta_mlt_se DOUBLE PRECISION, ena_bruta_mlt_s DOUBLE PRECISION, ena_bruta_mlt_ne DOUBLE PRECISION, ena_bruta_mlt_n DOUBLE PRECISION,
    ena_arm_mw_se DOUBLE PRECISION, ena_arm_mw_s DOUBLE PRECISION, ena_arm_mw_ne DOUBLE PRECISION, ena_arm_mw_n DOUBLE PRECISION,
    ena_arm_mlt_se DOUBLE PRECISION, ena_arm_mlt_s DOUBLE PRECISION, ena_arm_mlt_ne DOUBLE PRECISION, ena_arm_mlt_n DOUBLE PRECISION,
    vol_ena_arm_mlt_se DOUBLE PRECISION, vol_ena_arm_mlt_s DOUBLE PRECISION, vol_ena_arm_mlt_ne DOUBLE PRECISION, vol_ena_arm_mlt_n DOUBLE PRECISION,
    dias_contabilizados INTEGER
);
CREATE INDEX idx_ena_semanal_data ON volatilidade.tabela_ena_semanal (data_inicio);

WITH semanas_unicas AS (
    SELECT DISTINCT ano, mes, semana, data_inicio, data_fim FROM volatilidade.historico_pld
)
INSERT INTO volatilidade.tabela_ena_semanal (
    ano, mes, semana_operativa, data_inicio, data_fim,
    ena_bruta_mw_se, ena_bruta_mw_s, ena_bruta_mw_ne, ena_bruta_mw_n,
    ena_bruta_mlt_se, ena_bruta_mlt_s, ena_bruta_mlt_ne, ena_bruta_mlt_n,
    ena_arm_mw_se, ena_arm_mw_s, ena_arm_mw_ne, ena_arm_mw_n,
    ena_arm_mlt_se, ena_arm_mlt_s, ena_arm_mlt_ne, ena_arm_mlt_n,
    vol_ena_arm_mlt_se, vol_ena_arm_mlt_s, vol_ena_arm_mlt_ne, vol_ena_arm_mlt_n,
    dias_contabilizados
)
SELECT 
    pld.ano, pld.mes, pld.semana, pld.data_inicio, pld.data_fim,
    AVG(ena.ena_bruta_mw_se), AVG(ena.ena_bruta_mw_s), AVG(ena.ena_bruta_mw_ne), AVG(ena.ena_bruta_mw_n),
    AVG(ena.ena_bruta_mlt_se), AVG(ena.ena_bruta_mlt_s), AVG(ena.ena_bruta_mlt_ne), AVG(ena.ena_bruta_mlt_n),
    AVG(ena.ena_arm_mw_se), AVG(ena.ena_arm_mw_s), AVG(ena.ena_arm_mw_ne), AVG(ena.ena_arm_mw_n),
    AVG(ena.ena_arm_mlt_se), AVG(ena.ena_arm_mlt_s), AVG(ena.ena_arm_mlt_ne), AVG(ena.ena_arm_mlt_n),
    COALESCE(STDDEV(ena.ena_arm_mlt_se), 0), COALESCE(STDDEV(ena.ena_arm_mlt_s), 0), COALESCE(STDDEV(ena.ena_arm_mlt_ne), 0), COALESCE(STDDEV(ena.ena_arm_mlt_n), 0),
    COUNT(ena.data)
FROM semanas_unicas pld
LEFT JOIN volatilidade.ena_diario_transposta ena 
    ON ena.data >= pld.data_inicio AND ena.data <= pld.data_fim
GROUP BY pld.ano, pld.mes, pld.semana, pld.data_inicio, pld.data_fim
ORDER BY pld.data_inicio DESC;

-- PASSO 4: LIMPEZA DE MEMÓRIA
DROP TABLE IF EXISTS volatilidade.temp_ena;