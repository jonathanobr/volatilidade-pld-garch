-- 1. Tratamento Diário do Gás
DROP TABLE IF EXISTS volatilidade.tabela_precos_gas;
CREATE TABLE volatilidade.tabela_precos_gas AS
SELECT
    -- Força a leitura no padrão Americano (MM/DD/YYYY)
    TO_DATE("Date", 'MM/DD/YYYY') as data,
    REPLACE("Price", ',', '')::FLOAT as valor_usd_gas
FROM volatilidade.temp_precos_gas
WHERE "Date" IS NOT NULL;
-- 2. Criação da Tabela Semanal
DROP TABLE IF EXISTS volatilidade.tabela_precos_gas_semanal;
CREATE TABLE volatilidade.tabela_precos_gas_semanal (
    id SERIAL PRIMARY KEY,
    ano INTEGER, mes INTEGER, semana_operativa INTEGER, 
    data_inicio DATE, data_fim DATE,
    valor_usd_gas DOUBLE PRECISION, 
    dias_contabilizados INTEGER, desvio_padrao_semanal DOUBLE PRECISION DEFAULT 0
);
CREATE INDEX idx_gas_semanal_data ON volatilidade.tabela_precos_gas_semanal (data_inicio);

-- 3. Povoamento da Tabela Semanal (Join com o PLD)
INSERT INTO volatilidade.tabela_precos_gas_semanal (ano, mes, semana_operativa, data_inicio, data_fim, valor_usd_gas, dias_contabilizados, desvio_padrao_semanal)
WITH semanas_unicas AS (
    SELECT DISTINCT ano, mes, semana, data_inicio, data_fim FROM volatilidade.historico_pld
)
SELECT 
    pld.ano, pld.mes, pld.semana, pld.data_inicio, pld.data_fim,
    gas.valor_usd_gas,
    CASE WHEN gas.valor_usd_gas IS NOT NULL THEN 1 ELSE 0 END,
    0
FROM semanas_unicas pld
LEFT JOIN volatilidade.tabela_precos_gas gas 
    ON gas.data >= pld.data_inicio AND gas.data <= pld.data_fim;

DROP TABLE IF EXISTS volatilidade.temp_precos_gas;