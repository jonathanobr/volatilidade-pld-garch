-- 1. Cria a tabela base para o Dólar PTAX Diário
DROP TABLE IF EXISTS volatilidade.cotacoes_ptax_diario;
CREATE TABLE volatilidade.cotacoes_ptax_diario (
    data DATE PRIMARY KEY,
    valor_ptax_compra DOUBLE PRECISION
);

-- 2. Tratamento e Inserção (removendo vazios e formatando decimais)
INSERT INTO volatilidade.cotacoes_ptax_diario (data, valor_ptax_compra)
SELECT 
    data::DATE,
    REPLACE(valor_usd_texto, ',', '.')::DOUBLE PRECISION
FROM volatilidade.temp_ptax
WHERE valor_usd_texto IS NOT NULL AND valor_usd_texto != '';

-- 3. Cria a tabela da PTAX Semanal
DROP TABLE IF EXISTS volatilidade.tabela_ptax_semanal;
CREATE TABLE volatilidade.tabela_ptax_semanal (
    data_inicio DATE PRIMARY KEY,
    ptax_medio_semanal DOUBLE PRECISION,
    dias_uteis_cambio INTEGER
);

-- 4. Povoa cruzando com as datas do PLD
INSERT INTO volatilidade.tabela_ptax_semanal (data_inicio, ptax_medio_semanal, dias_uteis_cambio)
SELECT 
    pld.data_inicio,
    AVG(ptax.valor_ptax_compra) as ptax_medio,
    COUNT(ptax.valor_ptax_compra) as dias_contabilizados
FROM volatilidade.historico_pld pld
LEFT JOIN volatilidade.cotacoes_ptax_diario ptax 
    ON ptax.data >= pld.data_inicio AND ptax.data <= pld.data_fim
GROUP BY pld.data_inicio;

-- 5. Limpeza da memória
DROP TABLE IF EXISTS volatilidade.temp_ptax;