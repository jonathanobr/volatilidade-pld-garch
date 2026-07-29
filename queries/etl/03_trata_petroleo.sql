-- 1. Cria a tabela base para os dados diários limpos
DROP TABLE IF EXISTS volatilidade.tabela_precos_petroleo;
CREATE TABLE volatilidade.tabela_precos_petroleo (
    data_completa DATE,
    codigo_serie TEXT,
    data_bruta TIMESTAMP,
    dia INTEGER,
    mes INTEGER,
    ano INTEGER,
    valor_usd_petroleo FLOAT
);

-- 2. Insere os dados da tabela temporária (com conversão de vírgula para ponto)
INSERT INTO volatilidade.tabela_precos_petroleo 
    (data_completa, codigo_serie, data_bruta, dia, mes, ano, valor_usd_petroleo)
SELECT
    tmp.data::DATE,
    tmp.codigo_serie,
    tmp.data_completa::TIMESTAMP,
    tmp.dia::INTEGER,
    tmp.mes::INTEGER,
    tmp.ano::INTEGER,
    REPLACE(tmp.valor_usd_texto, ',', '.')::FLOAT
FROM volatilidade.temp_precos_petroleo AS tmp;

-- 3. Deleta as duplicatas da tabela diária (mantendo o menor ctid)
DELETE FROM volatilidade.tabela_precos_petroleo a USING (
    SELECT min(ctid) as ctid, data_completa, codigo_serie
    FROM volatilidade.tabela_precos_petroleo 
    GROUP BY data_completa, codigo_serie 
    HAVING COUNT(*) > 1
) b
WHERE a.data_completa = b.data_completa 
AND a.codigo_serie = b.codigo_serie
AND a.ctid <> b.ctid;

-- 4. Cria a tabela semanal para o modelo
DROP TABLE IF EXISTS volatilidade.tabela_precos_petroleo_semanal;
CREATE TABLE volatilidade.tabela_precos_petroleo_semanal (
    id SERIAL PRIMARY KEY,
    ano INTEGER,
    mes INTEGER,
    semana_operativa INTEGER,
    data_inicio DATE,
    data_fim DATE,
    preco_medio_usd DOUBLE PRECISION,
    dias_contabilizados INTEGER,
    desvio_padrao_semanal DOUBLE PRECISION
);
CREATE INDEX idx_petroleo_semanal_data ON volatilidade.tabela_precos_petroleo_semanal (data_inicio);

-- 5. Povoa a tabela semanal cruzando com o PLD e filtrando a série do Brent
INSERT INTO volatilidade.tabela_precos_petroleo_semanal 
    (ano, mes, semana_operativa, data_inicio, data_fim, preco_medio_usd, dias_contabilizados, desvio_padrao_semanal)
SELECT 
    pld.ano,
    pld.mes,
    pld.semana AS semana_operativa,
    pld.data_inicio,
    pld.data_fim,
    AVG(pet.valor_usd_petroleo) AS preco_medio_usd,
    COUNT(pet.valor_usd_petroleo) AS dias_contabilizados,
    STDDEV(pet.valor_usd_petroleo) AS desvio_padrao_semanal
FROM volatilidade.historico_pld pld
LEFT JOIN volatilidade.tabela_precos_petroleo pet 
    ON pet.data_completa >= pld.data_inicio AND pet.data_completa <= pld.data_fim
WHERE pet.codigo_serie = 'EIA366_PBRENT366'
GROUP BY pld.ano, pld.mes, pld.semana, pld.data_inicio, pld.data_fim;

-- 6. Limpeza da memória
DROP TABLE IF EXISTS volatilidade.temp_precos_petroleo;