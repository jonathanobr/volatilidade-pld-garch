-- 1. Cria a tabela final
DROP TABLE IF EXISTS volatilidade.historico_pld;
CREATE TABLE volatilidade.historico_pld (
    ano INTEGER, mes INTEGER, semana INTEGER, data_inicio DATE, data_fim DATE,
    sudeste FLOAT, sul FLOAT, nordeste FLOAT, norte FLOAT
);

-- 2. Insere os dados tratados convertendo os textos que vieram do R
INSERT INTO volatilidade.historico_pld
SELECT
    ano::INTEGER,
    mes::INTEGER,
    semana::INTEGER,
    data_inicio::DATE,
    data_fim::DATE,
    REPLACE(sudeste, ',', '.')::FLOAT,
    REPLACE(sul, ',', '.')::FLOAT,
    REPLACE(nordeste, ',', '.')::FLOAT,
    REPLACE(norte, ',', '.')::FLOAT
FROM volatilidade.temp_historico_pld;

-- 3. Deleta duplicatas baseadas no CTID físico
DELETE FROM volatilidade.historico_pld a USING (
    SELECT min(ctid) as ctid, data_inicio, sudeste
    FROM volatilidade.historico_pld 
    GROUP BY data_inicio, sudeste
    HAVING COUNT(*) > 1
) b
WHERE a.data_inicio = b.data_inicio AND a.sudeste = b.sudeste AND a.ctid <> b.ctid;

-- 4. Limpeza da tabela temporária
DROP TABLE IF EXISTS volatilidade.temp_historico_pld;