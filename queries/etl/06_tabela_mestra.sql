-- PASSO 1: ESTRUTURA DA TABELA MESTRA
DROP TABLE IF EXISTS volatilidade.tabela_final_consolidada;
CREATE TABLE volatilidade.tabela_final_consolidada (
    id_semana SERIAL PRIMARY KEY, ano INTEGER, mes INTEGER, semana_operativa INTEGER, data_inicio DATE, data_fim DATE,
    pld_medio_se DOUBLE PRECISION, pld_medio_s DOUBLE PRECISION, pld_medio_ne DOUBLE PRECISION, pld_medio_n DOUBLE PRECISION,
    preco_petroleo_brent DOUBLE PRECISION, volatilidade_petroleo DOUBLE PRECISION, preco_gas_natural DOUBLE PRECISION,
    ptax_medio_semanal DOUBLE PRECISION, preco_petroleo_brl DOUBLE PRECISION, preco_gas_brl DOUBLE PRECISION,
    ena_bruta_mw_se DOUBLE PRECISION, ena_bruta_mlt_se DOUBLE PRECISION, ena_bruta_mw_s DOUBLE PRECISION, ena_bruta_mlt_s DOUBLE PRECISION,
    ena_bruta_mw_ne DOUBLE PRECISION, ena_bruta_mlt_ne DOUBLE PRECISION, ena_bruta_mw_n DOUBLE PRECISION, ena_bruta_mlt_n DOUBLE PRECISION,
    ena_arm_mw_se DOUBLE PRECISION, ena_arm_mlt_se DOUBLE PRECISION, ena_arm_mw_s DOUBLE PRECISION, ena_arm_mlt_s DOUBLE PRECISION,
    ena_arm_mw_ne DOUBLE PRECISION, ena_arm_mlt_ne DOUBLE PRECISION, ena_arm_mw_n DOUBLE PRECISION, ena_arm_mlt_n DOUBLE PRECISION,
    vol_ena_arm_mlt_se DOUBLE PRECISION, vol_ena_arm_mlt_s DOUBLE PRECISION, vol_ena_arm_mlt_ne DOUBLE PRECISION, vol_ena_arm_mlt_n DOUBLE PRECISION,
    preco_petroleo_brl_lag1 DOUBLE PRECISION, preco_gas_brl_lag1 DOUBLE PRECISION,
    ena_arm_mlt_se_lag1 DOUBLE PRECISION, ena_arm_mlt_s_lag1 DOUBLE PRECISION, ena_arm_mlt_ne_lag1 DOUBLE PRECISION, ena_arm_mlt_n_lag1 DOUBLE PRECISION,
    volatilidade_petroleo_lag1 DOUBLE PRECISION
);

CREATE INDEX idx_master_data ON volatilidade.tabela_final_consolidada (data_inicio);

-- PASSO 2: CARGA INICIAL
INSERT INTO volatilidade.tabela_final_consolidada (
    ano, mes, semana_operativa, data_inicio, data_fim,
    pld_medio_se, pld_medio_s, pld_medio_ne, pld_medio_n,
    preco_petroleo_brent, volatilidade_petroleo, preco_gas_natural,
    ena_bruta_mw_se, ena_bruta_mlt_se, ena_bruta_mw_s, ena_bruta_mlt_s, 
    ena_bruta_mw_ne, ena_bruta_mlt_ne, ena_bruta_mw_n, ena_bruta_mlt_n,
    ena_arm_mw_se, ena_arm_mlt_se, ena_arm_mw_s, ena_arm_mlt_s, 
    ena_arm_mw_ne, ena_arm_mlt_ne, ena_arm_mw_n, ena_arm_mlt_n,
    vol_ena_arm_mlt_se, vol_ena_arm_mlt_s, vol_ena_arm_mlt_ne, vol_ena_arm_mlt_n
)
WITH pld_unico AS (
    SELECT ano, mes, semana, data_inicio, data_fim, AVG(sudeste) as pld_se, AVG(sul) as pld_s, AVG(nordeste) as pld_ne, AVG(norte) as pld_n
    FROM volatilidade.historico_pld GROUP BY ano, mes, semana, data_inicio, data_fim
)
SELECT 
    pld.ano, pld.mes, pld.semana, pld.data_inicio, pld.data_fim,
    pld.pld_se, pld.pld_s, pld.pld_ne, pld.pld_n,
    pet.preco_medio_usd, pet.desvio_padrao_semanal, gas.valor_usd_gas,
    ena.ena_bruta_mw_se, ena.ena_bruta_mlt_se, ena.ena_bruta_mw_s, ena.ena_bruta_mlt_s,
    ena.ena_bruta_mw_ne, ena.ena_bruta_mlt_ne, ena.ena_bruta_mw_n, ena.ena_bruta_mlt_n,
    ena.ena_arm_mw_se, ena.ena_arm_mlt_se, ena.ena_arm_mw_s, ena.ena_arm_mlt_s,
    ena.ena_arm_mw_ne, ena.ena_arm_mlt_ne, ena.ena_arm_mw_n, ena.ena_arm_mlt_n,
    ena.vol_ena_arm_mlt_se, ena.vol_ena_arm_mlt_s, ena.vol_ena_arm_mlt_ne, ena.vol_ena_arm_mlt_n
FROM pld_unico pld
LEFT JOIN volatilidade.tabela_precos_petroleo_semanal pet ON pld.data_inicio = pet.data_inicio
LEFT JOIN volatilidade.tabela_precos_gas_semanal gas ON pld.data_inicio = gas.data_inicio
LEFT JOIN volatilidade.tabela_ena_semanal ena ON pld.data_inicio = ena.data_inicio
ORDER BY pld.data_inicio DESC;

-- PASSO 3: TRAZER O PTAX
UPDATE volatilidade.tabela_final_consolidada m
SET ptax_medio_semanal = p.ptax_medio_semanal
FROM volatilidade.tabela_ptax_semanal p WHERE m.data_inicio = p.data_inicio;

-- PASSO 4: IMPUTAÇÃO (LOCF)
UPDATE volatilidade.tabela_final_consolidada t1 SET preco_gas_natural = (SELECT t2.preco_gas_natural FROM volatilidade.tabela_final_consolidada t2 WHERE t2.data_inicio < t1.data_inicio AND t2.preco_gas_natural IS NOT NULL ORDER BY t2.data_inicio DESC LIMIT 1) WHERE t1.preco_gas_natural IS NULL;
UPDATE volatilidade.tabela_final_consolidada t1 SET preco_petroleo_brent = (SELECT t2.preco_petroleo_brent FROM volatilidade.tabela_final_consolidada t2 WHERE t2.data_inicio < t1.data_inicio AND t2.preco_petroleo_brent IS NOT NULL ORDER BY t2.data_inicio DESC LIMIT 1) WHERE t1.preco_petroleo_brent IS NULL;
UPDATE volatilidade.tabela_final_consolidada t1 SET ptax_medio_semanal = (SELECT t2.ptax_medio_semanal FROM volatilidade.tabela_final_consolidada t2 WHERE t2.data_inicio < t1.data_inicio AND t2.ptax_medio_semanal IS NOT NULL ORDER BY t2.data_inicio DESC LIMIT 1) WHERE t1.ptax_medio_semanal IS NULL;
UPDATE volatilidade.tabela_final_consolidada SET volatilidade_petroleo = 0 WHERE volatilidade_petroleo IS NULL;

-- PASSO 5: PREÇOS EM BRL
UPDATE volatilidade.tabela_final_consolidada SET preco_petroleo_brl = preco_petroleo_brent * ptax_medio_semanal, preco_gas_brl = preco_gas_natural * ptax_medio_semanal;

-- PASSO 6: FEATURE ENGINEERING (LAGS)
WITH calculo_lags AS (
    SELECT id_semana,
        LAG(preco_petroleo_brl, 1) OVER (ORDER BY data_inicio ASC) as lag_petroleo, LAG(preco_gas_brl, 1) OVER (ORDER BY data_inicio ASC) as lag_gas,
        LAG(ena_arm_mlt_se, 1) OVER (ORDER BY data_inicio ASC) as lag_ena_se, LAG(ena_arm_mlt_s, 1) OVER (ORDER BY data_inicio ASC) as lag_ena_s,
        LAG(ena_arm_mlt_ne, 1) OVER (ORDER BY data_inicio ASC) as lag_ena_ne, LAG(ena_arm_mlt_n, 1) OVER (ORDER BY data_inicio ASC) as lag_ena_n,
        LAG(volatilidade_petroleo, 1) OVER (ORDER BY data_inicio ASC) as lag_vol_pet
    FROM volatilidade.tabela_final_consolidada
)
UPDATE volatilidade.tabela_final_consolidada m SET preco_petroleo_brl_lag1 = c.lag_petroleo, preco_gas_brl_lag1 = c.lag_gas, ena_arm_mlt_se_lag1 = c.lag_ena_se, ena_arm_mlt_s_lag1 = c.lag_ena_s, ena_arm_mlt_ne_lag1 = c.lag_ena_ne, ena_arm_mlt_n_lag1 = c.lag_ena_n, volatilidade_petroleo_lag1 = c.lag_vol_pet FROM calculo_lags c WHERE m.id_semana = c.id_semana;