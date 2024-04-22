WITH joined_tables AS (
    SELECT
        d.*
        c.ID_PLANO,
        c.FAIXA_ETARIA,
        c.SEXO,
        c.CD_MUNICIPIO_BENEFICIARIO,
        c.PORTE,
        c.CD_MODALIDADE,
        c.NM_MODALIDADE,
        c.CD_MUNICIPIO_PRESTADOR,
        c.CD_CARATER_ATENDIMENTO,
        c.CD_TIPO_INTERNACAO,
        c.CD_REGIME_INTERNACAO,
        c.CD_MOTIVO_SAIDA,
        c.CID_1,
        c.CID_2,
        c.CID_3,
        c.CID_4,
        c.QT_DIARIA_ACOMPANHANTE,
        c.QT_DIARIA_UTI,
        c.IND_ACIDENTE_DOENCA,
        c.LG_VALOR_PREESTABELECIDO,
        
    FROM HOSP_CONS c
    JOIN HOSP_DET d ON c.ID_EVENTO_ATENCAO_SAUDE = d.ID_EVENTO_ATENCAO_SAUDE
),
