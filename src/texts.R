num_vars1 <- c(
	index='Índice de identificador da ocorrência, gerado após a coleta dos dados',
	TEMPO_DE_PERMANENCIA='Tempo de permanência no atendimento (dias)',
	CD_TABELA_REFERENCIA='Identificador de procedimento/item utilizado',
	QT_ITEM_EVENTO_INFORMADO='Quantidade utilizada do procedimento/item',
	VL_ITEM_EVENTO_INFORMADO='Valor individual do procedimento/item identificado',
	VL_ITEM_PAGO_FORNECEDOR='Valor total pago pela operadora do plano de saúde à fornecedora',
	IND_PACOTE='Faz parte de um pacote de procedimentos. 1=Sim, 0=Não',
	IND_TABELA_PROPRIA='Identificador do procedimento é próprio da operadora. 1=Sim, 0=Não',
	ID_PLANO='Identificador único do plano de saúde, não segue regulamento da ANS',
	CD_MUNICIPIO_BENEFICIARIO='Codigo de Municipio IBGE (residência do beneficiário)',
	CD_MODALIDADE='Código numério identificando a operadora',
	CD_MUNICIPIO_PRESTADOR='Codigo de Municipio IBGE (estabelecimento médico)',
	CD_CARATER_ATENDIMENTO='Caráter do atendimento conforme tabela externa TUSS 23',
	CD_TIPO_INTERNACAO='Tipo de internação conforme tabela externa TUSS 57',
	CD_REGIME_INTERNACAO='Regime de internação conforme tabela externa TUSS 41',
	CD_MOTIVO_SAIDA='Motivo do encerramento do atendimento conforme tabela externa TUSS 39',
	QT_DIARIA_ACOMPANHANTE='Número de diárias de acompanhante',
	QT_DIARIA_UTI='Número de diárias de UTI',
	IND_ACIDENTE_DOENCA='Especifica tipo de acidente ou doença do usuário conforme tabela externa TUSS 36',
	LG_VALOR_PREESTABELECIDO='Indica se o valor é preestabelecido em contrato. 1=Sim, 2=Não',
	VL_TOTAL_ITENS_INFORMADOS="Variável criada aqui ao agregar os dados presentes em 'QT_ITEM_EVENTO_INFORMADO' e 'VL_ITEM_EVENTO_INFORMADO', mostra o valor total de todos os itens/procedimentos adotados no atendimneto"
)

cat_vars1 <- c(
	ID_EVENTO_ATENCAO_SAUDE="Identificador único do evento de internação",
	UF_PRESTADOR="Estado de localização do prestador do atendimento",
	ANO_MES_EVENTO="Data da ocorrência com ano e mês",
	CD_PROCEDIMENTO="Código de identificação do item assistencial conforme tabela externa TUSS 63",
	FAIXA_ETARIA="Faixa etária em que o beneficiário se encaixa",
	SEXO="Sexo do beneficiário",
	PORTE="Porte do prestador de plano de saúde conforme a quantidade de funcionários divulgado no útimo SIB",
	NM_MODALIDADE="Classificação das prestadoras de planos de saúde de acordo com estatuto jurídico",
	CID_1="Código CID10 informado no primeiro diagnóstico",
	CID_2="Código CID10 informado no segundo diagnóstico (se houver)",
	CID_3="Código CID10 informado no terceiro diagnóstico (se houver)",
	CID_4="Código CID10 informado no quarto diagnóstico (se houver)")

id_var <- "ID_EVENTO_ATENCAO_SAUDE"

det_vars <- c(id_var,
	"UF_PRESTADOR", "TEMPO_DE_PERMANENCIA", "ANO_MES_ENVENTO", "CD_PROCEDIMENTO",
	"CD_TABELA_REFERENCIA", "QT_ITEM_EVENTO_INFORMADO", "VL_ITEM_EVENTO_INFORMADO",
	"VL_ITEM_PAGO_FORNECEDOR", "IND_PACOTE", "IND_TABELA_PROPRIA")

cons_vars <- c(
	"ID_PLANO", "FAIXA_ETARIA", "SEXO", "CD_MUNICIPIO_BENEFICIARIO", "PORTE", 
	"CD_MODALIDADE", "NM_MODALIDADE", "CD_MUNICIPIO_PRESTADOR", "UF_PRESTADOR", 
	"TEMPO_DE_PERMANENCIA", "ANO_MES_EVENTO", "CD_CARATER_ATENDIMENTO", 
	"CD_TIPO_INTERNACAO", "CD_REGIME_INTERNACAO", "CD_MOTIVO_SAIDA", "CID_1", "CID_2", 
	"CID_3", "CID_4", "QT_DIARIA_ACOMPANHANTE", "QT_DIARIA_UTI", "IND_ACIDENTE_DOENCA",
	"LG_VALOR_PREESTABELECIDO")

categorical_vars <- c(
	"UF_PRESTADOR", "ANO_MES_EVENTO", "FAIXA_ETARIA", "SEXO","PORTE","NM_MODALIDADE")

numeric_vars <- c(
	"QT_ITEM_EVENTO_INFORMADO", "VL_ITEM_EVENTO_INFORMADO",
	"VL_ITEM_PAGO_FORNECEDOR", "TEMPO_DE_PERMANENCIA")

agg_numeric_vars <- c(
	"VL_TOTAL_ITENS_INFORMADOS", "VL_ITEM_PAGO_FORNECEDOR",
	"TEMPO_DE_PERMANENCIA")