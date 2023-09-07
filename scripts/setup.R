#### Carregando dados ####
#========================#
# tabela unificada
df <- readRDS("pda_tiss_hosp_mini.rds")

# corrigindo valores ausentes
temp <- df[, c("VL_ITEM_EVENTO_INFORMADO", "VL_ITEM_PAGO_FORNECEDOR")]
temp[is.na(temp)] <- 0
df[, c("VL_ITEM_EVENTO_INFORMADO", "VL_ITEM_PAGO_FORNECEDOR")] <- temp

# dados com variaveis numericas de interesse agregadas por ocorrencia
aggnum_df <- df[,c(id_var, numeric_vars)] %>% 
	group_by(ID_EVENTO_ATENCAO_SAUDE) %>% 
	summarise(
		VL_TOTAL_ITENS_INFORMADOS=sum(QT_ITEM_EVENTO_INFORMADO*VL_ITEM_EVENTO_INFORMADO),
		VL_ITEM_PAGO_FORNECEDOR=sum(VL_ITEM_PAGO_FORNECEDOR),
		TEMPO_DE_PERMANENCIA=max(TEMPO_DE_PERMANENCIA))

# dados com variaveis categoricas de interesse agregadas por ocorrencia
aggcat_df <-  df[,c(id_var, categorical_vars)] %>% 
	group_by(ID_EVENTO_ATENCAO_SAUDE) %>%
	summarise_at(categorical_vars, max) %>%
	mutate(ANO_MES_EVENTO=zoo::as.yearmon(ANO_MES_EVENTO))

# tabela agregada
agg_df <- inner_join(aggcat_df, aggnum_df, by="ID_EVENTO_ATENCAO_SAUDE")

# dados sem outliers e sem valores ausentes
temp <- na.omit(agg_df)
s1 <- IQRsel(temp$VL_TOTAL_ITENS_INFORMADOS, sel=T)
s2 <- IQRsel(temp$VL_ITEM_PAGO_FORNECEDOR, sel=T)
s3 <- IQRsel(temp$TEMPO_DE_PERMANENCIA, sel=T)

tidy_df <- temp[s1 & s2 & s3,] %>% 
	mutate(
		log.valor_item_inf = log(VL_TOTAL_ITENS_INFORMADOS+1),
		log.valor_pago_forn = log(VL_ITEM_PAGO_FORNECEDOR+1),
		TEMPO_DE_PERMANENCIA = abs(TEMPO_DE_PERMANENCIA))

#### Modelagem ####
#=================#
# Criando variavel target
agg_df$ACIONAMENTO_DO_SEGURO <- agg_df$VL_ITEM_PAGO_FORNECEDOR > 0

# Obtendo informações sobre o balanceamento dos dados
QTD_ACIONAMENTOS <- sum(agg_df$ACIONAMENTO_DO_SEGURO)
QTD_CASOS <- nrow(agg_df)

# Obtendo indices de `agg_df`
df_index <- row.names(agg_df) %>% as.numeric()

# Obtendo indices das amostras balanceadas
### maximizando a quantidade de observações usando todos os casos positivos
negative_sample_index <- sample(x=df_index[!agg_df$ACIONAMENTO_DO_SEGURO], size=QTD_ACIONAMENTOS)
positive_index <- df_index[agg_df$ACIONAMENTO_DO_SEGURO]
mdl1_index <- c(negative_sample_index, positive_index)

# Selecionando variáveis úteis
mdl1_cols <- c(
	"NM_MODALIDADE", "UF_PRESTADOR", "FAIXA_ETARIA", 
	"ANO_MES_EVENTO", "ACIONAMENTO_DO_SEGURO")

# Criando novo dataframe com valores de "ACIONAMENTO_DO_SEGURO" balanceados
mdl1_df <- agg_df[mdl1_index, mdl1_cols]

# Feature engineering nos dados de treinamento
temp <- mdl1_df$NM_MODALIDADE
temp[temp %in% c("Cooperativa Médica", "Medicina De Grupo")] <- "Cooperativa"
temp[temp %in% c("Seguradora Especializada Em Saúde", 
					  "Filantropia", "Autogestão", NA)] <- "Não-cooperativa"
mdl1_df["modalidade"] <- factor(temp)

temp <- mdl1_df$UF_PRESTADOR
temp[temp %in% c("MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA")] <- "Nordeste"
temp[temp %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO")] <- "Norte"
temp[temp %in% c("GO", "MT", "MS", "DF")] <- "Centro-Oeste"
temp[temp %in% c("ES", "RJ", "SP", "MG")] <- "Sudeste"
temp[temp %in% c("PR", "RS", "SC")] <- "Sul"
mdl1_df["regiao"] <- factor(temp)

temp <- mdl1_df$FAIXA_ETARIA
temp[temp %in% c("Não identificado", NA)] <- "Não Identificado"
temp[temp %in% c("<1", "1 a 4", "5 a 9")] <- "Crianças"
temp[temp %in% c("10 a 14", "15 a 19", "20 a 29", 
					  "30 a 39", "40 a 49", "50 a 59")] <- "Jovens/Adultos"
temp[temp %in% c("60 a 69", "70 a 79", "80 ou mais")] <- "Idosos"
mdl1_df["idade"] <- factor(temp)

temp <- mdl1_df$ANO_MES_EVENTO |> format("%m") |> as.integer()
temp[temp %in% 1:3] <- "Primeiro"
temp[temp %in% 4:6] <- "Segundo"
temp[temp %in% 7:9] <- "Terceiro"
temp[temp %in% 10:12] <- "Quarto"
mdl1_df["trimestre"] <- factor(temp)

# Ajustando modelos
mdl_lg <- glm(ACIONAMENTO_DO_SEGURO ~ trimestre + idade + modalidade * regiao, data=mdl1_df)
mdl_nb <- naive_bayes(ACIONAMENTO_DO_SEGURO ~ trimestre + idade + modalidade * regiao, data=mdl1_df)
mdl_rf <- randomForest(ACIONAMENTO_DO_SEGURO ~ trimestre + idade + modalidade * regiao, data=mdl1_df)

#### Avaliação do modelo ####
#===========================#

# Feature engineering nos dados de teste 
df$ACIONAMENTO_DO_SEGURO <- df$VL_ITEM_PAGO_FORNECEDOR > 0

temp <- df$NM_MODALIDADE
temp[temp %in% c("Cooperativa Médica", "Medicina De Grupo")] <- "Cooperativa"
temp[temp %in% c("Seguradora Especializada Em Saúde", 
					  "Filantropia", "Autogestão", NA)] <- "Não-cooperativa"
df["modalidade"] <- factor(temp)

temp <- df$UF_PRESTADOR
temp[temp %in% c("MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA")] <- "Nordeste"
temp[temp %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO")] <- "Norte"
temp[temp %in% c("GO", "MT", "MS", "DF")] <- "Centro-Oeste"
temp[temp %in% c("ES", "RJ", "SP", "MG")] <- "Sudeste"
temp[temp %in% c("PR", "RS", "SC")] <- "Sul"
df["regiao"] <- factor(temp)

temp <- df$FAIXA_ETARIA
temp[temp %in% c("Não identificado", NA)] <- "Não Identificado"
temp[temp %in% c("<1", "1 a 4", "5 a 9")] <- "Crianças"
temp[temp %in% c("10 a 14", "15 a 19", "20 a 29", 
					  "30 a 39", "40 a 49", "50 a 59")] <- "Jovens/Adultos"
temp[temp %in% c("60 a 69", "70 a 79", "80 ou mais")] <- "Idosos"
df["idade"] <- factor(temp)

temp <- df$ANO_MES_EVENTO |> as.Date() |> format("%m") |> as.integer()

temp[temp %in% 1:3] <- "Primeiro"
temp[temp %in% 4:6] <- "Segundo"
temp[temp %in% 7:9] <- "Terceiro"
temp[temp %in% 10:12] <- "Quarto"
df["trimestre"] <- factor(temp)

# Tabela de previsões
pred_df <- data.frame(
	y=df$ACIONAMENTO_DO_SEGURO,
	Regressao.Logistica=predict(mdl_lg, df),
	Naive.Bayes=predict(mdl_nb, df, type="prob")[, "TRUE"],
	Random.Forest=predict(mdl_rf, df)
)