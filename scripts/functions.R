cores <- c("#272727","#333232","#c2c2c2","#332C2C","#543a3a","#b66868","#976969")
my_ggtheme <- function() {
	theme_minimal(base_size=14) +
	theme(
		plot.title=element_text(color=cores[3]),
		plot.background=element_rect(fill=cores[1], color=cores[1]),
		panel.background=element_rect(fill=cores[1], color=cores[1]),
		panel.grid=element_line(color=cores[2]),
		axis.text=element_text(color=cores[3], size=9),
		axis.title=element_text(color=cores[3]),
		strip.text=element_text(color=cores[3], face="bold", size=16)
	)
}

# Ordena fatores pela quantidade de ocorrencias para plotar
order_factor <- function(var, descend=TRUE){
	if (descend) reorder(var,var,function(x)+length(x))
	else reorder(var,var,function(x)-length(x))}

# Retorna seleção de valores não outliers de um vetor numérico, usando IQR
IQRsel <- function(x, sel=F) {
	iqr <- IQR(x, na.rm=T, type=6)*1.5
	q1 <- quantile(x, 1/4, names=F, na.rm=T)
	q3 <- quantile(x, 3/4, names=F, na.rm=T)
	if (sel) x >= q1-iqr & x <= q3+iqr
	else x[x >= q1-iqr & x <= q3+iqr]
}

# Formato padrão de exibição de tabelas
out_table <- function(df) {
	flextable(df) %>% 
		theme_zebra(odd_header=cores[5], odd_body=cores[2], even_body=cores[1]) %>% 
		font(part="all", fontname="Tahoma") %>%
		color(color="#C2C2C2", part="all") %>%
		align(align="center", part="header") %>%
		align(align="right", part="body") %>% 
		align(align="left", j=1, part="all") %>%
		autofit()
}

# formatando numeros para exibir em tabela
format_numbers <- function(x, decimals=2) {
	formatC(
		as.numeric(as.character(x)), 
		digits=decimals, 
		format="f", 
		big.mark=".",
		decimal.mark=","
	)
}

# Tabela formatada com medidas de tendencia central e de dispersao para cada variavel
summary_num <- function(x, vars) {
	require(dplyr)
	require(flextable)
	
	mins  <- c(); fstqs <- c(); medians <- c()
	means <- c(); trdqs <- c(); maxs    <- c()
	stds  <- c(); nas   <- c(); x       <- x[,vars]
	
	for (var in vars) {
		clean_var   <- na.omit(x[[var]])
		mins[var]   <- min(clean_var) %>% format_numbers()
		fstqs[var]  <- quantile(clean_var, probs=.25) %>% format_numbers()
		medians[var]<- median(clean_var) %>% format_numbers()
		means[var]  <- mean(clean_var) %>% format_numbers()
		trdqs[var]  <- quantile(clean_var, probs=.75) %>% format_numbers()
		maxs[var]   <- max(clean_var) %>% format_numbers()
		stds[var]   <- sqrt(var(clean_var)) %>% format_numbers()
		nas[var]    <- as.integer(sum(is.na(x[[var]]))) %>% format_numbers()
	}
	
	output <- tibble(
		Variáveis=vars, Mínimo=mins, `Primeiro Quartil`=fstqs, Mediana=medians,
		Média=means, `Terceiro Quartil`=trdqs, Máximo=maxs, `Desvio Padrão`=stds,
		`N/Ds`=nas)
	
	out_table(output)
}

# Tabela de frequencia para os dados categoricos
freq_table <- function(x, var, sel.var=NULL, sel.fun=function(x)is.numeric(x)){
	require(dplyr)
	require(flextable)
	
	N <- nrow(x)
	na_count <- sum(is.na(x[,var]))
	
	if (!is.null(sel.var) & !is.character(sel.var))
		warning("Argumento sel.var deve ser do tipo 'character', continuando sem agregar...")
	if (!(sel.var %in% names(x)))
		stop(paste(sel.var, "não existe em x"))
	if (is.character(sel.var)) {
		sel <- sel.fun( x[[sel.var]] )
		if (sum(sel)==0)
			stop("Seleção não deixou itens disponíveis, verifique o argumento 'sel.fun'.")
		tab <- x[sel, var]
	} else 
		tab <- x[,var]
	
	tab <- enframe(table(tab[,var])) %>%
		mutate(Freq=as.integer(value), Valor=name) %>%
		rows_insert(tibble(Valor="N/D", Freq=na_count), by="name") %>%
		mutate(Percent=Freq*100/N) %>% 
		select(Valor, Freq, Percent) %>%
		.[order(.$Freq, decreasing=T),]
	return(tab)
}

# Grafico de frequencia para os dados categoricos
freq_plot <- function(x, var, sort=F, aggr.var=NULL, aggr.fun=max, h=NULL, w=800){
	require(dplyr)
	require(ggplot2)
	require(plotly)
	
	N <- nrow(x)
	na_count <- sum(is.na(x[,var]))
	
	if (!is.null(aggr.var) & !is.character(aggr.var))
		warning("Argumento aggr.var deve ser do tipo 'character'")
	if (is.character(aggr.var)) 
		tab <- x[,c(var, aggr.var)] %>% group_by_at(aggr.var, aggr.fun) %>% .[,var]
	else
		tab <- x[,var]

	tab <- tab %>% table() %>% enframe() %>%
		mutate_at("value", as.integer) %>%
		rows_insert(tibble(name="N/Ds", value=na_count), by="name")
	
	title <- paste(
		"Frequência de", var, 
		ifelse(is.character(aggr.var), paste0("agrupado por\n", aggr.var), ""))
	plot <- ggplot(tab, aes(y=value, x=name)) + geom_col(fill=cores[6]) + coord_flip() + 
		my_ggtheme() + labs(y=title, x=NULL, title=NULL)
	 
	if (is.numeric(sort)) {
		s <- c(tab[sort, "name"])[["name"]]
		plot <- plot + scale_x_discrete(limits=s)
	} else if (is.logical(sort) & sort) {
		s <- c(tab[order(tab$value), "name"])[["name"]]
		plot <- plot + scale_x_discrete(limits=s)}
	ggplotly(plot, width=w, height=h)
}

# Graficos histograma testando transformações de um conjunto de dados
dist_transf_plot <- function(
		x, transf_funcs=list(log=function(x) log(abs(x)), sqrt=function(x) sqrt(abs(x))), 
		trim_outlier=TRUE, show_original=TRUE, var_name="Valor", title="") {
	x <- na.omit(x)
	lim <- c(
		min=quantile(x,1/4,names=F)-IQR(x)*1.5, 
		max=quantile(x,3/4,names=F)+IQR(x)*1.5)
	if (trim_outlier) x <- x[(x >= lim["min"]) & (x <= lim["max"])]
	
	df <- tibble(x); names(df) <- var_name
	df %>% mutate(across(.fns=transf_funcs, .names="{.fn} de {.col}")) %>%
		pivot_longer(cols=everything()) %>%
		group_by(name) %>%
	do(p=plot_ly(., x = ~value, name =~name, type = "histogram")) %>%
		subplot(nrows = 1)
}

# Calcula porcentagens e escreve bonitinho
ncent <- function(sn, sN, inside=T, decimals=2){
	if (inside) percentvalue <- format_numbers(sn*100/sN, decimals=decimals)
	else percentvalue <- format_numbers(100-(sn*100/sN), decimals=decimals)
	paste0(percentvalue, "%")
}

# Retorna uma matriz de confusão em formato de array
confusion_matrix <- function(predictions, actuals){
	cm <- c()
	cm["tp"] = sum(predictions & actuals)
	cm["fp"] = sum(predictions & !actuals)
	cm["tn"] = sum(!predictions & !actuals)
	cm["fn"] = sum(!predictions & actuals)
	return(cm)
}

# Obtem um dataframe com as probabilidades previstas e um vetor com as previsões
# Retorna um dataframe com tpr e fpr para cada combinação de threshold e modelo
prc_curve <- function(responses_prob_df, actuals) {
	responses_prob_df <- as.data.frame(responses_prob_df)
	thresholds <- c(1:99)/100
	out <- data.frame(thresholds=thresholds)
	col_names <- c('thresholds')
	
	for (mdl in colnames(responses_prob_df)) {
		tpr <- c()
		precision <- c()
		
		for (i in thresholds) {
			cm <- confusion_matrix(responses_prob_df[mdl], actuals, i)
			tpr <- append(tpr, cm[["tp"]]/(cm[["tp"]] + cm[["fn"]]))
			precision <- append(precision, cm[["tp"]]/(cm[["tp"]] + cm[["fp"]]))
		}
		tpr_name = paste0(mdl, "_tpr")
		precision_name = paste0(mdl, "_precision")
		col_names <- append(col_names, c(tpr_name, precision_name))
		out <- cbind(out, data.frame(tpr, precision))
		colnames(out) <- col_names
	}
	return(out)
}

# Obtem um vetor com modelos compatíveis com `stats::predict()` e um conjunto de dados
# Retorna um conjunto de dados de validação cruzada no método Monte Carlo com repetições
mccv_data <- function(formulas, models, types, dataset, balanced_for=NULL, n_repeats=20L, 
							 test_size=.25, q=.5){
	# `formulas`:		[vector] of R formulas with variables present in `dataset`
	# `models`:			[vector] of models compatible with syntax "mdl(formula, data)",
	#						must be stored as callable, and NOT call (e.g. glm instead of glm()),
	#						if named, names will be preserved in the output
	# `types`:			[vector] of `type` argument, used on predict(), for each model
	# `dataset`:		[data.frame] on which the models will be trained and tested
	# `balanced_for`: [character] with the name of the variable used to balance the 
	#						training sets
	# `n_repeats`:		[integer] with the number of repeats of train-test iterations
	# `test_size`:		[double] between 0 and 1, as percentage of `dataset` rows for testing
	# `q`:				[double] between 0 and 1, used only if `balanced_for` is defined,
	#						smaller value will grant more variability at cost of less data used
	#						for training
							
	library(dplyr)
	#checando variáveis
	n_repeats <- as.integer(n_repeats)
	if (n_repeats <= 0) 
		stop("`n_repeats` must be 1 or greater")
	if (!between(test_size, 0, 1) & !between(q, 0, 1))
		stop("`test_size` and `q` must be between 0 and 1")
	if (!is.vector(models))
		stop("You must pass an vector of models in `models`")
	if (!is.null(balanced_for))
		if (!(balanced_for %in% names(dataset)))
			stop("`balanced_for` must be a string name of a variable in `dataset`")
	
	for (f in formulas) {
		HAVE_Y_VARIABLE <- terms(f) |> attr(which="response") |> as.logical()
		HAS_ONE_Y_VAR <- all.vars(f[[2]]) |> length() == 1
		Y_VARIABLE_IS_DOT <- sum(all.vars(f[[2]]) == ".") |> as.logical()
		if (!HAVE_Y_VARIABLE | !HAS_ONE_Y_VAR | Y_VARIABLE_IS_DOT)
			stop("Your `formulas` must have one predicted variable each")
	}
		
	#variáveis e constantes essenciais
	Y_VARIABLES <- c()
	for (f in formulas)
		Y_VARIABLES <- c(Y_VARIABLES, all.vars(f[[2]])[[1]])
	IS_BALANCED <- is.character(balanced_for)
	TEST_SIZE <- as.integer(nrow(dataset) * test_size)
	TRAIN_SIZE <- as.integer(nrow(dataset) - TEST_SIZE)
	data_index <- 1:nrow(dataset)
	
	#definindo critérios para balancear os dados
	if (IS_BALANCED) {
		bal_var <- dataset[[balanced_for]]
		bal_unique_freqs <- table(bal_var)
		GROUP_SIZE <- min(bal_unique_freqs) * q
		TRAIN_SIZE <- GROUP_SIZE * length(bal_unique_freqs)
	}
	
	#ordenando amostras aleatórias de dados
	if (IS_BALANCED) {
		CLASSES <- names(bal_unique_freqs)
		#povoando uma lista com índices de cada classe a ser balanceada no treino
		classes_index <- list()
		for (class in CLASSES) {
			classes_index[[class]] <- data_index[as.character(bal_var) == class]}
		
		train_samples <- list()
		test_samples <- list()
		for (reps in 1:n_repeats) {
			TRAIN_NAME <- paste0("train", reps)
			TEST_NAME <- paste0("test", reps)
				
			class_samples <- lapply(classes_index, function(x) sample(x, GROUP_SIZE))
			train_samples[[TRAIN_NAME]] <- do.call("c", class_samples)
			subset_data_index <- data_index[!(data_index %in% train_samples[[TRAIN_NAME]])]
			test_samples[[TEST_NAME]] <- sample(subset_data_index, TEST_SIZE)
		}
	} else {
		train_samples <- list()
		test_samples <- list()
		for (reps in 1:n_repeats) {
			TRAIN_NAME <- paste0("train", reps)
			TEST_NAME <- paste0("test", reps)
			
			train_samples[[TRAIN_NAME]] <- sample(data_index, TRAIN_SIZE)
			subset_data_index <- data_index[!(data_index %in% train_samples[[TRAIN_NAME]])]
			test_samples[[TEST_NAME]] <- subset_data_index
		}
	}
	
	# treinando e obtendo previsões para todos os conjuntos de treino e teste
	recursive_training <- function(model, model_name, formula, type) {
		CURRENT_Y_VAR <- all.vars(formula[[2]])[[1]]

		results <- list()
		for (iter in seq_along(train_samples)){
			train_data <- dataset[train_samples[[iter]], ]
			test_data <- dataset[test_samples[[iter]], ]
			trained_mdl <- model(formula=formula, data=train_data)
			
			if (TYPE == "prob") {
				predictions <- predict(trained_mdl, test_data, type=TYPE)[, "TRUE"]
			} else {
				predictions <- predict(trained_mdl, test_data, type=TYPE)
			}
			results[[iter]] <- data.frame(
				iteration=rep(paste0(model_name, iter), TEST_SIZE),
				dataset_index=test_samples[[iter]],
				y_actual=test_data[[CURRENT_Y_VAR]],
				y_predict=predictions
			)
		}

		return(do.call("rbind", results))
	}
	
	#construindo lista de dataframes com resultados para cada modelo
	if (models |> names() |> is.null())
		names(models) <- paste("model", 1:length(models))
	model_names <- names(models)
	
	output <- list()
	for (iter in seq_along(models)) {
		NAME <- model_names[[iter]]
		MDL <- models[[iter]]
		FORMULA <- formulas[[iter]]
		TYPE <- types[[iter]]
		#CURRENT_Y_VAR <- Y_VARIABLES[[iter]]
		
		output[[NAME]] <- recursive_training(
			model=MDL, 
			model_name=NAME,
			formula=FORMULA,
			type=TYPE
		)
	}

	return(output)
}

select_threshold <- function(y_actual, y_probs, plot_=FALSE, model_name="o modelo") {
	thresholds <- 1:99/100
	recall <- c(); fnr <- c()
	
	for (i in thresholds) {
		prediction <- {y_probs >= i}
		cm <- confusion_matrix(y_actual, prediction)
		tp <- cm[["tp"]]
		fn <- cm[["fn"]]
		
		recall <- c(recall, tp/(tp+fn))
		fnr <- c(fnr, fn/(fn+tp))
	}
	metric <- recall - fnr
	if (plot_) {
		val <- thresholds[which.max(metric)]
		p <- ggplot(data=data.frame(Threshold=thresholds, metric=metric)) +
			geom_line(aes(x=Threshold, y=metric), color=cores[6]) + 
			geom_text(aes(x=.9, y=-.7, label=val), color=cores[3]) + 
			geom_vline(xintercept=val, color=cores[3]) +
			ylab(NULL) + ggtitle(paste("Melhor limiar para", model_name)) + my_ggtheme()
		return(plotly::ggplotly(p))
	} else {
		return(thresholds[which.max(metric)])
	}
}