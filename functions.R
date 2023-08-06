cores <- c("#272727","#333232","#c2c2c2","#332C2C","#543a3a","#b66868","#976969")
my_ggtheme <- function() {
	theme_minimal(base_size=14) +
	theme(
		plot.title=element_text(color=cores[3]),
		plot.background=element_rect(fill=cores[1], color=cores[1]),
		panel.background=element_rect(fill=cores[1], color=cores[1]),
		panel.grid=element_line(color=cores[2]),
		axis.text=element_text(color=cores[3]),
		axis.title=element_text(color=cores[3]),
		strip.text= element_text(color=cores[3], face="bold", size=16)
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
	else x[x >= q1-iqr & x <= q3+iqr]}

# Formato padrão de exibição de tabelas
out_table <- function(df) {
	flextable(df) %>% 
		theme_zebra(odd_header=cores[5], odd_body=cores[2], even_body=cores[1]) %>% 
		font(part="all", fontname="Tahoma") %>%
		color(color="#C2C2C2", part="all") %>%
		align(align="center", part="header") %>%
		align(align="right", part="body") %>% 
		align(align="left", j=1, part="all") %>%
		autofit()}

# formatando numeros para exibir em tabela
format_numbers <- function(x, decimals=2) {
	formatC(
		as.numeric(as.character(x)), 
		digits=decimals, 
		format="f", 
		big.mark=".",
		decimal.mark=",")}

# Tabela formatada com medidas de tendencia central e de dispersao para cada variavel
summary_num <- function(x, vars) {
	require(dplyr)
	require(flextable)
	
	mins  <-c(); fstqs <-c(); medians <-c()
	means <-c(); trdqs <-c(); maxs    <-c()
	stds  <-c(); nas   <-c(); x       <- x[,vars]
	
	for (var in vars) {
		clean_var   <-na.omit(x[[var]])
		mins[var]   <-min(clean_var) %>% format_numbers()
		fstqs[var]  <-quantile(clean_var, probs=.25) %>% format_numbers()
		medians[var]<-median(clean_var) %>% format_numbers()
		means[var]  <-mean(clean_var) %>% format_numbers()
		trdqs[var]  <-quantile(clean_var, probs=.75) %>% format_numbers()
		maxs[var]   <-max(clean_var) %>% format_numbers()
		stds[var]   <-sqrt(var(clean_var)) %>% format_numbers()
		nas[var]    <-as.integer(sum(is.na(x[[var]]))) %>% format_numbers()}
	
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
	
	if (!is.null(sel.var) & !is.character(sel.var)) {
		warning("Argumento sel.var deve ser do tipo 'character', continuando sem agregar...")}
	if (!(sel.var %in% names(x))) {
		stop(paste(sel.var, "não existe em x"))}
	if (is.character(sel.var)) {
		sel <- sel.fun( x[[sel.var]] )
		if (sum(sel)==0) {
			stop("Seleção não deixou itens disponíveis, verifique o argumento 'sel.fun'.")}
		tab <- x[sel, var]
	} else tab <- x[,var]
	
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
	
	if (!is.null(aggr.var) & !is.character(aggr.var)){
		warning("Argumento aggr.var deve ser do tipo 'character'")}
	if (is.character(aggr.var)) {
		tab <- x[,c(var, aggr.var)] %>% group_by_at(aggr.var, aggr.fun) %>% .[,var]
	} else tab <- x[,var]

	tab <- tab %>% table() %>% enframe() %>%
		mutate_at("value", as.integer) %>%
		rows_insert(tibble(name="N/Ds", value=na_count), by="name")
	
	title <- paste(
		"Frequência de", var, 
		ifelse(is.character(aggr.var), paste0("agrupado por\n", aggr.var), ""))
	plot <- ggplot(tab, aes(y=value, x=name)) + geom_col(fill=cores[6]) + coord_flip() + 
		my_ggtheme() + labs(y=title, x=NULL, title=NULL)
	 
	if(is.numeric(sort)){
		s <- c(tab[sort, "name"])[["name"]]
		plot <- plot + scale_x_discrete(limits=s)
	} else if(is.logical(sort) & sort){
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
	if (trim_outlier) x <- x[x >= lim["min"] & x <= lim["max"]]
	
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
	paste0(percentvalue, "%")}
