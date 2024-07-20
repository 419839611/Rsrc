library(Hmisc)#rcorr
library(corrplot)

#data is a matrix, Computes a matrix of correlation coefficients for all possible pairs of columns of a matrix
#eg: corr = calculate_correlation(as.matrix(t(kat_exp)))
calculate_correlation = function(data) {
	res = rcorr(data,type='spearman')
	cor = res$r
	p = res$P
	ut = upper.tri(cor) 
	result = data.frame(row = rownames(cor)[row(cor)[ut]] ,
		column = rownames(cor)[col(cor)[ut]], 
		cor =(cor)[ut], 
		p = p[ut] )
	write.table(result,'cor.txt',sep='\t',quote=F,row.names=F,col.names=T)
	return(result)
}

#relation
plot_correlation = function(figname,relation){
	pdf(figname)
	corrplot(relation,tl.col='black',type='upper')
	# corrplot(relation, p.mat = use_p,method='color',insig = "label_sig",sig.level = c(.001, .01, .05), pch.cex = 0.5, pch.col = "grey")
	dev.off()
}