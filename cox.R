library(survival)

#exp: row=gene,column=sample
#eg: groups = group_median(kat_exp)
group_median = function(exp) {
	exp_median = apply(exp,1,median)
	groups = apply(exp,2,function(x,cutoff=exp_median) {
		group = as.vector(x >= cutoff)
		group[grep('TRUE',group)] = 'high expression'
		group[grep('FALSE',group)] = 'low expression'
		return(group)
		})
	rownames(groups) = rownames(exp)
	return(groups)
}
group_quantile = function(exp) {
	quan_low = apply(exp,1,quantile,0.25)
	quan_up = apply(exp,1,quantile,0.75)
	groups = apply(exp,2,function(x,cutoff_low=quan_low,cutoff_up=quan_up) {
		group = ifelse(x >= quan_up,'high_expression',ifelse(x <= quan_low,'low_expression','mediate_expression'))
		return(group)
		})
	rownames(groups) = rownames(exp)
	return(groups)
	}

#groups: row=gene,column=sample
#survival: columns = sample OS OS.month DFS DFS.month group
#eg: os_cox = cox_matrix(groups,survival,'OS_cox.txt')
cox_matrix = function(groups,survival,filename) {
	get_cox = function(group) {
		tmp = cbind(survival,group)
		colnames(tmp)[ncol(tmp)] = 'group'
		tmp = subset(tmp,group!='mediate_expression')
		cox = coxph(Surv(OS.month, OS) ~ group, data = tmp)
		y = summary(cox)
		p.logrank=as.vector(signif(y$sctest["pvalue"],digits = 2))
		hr=round(y$coefficients[, "exp(coef)"][1],digits = 2)
		hr_lower=round(y$conf.int[,"lower .95"][1],digits = 2)
		hr_upper=round(y$conf.int[,"upper .95"][1],digits = 2)
		cox_result = c(p.logrank,hr,hr_lower,hr_upper)
		return(cox_result)
		}
	# cox_results = apply(groups,1,get_cox,survival=survival)
	cox_results = apply(groups,1,get_cox)
	rownames(cox_results) = c('pvalue','hr','hr_low','hr_high')
	cox_results = as.data.frame(t(cox_results))
	write.table(cox_results,filename,sep='\t',row.names=T,col.names=T,quote=F)
	return(cox_results)
}


