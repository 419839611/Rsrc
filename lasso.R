library(glmnet)

#data: row=gene, column=sample
#survival: time status, time shoule more than 0 
get_lasso = function(data,survival,figname,txtname) {
	x = as.matrix(t(data))
	pdf(figname)
	fit = glmnet(x,survival,family="cox")
	plot(fit)
	fit1_cv = cv.glmnet(x, survival, family = "cox")#10 fold cross validation
	plot(fit1_cv)
	dev.off()
	coeff = coef(fit1_cv$glmnet.fit,s=fit1_cv$lambda.min)
	result = data.frame(coefficient=coeff@x)
	rownames(result) = coeff@ Dimnames[[1]][coeff@i + 1]
	write.table(result,txtname,sep='\t',col.names=T,row.names=T,quote=F)
	return(result)
}

survival = read.delim('../6.prognosis/OS.txt')
survival = survival[which(survival$OS.time > 0),]
survival = as.matrix(survival[,2:1])
colnames(survival) = c('time','status')
lasso = get_lasso(use,survival,'lasso.pdf','lasso.txt')

score = function(data,coef,survive,txtname) {
	data = t(data[rownames(coef),])
	valiResu = matrix(NA,ncol=ncol(data),nrow=nrow(data))
	for (i in 1:ncol(data)) {
	    g = coef[i,1]*data[,i]
	    valiResu[,i] = g
	}
	dimnames(valiResu) = dimnames(data)
	valiResu = as.data.frame(valiResu)
	valiResu$score = rowSums(valiResu)
	med = median(valiResu$score)
	risk = vector()
	risk[valiResu$score >= med] = 'high'
	risk[valiResu$score < med] = 'low'
	valiResu$risk = risk
	valiResu$OS = survive[,2]
	valiResu$OS.month = survive[,1]
	write.table(valiResu,txtname,sep='\t',quote=F,row.names=T,col.names=T)
	return(valiResu)
}
tcga_score = score(use,lasso,survival,'tcga_score.txt')
