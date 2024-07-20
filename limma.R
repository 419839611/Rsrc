library(limma)

#phenotype: column=sample,group
#eg: de = DEG_limma(phenotype,exp)
DEG_limma = function(phenotype,exp) {
	de = phenotype
	de[,1] = "null"
	targets = as.data.frame(de)
	colnames(targets)=c("Cy3","Cy5")
	rownames(targets) = phenotype[,1]
	design1 = modelMatrix(targets,ref = "null")
	compare = colnames(design1)
	colnames(design1) = c("group1","group2")
	names(compare) = c("group1","group2")
	design1 = list(matrix = design1, info = compare)
	print(design1)
	fit = lmFit(exp,design1$matrix)
	contrast.matrix = makeContrasts(group1-group2, levels=design1$matrix)
	fit2 = contrasts.fit(fit, contrast.matrix)
	fit2 = eBayes(fit2)
	DEall = topTable(fit2,adjust = "BH",coef = 1, number = 30000, p.value = 1,lfc=0)
	write.table(DEall,"DEG.txt",sep="\t",quote=F,row.names=T,col.names=T)
	return(DEall)
} 
