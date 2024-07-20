library(edgeR)

x = read.delim('TCGA_protein_count.txt')
phenotype = read.delim('TCGA_phenotype.txt')
group = phenotype[,2]#facor: normal=1, tumor=2
y = DGEList(counts=x,group=group)#DGElist object
# keep = filterByExpr(y)#remove low counts across all libraries, filter with count-per-million(>0.5)
# y = y[keep,,keep.lib.sizes=FALSE]
y = calcNormFactors(y)#normalize for sample-specific effects like total library depths
design = model.matrix(~group)#normal=0, tumor=1
y = estimateDisp(y,design)#estimate common dispersion and tagwise dispersions in one run
##quasi-likelihood F-tests
# fit = glmQLFit(y,design)
# qlf = glmQLFTest(fit,coef=2)
# topTags(qlf)
# likelihood ratio tests, more obvious choice
fit = glmFit(y,design)
lrt = as.data.frame(topTags(glmLRT(fit),n=nrow(x)))#LFC is 1-0
write.table(lrt,'DEG.txt',sep='\t',quote=F,row.names=T,col.names=T)

library(pheatmap)

fpkm = read.delim('../1.preprocessing/TCGA_protein_zFPKM.txt')
lfc = 1.5
fdr = 0.05
degsig = subset(lrt,abs(logFC) > lfc & FDR < fdr)
phenotype = read.delim('../1.preprocessing/TCGA_phenotype.txt',header=T,row.names=1)
colnames(phenotype) = 'type'
cluster1=grep('tumor',phenotype$type)
cluster2=grep('normal',phenotype$type)
data = fpkm[rownames(degsig),c(cluster1,cluster2)]
heatmap = pheatmap(data,width=10,cluster_cols=F,cluster_rows=T,show_colnames=F,show_rownames=F,annotation_col=phenotype,filename='DEG_heatmap.pdf')

library(ggplot2)

plot_volcano = function(de) {
	# de = read.table('DEgene_infiltration.txt',header=T,sep='\t',stringsAsFactors=F)#read data about differential expression
	#set threshold
	Pthrshd = fdr
	LFCthrshd = lfc
	de$threshold = as.factor(ifelse(de$FDR < Pthrshd & abs(de$logFC) > LFCthrshd,
		ifelse(de$logFC > LFCthrshd ,'Up','Down'),'Not'))#add label for genes with your threshold
		ggplot(data=de,aes(x=logFC, y =-log10(FDR),colour=threshold,fill=threshold)) +
		scale_color_manual(values=c("blue", "grey","red")) +
		geom_point(alpha=0.4, size=1.2) +
		theme_bw() + #set background color is white, otherwise is grey 
		geom_vline(xintercept=c(-1*LFCthrshd,LFCthrshd),lty=4,col="grey",lwd=0.6) +#vertical line
		geom_hline(yintercept = -log10(Pthrshd),lty=4,col="grey",lwd=0.6) +#horizontal line
		theme(legend.position="right",panel.grid=element_blank(),legend.title = element_blank(),plot.title = element_text(hjust = 0.5)) +#legend
		labs(x="log2(FoldChange)",y="-log10(FDR)")
}
pdf('volcano.pdf')
plot_volcano(deg)
dev.off()

