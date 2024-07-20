library(ggplot2)

plot_volcano = function(de) {
	# de = read.table('DEgene_infiltration.txt',header=T,sep='\t',stringsAsFactors=F)#read data about differential expression
	#set threshold
	Pthrshd = 0.01
	LFCthrshd = 0
	de$threshold = as.factor(ifelse(de$P.Value < Pthrshd & abs(de$logFC) > LFCthrshd,
		ifelse(de$logFC > LFCthrshd ,'Up','Down'),'Not'))#add label for genes with your threshold
	ggplot(data=de,aes(x=logFC, y =-log10(P.Value),colour=threshold,fill=threshold)) +
		scale_color_manual(values=c("green", "grey","red")) +
		geom_point(alpha=0.4, size=1.2) +
		theme_bw() + #set background color is white, otherwise is grey 
		geom_vline(xintercept=c(-0,0),lty=4,col="grey",lwd=0.6) +#vertical line
		geom_hline(yintercept = -log10(0.01),lty=4,col="grey",lwd=0.6) +#horizontal line
		theme(legend.position="right",panel.grid=element_blank(),legend.title = element_blank(),plot.title = element_text(hjust = 0.5)) +#legend
		labs(x="log2 (fold change)",y="-log10 (p-value)",title="Volcano of infiltration DEG")
}
pdf('volcano.pdf')
DEall = topTable(fit2,adjust = "BH",coef = 1, number = 30000, p.value = 1,lfc=0)
plot_volcano(DEall)
dev.off()