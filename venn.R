library(VennDiagram)

pdf('venn.pdf')
draw.pairwise.venn(area1=20,area2=3410,cross.area=12,cex=rep(3,3),cat.cex=3,cat.pos=c(11,180),
	category=c('SETD7 lncRNA','NR1H4 lncRNA'),lwd=rep(2,2),lty=rep(1,1),col=c('black','black'),
	fill=c('green','red'),cat.col=rep('black',2),ext.text=F)
dev.off()