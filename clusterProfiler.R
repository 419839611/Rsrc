library(clusterProfiler)
library(org.Hs.eg.db)

DEG_overlap = read.table('../3.immuno_infiltration/DEG_overlap.txt',header=F,stringsAsFactors=F,sep='\n')[,1]
deg = mapIds(org.Hs.eg.db, DEG_overlap, 'ENTREZID', 'SYMBOL')

targets = c('BP','CC','MF')
for (target in targets) {
	yy <- enrichGO(deg, 'org.Hs.eg.db', ont=target)
	write.table(as.data.frame(yy),paste0(target,".txt"),sep='\t',row.names =F,quote=F)
	pdf(paste0(target,'.pdf'),height=10,width=10)
	dotplot(yy,showCategory=20,title=paste0("Top20 of GO_",target," enrichment"))
	dev.off()
}
kegg=enrichKEGG(deg,organism = 'hsa')
write.table(as.data.frame(kegg),"KEGG.txt",sep='\t',row.names =F,quote=F)
pdf('KEGG.pdf',height=10,width=10)
dotplot(kegg,showCategory=20,title="Top20 of KEGG enrichment")
dev.off()

library(DOSE)

do = enrichDO(deg,ont = "DO")
write.table(as.data.frame(do),"DO.txt",sep='\t',row.names =F,quote=F)
pdf('DO.pdf',height=10,width=10)
dotplot(do,showCategory=20,title="DO enrichment")
dev.off()