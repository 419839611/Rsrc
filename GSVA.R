library(GSEABase)#getGmt
library(GSVA)
library(pheatmap)

gene_set = read.delim('~/chip/immune_metagenes.txt')[,1:2]
list = split(as.matrix(gene_set)[,1], gene_set[,2])
gsva_matrix = gsva(as.matrix(exp), list,method='ssgsea',kcdf='Gaussian',abs.ranking=TRUE)


exp = read.delim('../3.KAT_cluster/tumor.txt')
kegg = getGmt("~/chip/c2.cp.kegg.v7.2.symbols.gmt",collectionType=BroadCollection(category="c2"),geneIdType=SymbolIdentifier())
gsva_kegg = gsva(as.matrix(exp), kegg,method='gsva')
cluster = read.delim('../3.KAT_cluster/2_km.txt',header=T,row.names=1)
cluster$project = c(rep('TCGA',367),rep('GSE62254',300))
cluster$cluster = paste0('KAT_KDAC.cluster',cluster$cluster)
pvalues = apply(gsva_kegg,1,function(x){test=wilcox.test(x~cluster$cluster);return(test$p.value)})
gsva_kegg = cbind(gsva_kegg,pvalues)
write.table(gsva_kegg,'gsva_kegg.txt',sep='\t',row.names=T,col.names=T,quote=F)
use = gsva_kegg[order(gsva_kegg[,ncol(gsva_kegg)]),]
cluster1=grep('KAT_KDAC.cluster1',cluster$cluster)
cluster2=grep('KAT_KDAC.cluster2',cluster$cluster)
use = use[1:20,c(cluster1,cluster2)]
gsva_heatmap = pheatmap(use,width=30,cluster_cols=F,cluster_rows=T,show_colnames=F,annotation_col=cluster,scale='column',filename='gsva_kegg.pdf')
